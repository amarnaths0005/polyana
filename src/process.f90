!
! Copyright (c) 2015-2018 Vasilios E. Raptis <polyana.software@gmail.com>
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!
!-----------------------------------------------------------------------
!
module process
    use omp_lib
    use system
    implicit none
    integer nr,ngrid,nthreads,leg_moltyp,leg_at1,leg_at2
    real(dblpr) rmax, drtbl
    real(dblpr), allocatable :: rho1(:), hist(:,:,:),pdf(:,:,:)
    real(dblpr), allocatable :: shell_vol(:), ball_vol(:)
    ! some default values 
    real(dblpr) :: dr = 0.1d0            ! a 'small enough' (r,r+dr) thickness
    logical :: rmax_user_defined=.FALSE. ! flag to control rmax
    private dr, hist, nr, rho1, rmax, pdf, rmax_user_defined
contains
    subroutine read_polyana_directives(io)
        implicit none
        integer io,p,nstart,nstop,n
        real(dblpr) t,rcut
        logical, save :: read_polyana=.FALSE.
        logical :: read_rcut=.FALSE.,read_rvdw=.FALSE.
        
        CALL system_set_npbc(NOTHING)
        do ! read lines from io file until EOF, and interpret them according to keywords contained therein
            if(.NOT.get_record(io)) &
                exit
            if(strstr("#",record)>0) &
                record(strstr("#",record):RECLEN)=" " ! remove comments
            p = strstr('polyana', record)
            if(p>0.AND.strstr('end',record)==0) then
                read_polyana=.TRUE.
            else if(p>0.AND.strstr('end',record)>0) then
                read_polyana=.FALSE.
            endif
            if(read_polyana) then
                p = find_keyword_value('start', record)
                if(p>0) then
                    read(record(p:RECLEN),*) nstart
                    CALL system_set_nstart(nstart)
                endif
                p = find_keyword_value('stop', record)
                if(p>0) then 
                    read(record(p:RECLEN),*) nstop
                    CALL system_set_nstop(nstop)
                endif
                p = find_keyword_value('every', record)
                if(p>0) then 
                    read(record(p:RECLEN),*) n
                    CALL system_set_every(n)
                endif
                p = find_keyword_value('rmax', record)
                if(p>0) then 
                    rmax_user_defined=.TRUE.
                    read(record(p:RECLEN),*) rmax
                endif
                p = find_keyword_value('rcut', record) ! a synonym for rmax
                if(p>0) then 
                    rmax_user_defined=.TRUE.
                    read(record(p:RECLEN),*) rmax
                endif
                p = find_keyword_value('width',record)
                if(p>0) &
                    read(record(p:RECLEN),*) dr
                p = find_keyword_value('dr',record) ! a synonym for width
                if(p>0) &
                    read(record(p:RECLEN),*) dr
                p = strstr('group',record) 
                if(p>0) then
                    CALL system_set_groups(.TRUE.)  
                    p = strstr('total',record)
                    if(p>0) &
                        CALL system_set_total(.TRUE.)
                endif
                p = strstr('smooth',record) 
                if(p>0) &
                    CALL system_set_smooth(.TRUE.)  
                p = find_keyword_value('pbc',record) ! will be overriden by DL_POLY input files
                if(p>0) then                        ! unless CONFIG is missing and HISTORY header is missing
                    read(record(p:RECLEN),*) n
                    CALL system_set_npbc(n)
                endif  
                p = find_keyword_value('omp',record) ! OpenMP: nr of threads
                if(p>0) then                        
                    read(record(p:RECLEN),*) nthreads
                    write(*,'(//T10,"Number of threads: ",I3)') nthreads
                endif
                p = find_keyword_value('threads',record) ! a synonym for omp
                if(p>0) then                        
                    read(record(p:RECLEN),*) nthreads
                    write(*,'(//T10,"Number of threads: ",I3)') nthreads
                endif
            endif
        enddo
        return
    end subroutine read_polyana_directives
!---
    subroutine process_setup_rdf() 
        implicit none
        integer n
        if(.NOT. rmax_user_defined) &
            rmax = system_get_rmax() ! update rmax according to cell read by system_read_trj
        if(.NOT. allocated(hist)) then  
            n = system_get_nmoltyp()
            nr = nint(rmax/dr)+1            
            allocate(hist(n,n,nr))
            allocate(pdf(n,n,nr))
            allocate(rho1(n))
            hist  = 0.0d0    
            pdf   = 0.0d0
            rho1  = 0.0d0
        endif
        CALL process_calc_shells()
    end subroutine process_setup_rdf
!---
    logical function process_calc_rdf(operation)
        implicit none
        integer n, nmol, a, b, i, j, nt, nconf, k
        integer msd_alloc_dim,vel_alloc_dim,leg_alloc_dim
        real(dblpr) r
        real(dblpr) v(3), c(3,3), ic(3,3), detc
        real(dblpr), save :: volave=0.0d0
        integer operation
        ! functions 
        real(dblpr) volume, shell_volume
        external volume, shell_volume
        integer nm
        real(dblpr), allocatable :: priv_hist(:,:,:)
        integer, external :: omp_get_thread_num, omp_get_max_threads
        process_calc_rdf=.TRUE.
        if(mod(system_get_nconf(),system_get_every())/=0) &
            return ! only process every 'every' steps  
            
        nmol=system_get_nmol()
        if(operation==COLLECT) then
            if(system_get_nconf()<system_get_nstart()) &
                return
            if(system_get_nconf()>system_get_nstop()) then
                process_calc_rdf=.FALSE.
                return
            endif
            ! prepare
            CALL molecule_get_cell(c,ic,detc)
            CALL system_calc_volume(c)
            do i=1,nmol
                CALL molecule_unfold(molecule(i))
                CALL molecule_calc_cm(molecule(i))
            enddo
            ! process
            volave=volave+system_get_volume()
            allocate(priv_hist(size(hist(:,1,1)),size(hist(1,:,1)),size(hist(1,1,:))))
            CALL omp_set_num_threads(nthreads)
            !write(*,'(//T10,"Number of threads: ",I3)') omp_get_max_threads()
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,J,A,B,R,N,NM,PRIV_HIST)  
            priv_hist=0.0d0
            nm=nmol
            !write(*,'(T10,"This is thread Nr: ",I3)') omp_get_thread_num()
!$OMP DO COLLAPSE(1) SCHEDULE(RUNTIME)
            do i=1,nm 
                a=molecule_get_type(molecule(i))
                do j=molecule_get_next(molecule(i)),nm ! i+1,nm ! MAYBE NOT COLLAPSE WHEN USING GET_NEXT
                    r = molecule_pair_distance(molecule(i),molecule(j))
                    if(r<rmax) then
                        b=molecule_get_type(molecule(j))
                        n=nint(r/dr)
                        priv_hist(a,b,n)=priv_hist(a,b,n)+1.0d0
                        priv_hist(b,a,n)=priv_hist(b,a,n)+1.0d0
                    endif
                enddo
            enddo
!$OMP END DO 
!$OMP CRITICAL
            do i=1,size(hist(:,1,1))
                do j=1,size(hist(1,:,1))
                    do k=1,size(hist(1,1,:))
                        hist(i,j,k)=hist(i,j,k)+priv_hist(i,j,k)
                    enddo
                enddo
            enddo
!$OMP END CRITICAL
!$OMP END PARALLEL  
            deallocate(priv_hist)
        else if(operation==AVERAGE) then ! it's the final countdown
            write(*,'(///)',advance='no')
            nconf=min(system_get_nstop(),system_get_nconf())
            nconf=nconf-system_get_nstart()+1
            if(nconf<=0) then
                write(*,'(/" Nr of configurations <= 0 (",I12,")"/" Check your bounds! "/)') nconf
                process_calc_rdf=.FALSE.
                return
            endif
            hist=hist/(dble(nconf)/dble(system_get_every())) ! average over configurations
            ! average cell volume
            volave=volave/(dble(nconf)/dble(system_get_every()))
            do i=1,nmol
                a=molecule_get_type(molecule(i))
                rho1(a)=rho1(a)+1.0d0
            enddo
            nt=system_get_nmoltyp()
            do a=1,nt
                hist(a,:,:)=hist(a,:,:)/rho1(a) ! average over a-sites
            enddo
            pdf = hist ! pdf will store rdf's; hist stores numbers of sites
            rho1=rho1/volave ! ideal gas densities 
            r=dr
            do while(r<rmax) 
                n=nint(r/dr)
                do a=1,nt
                    do b=1,nt
                        pdf(a,b,n)=pdf(a,b,n)/shell_vol(n) ! shell_volume(r-0.5d0*dr,r+0.5d0*dr) ! convert to density
                        pdf(a,b,n)=pdf(a,b,n)/rho1(b) ! normalise to i.g. density
                    enddo
                enddo
                r=r+dr
            enddo
        endif
        return                    
    end function process_calc_rdf
!---
    subroutine process_print_rdf(io)
        implicit none
        integer io,i,a,b,n,nt,nloopstart
        real(dblpr) r
        real(dblpr), allocatable :: tmpgr(:)
        ! sum up histogram to compute population of species 'b' around 
        ! species 'a' within a given radius
        if(io==POP) then
            n=size(hist(1,1,:))
            do i=2,n
                hist(:,:,i)=hist(:,:,i)+hist(:,:,i-1)
            enddo
        endif 
        ! Smooth rdf's if so requested by user 
        ! Calculations after Allen & Tildesley 'Computer Simulation of Liquids', Clarendon Press/Oxford, 1989, pp. 203-204
        if(io==RDF.AND.system_get_smooth()) then
            nt=system_get_nmoltyp()
            n=nint((rmax-dr)/dr)
            allocate(tmpgr(n))
            do a=1,nt
                do b=a,nt
                    tmpgr(1  )=(69.0d0*pdf(a,b,1  ) +  4.0d0*pdf(a,b,2  ) -  6.0d0*pdf(a,b,3  ) &
                              +  4.0d0*pdf(a,b,4  ) -        pdf(a,b,5  ))/ 70.0d0              
                    tmpgr(2  )=( 2.0d0*pdf(a,b,1  ) + 27.0d0*pdf(a,b,2  ) + 12.0d0*pdf(a,b,3  ) &
                              -  8.0d0*pdf(a,b,4  ) +  2.0d0*pdf(a,b,5  ))/ 35.0d0              
                    do i=3,n-2
                        tmpgr(i)=(-3.0d0*pdf(a,b,i-2) + 12.0d0*pdf(a,b,i-1) + 17.0d0*pdf(a,b,i) &
                                + 12.0d0*pdf(a,b,i+1) -  3.0d0*pdf(a,b,i+2))/ 35.0d0              
                    enddo
                    tmpgr(n-1)=( 2.0d0*pdf(a,b,n  ) + 27.0d0*pdf(a,b,n-1) + 12.0d0*pdf(a,b,n-2) &
                              -  8.0d0*pdf(a,b,n-3) +  2.0d0*pdf(a,b,n-4))/ 35.0d0              
                    tmpgr(n  )=(69.0d0*pdf(a,b,n  ) +  4.0d0*pdf(a,b,n-1) -  6.0d0*pdf(a,b,n-2) &
                              +  4.0d0*pdf(a,b,n-3) -        pdf(a,b,n-4))/ 70.0d0            
                    pdf(a,b,1:n)=tmpgr(1:n)  
                enddo
            enddo
            deallocate(tmpgr)  
        endif
        ! print results 
        write(io,'("#",A9)',advance="no") ' '
        write(io,'(3X)',advance="no")
        nt=system_get_nmoltyp()
        do a=1,nt
            write(io,'(4X,I1,"-",I1,3X)',advance="no") a, a
        enddo
        write(io,'(3X)',advance="no")
        nloopstart=1
        do a=1,nt
            if(io==RDF) &
                nloopstart=a+1
            do b=nloopstart,nt
                if(a/=b) &
                    write(io,'(4X,I1,"-",I1,3X)',advance="no") a, b
            enddo
        enddo
        write(io,'("")') 
        r=dr
        do while(r<rmax-dr) 
            write(io,'(F9.3)',advance="no") r
            write(io,'(3X)',advance="no")
            n=nint(r/dr)
            do a=1,nt
                if(io==RDF) then ! radial distribution functions
                    write(io,'(1X,F9.3)',advance="no") pdf(a,a,n)
                else if(io==POP) then ! population of species b around species a 
                    write(io,'(1X,F9.3)',advance="no") hist(a,a,n)*(4.0d0*PI*(n*dr)**3/3.0d0) &
                                                                  /ball_vol(n)  ! correction for distances > L/2
                endif
            enddo
            write(io,'(3X)',advance="no")
            do a=1,nt
                nloopstart=1
                if(io==RDF) &
                    nloopstart=a+1
                do b=nloopstart,nt
                    if(a/=b) then
                        if(io==RDF) then ! radial distribution functions
                            write(io,'(1X,F9.3)',advance="no") pdf(a,b,n)
                        else if(io==POP) then ! population of species b around species a 
                            write(io,'(1X,F9.3)',advance="no") hist(a,b,n)*(4.0d0*PI*(n*dr)**3/3.0d0) &
                                                                          /ball_vol(n)  ! correction for distances > L/2
                        endif
                    endif
                enddo
            enddo
            write(io,'("")') 
            r=r+dr
        enddo  
        return
    end subroutine process_print_rdf
!---
    subroutine process_calc_shells()
        ! calculate list of volumes and shell volumes to be used in R.D.F calculations
        ! for cubic PBCs. The extended form of Suter and Theodorou (J Chem Phys 82(1985), 955)
        ! will be used. 
        ! I need volumes of shells defined by r-dr and r+dr for a given dr  
        implicit none
        integer i,n
        real(dblpr) c(3,3),ic(3,3),d,r_min,r_max,r_max_1,r_max_2,rhi
        CALL molecule_get_cell(c,ic,d)
        r_min=0.5d0*dr
        r_max=0.5d0*c(X,X)
        if(system_get_npbc()==CUBIC) then
            r_max_1=r_max*dsqrt(2.0d0)
            r_max_2=r_max*dsqrt(3.0d0)
        else
            r_max_1=0.0d0
            r_max_2=0.0d0
        endif
        if(allocated(shell_vol)) &
            deallocate(shell_vol)
        if(allocated(ball_vol)) &
            deallocate(ball_vol)
        n=1+max(nint(r_max/dr),nint(r_max_1/dr),nint(r_max_2/dr))
        allocate(shell_vol(n),ball_vol(n))
        do i=1,n
            rhi=i*dr+0.5d0*dr
            if(rhi>rmax) & ! which is either user-defined or set automatically based on PBCs and simulation cell
                exit
            shell_vol(i)=process_sphere_vol(i*dr+0.5d0*dr,r_max,r_max_1,r_max_2) - &
                         process_sphere_vol(i*dr-0.5d0*dr,r_max,r_max_1,r_max_2)
            ball_vol (i)=process_sphere_vol(i*dr         ,r_max,r_max_1,r_max_2)
        enddo
        return
    end subroutine process_calc_shells   
!---
    real(dblpr) function process_sphere_vol(r,r_max,r_max_1,r_max_2)
        implicit none
        real(dblpr) r,r_max,r_max_1,r_max_2
        real(dblpr) lambda,t1,t2,t3,v
        if(r<r_max) then
            v=4.0d0*PI*r**3/3.0d0
        else if(r<r_max_1) then
            v=2.0d0*PI*((2.0d0/3.0d0)*r**3-(r-r_max)**2*(r+r+r_max))
        else if(r<r_max_2) then
            lambda=r/r_max
            t1=dsqrt(lambda**2-2.0d0)
            t2=   2.0d0*dasin(1.0d0/dsqrt(lambda**2-1.0d0))
            t2=t2-dasin(dsqrt((lambda**2-2.0d0)/(lambda**2-1.0d0)))
            t2=t2-PI/4.0d0
            t2=t2*(lambda**2-1.0d0/3.0d0)
            t3=   dasin((lambda-dsqrt(lambda**2-2.0d0))/(2.0d0*dsqrt(lambda**2-1.0d0)))
            t3=t3-dasin((lambda+dsqrt(lambda**2-2.0d0))/(2.0d0*dsqrt(lambda**2-1.0d0)))
            t3=t3-dasin((lambda**2-lambda-1.0d0)/((lambda-1.0d0)*dsqrt(lambda**2-1.0d0)))
            t3=t3+dasin((lambda**2+lambda-1.0d0)/((lambda+1.0d0)*dsqrt(lambda**2-1.0d0)))
            t3=t3*lambda**3/3.0d0
            v=8.0d0*r_max**3*(t1+t2-t3)
        else
            v=0.0d0
        endif
            process_sphere_vol=v
        return
    end function process_sphere_vol
end module process       
    
