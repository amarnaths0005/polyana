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
program verify_unfold
    implicit none
    integer, parameter :: FIELD=9,CONFIG=10,HISTORY=11,CONTROL=12
    real(kind(1.0d0)), parameter :: MAXMASS=32.0d0,BOX=60.0d0,distance=15.0d0
    integer i,j,k,natmol
    real(kind(1.0d0)) u(3),ux(3),uy(3),uz(3),smr(3),sm,RMOL
    real(kind(1.0d0)), allocatable :: rini(:,:,:),r(:,:,:),mass(:,:)
    character(100) myarg
    
    real(kind(1.0d0)) MYRANDOM
    EXTERNAL MYRANDOM
    
    if (command_argument_count()>=2) then
        call get_command_argument(1, myarg)
        read(myarg,'(I10)') NATMOL
        call get_command_argument(2, myarg)
        read(myarg,'(F20.3)') rmol ! 'radius' of molecules
        if(4*rmol>=BOX) &
            write(*,'(/T10,"Molecules'' dimensions exceed half box length"/T10,"polyana will fail"/)') 
    else
        write(*,'(/T10,"usage: a.out [nr of atoms per molecule] [molecular radius]"/)')
        STOP
    endif
    allocate(rini(2,3,natmol),r(2,3,natmol),mass(2,natmol))
    
    ! construct molecules by randomly placing atoms in sphere of radius rmol
    do j=1,2
        do i=1,natmol
            call ranor(u)
            rini(j,1:3,i)=u(1:3)*RMOL
            mass(j,i)=MAXMASS*myrandom() 
        enddo
    enddo
    
    ! Define rmax as directive in CONTROL file
    OPEN(UNIT=CONTROL,FILE='CONTROL')
    write(CONTROL,'("polyana"/T4,"rmax 16.0"/"end polyana")')
    CLOSE(CONTROL)
    
    ! write topology in FIELD file (actually, only atom masses are required)  
    OPEN(UNIT=FIELD,FILE='FIELD')
    write(FIELD,'("Random molecules")')
    write(FIELD,'("UNITS eV"/"MOLECULES 2")')
    write(FIELD,'("random1"/"NUMMOLS ",I10/"ATOMS   ",I10)') 1,natmol
    do i=1,natmol
        write(FIELD,'(A8,2(F20.10),I10)') 'X       ',mass(1,i),0.0d0,1
    enddo
    write(FIELD,'("FINISH")')
    write(FIELD,'("random2"/"NUMMOLS ",I10/"ATOMS   ",I10)') 1,natmol
    do i=1,natmol
        write(FIELD,'(A8,2(F20.10),I10)') 'X       ',mass(2,i),0.0d0,1
    enddo
    write(FIELD,'("FINISH"/"CLOSE")')
    CLOSE(FIELD)
    
    ! calculate centre of mass and redefine atomic coordinates relative to it 
    do j=1,2    
        sm=0.
        smr(:)=0.
        do i=1,natmol
            sm=sm+mass(j,i)
            smr(:)=smr(:)+rini(j,:,i)*mass(j,i)
        enddo
        smr = smr/sm
        
        do i=1,natmol
            rini(j,:,i)=rini(j,:,i)-smr(:)
        enddo
    enddo
    
    ! generate initial configuration file
    ! only header is required but I add the rest so as to visualise it
    OPEN(UNIT=CONFIG,FILE='CONFIG')
    write(CONFIG,'("Random molecules")') 
    write(CONFIG,'(2I10)') 0, 1
    write(CONFIG,'(3F20.10)') BOX,0.,0.
    write(CONFIG,'(3F20.10)') 0.,BOX,0.
    write(CONFIG,'(3F20.10)') 0.,0.,BOX
    do j=1,2
        do k=1,natmol
            r(j,1:3,k)=rini(j,1:3,k)
            r(j,1,k)=r(j,1,k)+dble(2*j-3)*distance/2.0d0  
            r(j,1,k)=r(j,1,k)-BOX*dnint(r(j,1,k)/BOX) ! Cubic periodic boundary conditions
            r(j,2,k)=r(j,2,k)-BOX*dnint(r(j,2,k)/BOX)
            r(j,3,k)=r(j,3,k)-BOX*dnint(r(j,3,k)/BOX)
            write(CONFIG,'(A8,I10)') 'X       ',(j-1)*natmol+k
            write(CONFIG,'(3F20.10)') r(j,1:3,k)
        enddo
    enddo
    CLOSE(CONFIG)
     
    ! Trajectory (HISTORY) file: 
    ! generate configurations by randomly rotating above defined molecules 
    OPEN(UNIT=HISTORY,FILE='HISTORY')
    write(HISTORY,'("Random molecules")') 
    write(HISTORY,'(2I10)') 0, 1
    do i=1,1000
        write(HISTORY,'("timestep",I10)') i
        write(HISTORY,'(3F20.10)') BOX,0.,0.
        write(HISTORY,'(3F20.10)') 0.,BOX,0.
        write(HISTORY,'(3F20.10)') 0.,0.,BOX
        do j=1,2
            ! first, rotate local (molecular) frame 
            ! generate random x-axis, ux (unit vector) 
            call ranor(ux)
            
            ! generate random unit vector, u
            call ranor(u) 
            
            ! define z-axis, uz=ux X u
            call uXv(ux,u,uz)
            call u1v(uz,uz)
            
            ! define y-axis, uy=uz X ux
            call uXv(uz,ux,uy)
            call u1v(uy,uy)
            
            ! define new molecular orientations 
            do k=1,natmol
                r(j,1,k)=dot_product(rini(j,1:3,k),ux)
                r(j,2,k)=dot_product(rini(j,1:3,k),uy)
                r(j,3,k)=dot_product(rini(j,1:3,k),uz)
                r(j,1,k)=r(j,1,k)+dble(2*j-3)*distance/2.0d0  
                r(j,1,k)=r(j,1,k)-BOX*dnint(r(j,1,k)/BOX) ! Cubic periodic boundary conditions
                r(j,2,k)=r(j,2,k)-BOX*dnint(r(j,2,k)/BOX)
                r(j,3,k)=r(j,3,k)-BOX*dnint(r(j,3,k)/BOX)
                write(HISTORY,'(A8,I10)') 'X       ',(j-1)*natmol+k
                write(HISTORY,'(3F20.10)') r(j,1:3,k)
            enddo
        enddo
    enddo
    CLOSE(HISTORY)    
    deallocate(rini,r,mass)
end program verify_unfold    

! random unit vector 
! References: 
! 1. Allen & Tildesley: 'Molecular Simulation of Liquids', Clarendon Press, Oxford, 1989
! 2. Frenkel & Smit: 'Understanding Molecular Simulation', Academic Press, 1996
subroutine ranor(v)
    implicit none
    
    real(kind(1.0d0)) v(3)
    real(kind(1.0d0)) ran1, ran2, ranh, ransq, myrandom

    external myrandom
    
    ransq = 2.0d0
    do while(ransq >= 1.0d0) 
        ran1 = 1.0d0 - 2.0d0 * myrandom() 
        ran2 = 1.0d0 - 2.0d0 * myrandom() 
        ransq = ran1*ran1 + ran2*ran2
    enddo
    ranh = 2.0d0*dsqrt(1.0d0-ransq)
    v(1) = ran1*ranh
    v(2) = ran2*ranh
    v(3) = 1.0d0 - 2.0d0*ransq
    
    return
end subroutine ranor

real(kind(1.0d0)) function myrandom()
    implicit none
    real r
    CALL random_number(r)
    myrandom=dble(r)
    return
end function myrandom

subroutine uXv(u,v,w)
    real(kind(1.0d0)) u(3), v(3), w(3)
        
    w(1) = u(2)*v(3)-u(3)*v(2)
    w(2) =-u(1)*v(3)+u(3)*v(1)
    w(3) = u(1)*v(2)-u(2)*v(1)
    return
end subroutine uXv

subroutine u1v(u,v)
    real(kind(1.0d0)) u(3),v(3)
    if(dabs(u(1))+dabs(u(2))+dabs(u(3)) < 1d-10) then
        print*, 'zero unit vector in u1v. Aborting...'
        STOP
    endif
    v=u/dsqrt(dot_product(u,u))
    return
end subroutine u1v
