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
module system
    use strings
    use molecules
    use groups
    implicit none
    integer system_nat, system_nmol, system_nmoltyp,keytrj
    integer :: nstartread=0, nstopread=1000000000, every=1, configuration=0 ! npbc moved to molecules
    real(dblpr) system_volume
    real(dblpr) :: rmax=huge(1.0d0) ! it will be reset to min(rmax, other stuff) during calculations
    logical :: smooth   =.FALSE.    ! smooth g(r)
    logical :: omades   =.FALSE.    ! breakdown molecules into groups of atoms
    logical :: totrdf   =.FALSE.    ! compute total instead of intermolecular r.d.f.  
    type (MOLECULE_T), allocatable :: molecule(:)
    private system_nat, system_nmol, system_nmoltyp, system_volume
    private every, nstartread, nstopread, keytrj, configuration, smooth, rmax, totrdf
    public  molecule
contains
! setters
    subroutine system_set_nat(n)
        implicit none
        integer n
        system_nat=n
        return
    end subroutine system_set_nat
!---    
    subroutine system_set_nmol(n)
        implicit none
        integer n
        system_nmol=n
        return
    end subroutine system_set_nmol
!---    
    subroutine system_set_nmoltyp(n)
        implicit none
        integer n
        system_nmoltyp=n
        return
    end subroutine system_set_nmoltyp
!---    
    subroutine system_set_nstart(n)
        implicit none
        integer n
        nstartread=n
        return
    end subroutine system_set_nstart
!---    
    subroutine system_set_nstop(n)
        implicit none
        integer n
        nstopread=n
        return
    end subroutine system_set_nstop
!---    
    subroutine system_set_every(n)
        implicit none
        integer n
        every=n
        return
    end subroutine system_set_every
!---    
    subroutine system_calc_volume(cell)
        real(dblpr) cell(3,3),volume
        external volume    
        system_volume=volume(cell)
        return
    end subroutine system_calc_volume
!---    
    subroutine system_set_groups(g)
        implicit none
        logical g
        omades=g
        return
    end subroutine system_set_groups
!---    
    subroutine system_set_total(t)
        implicit none
        logical t
        totrdf=t
        return
    end subroutine system_set_total
!---    
    subroutine system_set_smooth(s)
        implicit none
        logical s
        smooth=s
        return
    end subroutine system_set_smooth

! getters 
    integer function system_get_nat()
        implicit none
        system_get_nat=system_nat
        return
    end function system_get_nat
!---    
    integer function system_get_nmol()
        implicit none
        system_get_nmol=system_nmol
        return
    end function system_get_nmol
!---    
    integer function system_get_nmoltyp()
        implicit none
        system_get_nmoltyp=system_nmoltyp
        return
    end function system_get_nmoltyp
!---    
    integer function system_get_nconf()
        implicit none
        system_get_nconf=configuration
        return
    end function system_get_nconf
!---    
    integer function system_get_nstart()
        implicit none
        system_get_nstart=nstartread
        return
    end function system_get_nstart
!---    
    integer function system_get_nstop()
        implicit none
        system_get_nstop=nstopread
        return
    end function system_get_nstop
!---    
    integer function system_get_every()
        implicit none
        system_get_every=every
        return
    end function system_get_every
!---    
    real(dblpr) function system_get_volume()
        implicit none
        system_get_volume=system_volume
        return
    end function system_get_volume
!---    
    logical function system_get_groups()
        implicit none
        system_get_groups=omades
        return
    end function system_get_groups
!---    
    logical function system_get_total()
        implicit none
        system_get_total=totrdf
        return
    end function system_get_total
!---    
    logical function system_get_smooth()
        implicit none
        system_get_smooth=smooth
        return
    end function system_get_smooth
!---    
    real(dblpr) function system_get_rmax()
        implicit none
        system_get_rmax=rmax
        return
    end function system_get_rmax
        
! setup members 
    subroutine system_setup(io_fld,io_ctl)
        implicit none
        integer io_fld
        integer,optional :: io_ctl
        integer i, j, nmol, moltyp, n, natmol, p
        character(8) kwd
        real(dblpr) mass
        logical molecular_description,ok
        
        ! First pass: read nr of molecules and nr of molecule types and allocate molecules
        if(.NOT.get_record(io_fld)) then ! skip the label; may contain substrings resembling the keywords
            write(mystdout,'(/" Empty FIELD file?"/)')            
            STOP
        endif
        nmol=0
        molecular_description=.FALSE.
        do  ! read line from FIELD file and look for specific keywords
            if(.NOT.get_record(io_fld)) &
                exit
            p=find_keyword_value('MOLECU',record)
            if(.NOT. molecular_description .AND. p>0) then ! read number of types of molecules
                molecular_description=.TRUE.
                read(record(p:RECLEN),*) n
                CALL system_set_nmoltyp(n) 
            endif 
            p=find_keyword_value('NUMMOL', record)
            if(p>0) then ! read number of molecules per molecule type
                read(record(p:RECLEN),*) n
                nmol = nmol + n
            endif
        enddo 
        CALL system_set_nmol(nmol)
        allocate(molecule(system_get_nmol()))
        rewind(io_fld) 
        
        ! Second pass: read nr of atoms and allocate molecule(:)%atoms; also assign molecule types
        if(get_record(io_fld)) & ! skip the label
            continue 
        nmol=0
        moltyp=0
        do ! once again, scan the file and check for specific kwds
            if(.NOT.get_record(io_fld)) &
                exit 
            p=find_keyword_value('NUMMOL', record)
            if(p>0) then ! new molecule type
                moltyp = moltyp + 1
                ! re-read number of molecules per type 
                read(record(p:RECLEN),*) n
                nmol = nmol + n
            endif
           
            ! set-up atoms
            p=find_keyword_value('ATOMS', record)
            if(p>0) then 
                read(record(p:RECLEN ),*) natmol ! number of atoms per molecule
                do i=nmol-n+1, nmol              ! depending on molecule's type
                    CALL molecule_set_type(moltyp,molecule(i))
                    CALL molecule_set_nat(natmol, molecule(i))
#ifndef FORTRAN95                    
                    CALL molecule_alloc_atoms(molecule(i))
#endif                    
                    CALL system_set_nat(system_get_nat()+natmol) ! accumulate total number of atoms in the system  
                enddo
            endif            
        enddo 
        rewind(io_fld)
        
        ! Third pass: read atom records and assign each atom its properties 
        if(get_record(io_fld)) & ! skip the label
            continue 
        nmol=0
        do 
            if(.NOT.get_record(io_fld)) &
                exit
            p=find_keyword_value('NUMMOL', record)
            if(p>0) then ! new molecule type
                ! re-read number of molecules per type; I ll use it when reading atom records
                read(record(p:RECLEN),*) n
                nmol = nmol + n
            endif
            ! Now, let's read atom names, masses, charges...
            if(.NOT.read_atom_records(io_fld, n, nmol)) &
                exit
        enddo
        rewind(io_fld) ! maybe will be used by groups, below
        
        ! Finally, set molecular masses
        do i=1,nmol
            n=molecule_get_nat(molecule(i))
            mass=0.0d0
            do j=1,n
                mass=mass+atom_get_mass(molecule(i)%atom(j))
            enddo
            CALL molecule_set_mass(mass,molecule(i))
        enddo
        
        ! GROUPS        
        if(system_get_groups()) then ! Use groups instead of molecules?
            ok=.TRUE.
            if(present(io_ctl)) &
                ok=group_setup(io_ctl,molecule) ! first look for group definitions in CONTROL
            if(.NOT. present(io_ctl) .OR. .NOT.ok) &
                ok=group_setup(io_fld,molecule) ! if not found, search FIELD file
            if(.NOT.ok) then
                write(mystdout,'(/T10,"Warning: No groups definitions in CONTROL or FIELD, despite groups directive")')
                write(mystdout,'(T10,"Calculating centre-of-mass radial distribution functions instead"/)') 
                nmol=system_get_nmol()
                do i=1,nmol
                    CALL molecule_set_next(i+1,molecule(i))
                enddo
                RETURN
            endif
            
            deallocate(molecule)
            allocate(molecule(size(group))) ! molecule's karma is group
            do i=1,size(group)
                CALL molecule_copy(group(i),molecule(i))
            enddo
            CALL system_set_nmol(size(group))
            n=0
            do i=1,size(group) ! loop to find the highest
                n=max(n,molecule_get_type(molecule(i)))
            enddo
            CALL system_set_nmoltyp(n)
            deallocate(group) ! group array pronounced dead; it has been reincarnated in molecule
            if(system_get_total()) then ! calculate total R.D.F. (including all intramolecular pairs)
                nmol=system_get_nmol()   ! (overrides definition by group_setup)
                do i=1,nmol
                    CALL molecule_set_next(i+1,molecule(i))
                enddo
            endif
        else ! calculate total R.D.F. 
            nmol=system_get_nmol()
            do i=1,nmol
                CALL molecule_set_next(i+1,molecule(i))
            enddo
        endif
        
        return
    end subroutine system_setup
!---
    logical function read_atom_records(io, imol, imoltot)
        implicit none
        integer io, ierr, imol, imoltot
        integer i, irepeat, j, p
        character(CHARLEN) atnam
        real(dblpr) mass, charge
        integer, save :: n, natmol
        logical, save :: read_atoms=.FALSE.
        
        read_atom_records = .TRUE.
        p=find_keyword_value('ATOMS', record)
        if(p>0) then ! starts ATOMS section  
            read(record(p:RECLEN),*) natmol
            read_atoms = .TRUE.            
            if(.NOT.get_record(io)) then ! must read next line to skip present record and proceed
                read_atom_records = .FALSE. ! if reached EOF exit
                return
            endif
            n = 0 ! will count atom records
        endif
        
        if(strstr('FINISH',record)>0) then ! next line read contains the 'finish' keyword...
            if(n /= natmol) then ! final check
                write(mystdout,'(/" Molecules:",I5,"-",I5)') imoltot-imol+1,imoltot
                write(mystdout,'(" ERROR: nr of atom records, ",I5," differs from natmol, ",I5/)') n, natmol
                STOP
            endif
            read_atoms = .FALSE. ! ... so one more molecule type section has been read
            return
        endif

        if(read_atoms) then ! reads ATOMS section
            read(record,*,iostat=ierr) atnam, mass, charge, irepeat 
            if(ierr/=0) then ! maybe irepeat was missing; give it another try
                read(record,*,iostat=ierr) atnam, mass, charge
                irepeat=1
            endif
            if(ierr/=0) then
                write(mystdout,'(/" Molecules: failed to read atom records from FIELD file")')
                write(mystdout,'(" Last record read:")')
                write(mystdout,'(A/)') record
                STOP
            endif
            n = n+irepeat
            if(n>natmol) then
                write(mystdout,'(/" Molecules:",I5,"-",I5)') imoltot-imol+1,imoltot
                write(mystdout,'(" ERROR: nr of atom records, ",I5," exceeding natmol, ",I5/)') n, natmol
                STOP
            endif
            do i=imoltot-imol+1, imoltot
                do j=n-irepeat+1,n
                    CALL atom_set_mass(mass, molecule(i)%atom(j))
                    CALL atom_set_charge(charge,molecule(i)%atom(j))
                enddo
            enddo
            if(n==natmol) then
                read_atoms=.FALSE.
                return ! skip any other irrelevant records before FINISH keyword
            endif
        endif
        return
    end function read_atom_records    
!---
! Read and process trajectory 
    subroutine read_trj_header(io_history,io_config)
        implicit none
        integer, optional :: io_config
        integer io_history,i,n
        real(dblpr) a, b, c
#ifdef STREAMS
        integer j,nat
        if(.NOT.get_stream(io_history)) then ! end of file
            write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
            STOP
        endif
        ! dirty trick: infer useful data from first configuration and jump right to the next one with read_trj_step
        ! a small sacrifice to make it work
        if(strstr("timestep",record)>0) then ! label and keytrj records missing; infer keytrj
            do i=1,4 ! skip the cell tensor lines and the first atom label 
                if(.NOT.get_stream(io_history)) then ! end of file
                    write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                    STOP
                endif
            enddo
            keytrj=-1
            do ! read the first atom records (interrupt with error in reading three doubles) 
                if(.NOT.get_stream(io_history)) then ! end of file
                    write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                    STOP
                endif
                read(record,*,err=1) a, b, c
                keytrj=keytrj+1
                exit
            enddo
1           nat=system_get_nat()            
            do i=2,nat ! read rest of the atoms; skip this configuration because I cannot rewind (usually, the loss will be negligible)  
                do j=1,keytrj+2
                    if(.NOT.get_stream(io_history)) then 
                        write(mystdout,'(/" configuration ",I10," atom ",I10)') 1,i
                        write(mystdout,'(" End of history file while reading atom info, record",I2/)') j  
                        STOP
                    endif
                enddo ! loop over atom records
            enddo ! loop over atoms in system
            ! Look at CONFIG for missing info (periodic conditions style) 
            if(.NOT. present(io_config)) then 
                ! CONFIG file was missing; set PBCs to default value 
                if(system_get_npbc()==NOTHING) then 
                    write(mystdout,'(T10,"Periodic boundary conditions set to parallelepiped")')
                    write(mystdout,'(T10,"To override, set PBC directive in CONTROL file and rerun"/)')
                    CALL system_set_npbc(PARALLELEPIPED)
                else 
                    write(mystdout,'(T10,"Periodic boundary conditions set to ",I2," according to directive"/)') &
                    system_get_npbc()
                endif
            else
                if(.NOT.get_record(io_config)) then ! end of file
                    write(mystdout,'(/" read_trj_header: CONFIG end-of-file. Empty file??"/)')
                    STOP
                endif
                if(.NOT.get_record(io_config)) then ! end of file
                    write(mystdout,'(/" read_trj_header: CONFIG end-of-file. Empty file??"/)')
                    STOP
                endif
                ! First line was just a label; second one contains useful information - read it and decipher it
                read(record(1:20),'(2I10)') i, n ! do not read keytrj! Just inferred above (HISTORY and CONFIG may differ in this respect)
                CALL system_set_npbc(n)  
            endif
        else ! First line was just a label; second one contains useful information - read it and decipher it            
            if(.NOT.get_stream(io_history)) then ! end of file
                write(*,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                STOP
            endif
            read(record(1:20),'(2I10)') keytrj, n ! key=0,1 or 2 (only coordinates, +velocities, +accelerations)
            CALL system_set_npbc(n) 
        endif
#else        
        if(.NOT.get_record(io_history)) then ! end of file
            write(*,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
            STOP
        endif
        if(strstr("timestep",record)>0) then ! label and keytrj records missing; infer keytrj
            do i=1,4 ! skip the cell tensor lines and the first atom label 
                if(.NOT.get_record(io_history)) then ! end of file
                    write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                    STOP
                endif
            enddo
            keytrj=-1
            do ! read the first atom records (interrupt with error in reading three doubles) 
                if(.NOT.get_record(io_history)) then ! end of file
                    write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                    STOP
                endif
                read(record,*,err=1) a, b, c
                keytrj=keytrj+1
            enddo
1           rewind(io_history) ! ok, back to the beginning so read_trj_step will start reading configurations
            ! Look at CONFIG for missing info (periodic conditions style) 
            if(.NOT. present(io_config)) then 
                ! CONFIG file was missing; set PBCs to default value 
                if(system_get_npbc()==NOTHING) then 
                    write(mystdout,'(T10,"Periodic boundary conditions set to parallelepiped")')
                    write(mystdout,'(T10,"To override, set PBC directive in CONTROL file and rerun"/)')
                    CALL system_set_npbc(PARALLELEPIPED)
                else 
                    write(mystdout,'(T10,"Periodic boundary conditions set to ",I2," according to directive"/)') &
                    system_get_npbc()
                endif
            else
                if(.NOT.get_record(io_config)) then ! end of file
                    write(mystdout,'(/" read_trj_header: CONFIG end-of-file. Empty file??"/)')
                    STOP
                endif
                if(.NOT.get_record(io_config)) then ! end of file
                    write(mystdout,'(/" read_trj_header: CONFIG end-of-file. Empty file??"/)')
                    STOP
                endif
                ! First line was just a label; second one contains useful information - read it and decipher it
                read(record(1:20),'(2I10)') i, n ! do not read keytrj! Just inferred above (HISTORY and CONFIG may differ in this respect)
                CALL system_set_npbc(n)  
            endif
        else ! First line was just a label; second one contains useful information - read it and decipher it            
            if(.NOT.get_record(io_history)) then ! end of file
                write(mystdout,'(/" read_trj_header: HISTORY end-of-file. Empty file??"/)')
                STOP
            endif
            read(record(1:20),'(2I10)') keytrj, n ! key=0,1 or 2 (only coordinates, +velocities, +accelerations)
            CALL system_set_npbc(n) 
        endif
#endif
        write(mystdout,'(//)',advance='no')
        return
    end subroutine read_trj_header
!---
    logical function read_trj_step(io)
        implicit none
        integer i, io, imol, iat,  j, nat, natmol, nmol
        real(dblpr) h1, h2, h3, rx, ry, rz, IwI, w(3), cell(3,3)
        real(dblpr) vx, vy, vz

        read_trj_step=.TRUE.
        ! First, skip the timestep record and read the cell tensor
#ifdef STREAMS
        if(.NOT.get_stream(io)) then ! EOF?
#else
        if(.NOT.get_record(io)) then ! EOF?
#endif        
            read_trj_step=.FALSE.
            return
        endif
        configuration = configuration+1 ! not EOF yet
        if(strstr("timestep",record)>0) then
            if(configuration<nstartread.OR.configuration>nstopread) then
!                write(mystdout,'("Skipping:   ",A)') record(1:18)        
                write(mystdout,'(".")',advance='no')         
            else
!                write(mystdout,'("Processing: ",A)') record(1:18)
                write(mystdout,'("*")',advance='no')         
            endif
        endif
        if(mod(configuration,100)==0) &
            write(mystdout,'(2X,I0)') configuration
        if(configuration > nstopread) &
            return ! don't read beyond nstopread
        
        ! read configuration; first, the cell tensor  
        do i=1,3
#ifdef STREAMS
            if(.NOT.get_stream(io)) then 
#else
            if(.NOT.get_record(io)) then 
#endif        
                if(i==1) then
                    write(mystdout,'(/" Configuration ",I10)') configuration-1 
                    write(mystdout,'(" Apparently, that was the end of the trajectory file."/)') 
                    return
                else if(i>1) then
                    read_trj_step=.FALSE.
                    write(mystdout,'(/" configuration ",I10)') configuration 
                    write(mystdout,'(" End of history file while reading cell vector Nr",I2/)') i 
                    return
                endif
            endif
            read(record,*) cell(i,X), cell(i,Y), cell(i,Z)
        enddo

        ! I could have enclosed this block (incl. rmax update, further below) 
        ! in an if-block dependent on 'every' frequency, but it will only save me 
        ! little computational time while adding complexity (process_setup_rdf
        ! would have to be modified accordingly), thus risking new bugs; better 
        ! keep it simple and leave it as is.          
        CALL system_calc_volume(cell)
        CALL molecule_set_cell(cell)
        
        if(system_get_npbc()==CUBIC) then
            rmax=dmin1(rmax,0.5d0*cell(1,1)*sqrt(3.0d0))
        else           
            ! use cell to re-calculate (system's) rmax
            CALL uXv(cell(2,:),cell(3,:),w)      ! w = c2 X c3
            IwI = dsqrt(dot_product(w,w))
            w   = w/IwI                          ! convert w to unit vector 
            h1  = dabs(dot_product(cell(1,:),w)) ! project c1 to w
            CALL uXv(cell(1,:),cell(3,:),w)      ! w = c1 X c3
            IwI = dsqrt(dot_product(w,w))
            w   = w/IwI                          ! convert w to unit vector 
            h2  = dabs(dot_product(cell(2,:),w)) ! project c2 to w
            CALL uXv(cell(1,:),cell(2,:),w)      ! w = c1 X c2
            IwI = dsqrt(dot_product(w,w))
            w   = w/IwI                          ! convert w to unit vector 
            h3  = dabs(dot_product(cell(3,:),w)) ! project c3 to w
        
            ! update rmax (keep smallest value so far)
            rmax= dmin1(rmax,0.5d0*dmin1(h1,h2,h3))
        endif

        ! Then, the atoms: skip labels, read positions and skip velocities and accelerations, if any 
        nmol=system_get_nmol()
        nat =system_get_nat()
        do imol=1,nmol
            natmol=molecule_get_nat(molecule(imol))
            do iat=1,natmol
                do j=1,keytrj+2
#ifdef STREAMS
                    if(.NOT.get_stream(io)) then 
#else
                    if(.NOT.get_record(io)) then 
#endif        
                        read_trj_step=.FALSE.
                        write(mystdout,'(/" configuration ",I10," atom ",I10)') configuration,(imol-1)*natmol+iat
                        write(mystdout,'(" End of history file while reading atom info, record",I2/)') j  
                        return
                    endif
                    if(j==2) then ! this is the coordinates record
                        read(record,*) rx, ry, rz
                        CALL atom_set_coor(rx, ry, rz, molecule(imol)%atom(iat))
                    endif
                enddo ! loop over atom records
            enddo ! loop over atoms in molecule 
        enddo ! loop over molecules 
        return    
    end function read_trj_step
end module system
