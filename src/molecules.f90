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
module molecules
    use atoms
    implicit none
    type MOLECULE_T
        !private
        integer nat, typ, next ! next is to be used with groups
        real(dblpr) x, y, z, mass ! centre of mass and MW
        real(dblpr) vx,vy,vz
#ifdef FORTRAN95
        type (ATOM_T) atom(MAXATMOL)
#else        
        type (ATOM_T), allocatable :: atom(:)
#endif        
    end type     
    integer npbc ! must be hosted here because I cannot retrieve it from within system 
    real(dblpr) cell(3,3), icell(3,3), detcell
    private npbc, cell, icell, detcell
contains
! public member functions 
! setters 
    subroutine system_set_npbc(n)  ! must be hosted here because I cannot call from within system 
        implicit none
        integer n
        npbc=n
        return
    end subroutine system_set_npbc
!---
    subroutine molecule_set_mass(m, mol)
        implicit none
        real(dblpr) m
        type (MOLECULE_T) mol        
        mol%mass=m
        return
    end subroutine molecule_set_mass
!---
    subroutine molecule_set_cm(cx, cy, cz, mol)
        implicit none
        real(dblpr) cx, cy, cz
        type (MOLECULE_T) mol     
        mol%x=cx
        mol%y=cy
        mol%z=cz
        return
    end subroutine molecule_set_cm
!---
    subroutine molecule_set_vel(vx, vy, vz, mol)
        implicit none
        real(dblpr) vx, vy, vz
        type (MOLECULE_T) mol     
        mol%vx=vx
        mol%vy=vy
        mol%vz=vz
        return
    end subroutine molecule_set_vel
!---
    subroutine molecule_set_type(n, mol)
        implicit none
        integer n
        type (MOLECULE_T) mol        
        mol%typ = n
        return
    end subroutine molecule_set_type
!---
    subroutine molecule_set_nat(n, mol)
        implicit none
        integer n
        type (MOLECULE_T) mol        
        mol%nat = n
        return
    end subroutine molecule_set_nat
!---
#ifndef FORTRAN95
    subroutine molecule_alloc_atoms(mol)
        implicit none
        type (MOLECULE_T) mol   
        ! TODO: add error message if nat not set yet          
        if(.NOT. allocated(mol%atom)) &
            allocate(mol%atom(mol%nat))
        return
    end subroutine molecule_alloc_atoms
#endif    
!---
    subroutine molecule_set_cell(c)
        implicit none
        real(dblpr) c(3,3)
        cell=c
        CALL invert(c,icell,detcell)
        return
    end subroutine molecule_set_cell
!---
    subroutine molecule_set_next(n, mol)
        implicit none
        integer n
        type (MOLECULE_T) mol        
        mol%next=n
        return
    end subroutine molecule_set_next
!---
    subroutine molecule_copy(from,to)
        implicit none
        type (MOLECULE_T) from,to
        to%nat =from%nat
        to%typ =from%typ
        to%next=from%next
        to%mass=from%mass
        to%x   =from%x
        to%y   =from%y
        to%z   =from%z
#ifndef FORTRAN95
        if(.NOT. allocated(to%atom)) then
            allocate(to%atom(to%nat))
        else if(size(from%atom)/=size(to%atom)) then
            write(mystdout,'(/T10,"molecule_copy: size mismatch in atom arrays")')
            write(mystdout,'( T10,"source: ",I5,", destination: ",I5/)') &
            size(from%atom),size(to%atom)
            STOP
        endif
#endif    
        to%atom=from%atom
        return
    end subroutine molecule_copy              
! getters
    integer function system_get_npbc()  ! must be hosted here because I cannot call from within system 
        implicit none
        system_get_npbc=npbc
        return
    end function system_get_npbc
!---
    real(dblpr) function molecule_get_mass(mol)
        implicit none
        type (MOLECULE_T) mol        
        molecule_get_mass=mol%mass
        return
    end function molecule_get_mass
!---
    subroutine molecule_get_cm(mol, rx, ry, rz)
        implicit none
        real(dblpr) rx, ry, rz
        type (MOLECULE_T) mol        
        rx=mol%x
        ry=mol%y
        rz=mol%z
        return
    end subroutine molecule_get_cm
!---
    subroutine molecule_get_vel(mol, vx, vy, vz)
        implicit none
        real(dblpr) vx, vy, vz
        type (MOLECULE_T) mol        
        vx=mol%vx
        vy=mol%vy
        vz=mol%vz
        return
    end subroutine molecule_get_vel
!---
    integer function molecule_get_type(mol)
        implicit none
        type (MOLECULE_T) mol        
        molecule_get_type=mol%typ 
        return
    end function molecule_get_type
!---
    integer function molecule_get_nat(mol)
        implicit none
        type (MOLECULE_T) mol
        molecule_get_nat=mol%nat
        return
    end function molecule_get_nat
!---    
    subroutine molecule_get_cell(c,ic,d)
        implicit none
        real(dblpr) c(3,3),ic(3,3),d
        c =cell
        ic=icell
        d =detcell
        return
    end subroutine molecule_get_cell
!---
    integer function molecule_get_next(mol)
        implicit none
        type (MOLECULE_T) mol        
        molecule_get_next=mol%next
        return
    end function molecule_get_next
    
! operations
    subroutine molecule_unfold(mol)
        implicit none
        integer i, nat, npbc
        type (MOLECULE_T) mol
        real(dblpr) c(3,3),ic(3,3),rc(3,3),det
        real(dblpr) b(3),d(3),u(3),v(3)
        real(dblpr) r,s,sx,sy,xs,ys
        real(dblpr) :: small=1.d-8
        logical folded
        nat = molecule_get_nat(mol)
        npbc= system_get_npbc()
        if(nat==1) &
            return ! nothing to unfold
        CALL molecule_get_cell(c,ic,det)
        folded=.TRUE.
        do while(folded)
            folded=.FALSE.
            do i=2,nat
                CALL atom_get_coor(mol%atom(i-1),u(X),u(Y),u(Z))
                CALL atom_get_coor(mol%atom(i  ),v(X),v(Y),v(Z))
                if(npbc==NOPBC) then
                    b=v-u
                    d=b
                else if(npbc==CUBIC.OR.npbc==ORTHORHOMBIC.OR.npbc==PARALLELEPIPED) then
                    CALL mtv(ic,u,u) ! define reduced coordinates 
                    CALL mtv(ic,v,v)
                    b=v-u
                    r=dot_product(b,b)
                    d=b-dnint(b)
                    s=dot_product(d,d)
                    if(dabs(s-r)>small) then ! effect of PBCs 
                        folded=.TRUE.
                        v=u+d
                        CALL mtv(c,v,v) ! recalculate real coordinates
                        CALL atom_set_coor(v(X),v(Y),v(Z),mol%atom(i))
                    endif
                else if(npbc==SLAB) then
                    det = c(1,1)*c(2,2) - c(1,2)*c(2,1)
                    if(dabs(det) < 1.d-2) then
                        write(mystdout,'(/" Incorrect slab boundary conditions!"/)')
                        STOP
                    endif 
                    b=v-u
                    r=dot_product(b,b)
                    det     = 1.d0/det
                    rc(1,1) = det*c(2,2)
                    rc(1,2) =-det*c(1,2)
                    rc(2,1) =-det*c(2,1)
                    rc(2,2) = det*c(1,1)
                    sx=rc(1,1)*b(X)+rc(2,1)*b(Y)
                    sy=rc(1,2)*b(X)+rc(2,2)*b(Y)
                    xs=sx-dnint(sx)
                    ys=sy-dnint(sy)
                    d(X)=c(1,1)*xs+c(2,1)*ys
                    d(Y)=c(1,2)*xs+c(2,2)*ys
                    d(Z)=b(Z)
                    s=dot_product(d,d)
                    if(dabs(s-r)>small) then ! effect of PBCs 
                        folded=.TRUE.
                        v=u+d
                        CALL atom_set_coor(v(X),v(Y),v(Z),mol%atom(i))
                    endif
                endif                                        
            enddo
        enddo        
        return
    end subroutine molecule_unfold       
!---
    subroutine molecule_calc_cm(mol,com)
        implicit none
        real(dblpr), optional::com(3)
        integer i, nat, npbc
        real(dblpr) c(3,3),ic(3,3),rc(3,3),det 
        real(dblpr) cm(3),mat,sx,sy,xs,ys
        type (MOLECULE_T) mol
        ! First, calculate the cm using the UNFOLDED coordinates
        if(present(com)) then
            cm=com
        else
            nat = molecule_get_nat(mol)
            cm  = 0.0d0
            do i= 1,nat
                mat   = atom_get_mass(mol%atom(i))
                cm(X) = cm(X)+mat*atom_get_x(mol%atom(i))
                cm(Y) = cm(Y)+mat*atom_get_y(mol%atom(i))
                cm(Z) = cm(Z)+mat*atom_get_z(mol%atom(i))
            enddo
            cm = cm/molecule_get_mass(mol) ! mass
        endif
        ! Then check for PBCs (put it back in the box)
        CALL molecule_get_cell(c,ic,det)
        npbc=system_get_npbc()
        if(npbc==CUBIC.OR.npbc==ORTHORHOMBIC.OR.npbc==PARALLELEPIPED) then
            CALL mtv(ic,cm,cm) ! define reduced coordinates 
            cm=cm-dnint(cm)      ! apply PBCs
            CALL mtv(c,cm,cm)  ! back to real coordinates
        else if(npbc==SLAB) then
            det = c(1,1)*c(2,2) - c(1,2)*c(2,1)
            det     = 1.d0/det
            rc(1,1) = det*c(2,2)
            rc(1,2) =-det*c(1,2)
            rc(2,1) =-det*c(2,1)
            rc(2,2) = det*c(1,1)
            sx=rc(1,1)*cm(X)+rc(2,1)*cm(Y)
            sy=rc(1,2)*cm(X)+rc(2,2)*cm(Y)
            xs=sx-dnint(sx)
            ys=sy-dnint(sy)
            cm(X)=c(1,1)*xs+c(2,1)*ys
            cm(Y)=c(1,2)*xs+c(2,2)*ys
        endif 
        ! Done; set the cm 
        CALL molecule_set_cm(cm(X),cm(Y),cm(Z),mol)        
        return
    end subroutine molecule_calc_cm    
!---
    subroutine molecule_calc_vel(mol,vel)
        implicit none
        real(dblpr), optional::vel(3)
        integer i, nat
        real(dblpr) v(3),mat
        type (MOLECULE_T) mol
        ! calculate velocity based on total momentum
        if(present(vel)) then
            v=vel
        else
            nat = molecule_get_nat(mol)
            v   = 0.0d0
            do i= 1,nat
                mat   = atom_get_mass(mol%atom(i))
                v(X) = v(X)+mat*atom_get_vx(mol%atom(i))
                v(Y) = v(Y)+mat*atom_get_vy(mol%atom(i))
                v(Z) = v(Z)+mat*atom_get_vz(mol%atom(i))
            enddo
            v = v/molecule_get_mass(mol) ! mass
        endif
        ! Done; set the velocity 
        CALL molecule_set_vel(v(X),v(Y),v(Z),mol)        
        return
    end subroutine molecule_calc_vel  
!---
    real(dblpr) function molecule_pair_distance(a, b)
        implicit none
        integer npbc
        type (MOLECULE_T) a, b
        real(dblpr) c(3,3),ic(3,3),rc(3,3),r(3),u(3),v(3)
        real(dblpr) det,sx,sy,xs,ys
        CALL molecule_get_cell(c,ic,det)
        CALL molecule_get_cm(a,u(X),u(Y),u(Z))
        CALL molecule_get_cm(b,v(X),v(Y),v(Z))        
        npbc=system_get_npbc()
        if(npbc==NOPBC) then
            r=v-u
        else if(npbc==CUBIC.OR.npbc==ORTHORHOMBIC.OR.npbc==PARALLELEPIPED) then
            CALL mtv(ic,u,u) ! define reduced coordinates 
            CALL mtv(ic,v,v)
            r=v-u
            r=r-dnint(r)      ! apply PBCs
            CALL mtv(c,r,r)  ! back to real coordinates
        else if(npbc==SLAB) then
            det = c(1,1)*c(2,2) - c(1,2)*c(2,1)
            r=v-u
            det     = 1.d0/det
            rc(1,1) = det*c(2,2)
            rc(1,2) =-det*c(1,2)
            rc(2,1) =-det*c(2,1)
            rc(2,2) = det*c(1,1)
            sx=rc(1,1)*r(X)+rc(2,1)*r(Y)
            sy=rc(1,2)*r(X)+rc(2,2)*r(Y)
            xs=sx-dnint(sx)
            ys=sy-dnint(sy)
            r(X)=c(1,1)*xs+c(2,1)*ys
            r(Y)=c(1,2)*xs+c(2,2)*ys
        endif                                        
        molecule_pair_distance = dsqrt(dot_product(r,r))   
        return
    end function molecule_pair_distance     
!---
    subroutine molecule_new_cm(cx, cy, cz, mol)
        implicit none
        integer i,nat
        real(dblpr) cx, cy, cz
        real(dblpr) c(3,3),ic(3,3),rc(3,3),det,sx,sy,xs,ys,r(3)
        type (MOLECULE_T) mol     
        ! Force input com and apply PBCs to it
        CALL molecule_calc_cm(mol,(/cx,cy,cz/))                
        ! Unfold molecule (doesnt use centre of mass, therefore no risk to alter old atom positions)
        CALL molecule_unfold(mol)
        nat=molecule_get_nat(mol)
        do i=1,nat
            CALL molecule_get_cell(c,ic,det)
            npbc=system_get_npbc()
            ! calculate unfolded atom vectors wrt to NEW com;
            r=(/atom_get_x(mol%atom(i))-mol%x+cx,&
                atom_get_y(mol%atom(i))-mol%y+cy,&
                atom_get_z(mol%atom(i))-mol%z+cz/)
            ! apply PBCs to new positions;
            if(npbc==CUBIC.OR.npbc==ORTHORHOMBIC.OR.npbc==PARALLELEPIPED) then
                CALL mtv(ic,r,r) ! define reduced coordinates 
                r=r-dnint(r)      ! apply PBCs
                CALL mtv(c,r,r)  ! back to real coordinates
            else if(npbc==SLAB) then
                det = c(1,1)*c(2,2) - c(1,2)*c(2,1)
                det     = 1.d0/det
                rc(1,1) = det*c(2,2)
                rc(1,2) =-det*c(1,2)
                rc(2,1) =-det*c(2,1)
                rc(2,2) = det*c(1,1)
                sx=rc(1,1)*r(X)+rc(2,1)*r(Y)
                sy=rc(1,2)*r(X)+rc(2,2)*r(Y)
                xs=sx-dnint(sx)
                ys=sy-dnint(sy)
                r(X)=c(1,1)*xs+c(2,1)*ys
                r(Y)=c(1,2)*xs+c(2,2)*ys
            endif   
            ! set new atom coordinates accordingly 
            CALL atom_set_coor(r(X),r(Y),r(Z),mol%atom(i))                                             
        enddo
        return
    end subroutine molecule_new_cm
end module molecules
