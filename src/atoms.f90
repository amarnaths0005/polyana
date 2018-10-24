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
module atoms
    use parameters
    implicit none
    type ATOM_T
        !private
        character(CHARLEN) nam
        integer indx, typ, parent
        real(dblpr)  x, y, z, mass, charge
        real(dblpr)  vx,vy,vz
    end type     
contains
! public member functions 
! setters 
    subroutine atom_set_charge(c, at)
        implicit none
        real(dblpr) c
        type (ATOM_T) at        
        at%charge=c
        return
    end subroutine atom_set_charge
!---    
    subroutine atom_set_coor(rx, ry, rz, at)
        implicit none
        real(dblpr) rx, ry, rz
        type (ATOM_T) at
        at%x=rx
        at%y=ry
        at%z=rz
        return
    end subroutine atom_set_coor
!---    
    subroutine atom_set_vel(vx, vy, vz, at)
        implicit none
        real(dblpr) vx, vy, vz
        type (ATOM_T) at
        at%vx=vx
        at%vy=vy
        at%vz=vz
        return
    end subroutine atom_set_vel
!---    
    subroutine atom_set_mass(m, at)
        implicit none
        real(dblpr) m
        type (ATOM_T) at        
        at%mass=m
        return
    end subroutine atom_set_mass
! getters
    real(dblpr) function atom_get_charge(at)
        implicit none
        type (ATOM_T) at        
        atom_get_charge=at%charge
        return
    end function atom_get_charge
!---    
    subroutine atom_get_coor(at, rx, ry, rz)
        implicit none
        real(dblpr) rx, ry, rz
        type (ATOM_T) at
        rx=at%x
        ry=at%y
        rz=at%z
        return
    end subroutine atom_get_coor
!---    
    subroutine atom_get_vel(at, vx, vy, vz)
        implicit none
        real(dblpr) vx, vy, vz
        type (ATOM_T) at
        vx=at%vx
        vy=at%vy
        vz=at%vz
        return
    end subroutine atom_get_vel
!---
    real(dblpr) function atom_get_x(at)
        implicit none
        type (ATOM_T) at
        atom_get_x=at%x
        return
    end function atom_get_x
!---
    real(dblpr) function atom_get_y(at)
        implicit none
        type (ATOM_T) at
        atom_get_y=at%y
        return
    end function atom_get_y
!---
    real(dblpr) function atom_get_z(at)
        implicit none
        type (ATOM_T) at
        atom_get_z=at%z
        return
    end function atom_get_z
!---
    real(dblpr) function atom_get_vx(at)
        implicit none
        type (ATOM_T) at
        atom_get_vx=at%vx
        return
    end function atom_get_vx
!---
    real(dblpr) function atom_get_vy(at)
        implicit none
        type (ATOM_T) at
        atom_get_vy=at%vy
        return
    end function atom_get_vy
!---
    real(dblpr) function atom_get_vz(at)
        implicit none
        type (ATOM_T) at
        atom_get_vz=at%vz
        return
    end function atom_get_vz
!---    
    real(dblpr) function atom_get_mass(at)
        implicit none
        type (ATOM_T) at        
        atom_get_mass=at%mass
        return
    end function atom_get_mass
! calculators
    subroutine atom_pair_vec(a,b,c,v)
        implicit none
        type (ATOM_T) a,b
        real(dblpr) c(3,3),v(3)
        real(dblpr) dx,dy,dz       
        dx=b%x-a%x
        v(X)=dx-dnint(dx/c(X,X))*c(X,X)
        dy=b%y-a%y
        v(Y)=dy-dnint(dy/c(Y,Y))*c(Y,Y)
        dz=b%z-a%z
        v(Z)=dz-dnint(dz/c(Z,Z))*c(Z,Z)
        return
    end subroutine atom_pair_vec
!---    
    real(dblpr) function atom_pair_dist(a,b,c)
        implicit none
        type (ATOM_T) a,b
        real(dblpr) c(3,3)
        real(dblpr) dx,dy,dz       
        dx=b%x-a%x
        dx=dx-dnint(dx/c(X,X))*c(X,X)
        dy=b%y-a%y
        dy=dy-dnint(dy/c(Y,Y))*c(Y,Y)
        dz=b%z-a%z
        dz=dz-dnint(dz/c(Z,Z))*c(Z,Z)
        atom_pair_dist=dsqrt(dx*dx+dy*dy+dz*dz)
        return
    end function atom_pair_dist
end module atoms    
