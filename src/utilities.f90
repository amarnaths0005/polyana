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
real(kind(1.0d0)) function shell_volume(rlo,rup)
    implicit none
    real(kind(1.0d0)) rlo,rup
    real(kind(1.0d0)), parameter :: PI=3.14159265358979323846d0
    rlo=dmax1(rlo,0.0d0)
    shell_volume=(4.0d0/3.0d0)*PI*(rup**3-rlo**3)
    return    
end function shell_volume
!---
subroutine mtv(m, u, v)
    ! matrix times vector
    implicit none
    real(kind(1.0d0)) m(3,3), u(3), v(3), tmp(3)
    tmp(1) = m(1,1)*u(1) + m(2,1)*u(2) + m(3,1)*u(3)
    tmp(2) = m(1,2)*u(1) + m(2,2)*u(2) + m(3,2)*u(3)
    tmp(3) = m(1,3)*u(1) + m(2,3)*u(2) + m(3,3)*u(3)
    v=tmp
    return     
end subroutine mtv
!---
subroutine invert(a,b,d)
    ! Invert a 3X3 matrix. 
    ! An adaptation of the corresponding routine in DL_POLY 2.17 source
    ! code, developed by William Smith
    implicit none

    real(kind(1.0d0)) a(3,3),b(3,3),d

    ! calculate adjoint matrix
    b(1,1) = a(2,2)*a(3,3) - a(2,3)*a(3,2)
    b(1,2) = a(1,3)*a(3,2) - a(1,2)*a(3,3)
    b(1,3) = a(1,2)*a(2,3) - a(1,3)*a(2,2)
    b(2,1) = a(2,3)*a(3,1) - a(2,1)*a(3,3)
    b(2,2) = a(1,1)*a(3,3) - a(1,3)*a(3,1)
    b(2,3) = a(1,3)*a(2,1) - a(1,1)*a(2,3)
    b(3,1) = a(2,1)*a(3,2) - a(2,2)*a(3,1)
    b(3,2) = a(1,2)*a(3,1) - a(1,1)*a(3,2)
    b(3,3) = a(1,1)*a(2,2) - a(1,2)*a(2,1)

    ! calculate determinant and invert matrix
    d = a(1,1)*b(1,1) + a(2,1)*b(1,2) + a(3,1)*b(1,3)
    if(dabs(d) > tiny(d)) then
        b=b/d
    else
        b=0.0d0
    endif
    
    return
end subroutine invert
!---
real(kind(1.0d0)) function volume(cell)
    ! calculates volume of simulation cell - generic parallelepiped case
    real(kind(1.0d0)) cell(3,3), b(3), h(3)
    
    ! height
    h(:) = cell(:,3)
    
    ! cross product of base vectors 
    b(1) = cell(2,1)*cell(3,2) - cell(2,2)*cell(3,1)
    b(2) = cell(1,2)*cell(3,1) - cell(1,1)*cell(3,2)
    b(3) = cell(1,1)*cell(2,2) - cell(1,2)*cell(2,1)

    ! dot product of base times height 
    volume = dot_product(b,h)
    
    return
end function volume
!---
subroutine uXv(u,v,w) ! NEW
    ! returns (w) the cross product of u and v
    real(kind(1.0d0)) u(3), v(3), w(3)

    w(1) = u(2)*v(3) - u(3)*v(2)
    w(2) = u(3)*v(1) - u(1)*v(3)
    w(3) = u(1)*v(2) - u(2)*v(1)
    
    return
end subroutine uXv
!---
subroutine ranor(v) !, seed) 
    ! random unit vector 
    ! References: 
    ! 1. Allen & Tildesley: 'Molecular Simulation of Liquids', Clarendon Press, Oxford, 1989
    ! 2. Frenkel & Smit: 'Understanding Molecular Simulation', Academic Press, 1996
    implicit none
    
    real(kind(1.0d0)) v(3)
    ! integer seed
    real(kind(1.0d0)) ran1, ran2, ranh, ransq, myrandom
    external myrandom
    
    ransq = 2.0d0
    do while(ransq >= 1.0d0) 
        ran1 = 1.0d0 - 2.0d0 * myrandom() ! seed)
        ran2 = 1.0d0 - 2.0d0 * myrandom() ! seed)
        ransq = ran1*ran1 + ran2*ran2
    enddo
    ranh = 2.0d0*dsqrt(1.0d0-ransq)
    v(1) = ran1*ranh
    v(2) = ran2*ranh
    v(3) = 1.0d0 - 2.0d0*ransq
    
    return
end subroutine ranor
!---
real(kind(1.0d0)) function myrandom()
    implicit none
    real r
    CALL random_number(r)
    myrandom=dble(r)
    return
end function myrandom

