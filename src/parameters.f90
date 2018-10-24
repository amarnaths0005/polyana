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
module parameters
    implicit none
    integer, parameter :: dblpr=kind(1.0d0) ! double precision
    integer, parameter :: NOTHING=-1000
    integer, parameter :: CONTROL=10,FIELD=11,HISTORY=12,RDF=13,POP=14,CFG=15,REVCON=16
    integer, parameter :: POLYOUT=30
    integer, parameter :: CHARLEN=8, INTLEN=10, DPLEN=20
    integer, parameter :: X=1, Y=2, Z=3
    integer, parameter :: COLLECT=1,AVERAGE=2,TRANSLATE=1,ROTATE=2
    integer, parameter :: MAXATMOL=250 ! max nr of atoms in a molecule; used when FORTRAN95 cpp macro is defined 
    integer, parameter :: NOPBC=0,CUBIC=1,ORTHORHOMBIC=2,PARALLELEPIPED=3,SLAB=6
    integer, parameter :: OCTAHEDRAL=4,DODECAHEDRAL=5,HEXAGONAL=7 ! not implemented yet
    real(dblpr), parameter :: PI=3.14159265358979323846D0
    ! global variables here
    integer mystdout
end module parameters
