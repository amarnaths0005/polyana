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
module Akima
    implicit none
        integer, parameter :: CLEAR=0, TOOSMALL=1, UNDERFLOW=2, OVERFLOW=3, UNSORTED=4, TOTAL=-1
        integer, parameter :: ON=1, OFF=0
        logical :: Akima_success=.FALSE.
        integer :: Akima_fail(TOTAL:UNSORTED)=OFF
        type AKIMAPOINT ! A known (x,y) point endowed with an Akima slope
            double precision x,y,t
        end type
        type AKIMAINTERVAL ! An interval defined by two Akima points, [a,b), endowed with an Akima polynomial
            type (AKIMAPOINT) :: a,b
            double precision :: q(0:3)
        end type
        ! Public methods
        public Akima_get_success, Akima_interpolate, Akima_inquire        
        ! Private members 
        ! Variables:
        private Akima_fail, Akima_success
        ! Methods: 
        private Akima_belongs, Akima_coefficients, Akima_copy_point
        private Akima_define_intervals, Akima_der, Akima_dder
        private Akima_endpoints, Akima_get_fail, Akima_set_fail
        private Akima_set_interval, Akima_set_point, Akima_set_success
        private Akima_slope, Akima_total_fail, Akima_value
    contains 
        ! Public methods
        ! 
        ! Methods to check status
        logical function Akima_get_success()
            implicit none
            Akima_get_success=Akima_success
            return
        end function Akima_get_success
!---
        integer function Akima_inquire(io)
            implicit none
            integer, optional :: io
            integer :: ierr=0 ! number of violations 
            integer stdout
            if(present(io)) then
                stdout=io
            else
                stdout=6
            endif
            if(.NOT. Akima_get_fail(TOTAL)) &
                write(stdout,'(/T5"Akima reports:"/T5"No violation of basic assumptions"/)') 
            if(Akima_get_fail(TOOSMALL)) then
                write(stdout,'(/T5"Akima reports:"/T5"Source data set too small (< 5 elements)"/)')
                ierr=ierr+1
            endif
            if(Akima_get_fail(UNDERFLOW)) then
                write(stdout,'(/T5"Akima reports:"/T5"Some target elements are smaller than minimum source-x"/)')
                ierr=ierr+1
            endif
            if(Akima_get_fail(OVERFLOW)) then
                write(stdout,'(/T5"Akima reports:"/T5"Some target elements are bigger than maximum source-x"/)')
                ierr=ierr+1
            endif
            if(Akima_get_fail(UNSORTED)) then
                write(stdout,'(/T5"Akima reports:"/T5"X-source array is unsorted"/)')
                ierr=ierr+1
            endif
            Akima_inquire=ierr
            return
        end function Akima_inquire
        ! 
        ! Public call to Akima interpolation 
        subroutine Akima_interpolate(ns,xs,ys,nt,xt,yt,ydt,yddt) ! s=source, t=target
            implicit none
            integer ns,nt
            double precision xs(ns),ys(ns),xt(nt),yt(nt)
            double precision, optional :: ydt(nt), yddt(nt)
            integer i,interval,j
            double precision x
            logical ok
            type (AKIMAINTERVAL), allocatable :: AI(:)
            ! Check assumptions
            CALL Akima_set_success(.FALSE.)
            CALL Akima_set_fail(CLEAR)
            if(ns<5) &
                CALL Akima_set_fail(TOOSMALL)
            if(minval(xs)>minval(xt)) &
                CALL Akima_set_fail(UNDERFLOW)
            if(maxval(xs)<maxval(xt)) &
                CALL Akima_set_fail(OVERFLOW)
            do i=1,ns-1
                if(xs(i)>xs(i+1)) then
                    CALL Akima_set_fail(UNSORTED)
                    exit
                endif
            enddo
            if(Akima_get_fail(TOTAL)) &
                return
            ! Interpolate
            allocate(AI(ns-1))
            CALL Akima_define_intervals(xs,ys,AI,ns)
            interval=1
            i=1
            do while(i<=nt)
                ok=.FALSE.
                x=xt(i)
                if(x==xs(ns)) then 
                    yt(i)=Akima_value(AI(ns-1),x) ! by convention
                    if(present(ydt)) &
                        ydt(i)=Akima_der(AI(ns-1),x)
                    if(present(yddt)) &
                        yddt(i)=Akima_dder(AI(ns-1),x)
                    ok=.TRUE.
                else if(Akima_belongs(AI(interval),x)) then
                    yt(i)=Akima_value(AI(interval),x)  
                    if(present(ydt)) &
                        ydt(i)=Akima_der(AI(interval),x)
                    if(present(yddt)) &
                        yddt(i)=Akima_dder(AI(interval),x)
                    ok=.TRUE.
                else
next:               do j=interval+1,ns-1
                        if(Akima_belongs(AI(j),x)) then
                            yt(i)=Akima_value(AI(j),x)  
                            if(present(ydt)) &
                                ydt(i)=Akima_der(AI(j),x)
                            if(present(yddt)) &
                                yddt(i)=Akima_dder(AI(j),x)
                            interval=j
                            ok=.TRUE.
                            exit next
                        endif
                    enddo next
                    if(.NOT. ok) then ! maybe xt is unsorted so check previous intervals too
next1:                  do j=1,interval-1
                            if(Akima_belongs(AI(j),x)) then
                                yt(i)=Akima_value(AI(j),x)  
                                if(present(ydt)) &
                                    ydt(i)=Akima_der(AI(j),x)
                                if(present(yddt)) &
                                    yddt(i)=Akima_dder(AI(j),x)
                                interval=j
                                ok=.TRUE.
                                exit next1
                            endif
                        enddo next1
                    endif
                endif
                i=i+1
            enddo
            deallocate(AI) 
            CALL Akima_set_success(.TRUE.)     
            return
        end subroutine Akima_interpolate
        ! 
        ! 
        ! Private methods 
        ! 
        ! Status getters 
        logical function Akima_get_fail(f)
            implicit none
            integer f
            Akima_get_fail=.FALSE.
            if(f==TOTAL .AND. Akima_total_fail()>CLEAR) then
                Akima_get_fail=.TRUE.
            else if(Akima_fail(f)==ON) then
                Akima_get_fail=.TRUE.
            endif
            return
        end function Akima_get_fail            
!---    
        integer function Akima_total_fail()
            implicit none
            Akima_total_fail=sum(Akima_fail)
            return
        end function Akima_total_fail
        ! 
        ! Status setters 
        subroutine Akima_set_fail(f)
            implicit none
            integer f, i
            if(f==CLEAR) then
                Akima_fail(:)=OFF
            else
                Akima_fail(f)=ON
            endif
            return
        end subroutine Akima_set_fail
!---
        subroutine Akima_set_success(s)
            implicit none
            logical s
            Akima_success=s
            return
        end subroutine Akima_set_success
        ! 
        ! Implementation of Akima interpolation   
        ! 
        ! Setters
        ! 
        subroutine Akima_copy_point(a,b)
            implicit none
            type (AKIMAPOINT) :: a,b
            b%x=a%x
            b%y=a%y
            b%t=a%t
            return
        end subroutine Akima_copy_point
!---         
        subroutine Akima_set_interval(a,b,AI)
            implicit none
            double precision q(0:3)
            type (AKIMAPOINT) :: a,b
            type (AKIMAINTERVAL) :: AI
            CALL Akima_coefficients(a%x,a%y,a%t,b%x,b%y,b%t,q)
            AI%a=a
            AI%b=b
            AI%q=q
            return
        end subroutine Akima_set_interval
!---         
        subroutine Akima_set_point(x,y,a)
            implicit none
            double precision x(5),y(5)
            type (AKIMAPOINT) :: a
            a%x=x(3)
            a%y=y(3)
            a%t=Akima_slope(x,y)
            return
        end subroutine Akima_set_point
        ! 
        ! Implementation 
        logical function Akima_belongs(AI,x)
            implicit none
            type (AKIMAINTERVAL) :: AI
            double precision x
            logical b
            b=.FALSE.
            if(AI%a%x<=x .AND. x<AI%b%x) &
                b=.TRUE.
            Akima_belongs=b
            return
        end function Akima_belongs
!---
        subroutine Akima_coefficients(x1,y1,t1,x2,y2,t2,q)
            implicit none
            double precision x1,x2,y1,y2,t1,t2,q(0:3)
            double precision p0,p1,p2,p3,p4
            p0  = y1
            p1  = t1
            p2  =(3.0d0*(y2-y1)/(x2-x1)-t1-t1-t2)/(x2-x1)
            p3  =(t1+t2-2.0d0*(y2-y1)/(x2-x1))/(x2-x1)**2   
            q(0)= p0-p1*x1+p2*x1**2-p3*x1*x1*x1
            q(1)= p1-2.0d0*p2*x1+3.0d0*p3*x1*x1
            q(2)= p2-3.0d0*p3*x1
            q(3)= p3        
            return
        end subroutine Akima_coefficients        
!---         
        subroutine Akima_define_intervals(x,y,AI,n)
            implicit none
            integer n
            double precision x(n),y(n)
            type (AKIMAINTERVAL) :: AI(n-1)
            integer i
            double precision x5(5),y5(5)
            type (AKIMAPOINT) :: a,b
            ! Define Akima points
            ! For each pair of successive Akima points, define an Akima interval
            ! First, the two starting points
            x5=0.0d0
            y5=0.0d0
            x5(3:5)=x(1:3)
            y5(3:5)=y(1:3)
            CALL Akima_endpoints(x5(5:1:-1),y5(5:1:-1))
            CALL Akima_set_point(x5,y5,a)
            x5(1)=x5(2)
            x5(2:5)=x(1:4)
            y5(1)=y5(2)
            y5(2:5)=y(1:4)
            CALL Akima_set_point(x5,y5,b)
            CALL Akima_set_interval(a,b,AI(1))
            ! Now the main set
            do i=5,n
                CALL Akima_copy_point(b,a) ! Copy last b to a to roll intervals over
                x5(1:5)=x(i-4:i)
                y5(1:5)=y(i-4:i)
                CALL Akima_set_point(x5,y5,b)
                CALL Akima_set_interval(a,b,AI(i-3))
            enddo
            ! And now, the end points
            CALL Akima_copy_point(b,a)
            x5(1:4)=x(n-3:n)
            x5(5)=0.0d0
            y5(1:4)=y(n-3:n)
            y5(5)=0.0d0
            CALL Akima_set_point(x5,y5,b)
            CALL Akima_set_interval(a,b,AI(n-2))
            CALL Akima_copy_point(b,a)
            x5(1:3)=x(n-2:n)
            x5(4:5)=0.0d0
            y5(1:3)=y(n-2:n)
            y5(4:5)=0.0d0
            CALL Akima_endpoints(x5,y5)
            CALL Akima_set_point(x5,y5,b)
            CALL Akima_set_interval(a,b,AI(n-1))
            return
        end subroutine Akima_define_intervals
!---
        subroutine Akima_endpoints(x,y)
            ! usage: 
            !       call Akima_endpoints(x(ndim-4:ndim),y(ndim-4:ndim))
            !   and
            !       call Akima_endpoints(x(5:1:-1),y(5:1:-1))
            !   where ndim=size(x)=size(y)
            implicit none
            double precision x(1:5),y(1:5) ! points 1 to 3 are known; points 4, 5 are unknown end points
            double precision r1,r2,r3,r4
            x(4)=-x(1)+x(2)+x(3)
            x(5)=-x(2)+x(3)+x(4)
            r1  =(y(2)-y(1))/(x(2)-x(1))
            r2  =(y(3)-y(2))/(x(3)-x(2))
            r3  = r2+r2-r1
            r4  = r3+r3-r2
            y(4)= r3+y(3)/(x(4)-x(3))
            y(5)= r3+y(4)/(x(5)-x(4))
            return
        end subroutine Akima_endpoints
!---        
        double precision function Akima_slope(x,y)
            implicit none
            double precision x(5),y(5)
            double precision m1,m2,m3,m4,t
            ! Compute local slopes, Dy/Dx
            m1=(y(2)-y(1))/(x(2)-x(1))
            m2=(y(3)-y(2))/(x(3)-x(2))
            m3=(y(4)-y(3))/(x(4)-x(3))
            m4=(y(5)-y(4))/(x(5)-x(4))
            ! Handle Inf (ocurring when Dy >> Dx)
            if(m1==m1-1.0d0) &
                m1=dsign(huge(1.0d0),m1)
            if(m2==m2-1.0d0) &
                m2=dsign(huge(1.0d0),m2)
            if(m3==m3-1.0d0) &
                m3=dsign(huge(1.0d0),m3)
            if(m4==m4-1.0d0) &
                m4=dsign(huge(1.0d0),m4)
            ! Calculate Akima slope (and handle special case too)
            t =(dabs(m4-m3)*m2+dabs(m2-m1)*m3)/ & ! numerator
               (dabs(m4-m3)   +dabs(m2-m1)   )    ! denominator
            if(t/=t) & ! benefit from NaN to invoke Akima rule for special case
                t=0.5d0*(m2+m3)
            ! Handle Inf ,just in case
            if(t==t-1.0d0) &
                t=dsign(huge(1.0d0),t)
            ! return slope
            Akima_slope=t
            return
        end function Akima_slope
!---   
        double precision function Akima_value(AI,x)
            implicit none
            type (AKIMAINTERVAL) :: AI
            double precision x,q(0:3)
            q=AI%q
            Akima_value=q(0)+x*(q(1)+x*(q(2)+x*q(3)))
            return
        end function Akima_value     
!---   
        double precision function Akima_der(AI,x) ! First derivative 
            implicit none
            type (AKIMAINTERVAL) :: AI
            double precision x,q(3)
            q=AI%q(1:3)
            Akima_der=q(1)+x*(q(2)*2+x*q(3)*3)
            return
        end function Akima_der     
!---   
        double precision function Akima_dder(AI,x) ! Second derivative 
            implicit none
            type (AKIMAINTERVAL) :: AI
            double precision x,q(2:3)
            q=AI%q(2:3)
            Akima_dder=q(2)*2+x*q(3)*6
            return
        end function Akima_dder     
end module Akima

