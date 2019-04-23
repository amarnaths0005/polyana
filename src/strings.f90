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
module strings
    implicit none
    integer STRERR
    integer, parameter :: RECLEN=250
    integer, parameter :: EOF=0, Cancel=3, newl=10
    integer, parameter :: stdin=5, stdout=6
    character(RECLEN) record
    !private stdin, stdout
contains
    character(1) function fgetc(io)
        integer :: io   
        read(io, '(a1)', advance='no', EOR=100, END=101, ERR=102) fgetc 
        return
100     fgetc = char(newl)
        return
101     fgetc = char(EOF)
        return
102     fgetc = char(Cancel)
        return
    end function fgetc
!---
    logical function get_stream(io,mute)
        implicit none
        integer :: io,i
        character(1) :: c
        logical :: echo=.TRUE.
        logical, optional :: mute
        if(present(mute)) &
            echo=mute
        i=1
        get_stream=.TRUE.
        record(1:RECLEN)=' '
        do 
            c=fgetc(io) ! catch streamed bytes 
            if(ichar(c)==Cancel) then
                ! write(stdout,'(A)',advance='no') record(1:i)
                exit
            endif
            if(i==RECLEN) then ! if buffer full stream it on
                ! write(stdout,'(A)',advance='no') record(1:i)
                exit
            endif            
            if(ichar(c)==EOF) then
                get_stream=.FALSE.
                ! write(stdout,'(A)',advance='no') record(1:i)
                exit
            endif
            record(i:i)=c
            if(ichar(c)==newl) then
                ! write(stdout,'(A)',advance='no') record(1:i)
                exit
            endif            
            i=i+1    
        enddo
        if(echo) &
            CALL put_record(stdout)
        return        
    end function get_stream   
!---
    logical function get_record(io)
        implicit none
        integer io
        get_record=.TRUE.
        record(:)=' '
        read(io,'(A)',iostat=STRERR) record
        if(STRERR/=0) &
            get_record=.FALSE.
!        if(STRERR<0) &
!            write(*,'(/" get_record: end of file"/)')
!        if(STRERR>0) &
!            write(*,'(/" get_record: error reading file",I2/)') io
        return        
    end function get_record   
!---
    subroutine put_record(io)
        implicit none
        integer io
        write(io,'(A)') record(1:strlen(record))
        return        
    end subroutine put_record   
!---
    integer function strlen(s)
        implicit none
        character(*) s
        integer i,length
        strlen=0      
        length=min(len(s),RECLEN)
        do i=1,length
            ! assumes ascii encoding; skips space (32) and non-printable ( < 32) characters
            if(ichar(s(i:i))>32) &
                strlen=i
        enddo
        return 
    end function strlen
!---
    logical function is_digit(c) ! assumes ascii encoding 
        character(1) c
        integer nc
        is_digit=.FALSE.
        nc=ichar(c)
        if(48<=nc .AND. nc<=57) &
            is_digit=.TRUE.
        return
    end function is_digit
!---
    integer function strstr(token, string) 
        implicit none
        character(*) token, string
        character(RECLEN) t, s
        integer i, n, ntok, nstr
        CALL to_up(token,t)
        CALL to_up(string,s)
        ntok=len(token)
        nstr=len(string)
        strstr=0
        if(ntok>nstr) &
            return
        n=nstr-ntok+1
        do i=1,n
            if(t(1:ntok)==s(i:i+ntok-1)) then
                strstr=i
                exit
            endif
        enddo
        return        
    end function strstr   
!---
    subroutine to_up(c,d) ! assumes ascii encoding
        implicit none
        character(*) c
        character(RECLEN) d
        integer i,n,nc
        n=min(RECLEN,len(c))
        d=c
        do i=1,n
            nc=ichar(c(i:))
            if(nc>=97 .AND. nc<=122) &
                d(i:i)=char(nc-32)
        enddo
        return
    end subroutine to_up    
!---
    integer function find_keyword_value(token,string)
        character(*) token, string
        character(1) c
        integer p
        find_keyword_value=-1
        p=strstr(token,string)
        if(p==0) &
            return
        c=record(p:p) ! if not 0, then p>0
        do while(.NOT. is_digit(c)) ! find number to read
            p=p+1
            if(p>RECLEN) & ! discard if longer than a record
                return
            c=record(p:p)
        enddo
        find_keyword_value=p
        return
    end function find_keyword_value             
end module strings
