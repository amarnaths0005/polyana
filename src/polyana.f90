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
program polyana 
    use process
    implicit none
    logical control_exists, field_exists, config_exists, history_exists, revcon_exists
    real start,finish
    integer clck_counts_beg, clck_counts_end, clck_rate    
    
    ! define messaging channel
#ifdef STREAMS
    mystdout=POLYOUT
    open(unit=mystdout,file='STDOUT')
#else
    mystdout=6
#endif    
    ! read post-processing directives, if any, from the control file
    inquire(file='CONTROL',EXIST=control_exists)
    if(control_exists) then
        open(unit=CONTROL,file='CONTROL')
        CALL read_polyana_directives(CONTROL)
        close(unit=CONTROL)     
    else
        write(mystdout,'(/T10,"CONTROL file missing")')
        write(mystdout,'(T10,"Default values will be used; no coarse-graining, no tabulated potential"/)')
    endif
    
    ! check for necessary files 
    inquire(file='FIELD',EXIST=field_exists)
    if(.NOT. field_exists) &
        write(mystdout,'(/T10,"FIELD file missing")')
#ifdef STREAMS
    history_exists=.TRUE.
#else    
    inquire(file='HISTORY',EXIST=history_exists)
    if(.NOT. history_exists) &
        write(mystdout,'(/T10,"HISTORY file missing")')
#endif        
!    if((.NOT. field_exists) .OR. (.NOT. config_exists) .OR. (.NOT. history_exists)) then
    if((.NOT. field_exists) .OR. (.NOT. history_exists)) then
        write(mystdout,'(T10,"Cannot process trajectory; aborting..."/)')
        STOP
    endif
    
    ! Passed check for files  
    ! read the field file
    open(unit=FIELD,file='FIELD')
    if(control_exists) then
        open(unit=CONTROL,file='CONTROL')
        CALL system_setup(FIELD,CONTROL) ! control passed in case group definitios are to be found therein
        close(CONTROL)
    else
        CALL system_setup(FIELD) ! control passed in case group definitios are to be found therein
    endif
    close(unit=FIELD)

    ! read headers from config, history files
#ifndef STREAMS    
    open(unit=HISTORY,file='HISTORY')
#endif
    inquire(file='CONFIG',EXIST=config_exists)
    if(config_exists) then
        open(unit=CFG,file='CONFIG')
#ifdef STREAMS
        CALL read_trj_header(STDIN  ,CFG)
#else
        CALL read_trj_header(HISTORY,CFG)
#endif        
        close(unit=CFG) 
    else 
        write(mystdout,'(/T10,"CONFIG file missing")')
#ifdef STREAMS
        CALL read_trj_header(STDIN  )
#else
        CALL read_trj_header(HISTORY)
#endif        
    endif
    
    ! read the trajectory file
    CALL cpu_time(start)
    CALL system_clock ( clck_counts_beg, clck_rate )
    do 
#ifdef STREAMS
        if(.NOT.read_trj_step(STDIN  )) &
            exit ! end of file
#else
        if(.NOT.read_trj_step(HISTORY)) &
            exit ! end of file
#endif            
        CALL process_setup_rdf() ! updates rmax if not user-defined; the rest executed only once if arrays not yet allocated
        if(.NOT.process_calc_rdf(COLLECT)) & ! update computed quantities 
            exit ! end of file or stop-read nr of configurations reached
    enddo
    CALL cpu_time(finish)
    CALL system_clock ( clck_counts_end, clck_rate )
    write(mystdout,'(//T10,"Trajectory processed.    CPU time:",F12.5)') finish-start         
    write(mystdout,'( /T10,"                      System time:",F12.5)') &
    (clck_counts_end - clck_counts_beg) / real (clck_rate)
    if(.NOT.process_calc_rdf(AVERAGE)) & ! average, normalise etc.
        STOP 'failed to average rdf''s'
#ifndef STREAMS    
    close(unit=HISTORY)
#endif    
    ! print results
    ! TODO: break process_print_rdf to ..._rdf and ..._pop
    open(unit=RDF,file='RDF')  ! radial distribution functions 
    CALL process_print_rdf(RDF)
    close(unit=RDF)
    open(unit=POP,file='POP')  ! population of species b around species a 
    CALL process_print_rdf(POP) 
    close(unit=POP)
    
    ! close messaging channel
#ifdef STREAMS
    close(mystdout)
#endif    
end program polyana

