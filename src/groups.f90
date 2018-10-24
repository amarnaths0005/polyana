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
module groups 
    use molecules
    use strings
    implicit none
    integer ngroup, ngrouptyp
    integer, parameter :: MAXGTYP=1000
    character(1), parameter :: leftparen="(", rightparen=")"
    character(10*MAXGTYP) :: gtdict_static=" "
    type (MOLECULE_T), allocatable :: group(:)
    private ngroup, ngrouptyp
    public  group
contains
! Setters
    subroutine group_set_num(n)
        implicit none
        integer n
        ngroup=n
        return
    end subroutine group_set_num
!---    
    subroutine group_set_ntyp(n)
        implicit none
        integer n
        ngrouptyp=n
        return
    end subroutine group_set_ntyp
! Getters
    integer function group_get_num()
        implicit none
        group_get_num=ngroup
        return
    end function group_get_num
!---    
    integer function group_get_ntyp()
        implicit none
        group_get_ntyp=ngrouptyp
        return
    end function group_get_ntyp
! Setup methods
    logical function group_setup(io,molecule)
        implicit none
        integer io
        type (MOLECULE_T) molecule(*)
        integer p
        integer natmol,nm,nmol,imol,imoltyp
        
        group_setup=.FALSE.
        rewind(io)
        
        if(allocated(group)) then ! groups have already been defined elsewhere
            group_setup=.TRUE.
            RETURN
        endif 
        
        CALL group_set_ntyp(0)            
        CALL group_set_num (0)
        nmol=0      
        imoltyp=0      
        do  
            if(.NOT.get_record(io)) &
                exit
            if(strstr("#",record)>0) &
                record(strstr("#",record):RECLEN)=" " ! remove comments
            p=find_keyword_value('NUMMOL', record)
            if(p>0) then 
                read(record(p:RECLEN),*) nm
                nmol=nmol+nm
                imoltyp=imoltyp+1
            endif
            p=find_keyword_value('ATOMS',record)
            if(p>0) then
                read(record(p:RECLEN ),*) natmol ! number of atoms per molecule 
                do imol=nmol-nm+1,nmol
                    CALL group_parser(molecule,imol,imoltyp,natmol)
                enddo
                group_setup=.TRUE.
            endif
        enddo
        RETURN

    end function group_setup
!---
    subroutine group_parser(molecule,imol,imoltyp,natmolread)
        ! 
        ! A parser based on the unjustly defamed goto statement (see D. E. Knuth 
        ! Computing Surveys, 6(4), 1974,  261-301  for a balanced  discussion of 
        ! the topic)
        ! 
        ! The syntax to parse is the following: 
        !
        !   (...([char] [int] [int]) [...([char] [int] [int])] [int] ) [(...)]
        !
        ! In particular, a molecule is divided into groups, defined thus:
        !
        !                       (grouptype nat  nrep) 
        !
        ! where grouptype is a 8-character string, nat is the number of atoms in
        ! the group and nrep is the number of times  the group is repeated along
        ! the array of the atoms that comprise the molecule. 
        ! Groups  of different types  can be grouped themselves  to form  larger 
        ! 'supergroups' which can be repeated many times, as for instance in the  
        ! case of copolymers. Then, definitions are enclosed in parentheses with 
        ! the number of times the 'supergroup' is repeated at the end, such as: 
        ! 
        !                      ( (A 3 1) (B 2 1) 100)
        ! 
        ! This  pattern  can be applied  recursively  to define large structures 
        ! of arbitrary complexity. 
        ! The group definitions are placed in FIELD's ATOM statements, after the
        ! number of atoms, like this: 
        !
        !                  ATOMS [int] [group-definitions]
        !
        ! If a molecule is to be divided into groups, these must be defined such 
        ! that all atoms belong to one of the groups and the sum of the atoms in 
        ! all repeated groups be equal to the argument of the ATOM keyword. 
        ! 
        !    Examples
        !
        ! 1. United-atom n-hexane as a trimer:
        !
        !         ATOMS 6  (terminal 2 1) (midsegm 2 1) (terminal 2 1)
        !
        ! 2. United-atom n-hexane as a dimer: 
        !
        !         ATOMS 6          (bead 3 1) (bead 3 1) 
        !
        !    or
        !
        !         ATOMS 6              (bead 3 2)
        !
        ! 3. United-atom n-dodecane as a tetramer:
        !
        !         ATOMS 12 (terminal 3 1) (midsegm 3 2) (terminal 3 1)
        !
        ! 4. Copolymer A-(B2C3)100-A where consecutive B's form one bead and C's
        !    form another:
        !
        !         ATOMS 502 (A 1 1) ((B 2 1) (C 3 1) 100) (A 1 1)
        !
        ! NOTE: There is a 'stricter' way to specify the group syntax that could
        ! could be defined in a recursive manner  down to the 'primitive'  group 
        ! level. In it, a group definition would be simpler
        ! 
        !                          (grouptype nat) 
        ! 
        ! but the way to specify its repeat number would be to write:
        !
        !                       ((grouptype nat) nrep) 
        ! 
        ! This is in fact a valid POLYANA syntax  and the user is free to choose
        ! between these two ways  to denote groups of atoms.  The reason why the 
        ! first, 'simple', definition was implemented has to do with simplicity.
        ! Look at the way Example 4, above, is rewritten in 'strictly recursive'
        ! manner, 
        ! 
        !          ATOMS 502 ((A 1) 1) (((B 2) 1) ((C 3) 1) 100) ((A 1) 1)
        ! 
        ! and compare to its previous, simple form. Which one is easier to read? 
        ! As a compromise,  let's note that  a single 'primitive' group  that is
        ! repeated only once, does not have to be enclosed in outer parentheses.
        ! Thus, ((A 3) 1)  and  (A 3) mean the same thing and are equally valid.
        ! 
        implicit none
        integer imol,imoltyp,natmolread
        type (MOLECULE_T) molecule(*)
        integer,save::ngrtot
        integer error_code
        integer natmol,ngr,ngtyp,i,chkpar,p,q,nat,nrep,nrepg,sdepth,ntype,j
        integer loop_counter(RECLEN)
        real(dblpr) charge,mass
        character(1) a,b,c
        character(8) gname
        logical type_exists
        
        if(imol==1) &
            ngrtot=0
        ngr   =0
        natmol=0
        ngtyp =0
        sdepth=0
        loop_counter=1
        
        ! sanity check: look out for non-matching parentheses
        chkpar=0
        do i=1,RECLEN
            c=record(i:i)
            if(c== leftparen) &
                chkpar=chkpar+1
            if(c==rightparen) &
                chkpar=chkpar-1
        enddo
        if(chkpar/=0) then
            write(mystdout,'(/T10,"CONTROL or FIELD file - unmatched parentheses in following record:")')
            write(mystdout,'(T10,A/)') record
            STOP
        endif                
        
        ! Start parsing the record.  We are going  to look at a segment enclosed 
        ! between the parentheses found above and act according to their types. 
        ! Once we are done, we are going to come back here to read the remaining 
        ! part of the record.  If a superunit  is repeated  more than once, the 
        ! 'scanning head' (q variable) will be reset accordingly  so we get back
        ! at the beginning of the supergroup's definition. 
        q=1
1       do p=q,RECLEN
            b=record(p:p)
            if(b==leftparen.OR.b==rightparen) &
                exit
        enddo
        do q=p+1,RECLEN
            c=record(q:q)
            if(c==leftparen.OR.c==rightparen) &
                exit
        enddo
        
        ! Case 1: " ( ( ". Keep track of stack depth and continue reading
        if(b==leftparen.AND.c==leftparen) then
            sdepth=sdepth+1
            goto 1 ! read remainder
        endif
        
        ! Case 2: " ( ... ) ". This is a group definition
        if(b==leftparen.AND.c==rightparen) then
            if(p+1==q) then
                write(mystdout,'(/T10,"Error while reading group definition in record:"/)')
                write(mystdout,'( T10,A)') record
                write(mystdout,'( T10,"Stacked () parentheses"/)') 
                STOP
            endif
            if(p+1<q) &
                read(record(p+1:q-1),*,iostat=error_code)gname,nat,nrep
            if(error_code/=0) then ! try once more, this time the 'strictly recursive' definition
                read(record(p+1:q-1),*,iostat=error_code)gname,nat
                if(error_code/=0) then
                    write(mystdout,'(/T10,"Error while reading group definition in record:"/)')
                    write(mystdout,'( T10,A)') record
                    write(mystdout,'( T10,"Missing essential info from () parentheses: ",A/)') record(p+1:q-1) 
                    STOP
                endif
                nrep=1
            endif
            type_exists=.FALSE.
            do i=1,len(gtdict_static(1:len_trim(gtdict_static))),10
                if(strstr(gname,gtdict_static(i+1:i+8))>0) then
                    type_exists=.TRUE.
                    exit
                endif
            enddo
            ntype=i/10+1
            if(.NOT.type_exists) then
                ntype=group_get_ntyp()+1
                CALL group_set_ntyp(ntype)
                write(gtdict_static(len_trim(gtdict_static)+1:),'("(",A8,")")') gname
            endif
            do j=1,nrep            
                ngr =ngr +1
                ngrtot = ngrtot+1
                natmol=natmol+nat
                CALL group_extend(ngrtot) ! reallocate group to accomodate one more element
                CALL molecule_set_type(ntype,group(ngrtot))
                CALL molecule_set_nat (nat  ,group(ngrtot))
#ifndef FORTRAN95                    
                CALL molecule_alloc_atoms(group(ngrtot))
#endif    
                do i=1,nat ! loop over group atoms
                    charge=atom_get_charge(molecule(imol)%atom(natmol-nat+i))
                    mass  =atom_get_mass  (molecule(imol)%atom(natmol-nat+i))
                    CALL   atom_set_charge(charge,group(ngrtot)%atom(i))
                    CALL   atom_set_mass  (mass,group(ngrtot)%atom(i))
                enddo            
                nat=molecule_get_nat(group(ngrtot))
                mass=0.0d0
                do i=1,nat
                    mass=mass+atom_get_mass(group(ngrtot)%atom(i))
                enddo
                CALL molecule_set_mass(mass,group(ngrtot))
            enddo            
            goto 1 ! read remainder
        endif
        
        ! Case 3: " ) ( ". Keep reading
        if(b==rightparen.AND.c==leftparen) then
            goto 1 ! read remainder
        endif
        
        ! Case 4: " ) [.] ) ". May be there is a number here
        if(b==rightparen.AND.c==rightparen) then
            nrepg=1
            ! Design decision not to allow 'flexible' syntax; always look for an integer or abort
            ! This way, one can opt to use the 'strictly recursive' syntax without errors
            ! Also confusing redundant parentheses are disallowed, this way.  
            if(p+1==q) then 
                write(mystdout,'(/T10,"Error while reading repeat index in record:"/)')
                write(mystdout,'( T10,A)') record
                write(mystdout,'( T10,"Stacked )) parentheses"/)') 
                STOP
            endif
            if(p+1<q) &          
                read(record(p+1:q-1),*,iostat=error_code)nrep
            if(error_code/=0) then
                write(mystdout,'(/T10,"Error while reading repeat index in record:"/)')
                write(mystdout,'( T10,A)') record
                write(mystdout,'( T10,"Missing repeat index from )) parentheses: ",A/)') record(p+1:q-1) 
                STOP
            endif
            if(error_code==0) &
                nrepg=nrep
            if(loop_counter(sdepth)==nrepg)then 
                !q=q+1
                loop_counter(sdepth)=1
                sdepth=sdepth-1
                goto 1 ! end of loop; read remainder
            endif
            loop_counter(sdepth)=loop_counter(sdepth)+1 
            chkpar=0
            ! find beginning of this supergroup, go back there and re-read
            do i=p,1,-1
                a=record(i:i)            
                if(a==rightparen)&
                    chkpar=chkpar+1
                if(a== leftparen)&
                    chkpar=chkpar-1
                if(chkpar<0)&
                    exit
            enddo
            q=i+1 
            goto 1
        endif
        
        ! Case 5: " ) [blank] " or "[blank][blank]". 
        ! end reached; nothing to parse
        
        ! One last check, just in case
        if(ngr==0) then ! If no group definitions, treat molecule as single group
            write(gname,'("moltyp",2I1)') imoltyp/10,imoltyp ! I don't expect more than 99 mol types
            type_exists=.FALSE.
            do i=1,len(gtdict_static(1:len_trim(gtdict_static))),10
                if(strstr(gname,gtdict_static(i+1:i+8))>0) then
                    type_exists=.TRUE.
                    exit
                endif
            enddo
            ntype=i/10+1
            if(.NOT.type_exists) then
                ntype=group_get_ntyp()+1
                CALL group_set_ntyp(ntype)
                write(gtdict_static(len_trim(gtdict_static)+1:),'("(",A8,")")') gname
            endif
            ngr =ngr +1
            ngrtot = ngrtot+1
            natmol=natmol+molecule_get_nat(molecule(imol))
            CALL group_extend(ngrtot) ! reallocate group to accomodate one more element
            CALL molecule_copy(molecule(imol),group(ngrtot))
            CALL molecule_set_type(ntype,group(ngrtot))
        endif
        if(natmol/=natmolread) then
            write(mystdout,&
            '(/T10,"CONTROL or FIELD file - nrs of group atoms don''t match nr of mol atoms in record:")')
            write(mystdout,'(T10,A/)') record
            STOP
        endif
        
        ! last info: only inter-molecular RDFs (optionally overriden elsewhere)
        CALL group_set_num(ngrtot)
        do i=ngrtot-ngr+1,ngrtot 
            CALL molecule_set_next(ngrtot+1,group(i))
        enddo
        
        return
    end subroutine group_parser  
!---
    subroutine group_extend(ndim)
        ! TODO: use linked list instead of dealloc/alloc which can cause memory issues
        implicit none
        integer ndim
        integer i
        type (MOLECULE_T), allocatable :: tmp(:)
        if(allocated(group)) then
            if(ndim<=size(group))then
                write(mystdout, &
                '(/T10,"group_extend: proposed new size of ",I5," smaller than original (",I5,")"    )') &
                ndim,size(group)
                write(mystdout,'(/T10,"group array remains unchanged"/)')
                return
            endif
            allocate(tmp(1:size(group)))
            do i=1,size(group)
                CALL molecule_copy(group(i),tmp(i))
            enddo
            deallocate(group)
            allocate(group(1:ndim))
            do i=1,size(tmp)
                CALL molecule_copy(tmp(i),group(i))
            enddo
            deallocate(tmp)
        else
            allocate(group(1:ndim))
        endif
        return
    end subroutine group_extend     
end module groups    
