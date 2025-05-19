      subroutine open_input_file(infile,luni,errnum,inpstat)
c
c  Input:
c    infile    C*(*)  Name of file to open
c    luni      INT    Logical Unit Number for file I/O
c
c  Input/Output:
c    errnum    INT    Error code (0=ok, <0=fatal, >0=recoverable)
c    inpstat   INT    Value of IOSTAT returned by file I/O
c
      implicit none

      integer
     & luni,       ! Subroutine input argument (see above)
     & errnum,     ! Subroutine input/output argument (see above)
     & inpstat     ! Subroutine input/output argument (see above)

      logical
     & filexist    ! Keeps track of file existence

      character
     & infile*(*)  ! Subroutine input argument (see above)

      if((errnum.eq.0).and.(inpstat.eq.0)) then
        inquire(file=infile,exist=filexist,iostat=inpstat)
        if(inpstat.ne.0) then
          errnum=-1
          write(*,'(2a)')'Error: inquire failed on file ',infile
        elseif(filexist) then
          open(unit=luni,file=infile,status='old',iostat=inpstat)
          if(inpstat.ne.0) then
            errnum=-1
            write(*,'(2a)')'Error: open failed on file ',infile
          endif
        else
          errnum=-1
          write(*,'(2a)')'Error: please provide input file ',infile
        endif
      endif        ! ((errnum.eq.0).and.(inpstat.eq.0))
      return
      end
