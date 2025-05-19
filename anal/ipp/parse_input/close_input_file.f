      subroutine close_input_file(infile,luni,inpstat)
c
c  Input:
c    infile    C*(*)  Name of input file
c    luni      INT    Logical Unit Number for file I/O
c
c  Output:
c    inpstat   INT    Value of IOSTAT returned by file I/O
c
      implicit none

      integer
     & luni,       ! Subroutine input argument (see above)
     & inpstat     ! Subroutine output argument (see above)

      character
     & infile*(*)  ! Subroutine input argument (see above)

      close(unit=luni,iostat=inpstat)
      if(inpstat.ne.0) then
        write(*,'(2a)')'Error: close failed on file ',infile
      endif

      return
      end
