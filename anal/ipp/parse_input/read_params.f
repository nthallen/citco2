      subroutine read_params(infile,luni,mprm,default_val,
     & errnum,inpstat,params)
c
c  Input:
c    infile    C*(*)  Name of input file
c    luni      INT    Logical Unit Number for the parameter file
c    mprm      INT    Maximum number of parameters
c    default_val INT  Default value for all parameters
c
c  Input/Output:
c    errnum    INT    Program error status: do nothing if not 0 (=ok)
c    inpstat   INT    Value of IOSTAT returned from input file read
c
c  Output:
c    params(mprm)INT  Values of parameters from file, or default
c
c  This subroutine is intended to read "hidden" parameters that can
c  be used to affect the program execution beyond its default behavior.
c  It is not meant to be user-friendly.  The parameters are first set
c  to the supplied default value.  Then the existence of the input file 
c  is checked: it is okay for the file to be missing, as the default
c  value is already established.  If present, the file is read until
c  its end, possibly overwriting the parameters, one element per line.
c  There is no need for the element count in the file to match the size
c  of the array.  Unless we encounter a serious error, the subroutine
c  should be silent.  Everything is an integer: the mainline can fake
c  floating point values by scaling and converting.  Comments in the
c  input file are okay, either full lines or in-line. 
c  
      implicit none

      integer
     & luni,       ! Subroutine input argument (see above)
     & mprm,       ! Subroutine input argument (see above)
     & default_val,! Subroutine input argument (see above)
     & errnum,     ! Subroutine input/output argument (see above)
     & inpstat,    ! Subroutine input/output argument (see above)
     & params(mprm),! Subroutine output argument (see above)
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & indexa      ! General loop index

      logical
     & filexist    ! Keeps track of file existence

      character
     & infile*(*), ! Subroutine input argument (see above)
     & inpstr*255  ! String used to read entries from input file
c
c  Set the full parameter vector to the supplied default value.
c  We do this even if errnum is set, as this is always safe.
c
      do indexa=1,mprm
        params(indexa)=default_val
      enddo
c
c  The remainder of the work is conditional on error-free status.
c
      if((errnum.eq.0).and.(inpstat.eq.0)) then
c
c  Now see if there is a parameter file.  We do this with minimal
c  error checking (no "inpstat") as we want to fail silently on
c  missing parameter files or system errors (just this once!).
c
        inquire(file=infile,exist=filexist)
        if(filexist) then
c
c  So there is a file...  Open it with full error checks then...
c
          call open_input_file(infile,luni,errnum,inpstat)
c
c  Obtain user-supplied program parameters.
c
c  Most compilers will signal an error via iostat for out-of-range
c  values.  However, g77-3.4.6 and f2c will not flag an error and
c  will instead return the value -(2^31), i.e. -2147483648.  So
c  we check for that and return an error.  That particular constant
c  triggers a gcc warning, so me use .lt.(-2147483647) rather than
c  .eq.(-2147483648).
c
          indexa=1
          do while ((errnum.eq.0).and.(inpstat.eq.0).and.
     &              (indexa.le.mprm))
            call read_input_line(luni,errnum,inpstat,inpstr)
            if((errnum.eq.0).and.(inpstat.eq.0)) then
              read(inpstr,*,iostat=inpstat) params(indexa)
              if((inpstat.ne.0).or.
     &           (params(indexa).lt.(-2147483647))) then
                write(*,'(5a)')
     &           'Error in parameter file ',
     &           infile(1:lnbc(infile)),
     &           ' entry ',
     &           inpstr(1:lnbc(inpstr)),
     &           ' is not an integer or is out of range'
                errnum=-2
              endif
            endif
            indexa=indexa+1
          enddo
c
c  All done: close the input file with full error checks.
c
          call close_input_file(infile,luni,inpstat)

        endif      ! (filexist)
      endif        ! ((errnum.eq.0).and.(inpstat.eq.0))
      return
      end
