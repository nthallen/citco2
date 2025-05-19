      program slice_ipp
c
c  Program to convert IFS125 interferogram slices to spectra.
c
c  External parameters and a list of scan sets are obtained from
c  the input file "slice-ipp.in" which is heavily commented.
c
c  This program also reads the information files produced by the
c  real-time software, some actions (e.g. scan rejection) are based
c  on that information, and most of it is saved in the OPUS file
c  headers.
c
      implicit none
c
c  We first define a version string and a program release date.
c  Please remember to update the include file when you modify this
c  program or any of its subroutines.
c
      include 'slice-ipp-version.inc'
c
c  The parameters below control how much memory is used by the
c  program, keeping in mind that QNX allocates memory statically.
c
c  The dominant term is MIP which is both the maximum number of
c  interferogram points and the largest FFT size that the program
c  can compute.  Because the program currently duplicates the long
c  side of the interferogram just before the FFT, the actual maximum
c  number of interferogram points is MIP/2 plus the number of points
c  on the short side of ZPD.  For the value of 2**24, the maximum
c  number of points on the long side of ZPD is 8388608.  Given that
c  two points are acquired per laser fringe and that the laser
c  wavenumber is 15798 cm-1, the maximum path difference currently
c  supported is: 8388608 / (2 * 15798) = 265 cm, which exceeds the
c  capability of the interferometer (148 cm).
c
c  Parameter MNS is the maximum number of scans in a set.  In the
c  IFS125, this number is controlled by NSS (Number of Sample Scans).
c  Routinely, NSS will be either 1, 2, or 4.  However the vectors
c  controlled by MNS consume little memory, so we can base this choice
c  on a continuously scanning instrument (hoping for a firmware upgrade
c  that would allow interruptible scanning).  With the current value
c  of MNS=100, we could scan for roughly 2.5 hours without interruptions.
c  Also, some lamp runs now request 32 scans (covered with MNS=100).
c
c  In deriving MSL, the maximum number of data slices in a set of scans,
c  we have used the maximum total path of (6 + 148) cm and the fact that
c  data slices contain about 190000 points.  So the maximum number of
c  data slices per run is: (6 + 148) * 2 * 15798 / 190000 = 26.
c
c  Parameter MCH is fixed at 2 because the IFS125 supports only two
c  detectors (master+slave) in its acquisition system.
c
c  Parameter MDTC is the maximum number of detectors that can be handled
c  in a single program execution.  Some information from the input file
c  is detector specific and must be held in appropriately-sized vectors.
c
c  Parameters MI4 and MR8 are the maximum sizes of vectors used to gather
c  all the OPUS header items.
c
c  Parameter MIF is the maximum number of items read from the information
c  files produced by the real-time recording system.
c
      integer*4
     & mip,        ! Maximum number of input points or FFT size
     & mns,        ! Maximum number of interferograms per scan set (max NSS)
     & msl,        ! Maximum number of interferogram slices per scan set
     & mch,        ! Maximum number of data channels
     & mdtc,       ! Maximum number of detectors
     & mi4,        ! Maximum number of I*4 items in file header
     & mr8,        ! Maximum number of R*8 items in file header
     & mif,        ! Maximum number of info channels
     & mphz        ! Maximum number of phase parameters
      parameter (mip=2**24)
      parameter (mns=100)
      parameter (msl=mns*26)
      parameter (mch=2)
      parameter (mdtc=5)
      parameter (mi4=40)
      parameter (mr8=40)
      parameter (mif=40)
      parameter (mphz=10)
c
c  The remaining data declarations are split into two groups.  First here
c  are the variables that the program uses for its internal functions or
c  to store the parameters read from the input file "slice-ipp.in".
c
      integer*4
     & errnum,     ! Error code (0=ok, <0=fatal, >0=recoverable)
     & inpstat,    ! Value of IOSTAT from parameter input file read
     & lun,        ! Logical Unit Number for file I/O
     & igrmode,    ! Interferogram saving mode (none/raw/deglitched)
     & chan1,      ! Starting channel number to process
     & chan2,      ! Ending channel number to process
     & outfmt,     ! Format selection for output file
     & minmax,     ! Count of min-max pairs in ASCII output file (0=no min-max)
     & scanlim,    ! Maximum number of scans analyzed in a single execution
     & fftlim(mdtc),! Maximum log-base-2 of the FFT size for each detector
     & proclim,    ! Maximum processing stage performed by the program
     & verbose,    ! Level of verbosity for displayed messages
     & catyear,    ! Year from catalog
     & catmonth,   ! Month from catalog
     & catday,     ! Day from catalog
     & catbatch,   ! Batch (save set) number from catalog
     & slice,      ! Slice number from catalog, used in slice file name
     & slicmem,    ! Previous slice number for catalog work-around
     & memyear,    ! Memory of year to control reset of run number
     & memmonth,   ! Memory of month to control reset of run number
     & memday,     ! Memory of day to control reset of run number
     & runno,      ! Run number increasing throughout the day
     & scanno,     ! Scan number within a set of NSS scans
     & scancnt,    ! Total number of scans analyzed during program execution
     & channel,    ! Channel number (1=InGaAs=slave, 2=Si=master)
     & counter,    ! Point count, data destination into 'buffer'
     & runsta(mns),! Run starting slice number (if > 0) or run error (if < 0)
     & runend(mns),! Run ending slice number
     & phzprm(mphz),! Secret phase parameters
     & indexa      ! General loop index
     
      real*4 
     & stlimavg,   ! Limit for suntracker average intensity
     & stlimstd,   ! Limit for suntracker intensity standard deviation
     & mnymxy(msl,mch,2),! Values of MNY and MXY from each channel and slice
     & mnymxyth(mch,2),! Threshold values for MNY and MXY
     & xcorlim(mdtc)! Minimum value of the ZPD cross-correlation

      character
     & infile*(*), ! Name of program input file
     & inpath*99,  ! Directory path to input slice files
     & outpath*99, ! Directory path for output spectrum files
     & igrmpath*99,! Directory path for output interferogram files
     & flimit*99,  ! Name of file containing the frequency limits
     & pattern*99, ! Pattern for CIT file-naming convention
     & srcindic*26,! List of source indicators for output files
     & dtcigrm*26, ! List of detector indicators for interferogram files
     & dtcspec*26, ! List of detector indicators for spectrum files
     & delimit*1,  ! Field delimiter for ASCII output file
     & inpstr*999, ! String used to read entries from input file
     & filename*99 ! Full file name for output files

      parameter (infile='slice-ipp.in')
      parameter (lun=20)
c
c  The second group of data declarations holds the values of the OPUS data
c  and header items.  These are read (or derived) from the interferogram
c  slices and some are stored in the output interferogram/spectrum.  A few
c  items (e.g.: nss, tpx) have meanings specific to this program, but most
c  header items are generic: those are stored in two vectors to allow for
c  easy expansion without constantly changing the subroutine argument lists.
c  The indices of the header items kept in those vectors are defined in the
c  following include file:
c
      include 'opus-comn/header_indices.inc'

      integer*4
     & nptvec(msl),! Number of PoinTs in each slice
     & bpdata(msl,mch),! Byte pointers into the data blocks of slices
     & nss,        ! Number of Sample Scans in the input slices combined
     & tpx,        ! Total Points in X (along OPD) in each scan direction
     & i4head(mi4) ! Vector to hold the I*4 header items

      real*4 
     & buffer(mip) ! Data storage and FFT buffer

      real*8
     & r8head(mr8),! Vector to hold the R*8 header items
     & timvec(mns),! Time vector contains one entry for each scan
     & timsli(msl),! Time vector contains one entry for each slice
     & minfold,    ! Wavenumber of the start of this spectral fold
     & maxfold,    ! Wavenumber of the start of the next spectral fold
     & pointspac,  ! Spectral point spacing
     & maxfoldp1,  ! Wavenumber of the point after the Nyquist
     & infovec(mif),   ! Information produced by real-time algorithm
     & infomat(mif,mns)! Information produced by real-time algorithm
c
c  Program execution starts here: display version string.
c
      write(*,'(a)')verstr
c
c  Initialize variables.
c
      errnum=0
      inpstat=0
      scancnt=0
      i4head(i_bfw)=0    ! We never save bad runs: BFW=BBW=0.
      i4head(i_bbw)=0
      call default_run_info(mif,mns,infomat)
      call read_params('phase_param.egg',lun,mphz,-1,
     & errnum,inpstat,phzprm)
c
c  Check that input file exists, if so open it.
c
      call open_input_file(infile,lun,errnum,inpstat)
c
c  Parse the top section of the input file containing general parameters.
c
      call parse_input_top(lun,mdtc,errnum,inpstat,inpath,outpath,
     & igrmode,igrmpath,chan1,chan2,flimit,pattern,srcindic,
     & dtcigrm,dtcspec,outfmt,delimit,minmax,stlimstd,stlimavg,
     & mnymxyth,xcorlim,scanlim,fftlim,proclim,verbose)
c
c  Position input file to first slice entry (or to end-of-file if none).
c
      call read_input_line(lun,errnum,inpstat,inpstr)
c
c  Initialize the memory of date to force reset of the run number.
c
      memyear=-1
      memmonth=-1
      memday=-1
      slice=-1    ! Avoid catalog work-around on first entry.
c
c  Main program loop, over the scan sets listed in the input file.
c
      do while((inpstat.eq.0).and.              ! Not at end-of-file
     &         (errnum.eq.0).and.               ! And no errors
     & ((scanlim.eq.0).or.(scancnt.lt.scanlim)))! And no/before time limit
c
c  Reset "Number of Sample Scans" in case there is an error detected
c  before it can be read from the slice headers.
c
        nss=0
c
c  Parse one line of the input file.
c
        slicmem=slice
        read(inpstr,*,iostat=inpstat)
     &   catyear,catmonth,catday,catbatch,slice
        if(inpstat.ne.0) then
          write(*,'(a)')
     &     'Error in input file: format error in catalog'
          errnum=1
          inpstat=0
        elseif(slicmem.eq.slice) then
          slice=slice+1
          write(*,'(a)')
     &     'ACHTUNG! catalog work-around activated: incremented slice'
        endif
c
c  Reset run number on date change.
c
        if(errnum.eq.0) then
          if((catyear.ne.memyear).or.
     &       (catmonth.ne.memmonth).or.
     &       (catday.ne.memday)) then
            runno=1
            memyear=catyear
            memmonth=catmonth
            memday=catday
          endif
c
c  See if we need to skip a run number: each line of the input file
c  starts with a FWD, and FWD runs must have an odd number.
c
          if(mod(runno,2).eq.0) runno=runno+1
        endif
c
c  Obtain all IFS125 parameters by reading the headers of the slice files.
c
        if((errnum.eq.0).and.(proclim.ge.1)) then
          call get_run_parameters(inpath,catyear,catmonth,catday,
     &     catbatch,slice,runno,verbose,mns,msl,mip,mch,mi4,mr8,
     &     chan1,chan2,mnymxy,mnymxyth,errnum,nptvec,bpdata,nss,tpx,
     &     timvec,timsli,runsta,runend,i4head,r8head)
        endif
c
c  Obtain auxiliary information produced by real-time software.
c
        if((errnum.eq.0).and.(proclim.ge.2)) then
          call get_run_info(inpath,catyear,catmonth,catday,
     &     catbatch,slice,runno,verbose,mif,mns,nss,i4head(i_sfmcode),
     &     stlimavg,stlimstd,runsta,timvec,infomat)
        endif
c
c  Setup the OPUS header items that will remain unchanged throughout
c  this scan set.  The running sample number (a "serial number") is set
c  to the starting slice number.  The scan duration is computed as the
c  number of data points divided by data rate.
c
        if((errnum.eq.0).and.(proclim.ge.1)) then
          i4head(i_rsn)=slice
          r8head(i_dur)=dble(tpx)/((2.d0*r8head(i_laserate)*
     &     dble(i4head(i_ssm)))/dble(i4head(i_ssp)))
c
c  FIXME: the zero-filling factor could be set to values greater than 2
c  if we allowed the FFT size to grow beyond the nearest power of two.
c
          i4head(i_zff)=2
c
c  Compute the folding limits for this spectral domain.  We don't use the
c  information from the interferogram headers because it is not accurate
c  enough.  We did make sure that LWN is correct, so we use that instead.
c
          minfold=0.d0
          maxfold=minfold+((r8head(i_lwn)*
     &     dble(i4head(i_ssm)))/dble(i4head(i_ssp)))
        endif
c
c  Now we process each scan from the set of NSS.  Scans that were rejected
c  or found in error have a negative 'runsta'.  There may be a limit on
c  the total number of scans that can be processed (for overnight checks).
c
        do scanno=1,nss
          if((runsta(scanno).gt.0).and.
     &     ((scanlim.eq.0).or.(scancnt.lt.scanlim))) then
c
c  Copy the column of 'infomat' for this scan into 'infovec'.
c
            do indexa=1,mif
              infovec(indexa)=infomat(indexa,scanno)
            enddo
c
c  Set run direction information based on parity of run number (odd=fwd).
c
            if(mod(runno,2).ne.0) then
              i4head(i_gfw)=1
            else
              i4head(i_gfw)=0
            endif
            i4head(i_gbw)=1-i4head(i_gfw)
c
c  Now we process each channel from this one scan.
c
            do channel=chan1,chan2
c
c  Read interferogram and if enabled save data in the requested format.
c
              if((errnum.eq.0).and.(proclim.ge.3)) then
                call get_run_data(inpath,catyear,catmonth,catday,
     &           catbatch,slice,runsta(scanno),runend(scanno),verbose,
     &           msl,mip,mch,nptvec,bpdata,channel,counter,buffer)

                if(counter.eq.(2*tpx)) then
                write(*,*)'SINGLE SLICE MULTI SCAN RUN - DOING FWD ONLY'
                  counter=tpx
                endif

                if(igrmode.eq.1) then
                  call build_cit_name(igrmpath,pattern,dtcigrm,srcindic,
     &             catyear,catmonth,catday,channel,i4head(i_srccode),
     &             runno,filename)
                  call save_to_file(1,flimit,filename,channel,outfmt,
     &             verbose,progver,mip,mif,mi4,mr8,buffer,counter,
     &             minfold,maxfold,delimit,minmax,tpx,timvec(scanno),
     &             i4head,r8head,infovec,tla_ext_full,errnum)
                endif
              endif
c
c  Perform Fourier transform and save data in the requested format.
c
              if((errnum.eq.0).and.(proclim.ge.4)) then
                call ipplite(mip,mi4,mr8,mphz,fftlim(channel),verbose,
     &           i4head(i_gfw),phzprm,buffer,counter,i4head,r8head)
                call build_cit_name(outpath,pattern,dtcspec,srcindic,
     &           catyear,catmonth,catday,channel,i4head(i_srccode),
     &           runno,filename)
                if(counter.gt.1) then
                  pointspac=(maxfold-minfold)/dble(counter-1)
                  maxfoldp1=maxfold+pointspac
                else
                  maxfoldp1=maxfold
                endif
                call save_to_file(2,flimit,filename,channel,outfmt,
     &           verbose,progver,mip,mif,mi4,mr8,buffer,counter,
     &           minfold,maxfoldp1,delimit,minmax,tpx,timvec(scanno),
     &           i4head,r8head,infovec,tla_ext_full,errnum)
              endif
            enddo      ! channel=chan1,chan2
            if(errnum.eq.0) scancnt=scancnt+1
          endif        ! (runsta(scanno).gt.0).and.((scanlim.eq.0).or.(....))
c
c  Increment the run number used for file names, unless this was an idle scan.
c
          if(runsta(scanno).ne.(-100)) runno=runno+1
        enddo          ! scanno=1,nss
c
c  Positive error codes indicate transient errors, specific to one set
c  of NSS scans.  The program can proceed to the next set of scans.
c
        if(errnum.gt.0) then
          errnum=0
        endif
c
c  Position input file to next slice entry or to end-of-file.
c
        call read_input_line(lun,errnum,inpstat,inpstr)

      enddo      ! while((inpstat.eq.0).and.(errnum.eq.0).and((scanlim.eq.0)...
c
c  Close the input file.
c
      call close_input_file(infile,lun,inpstat)
c
c  Information message if program maximum execution has been reached.
c
      if((scanlim.ne.0).and.(scancnt.ge.scanlim)) then
        write(*,'(a)') 'Maximum number of scans has been reached'
      endif
c
c  Terminate.
c
      stop
      end
c ===================================================================
      subroutine parse_input_top(luni,mdtc,errnum,inpstat,inpath,
     & outpath,igrmode,igrmpath,dtc1,dtc2,flimit,pattern,srcindic,
     & dtcigrm,dtcspec,outfmt,delimit,minmax,stlimstd,stlimavg,
     & mnymxyth,xcorlim,scanlim,fftlim,proclim,verbose)
c
c  Input:
c    luni      I*4    Logical Unit Number for the parameter input file
c    mdtc      I*4    Maximum number of detectors
c
c  Input/Output:
c    errnum    I*4    Error code (0=ok, <0=fatal, >0=recoverable)
c    inpstat   I*4    Value of IOSTAT return from input file read
c
c  Output:
c    inpath    C*(*)  Directory path to input slice files
c    outpath   C*(*)  Directory path for output spectrum files
c    igrmode   I*4    Interferogram saving mode (none/raw/deglitched)
c    igrmpath  C*(*)  Directory path for output interferogram files
c    dtc1      I*4    Starting detector number to process
c    dtc2      I*4    Ending detector number to process
c    flimit    C*(*)  Name of file containing the frequency limits
c    pattern   C*(*)  Pattern for CIT file-naming convention
c    srcindic  C*(*)  List of source indicators for output files
c    dtcigrm   C*(*)  List of detector indicators for interferogram files
c    dtcspec   C*(*)  List of detector indicators for spectrum files
c    outfmt    I*4    Format selection for output file
c    delimit   C*(*)  Field delimiter for ASCII output file
c    minmax    I*4    Count of min-max pairs in ASCII output file (0=no min-max)
c    stlimavg  R*4    Limit for suntracker average intensity
c    stlimstd  R*4    Limit for suntracker intensity standard deviation
c    mnymxyth(2,2)R*4  Threshold values for MNY and MXY
c    xcorlim(mdtc)R*4 Minimum value of the ZPD cross-correlation
c    scanlim   I*4    Maximum number of scans analyzed in a single execution
c    fftlim(mdtc)I*4  Maximum log-base-2 of the FFT size for each detector
c    proclim   I*4    Maximum processing stage performed by the program
c    verbose   I*4    Level of verbosity for displayed messages
c
      implicit none

      integer*4
     & luni,       ! Subroutine input argument (see above)
     & mdtc,       ! Subroutine input argument (see above)
     & inpstat,    ! Subroutine input/output argument (see above)
     & errnum,     ! Subroutine input/output argument (see above)
     & igrmode,    ! Subroutine output argument (see above)
     & dtc1,       ! Subroutine output argument (see above)
     & dtc2,       ! Subroutine output argument (see above)
     & outfmt,     ! Subroutine output argument (see above)
     & minmax,     ! Subroutine output argument (see above)
     & scanlim,    ! Subroutine output argument (see above)
     & fftlim(mdtc),!Subroutine output argument (see above)
     & proclim,    ! Subroutine output argument (see above)
     & verbose,    ! Subroutine output argument (see above)
     & indexa,     ! General loop index
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc        ! Integer function First Non-Blank Character in string

      real*4
     & stlimavg,   ! Subroutine output argument (see above)
     & stlimstd,   ! Subroutine output argument (see above)
     & mnymxyth(2,2),!Subroutine output argument (see above)
     & xcorlim(mdtc)!Subroutine output argument (see above)

      character
     & inpath*(*), ! Subroutine output argument (see above)
     & outpath*(*),! Subroutine output argument (see above)
     & igrmpath*(*),!Subroutine output argument (see above)
     & flimit*(*), ! Subroutine output argument (see above)
     & pattern*(*),! Subroutine output argument (see above)
     & srcindic*(*),!Subroutine output argument (see above)
     & dtcigrm*(*),! Subroutine output argument (see above)
     & dtcspec*(*),! Subroutine output argument (see above)
     & delimit*(*),! Subroutine output argument (see above)
     & inpstr*99,  ! String used to read numbers from input file
     & stringa*11, ! String used to format integer display
     & stringb*11  ! String used to format integer display

      logical*4
     & filexist    ! Keeps track of file existence
c
c  Read path to input files or special keyword.  Normally, the
c  first uncommented line in the input file holds the directory
c  name used for storage of the data slices.  However, the special
c  keyword "Resistors" will cause slice-ipp to create the header
c  file 'resistors.h' needed by the real-time software build.
c
      call read_input_line(luni,errnum,inpstat,inpath)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no path to data slices'
        errnum=-2
      elseif((errnum.eq.0).and.(index(inpath,"Resistors").eq.1)) then
        read(inpath,*,iostat=inpstat) inpstr,errnum
        if(inpstat.ne.0) then
          write(*,'(a)') 'Error in input file: bad resistor request'
          errnum=-2
        else
          call check_resistor_info(errnum)
          errnum=errnum+1000
        endif
      elseif(errnum.eq.0) then
        inquire(file=inpath,exist=filexist,iostat=inpstat)
        if(inpstat.ne.0) then
          errnum=-3
          write(*,'(2a)')'Error: inquire failed on path ',
     &     inpath(1:lnbc(inpath))
        elseif(.not.filexist) then
          errnum=-3
          write(*,'(3a)')'Error: path to data slices ',
     &     inpath(1:lnbc(inpath)),' does not exist'
        endif
      endif
c
c  Read path for output files.
c
      call read_input_line(luni,errnum,inpstat,outpath)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no path for spectra'
        errnum=-2
      elseif(errnum.eq.0) then
        inquire(file=outpath,exist=filexist,iostat=inpstat)
        if(inpstat.ne.0) then
          errnum=-4
          write(*,'(2a)')'Error: inquire failed on path ',
     &     outpath(1:lnbc(outpath))
        elseif(.not.filexist) then
          errnum=-4
          write(*,'(3a)')'Error: path for spectra ',
     &     outpath(1:lnbc(outpath)),' does not exist'
        endif
      endif
c
c  Read interferogram saving parameters.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no interferogram saving mode'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) igrmode
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'interferogram saving mode'
          errnum=-2
        elseif((igrmode.lt.0).or.(igrmode.gt.2)) then
          write(stringa,'(i11)') igrmode
          write(*,'(3a)')'Error in input file: ',
     &     'invalid interferogram saving mode of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
      call read_input_line(luni,errnum,inpstat,igrmpath)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no path for interferograms'
        errnum=-2
      elseif(errnum.eq.0) then
        inquire(file=igrmpath,exist=filexist,iostat=inpstat)
        if(inpstat.ne.0) then
          errnum=-5
          write(*,'(2a)')'Error: inquire failed on path ',
     &     igrmpath(1:lnbc(igrmpath))
        elseif(.not.filexist) then
          errnum=-5
          write(*,'(3a)')'Error: path for interferograms ',
     &     igrmpath(1:lnbc(igrmpath)),' does not exist'
        endif
      endif
c
c  Read starting and ending detector numbers to be processed.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no detector specified'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) dtc1,dtc2
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'detector selections'
          errnum=-2
        elseif((dtc1.lt.1).or.(dtc1.gt.mdtc)) then
          write(stringa,'(i11)') dtc1
          write(stringb,'(i11)') mdtc
          write(*,'(4a)')'Error in input file: starting detector of ',
     &     stringa(fnbc(stringa):11),
     &     ' is not between 1 and ',stringb(fnbc(stringb):11)
          errnum=-2
        elseif((dtc2.lt.1).or.(dtc2.gt.mdtc)) then
          write(stringa,'(i11)') dtc2
          write(stringb,'(i11)') mdtc
          write(*,'(4a)')'Error in input file: ending detector of ',
     &     stringa(fnbc(stringa):11),
     &     ' is not between 1 and ',stringb(fnbc(stringb):11)
          errnum=-2
        elseif((dtc1.gt.dtc2)) then
          write(stringa,'(i11)') dtc1
          write(stringb,'(i11)') dtc2
          write(*,'(5a)')'Error in input file.  ',
     &     'Incorrect detector order: ',
     &     stringa(fnbc(stringa):11),
     &     ' is after ',stringb(fnbc(stringb):11)
          errnum=-2
        endif
      endif
c
c  Read the full name of the frequency limit file.
c
      call read_input_line(luni,errnum,inpstat,flimit)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no frequency limit file' 
        errnum=-2
      elseif(errnum.eq.0) then
        inquire(file=flimit,exist=filexist,iostat=inpstat)
        if(inpstat.ne.0) then
          errnum=-6
          write(*,'(2a)')'Error: inquire failed on file ',
     &     flimit(1:lnbc(flimit))
        elseif(.not.filexist) then
          errnum=-6
          write(*,'(3a)')'Error: frequency limit file ',
     &     flimit(1:lnbc(flimit)),' does not exist'
        endif
      endif
c
c  Read pattern for output file naming.
c
      call read_input_line(luni,errnum,inpstat,pattern)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no output file pattern' 
        errnum=-2
      endif
c
c  Read source indicator.
c
      call read_input_line(luni,errnum,inpstat,srcindic)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no source naming pattern' 
        errnum=-2
      elseif((errnum.eq.0).and.(lnbc(srcindic).ne.3)) then
        write(stringa,'(i11)') lnbc(srcindic)
        write(*,'(4a)')'Error in input file: ',
     &   'source naming pattern is of length ',
     &   stringa(fnbc(stringa):11),' instead of 3'
        errnum=-2
      endif
c
c  Read detector indicators for interferogram file naming.
c
      call read_input_line(luni,errnum,inpstat,dtcigrm)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no interferogram naming pattern' 
        errnum=-2
      elseif((errnum.eq.0).and.(lnbc(dtcigrm).lt.dtc2)) then
        write(stringa,'(i11)') lnbc(dtcigrm)
        write(stringb,'(i11)') dtc2
        write(*,'(5a)')'Error in input file.  ',
     &   'Too few detector indicators for interferograms: got ',
     &   stringa(fnbc(stringa):11),
     &   ' but need ',stringb(fnbc(stringb):11)
        errnum=-2
      endif
c
c  Read detector indicators for spectrum file naming.
c
      call read_input_line(luni,errnum,inpstat,dtcspec)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no spectrum naming pattern' 
        errnum=-2
      elseif((errnum.eq.0).and.(lnbc(dtcspec).lt.dtc2)) then
        write(stringa,'(i11)') lnbc(dtcspec)
        write(stringb,'(i11)') dtc2
        write(*,'(5a)')'Error in input file.  ',
     &   'Too few detector indicators for spectra: got ',
     &   stringa(fnbc(stringa):11),
     &   ' but need ',stringb(fnbc(stringb):11)
        errnum=-2
      endif
c
c  Read output file format selection.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no output format selection'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) outfmt
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'output file format selection'
          errnum=-2
        elseif((outfmt.lt.0).or.(outfmt.gt.3)) then
          write(stringa,'(i11)') outfmt
          write(*,'(3a)')'Error in input file: ',
     &     'invalid output format of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
c
c  Read delimiter for ASCII output files.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if(errnum.eq.0) then
        if(inpstat.ne.0) then
          write(*,'(a)')
     &     'Error in input file: no delimiter for ASCII output files'
          errnum=-2
        elseif(lnbc(inpstr).ne.3) then
          write(stringa,'(i11)') lnbc(inpstr)
          write(*,'(4a)')'Error in input file: ',
     &     'ASCII delimiter selection is of length ',
     &     stringa(fnbc(stringa):11),' instead of 3'
          errnum=-2
        elseif((inpstr(1:1).ne.'"').or.(inpstr(3:3).ne.'"')) then
          write(*,'(2a)')'Error in input file: ',
     &     'ASCII delimiter must be enclosed in double-quotes'
          errnum=-2
        else
          delimit=inpstr(2:2)
        endif
      endif
c
c  Read number of min-max pairs for output ASCII file.  A value of zero
c  means that output is a straight conversion of input.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no min-max specified'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) minmax
        if(inpstat.ne.0) then
          write(*,'(a)')
     &     'Error in input file: format error in min-max selection'
          errnum=-2
        elseif(minmax.lt.0) then
          write(stringa,'(i11)') minmax
          write(*,'(3a)')'Error in input file: ',
     &     'invalid minmax specification of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
c
c  Read suntracker total intensity limits (STD and AVG).
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no suntracker intensity limits'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) stlimstd,stlimavg
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'suntracker intensity limits'
          errnum=-2
        elseif((stlimstd.lt.0.0).or.(stlimavg.lt.0.0)) then
          write(*,'(2a)')'Error in input file: ',
     &     'suntracker intensity limits must be positive'
          errnum=-2
        endif
      endif
c
c  Read MNY and MXY thresholds.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no MNY and MXY thresholds'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat)(mnymxyth(indexa,1),
     &                                mnymxyth(indexa,2),indexa=1,dtc2) 
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'MNY and MXY thresholds'
          errnum=-2
        endif
      endif
c
c  Read minimum value of the cross-correlation between runs.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no values for minimum cross-correlation'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat)(xcorlim(indexa),indexa=1,dtc2) 
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'values for minimum cross-correlation'
          errnum=-2
        else
          do indexa=1,dtc2
            if((errnum.eq.0).and.(xcorlim(indexa).lt.0.0)) then
              write(*,'(2a)')'Error in input file: ',
     &         'cross-correlation limits must be positive'
              errnum=-2
            endif
          enddo
        endif
      endif
c
c  Read maximum number of scans to analyze.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no maximum number of scans'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) scanlim
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'maximum number of scans'
          errnum=-2
        elseif(scanlim.lt.0) then
          write(stringa,'(i11)') scanlim
          write(*,'(3a)')'Error in input file: ',
     &     'invalid maximum number of scans of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
c
c  Read maximum log-base-2 of the FFT size.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no values for maximum FFT size'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat)(fftlim(indexa),indexa=1,dtc2) 
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'values of maximum FFT size'
          errnum=-2
        else
          do indexa=1,dtc2
            if((errnum.eq.0).and.(fftlim(indexa).lt.0)) then
              write(*,'(2a)')'Error in input file: ',
     &         'log-base-2 FFT size limits must be positive'
              errnum=-2
            endif
          enddo
        endif
      endif
c
c  Read maximum level of processing.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')
     &   'Error in input file: no maximum level of processing'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) proclim
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'maximum level of processing'
          errnum=-2
        elseif((proclim.lt.0).or.(proclim.gt.4)) then
          write(stringa,'(i11)') proclim
          write(*,'(3a)')'Error in input file: ',
     &     'invalid maximum level of processing of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
c
c  Read verbosity level.
c
      call read_input_line(luni,errnum,inpstat,inpstr)
      if((errnum.eq.0).and.(inpstat.ne.0)) then
        write(*,'(a)')'Error in input file: no verbosity specified'
        errnum=-2
      elseif(errnum.eq.0) then
        read(inpstr,*,iostat=inpstat) verbose
        if(inpstat.ne.0) then
          write(*,'(2a)')
     &     'Error in input file: format error in ',
     &     'verbosity specification'
          errnum=-2
        elseif((verbose.lt.1).or.(verbose.gt.5)) then
          write(stringa,'(i11)') verbose
          write(*,'(3a)')'Error in input file: ',
     &     'invalid verbosity specification of ',
     &     stringa(fnbc(stringa):11)
          errnum=-2
        endif
      endif
c
      return
      end
c ===================================================================
      subroutine get_run_parameters(inpath,catyear,catmonth,catday,
     & catbatch,slice,runno,verbose,mns,msl,mip,mch,mi4,mr8,
     & chan1,chan2,mnymxy,mnymxyth,errnum,nptvec,bpdata,nss,tpx,
     & timvec,timsli,runsta,runend,i4head,r8head)
c
c  Input:
c    inpath    C*(*)  Directory path to input slice files
c    catyear   I*4    Year from catalog
c    catmonth  I*4    Month from catalog
c    catday    I*4    Day from catalog
c    catbatch  I*4    Batch (save set) number from catalog
c    slice     I*4    Slice number from catalog, used in slice file name
c    runno     I*4    Run number increasing throughout the day
c    verbose   I*4    Level of verbosity for displayed messages
c    mns       I*4    Maximum number of interferograms per scan set (max NSS)
c    msl       I*4    Maximum number of interferogram slices per scan set
c    mip       I*4    Maximum number of input points
c    mch       I*4    Maximum number of data channels
c    mi4       I*4    Maximum number of I*4 items in file header
c    mr8       I*4    Maximum number of R*8 items in file header
c    chan1     I*4    Starting channel number to process
c    chan2     I*4    Ending channel number to process
c    mnymxyth(mch,2)R*4  Threshold values for MNY and MXY
c
c  Input/output:
c    errnum    I*4    Error code (0=ok, <0=fatal, >0=recoverable)
c
c  Output:
c    nptvec(msl)I*4   Number of PoinTs, from OPUS header
c    bpdata(msl,mch)I*4 Byte pointers into the data blocks of slices
c    nss       I*4    Number of Sample Scans in this set of slices
c    tpx       I*4    Number of points in one FWD or REV scan
c    timvec(mns)R*8   Time vector contains one entry for each scan
c    timsli(msl)R*8   Time vector contains one entry for each slice
c    runsta(mns)I*4   Run starting slice number (if > 0) or run error (if < 0)
c    runend(mns)I*4   Run ending slice number
c    i4head(mi4)I*4   Vector to hold the I*4 header items
c    r8head(mr8)R*8   Vector to hold the R*8 header items
c
c  No value passed either way, but "f2c" wants it declared in mainline:
c    mnymxy(msl,mch,2)R*4  Values of MNY and MXY from each channel and slice
c
      implicit none

      integer*4
     & catyear,    ! Subroutine input argument (see above)
     & catmonth,   ! Subroutine input argument (see above)
     & catday,     ! Subroutine input argument (see above)
     & catbatch,   ! Subroutine input argument (see above)
     & slice,      ! Subroutine input argument (see above)
     & runno,      ! Subroutine input argument (see above)
     & verbose,    ! Subroutine input argument (see above)
     & mns,        ! Subroutine input argument (see above)
     & msl,        ! Subroutine input argument (see above)
     & mip,        ! Subroutine input argument (see above)
     & mch,        ! Subroutine input argument (see above)
     & mi4,        ! Subroutine input argument (see above)
     & mr8,        ! Subroutine input argument (see above)
     & chan1,      ! Subroutine input argument (see above)
     & chan2,      ! Subroutine input argument (see above)
     & errnum,     ! Subroutine input/output argument (see above)
     & nptvec(msl),! Subroutine output argument (see above)
     & bpdata(msl,mch),! Subroutine output argument (see above)
     & nss,        ! Subroutine output argument (see above)
     & tpx,        ! Subroutine output argument (see above)
     & runsta(mns),! Subroutine output argument (see above)
     & runend(mns),! Subroutine output argument (see above)
     & i4head(mi4),! Subroutine output argument (see above)
     & slicnt,     ! Slice count, number of elements of npt and bpdata vectors
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc,       ! Integer function First Non-Blank Character in string
     & slicind,    ! Slice index
     & scancnt,    ! Count of scans
     & counter,    ! Count of data points
     & slice1,     ! First slice of a scan
     & errmem,     ! Value of errnum at start of scan splitting
     & indexa,     ! General loop index
     & jd,         ! Julian Day
     & j1,         ! Julian Day of 1-Jan-2000
     & zpdslic,    ! ZPD slice index
     & chan        ! Channel number (1=InGaAs=slave, 2=Si=master)

      real*4
     & mnymxyth(mch,2),! Subroutine input argument (see above)
     & mnymxy(msl,mch,2),! Values of MNY and MXY from each channel and slice
     & zpdval      ! Max value of mnymxy for one scan & one chan

      real*8
     & timvec(mns),! Subroutine output argument (see above)
     & timsli(msl),! Subroutine output argument (see above)
     & r8head(mr8),! Subroutine output argument (see above)
     & firminst    ! Time of the install of the "1.300 Apr 14 2004" firmware

      character
     & inpath*(*), ! Subroutine input argument (see above)
     & filename*99,! Full file name for data slices
     & stringa*11, ! String used to format integer display
     & stringb*11, ! String used to format integer display
     & stringc*11, ! String used to format integer display
     & stringd*11, ! String used to format integer display
     & rejlabl*35  ! String used for rejection message with run label

      logical*4
     & filexist,   ! Keeps track of file existence
     & inrun       ! True while inside NSS interferometer runs

      include 'opus-comn/header_indices.inc'
c
c  Write a blank line to the screen to separate scan sets.
c  (This write format produces the same output for all compilers)
c
      if(verbose.ge.3) then
        write(*,'("")')
      endif
c
c  Check that the first slice file exists.
c
      call build_slice_name(inpath,catyear,catmonth,catday,catbatch,
     & slice,filename)
      inquire(file=filename,exist=filexist)
      if(.not.filexist) then
        errnum=1
        write(*,'(3a)')'Error: first slice ',
     &   filename(1:lnbc(filename)),' does not exist (missing run)'
      endif
c
c  Initialize variables.
c
      inrun=filexist
      slicnt=0
      i4head(i_sfmcode)=0  ! Must be initialized: it is used to reject scans.
c
c  Loop over the slices that make up one scan set.
c
      do while(inrun.and.(errnum.eq.0))
c
c  Read full set of parameters from this slice.
c  Note: 'get_opusigram_params' sets 'inrun' to false at end of scan set.
c
        call get_opusigram_params(filename,verbose,msl,mch,mi4,mr8,
     &   chan1,chan2,errnum,slicnt,inrun,nptvec,bpdata,timsli,nss,tpx,
     &   i4head,r8head,mnymxy)
c
c  Check if next slice file exists if we are still inside of the scan set.
c
        if(inrun.and.(errnum.eq.0)) then
          call build_slice_name(inpath,catyear,catmonth,catday,
     &     catbatch,slice+slicnt,filename)
          inquire(file=filename,exist=filexist)
          if(.not.filexist) then
            errnum=3
            write(*,'(3a)')'Error: file ',
     &       filename(1:lnbc(filename)),
     &       ' does not exist (incomplete run)'
          endif
        endif
      enddo       ! while(inrun.and.(errnum.eq.0))
c
c  If slices cover more than one scan, divide TPX by two to separate
c  FWD from REV runs.
c
      if((errnum.eq.0).and.(nss.gt.1)) then
        if(mod(tpx,2).ne.0) then
          errnum=2
          write(stringa,'(i11)') tpx
          write(*,'(4a)')'Error: ',filename(1:lnbc(filename)),
     &     ' has odd TPX of ',stringa(fnbc(stringa):11)
        else
          tpx=tpx/2
        endif
      endif
c
c  Make sure TPX does not exceed MIP, the maximum number of input points.
c
      if((errnum.eq.0).and.(tpx.gt.mip)) then
        errnum=2
        write(stringa,'(i11)') tpx
        write(*,'(2a)')'Error: increase parameter MIP to ',
     &   stringa(fnbc(stringa):11)
      endif
c
c  Fix laser wavenumber: IFS125-1 instrument returns an incorrect value
c
      if((errnum.eq.0).and.
     & (dabs(r8head(i_lwn)-15798.1d0).lt.(0.01d0))) then
        r8head(i_lwn)=15798.0138d0
        if(verbose.ge.3) then
          write(*,'(a,f11.4)')
     &     'Replacing laser wavenumber of 15798.1 with',r8head(i_lwn)
        endif
      endif
c
c  Initialize scan-splitting variables.
c
      counter=0
      scancnt=0
      slice1=slice
      slicind=1
      errmem=errnum
      call julian(2004,8,11,jd)    ! Firmware "upgraded" on 11-Aug-2004
      call julian(2000,1,1,j1)
      jd=jd-j1
      firminst=dble(((jd*24)*60)*60)  ! FIXME: this could be a parameter
c
c  Loop over the slices in this scan set to separate the individual scans.
c
      do while ((errnum.eq.0).and.(slicind.le.slicnt))
        counter=counter+nptvec(slicind)
        if((counter.eq.(2*tpx)).and.(slicnt.eq.1)) then
          write(*,*)'SINGLE SLICE MULTI SCAN RUN - NO WARRANTEE'
          counter=tpx
        endif
        if((counter.eq.tpx).and.(scancnt.eq.mns)) then
          errnum=5
          write(*,'(a)') 'Error: increase parameter MNS'
        elseif(counter.eq.tpx) then
          if(verbose.ge.3) then
            write(stringa,'(i11)') runno+scancnt
            write(stringb,'(i11)') slice1
            write(*,'(4a)') 'Run ',stringa(fnbc(stringa):11),
     &       ' found, starting at slice ',stringb(fnbc(stringb):11)
          endif
          scancnt=scancnt+1
          runsta(scancnt)=0   ! Indicates no error found so far
c
c  Reject idle scans.
c
          if((runsta(scancnt).eq.0).and.
     &     (i4head(i_sfmcode).eq.sfm_idle)) then
            runsta(scancnt)=-100
            write(*,'(3a)') 'Reject: this run is an idle scan'
          endif
c
c  Reject scans affected by download stress: these have incorrect time.
c
          indexa=2
          do while ((runsta(scancnt).eq.0).and.
     &     (indexa.lt.(slice1-slice+3)))
            if(((timsli(indexa+1)-timsli(indexa)).gt.23.d0)
     &       .and.((timsli(indexa)-timsli(indexa-1)).ne.0.d0)) then
              runsta(scancnt)=-103
              call build_reject_label(catyear,catmonth,catday,
     &         runno+scancnt-1,rejlabl)
              write(*,'(2a)') rejlabl(1:lnbc(rejlabl)),
     &         ' is affected by download stress'
            endif
            indexa=indexa+1
          enddo
c
c  Reject scans with MNY or MXY that exceed the thresholds (dark or saturated).
c  We avoid checking MNY of ZPD slice, as modulation can produce a low value.
c
          do chan=chan1,chan2
            if (runsta(scancnt).eq.0) then
              zpdslic=slice1-slice+1
              zpdval=mnymxy(zpdslic,chan,2)
              do indexa=slice1-slice+2,slicind
                if (mnymxy(indexa,chan,2).gt.zpdval) then
                  zpdslic=indexa
                  zpdval=mnymxy(zpdslic,chan,2)
                endif
              enddo      ! indexa=slice1-slice+2,slicind
              do indexa=slice1-slice+1,slicind
                if (runsta(scancnt).ne.0) then
                  continue
                elseif ((mnymxy(indexa,chan,1).lt.mnymxyth(chan,1)).and.
     &                  (indexa.ne.zpdslic)) then
                  runsta(scancnt)=-106
                  call write_reject_msg_mnymxy(catyear,catmonth,catday,
     &             runno+scancnt-1,mnymxy(indexa,chan,1),
     &             mnymxyth(chan,1),chan,1)
                elseif (mnymxy(indexa,chan,2).gt.mnymxyth(chan,2)) then
                  runsta(scancnt)=-107
                  call write_reject_msg_mnymxy(catyear,catmonth,catday,
     &             runno+scancnt-1,mnymxy(indexa,chan,2),
     &             mnymxyth(chan,2),chan,2)
                endif 
              enddo      ! indexa=slice1-slice+1,slicind
            endif        ! runsta(scancnt).eq.0
          enddo          ! chan=chan1,chan2
c
c  For remaining runs, set starting+ending slices and time.
c
          if(runsta(scancnt).eq.0) then
            runsta(scancnt)=slice1
            runend(scancnt)=slice+slicind-1
            if((runend(scancnt)-runsta(scancnt)).lt.2) then
              timvec(scancnt)=timsli(slice1-slice+1) ! Low resolution scan
            elseif(timsli(slicind).lt.firminst) then ! Before firmware upgrade
              timvec(scancnt)=(2.d0*timsli(slice1-slice+2))
     &                             -timsli(slice1-slice+3)
            else                                     ! After firmware upgrade
              timvec(scancnt)=(3.d0*timsli(slice1-slice+2))
     &                       -(2.d0*timsli(slice1-slice+3))
            endif
          endif
          counter=0
          slice1=slice+slicind
c
c  If the point count exceeds the expected value of TPX, the run is
c  not ending on a slice boundary.  This is an error.
c
        elseif(counter.gt.tpx) then
          errnum=5
          write(stringa,'(i11)') runno+scancnt
          write(stringb,'(i11)') slice+slicind-1
          write(stringc,'(i11)') tpx
          write(stringd,'(i11)') counter
          write(*,'(8a)')'Error in run ',stringa(fnbc(stringa):11),
     &     ': slice ',stringb(fnbc(stringb):11),
     &     ' goes past the end of scan.  Expected ',
     &     stringc(fnbc(stringc):11),' points, but got ',
     &     stringd(fnbc(stringd):11)
        endif
        slicind=slicind+1
      enddo
c
c  At end-of-set check that all data was processed.  If not, the
c  last scan ended prematurely.
c
      if((errnum.eq.0).and.(counter.ne.0)) then
        errnum=5
        write(stringa,'(i11)') runno+scancnt
        write(stringb,'(i11)') slice+slicnt
        write(stringc,'(i11)') tpx
        write(stringd,'(i11)') counter
        write(*,'(8a)')'Error in run ',stringa(fnbc(stringa):11),
     &   ': slice ',stringb(fnbc(stringb):11),
     &   ' caused early scan termination.  Expected  ',
     &   stringc(fnbc(stringc):11),' points, but got ',
     &   stringd(fnbc(stringd):11)
      endif
c
c  If there was an error, flag undetected runs as invalid.
c
      do while (scancnt.lt.nss)
        scancnt=scancnt+1
        timvec(scancnt)=0.d0
        runsta(scancnt)=(-errnum)
      enddo
c
c  Restore error state to its value before scan splitting so that runs
c  before the error condition (if any) can be processed.
c
      errnum=errmem

      return
      end
c ===================================================================
      subroutine write_reject_msg_mnymxy(year,month,day,
     & runno,yvalue,ythresh,channel,mode_mnymxy)
c
c  Input:
c    year      I*4    Year from catalog
c    month     I*4    Month from catalog
c    day       I*4    Day from catalog
c    runno     I*4    Run number increasing throughout the day
c    yvalue    R*4    Value that triggered the reject
c    ythresh   R*4    Rejection threshold
c    channel   I*4    Data channel in which yvalue was found
c    mode_mnymxy I*4  Mode for either MNY or MXY message
c
c  Output: none (message written to stdout)
c
      implicit none

      integer*4
     & year,       ! Subroutine input argument (see above)
     & month,      ! Subroutine input argument (see above)
     & day,        ! Subroutine input argument (see above)
     & runno,      ! Subroutine input argument (see above)
     & channel,    ! Subroutine input argument (see above)
     & mode_mnymxy,! Subroutine input argument (see above)
     & fnbc,       ! Integer function First Non-Blank Character in string
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & valind,     ! Points to start of yvalue in character string
     & thrind      ! Points to start of ythresh in character string

      real*4
     & yvalue,     ! Subroutine input argument (see above)
     & ythresh     ! Subroutine input argument (see above)

      character
     & rejlabl*35, ! String used for rejection message with run label
     & valstr*11,  ! String used to hold yvalue
     & thrstr*11   ! String used to hold ythresh

      call build_reject_label(year,month,day,runno,rejlabl)

      write(valstr,'(f11.3)')yvalue
      valind=fnbc(valstr)

      write(thrstr,'(f11.3)')ythresh
      thrind=fnbc(thrstr)

      if (mode_mnymxy.eq.1) then
        write(*,'(6a,i1.1)') rejlabl(1:lnbc(rejlabl)),
     &   ' has MNY of ',valstr(valind:11),
     &   ' below threshold of ',thrstr(thrind:11),
     &   ' in channel ',channel
      else
        write(*,'(6a,i1.1)') rejlabl(1:lnbc(rejlabl)),
     &   ' has MXY of ',valstr(valind:11),
     &   ' above threshold of ',thrstr(thrind:11),
     &   ' in channel ',channel
      endif 

      return
      end
c ===================================================================
      subroutine default_run_info(mif,mns,infovec)
c
c  Input:
c    mif       I*4    Maximum number of info channels
c    mns       I*4    Maximum number of interferograms per scan set (max NSS)
c
c  Output:
c    infovec(mif,mns)R*8 Information produced by real-time algorithm
c
      implicit none

      integer*4
     & mif,        ! Subroutine input argument (see above)
     & mns,        ! Subroutine input argument (see above)
     & scanind     ! Scan index

      real*8
     & infovec(mif,mns)! Subroutine output argument (see above)
c
c  Fill output vectors with default or "not found" values.
c
      do scanind=1,mns
        infovec(1,scanind)=100.0d0       ! HUM  IFHum
        infovec(2,scanind)=27.0d0        ! TLP  IFSSrcT
        infovec(3,scanind)=0.71972d0     ! PIM  IFS_P
        infovec(4,scanind)=28.1d0        ! TSC  ScBlkl_T
        infovec(5,scanind)=1500.0d0      ! PGR  InGaAs_R
        infovec(6,scanind)=2800.0d0      ! PGR  Si_R
        infovec(7,scanind)=45.9448d0     ! LAT  Latitude
        infovec(8,scanind)=-90.2732d0    ! LON  Longitude
        infovec(9,scanind)=442.0d0       ! ALT  Altitude
        infovec(10,scanind)=2.7d0        ! WSA  Zeno_WindSpeed_avg
        infovec(11,scanind)=2.2d0        ! WSS  Zeno_WindSpeed_std
        infovec(12,scanind)=7.9d0        ! WSM  Zeno_WindSpeed_max
        infovec(13,scanind)=106.0d0      ! WDA  Zeno_WindDir_avg
        infovec(14,scanind)=79.6d0       ! WDS  Zeno_WindDir_std
        infovec(15,scanind)=24.3d0       ! TOU  Zeno_Temp_avg
        infovec(16,scanind)=48.5d0       ! HOU  Zeno_RH_avg
        infovec(17,scanind)=636.7d0      ! ZSA  Zeno_SolarRadiance_avg
        infovec(18,scanind)=0.8d0        ! ZSS  Zeno_SolarRadiance_std
        infovec(19,scanind)=965.1d0      ! POU  Zeno_Press_avg
        infovec(20,scanind)=0.0d0        ! ZRM  Zeno_Rain_max
        infovec(21,scanind)=9.0d0        ! ZLM  Zeno_Lightning_max
        infovec(22,scanind)=15.1d0       ! ZVA  Zeno_VBatt_avg
        infovec(23,scanind)=206.9d0      ! DAA  Dome_azi_avg
        infovec(24,scanind)=0.0d0        ! DSM  Dome_Status_max
        infovec(25,scanind)=205.7d0      ! SAA  ST_tpg_azi_avg
        infovec(26,scanind)=42.1d0       ! SEA  ST_tpg_ele_avg
        infovec(27,scanind)=0.0d0        ! STM  ST_TPS_max
        infovec(28,scanind)=-1.0d0       ! SIA  ST_t_int_avg  VALUE IMPORTANT
        infovec(29,scanind)=-1.0d0       ! SIS  ST_t_int_std  VALUE IMPORTANT
        infovec(30,scanind)=-0.5d0       ! SOA  ST_off_azi_avg
        infovec(31,scanind)=-0.1d0       ! SOE  ST_off_ele_avg
        infovec(32,scanind)=3.0d0        ! SDA  ST_Tdrift_avg
        infovec(33,scanind)=-9999999.0d0 ! IDA  IFSDT_avg IFS125_TIME - NTP_TIME
        infovec(34,scanind)=0.0d0        ! SFM  ScanType
        infovec(35,scanind)=-1.0d0       ! ISS  ScanStatus    VALUE IMPORTANT
      enddo
      return
      end
c ===================================================================
      subroutine get_run_info(inpath,catyear,catmonth,catday,
     & catbatch,slice,runno,verbose,mif,mns,nss,sfmcode,
     & stlimavg,stlimstd,runsta,timvec,infovec)
c
c  Input:
c    inpath    C*(*)  Directory path to info files
c    catyear   I*4    Year from catalog
c    catmonth  I*4    Month from catalog
c    catday    I*4    Day from catalog
c    catbatch  I*4    Batch (save set) number from catalog
c    slice     I*4    Slice number from catalog, used in slice file name
c    runno     I*4    Run number increasing throughout the day
c    verbose   I*4    Level of verbosity for displayed messages
c    mif       I*4    Maximum number of info channels
c    mns       I*4    Maximum number of interferograms per scan set (max NSS)
c    nss       I*4    Number of Sample Scans in this set of slices
c    sfmcode   I*4    Numerical code to identify scan type (Sample ForM)
c    stlimavg  R*4    Limit for suntracker average intensity
c    stlimstd  R*4    Limit for suntracker intensity standard deviation
c
c  Input/output:
c    runsta(mns)I*4   Run starting slice number (if > 0) or run error (if < 0)
c    timvec(mns)R*8   Time vector contains one entry for each scan
c
c  Output:
c    infovec(mif,mns)R*8 Information produced by real-time algorithm
c
      implicit none

      integer*4
     & catyear,    ! Subroutine input argument (see above)
     & catmonth,   ! Subroutine input argument (see above)
     & catday,     ! Subroutine input argument (see above)
     & catbatch,   ! Subroutine input argument (see above)
     & slice,      ! Subroutine input argument (see above)
     & runno,      ! Subroutine input argument (see above)
     & verbose,    ! Subroutine input argument (see above)
     & mif,        ! Subroutine input argument (see above)
     & mns,        ! Subroutine input argument (see above)
     & nss,        ! Subroutine input argument (see above)
     & sfmcode,    ! Subroutine input argument (see above)
     & runsta(mns),! Subroutine input/output argument (see above)
     & luni,       ! Logical Unit Number for info file read
     & inpstat,    ! Value of IOSTAT from parameter input file read
     & errcode,    ! Error code (0=ok)
     & scanind,    ! Scan index
     & indexa,     ! General loop index
     & indexb,     ! General loop index
     & fnbc,       ! Integer function First Non-Blank Character in string
     & lnbc        ! Integer function Last Non-Blank Character in string

      real*4
     & stlimavg,   ! Subroutine input argument (see above)
     & stlimstd    ! Subroutine input argument (see above)

      real*8
     & timvec(mns), ! Subroutine input/output argument (see above)
     & infovec(mif,mns),! Subroutine output argument (see above)
     & avgdrift     ! Average of time drifts

      character
     & inpath*(*), ! Subroutine input argument (see above)
     & filename*99,! Full file name for auxiliary info files
     & inpstr*999, ! String used to read entries from input file
     & stringa*11, ! String used to format integer display
     & stringb*11, ! String used to format integer/float display
     & stringc*11, ! String used to format integer/float display
     & rejlabl*35  ! String used for rejection message with run label

      logical*4
     & filexist    ! Keeps track of file existence

      parameter (luni=22)

      include 'opus-comn/header_indices.inc'
c
c  Fill critical elements of output vectors with "not found" values.
c
      do scanind=1,nss
        infovec(28,scanind)=-1.0d0       ! SIA  ST_t_int_avg  VALUE IMPORTANT
        infovec(29,scanind)=-1.0d0       ! SIS  ST_t_int_std  VALUE IMPORTANT
        infovec(33,scanind)=-9999999.0d0 ! IDA  IFSDT_avg IFS125_TIME - NTP_TIME
        infovec(35,scanind)=-1.0d0       ! ISS  ScanStatus    VALUE IMPORTANT
      enddo
c
c  Loop over the scans in this scan set to locate their info files.
c  We do this for scans that are not flagged in error by checking 'runsta'.
c
      do scanind=1,nss
        if(runsta(scanind).gt.0) then
          errcode=0
          call build_info_name(inpath,catyear,catmonth,catday,
     &     catbatch,slice,scanind-1,filename)
          inquire(file=filename,exist=filexist)
          if(filexist) then
            open(luni,file=filename,status='old')
            if(verbose.ge.3) then
              write(stringa,'(i11)') runno+scanind-1
              write(*,'(4a)') 'Run ',stringa(fnbc(stringa):11),
     &        ' auxiliary info: ',filename(1:lnbc(filename))
            endif
c
c  Reject runs that don't have info files
c
          else
            errcode=1
            runsta(scanind)=-105
            call build_reject_label(catyear,catmonth,catday,
     &       runno+scanind-1,rejlabl)
            write(*,'(3a)') rejlabl(1:lnbc(rejlabl)),
     &       ' is missing info file ',filename(1:lnbc(filename))
          endif
c
c  If found, read and parse the contents of the info file.
c
c FIXME: Turn most of this in a loop with strings and indices in include file.
c
          inpstat=0
          do while((inpstat.eq.0).and.     ! Not at end-of-file
     &             (errcode.eq.0))         ! And no errors
            call read_input_line(luni,errcode,inpstat,inpstr)

            if(index(inpstr,"ScanType").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              if((index(inpstr,"Error").le.0).and.
     &         (((sfmcode.eq.sfm_solar).and.
     &         (index(inpstr,"Solar").le.0)).or.
     &         ((sfmcode.eq.sfm_cell).and.
     &         (index(inpstr,"Cell").le.0)).or.
     &         ((sfmcode.eq.sfm_idle).and.
     &         (index(inpstr,"IdleScan").le.0)).or.
     &         ((sfmcode.eq.sfm_aeros).and.
     &         (index(inpstr,"Aerosol").le.0))))then
                if(verbose.ge.2) then
                  write(stringa,'(i11)') sfmcode
                  write(*,'(4a)')
     &             'Warning: scan type mismatch between code ',
     &             stringa(fnbc(stringa):11),' and ',
     &             inpstr(1:lnbc(inpstr))
                endif
              endif
            endif

            if(index(inpstr,"ScanStatus").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              if(index(inpstr,"OK").gt.0) then
                infovec(35,scanind)=0.d0
              elseif(index(inpstr,"Stress").gt.0) then
                infovec(35,scanind)=1.d0
              elseif(index(inpstr,"Error").gt.0) then
                infovec(35,scanind)=2.d0
              elseif(index(inpstr,"Reset").gt.0) then
                infovec(35,scanind)=3.d0
              endif
              if((verbose.ge.2).and.
     &         (nint(infovec(35,scanind)).ne.0)) then
                write(*,'(a)') 'Warning: scan status is not OK'
              endif
            endif

            if(index(inpstr,"IFHum").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(1,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for IFHum'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"IFSSrcT").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(2,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for IFSSrcT'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"IFS_P").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(3,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for IFS_P'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ScBlkl_T").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(4,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ScBlkl_T'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"InGaAs_R").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(5,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for InGaAs_R'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Si_R").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(6,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Si_R'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Latitude").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(7,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Latitude'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Longitude").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(8,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Longitude'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Altitude").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(9,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Altitude'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_WindSpeed_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(10,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &          'Warning: info file format error for Zeno_WindSpeed_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_WindSpeed_std").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(11,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &          'Warning: info file format error for Zeno_WindSpeed_std'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_WindSpeed_max").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(12,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &          'Warning: info file format error for Zeno_WindSpeed_max'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_WindDir_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(13,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &            'Warning: info file format error for Zeno_WindDir_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_WindDir_std").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(14,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &            'Warning: info file format error for Zeno_WindDir_std'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_Temp_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(15,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Zeno_Temp_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_RH_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(16,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Zeno_RH_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_SolarRadiance_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(17,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &      'Warning: info file format error for Zeno_SolarRadiance_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_SolarRadiance_std").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(18,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &      'Warning: info file format error for Zeno_SolarRadiance_std'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_Press_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(19,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Zeno_Press_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_Rain_max").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(20,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Zeno_Rain_max'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_Lightning_max").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(21,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &          'Warning: info file format error for Zeno_Lightning_max'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Zeno_VBatt_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(22,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Zeno_VBatt_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Dome_azi_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(23,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Dome_azi_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"Dome_Status_max").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(24,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for Dome_Status_max'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_tpg_azi_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(25,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_tpg_azi_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_tpg_ele_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(26,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_tpg_ele_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_TPS_max").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(27,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_TPS_max'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_t_int_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(28,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_t_int_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_t_int_std").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(29,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_t_int_std'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_off_azi_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(30,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_off_azi_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_off_ele_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(31,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_off_ele_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"ST_Tdrift_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(32,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for ST_Tdrift_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif

            if(index(inpstr,"IFSDT_avg").gt.0) then
              if(verbose.ge.4) then
                write(*,*) inpstr(1:lnbc(inpstr))
              endif
              indexb=index(inpstr,":")
              read(inpstr(indexb+1:lnbc(inpstr)),*,iostat=inpstat)
     &         infovec(33,scanind)
              if(inpstat.ne.0) then
                if(verbose.ge.2) then
                  write(*,'(a)')
     &             'Warning: info file format error for IFSDT_avg'
                endif
                errcode=2
                inpstat=0
              endif
            endif
          enddo   ! while((inpstat.eq.0).and. [...]
c
c  If the instrument is vented, replace the IFS125 pressure reading
c  with the more accurate Zeno value.  This is necessary for the
c  air-to-vacuum correction of the wavenumber scale.
c
          if((errcode.eq.0).and.(infovec(3,scanind).gt.400.d0)) then
            if(verbose.ge.2) then
              write(*,'(2a)') 'Warning: instrument is vented, ',
     &         'replacing its pressure with external value'
            endif
            infovec(3,scanind)=infovec(19,scanind)
          endif
c
c  Reject scans based on solar tracker total intensity.
c
          if((sfmcode.eq.sfm_solar)
     &     .and.(nint(infovec(28,scanind)).ne.(-1))
     &     .and.(stlimavg.ne.0.0)
     &     .and.(infovec(28,scanind).le.dble(stlimavg))) then
            runsta(scanind)=-101
            write(stringb,'(f11.1)') infovec(28,scanind)
            write(stringc,'(f11.1)') stlimavg
            call build_reject_label(catyear,catmonth,catday,
     &       runno+scanind-1,rejlabl)
            write(*,'(5a)') rejlabl(1:lnbc(rejlabl)),
     &       ' has solar intensity AVG of ',
     &       stringb(fnbc(stringb):index(stringb,".")+1),' <= ',
     &       stringc(fnbc(stringc):index(stringc,".")+1)
          endif

          if((sfmcode.eq.sfm_solar)
     &     .and.(nint(infovec(28,scanind)).ne.(-1))
     &     .and.(nint(infovec(29,scanind)).ne.(-1))
     &     .and.(stlimstd.ne.0.0)
     &     .and.(infovec(29,scanind).ge.
     &          (dble(stlimstd)*infovec(28,scanind)))) then
            runsta(scanind)=-102
            write(stringb,'(f11.1)') infovec(29,scanind)
            write(stringc,'(f11.1)') dble(stlimstd)*infovec(28,scanind)
            call build_reject_label(catyear,catmonth,catday,
     &       runno+scanind-1,rejlabl)
            write(*,'(5a)') rejlabl(1:lnbc(rejlabl)),
     &       ' has solar intensity STD of ',
     &       stringb(fnbc(stringb):index(stringb,".")+1),' >= ',
     &       stringc(fnbc(stringc):index(stringc,".")+1)
          endif

        endif
      enddo   ! scanind=1,nss
c
c  Check consistency of time drifts.
c
      do scanind=1,nss-1
        if((nint(infovec(33,scanind)).ne.(-9999999)).and.
     &   (nint(infovec(33,scanind+1)).ne.(-9999999)).and.
     &   (dabs(infovec(33,scanind)-infovec(33,scanind+1)).gt.2.d0))then
          write(stringa,'(f11.1)') infovec(33,scanind)
          write(stringb,'(f11.1)') infovec(33,scanind+1)
          if(dabs(infovec(33,scanind)).lt.
     &     dabs(infovec(33,scanind)+3600.d0)) then
            if(dabs(infovec(33,scanind)).lt.
     &       dabs(infovec(33,scanind+1))) then
              infovec(33,scanind+1)=infovec(33,scanind)
            else
              infovec(33,scanind)=infovec(33,scanind+1)
            endif
          else
            if(dabs(infovec(33,scanind)+3600.d0).lt.
     &       dabs(infovec(33,scanind+1)+3600.d0)) then
              infovec(33,scanind+1)=infovec(33,scanind)
            else
              infovec(33,scanind)=infovec(33,scanind+1)
            endif
          endif
          if(verbose.ge.2) then
            write(stringc,'(f11.1)') infovec(33,scanind)
            write(*,'(6a)') 'Warning: inconsistent time drifts ',
     &       stringa(fnbc(stringa):),' and ',
     &       stringb(fnbc(stringb):),', using ',
     &       stringc(fnbc(stringc):)
          endif
        endif
      enddo   ! scanind=1,nss-1
c
c  Compute average of individual scan time drifts.
c
      avgdrift=0.d0
      indexa=0
      do scanind=1,nss
        if(nint(infovec(33,scanind)).ne.(-9999999)) then
          avgdrift=avgdrift+infovec(33,scanind)
          indexa=indexa+1
        endif
      enddo
      if(indexa.ne.0) avgdrift=avgdrift/dble(indexa)
      if(verbose.ge.3) then
        write(*,'(a,f7.1)')'Average time drift',avgdrift
      endif
c
c  Apply average time drift correction to the slices of the whole scan set.
c
      do indexa=1,nss
        timvec(indexa)=timvec(indexa)-avgdrift
      enddo

      return
      end
c ===================================================================
      subroutine get_run_data(inpath,catyear,catmonth,catday,
     & catbatch,slice,slice1,slicen,verbose,
     & msl,mip,mch,nptvec,bpdata,channel,counter,buffer)
c
c  Input:
c    inpath    C*(*)  Directory path to input slice files
c    catyear   I*4    Year from catalog
c    catmonth  I*4    Month from catalog
c    catday    I*4    Day from catalog
c    catbatch  I*4    Batch (save set) number from catalog
c    slice     I*4    Slice number from catalog, first slice of the scan set
c    slice1    I*4    Starting slice number for this run
c    slicen    I*4    Ending slice number for this run
c    verbose   I*4    Level of verbosity for displayed messages
c    msl       I*4    Maximum number of interferogram slices per scan set
c    mip       I*4    Maximum number of input points
c    mch       I*4    Maximum number of data channels
c    nptvec(msl)I*4   Number of PoinTs, from OPUS header
c    bpdata(msl,mch)I*4 Byte pointers into the data blocks of slices
c    channel   I*4    Channel number (1=InGaAs=slave, 2=Si=master)
c
c  Output:
c    counter   I*4    Point count, data destination into 'buffer'
c    buffer(mip)R*4   Data storage and FFT buffer
c
      implicit none

      integer*4
     & catyear,    ! Subroutine input argument (see above)
     & catmonth,   ! Subroutine input argument (see above)
     & catday,     ! Subroutine input argument (see above)
     & catbatch,   ! Subroutine input argument (see above)
     & slice,      ! Subroutine input argument (see above)
     & slice1,     ! Subroutine input argument (see above)
     & slicen,     ! Subroutine input argument (see above)
     & verbose,    ! Subroutine input argument (see above)
     & msl,        ! Subroutine input argument (see above)
     & mip,        ! Subroutine input argument (see above)
     & mch,        ! Subroutine input argument (see above)
     & nptvec(msl),! Subroutine input argument (see above)
     & bpdata(msl,mch),! Subroutine input argument (see above)
     & channel,    ! Subroutine input argument (see above)
     & counter,    ! Subroutine output argument (see above)
     & iendian,    ! Endianness of computer
     & bigendian,  ! Named constant for big endian detection 
     & luns,       ! Logical Unit Number for slice input file
     & reclen,     ! Record length in bytes for single values
     & blklen,     ! Record length in bytes for data blocks
     & blkpnt,     ! Number of data points in a block
     & slicind,    ! Slice index
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc,       ! Integer function First Non-Blank Character in string
     & npts,       ! Number of points in slice
     & inpoint,    ! Byte pointer to data values in slice
     & rs1,        ! Starting record number single value reads at start
     & re1,        ! Ending record number single value reads at start
     & rs2,        ! Starting record number block reads
     & re2,        ! Ending record number block reads
     & rs3,        ! Starting record number single value reads at end
     & re3,        ! Ending record number single value reads at end
     & recnum,     ! Number of current data record from slice file
     & indexa      ! Index into buffer during block read and byte swapping

      parameter (bigendian=1)
      parameter (luns=21)
      parameter (reclen=4)
      parameter (blklen=2**9)
      parameter (blkpnt=blklen/reclen)

      real*4 buffer(mip) ! Subroutine output argument (see above)

      character
     & inpath*(*), ! Subroutine input argument (see above)
     & filename*99,! Full file name for data slices
     & stringa*11, ! String used to format integer display
     & stringb*11  ! String used to format integer display
c
c  Initialize variables.
c
      call getendian(iendian)
      slicind=slice1
      counter=0
c
c  Loop over the slices that make up this run.
c
      do while (slicind.le.slicen)
        inpoint=bpdata(slicind-slice+1,channel)
        npts=nptvec(slicind-slice+1)

        call build_slice_name(inpath,catyear,catmonth,catday,catbatch,
     &   slicind,filename)

        if(verbose.ge.4) then
          write(stringa,'(i11)') npts
          write(stringb,'(i11)') inpoint
          write(*,'(5a)') filename(1:lnbc(filename)),
     &     '  npts=',stringa(fnbc(stringa):11),
     &     '  byte=',stringb(fnbc(stringb):11)
        endif
c
c  We pre-compute the limits of three zones for reading the data.  Zone 1
c  is read in one-point increments until we get to a block boundary.
c  Zone 2 is read in full blocks.  Zone 3 goes back to one-point reads
c  to the end of the data section.
c
c  First compute rsX and reX in units of bytes, numbered from 0.
c
        rs1=inpoint
        rs2=(inpoint/blklen)*blklen
        if(rs2.lt.inpoint) rs2=rs2+blklen
        rs3=((inpoint+(npts*reclen))/blklen)*blklen

        re1=rs2-1
        re2=rs3-1
        re3=(inpoint+(npts*reclen))-1 
c
c  Now convert rsX and reX to blocks, numbered from 1.
c
        rs1=1+(rs1/reclen)
        re1=1+(re1/reclen)
        rs2=1+(rs2/blklen)
        re2=1+(re2/blklen)
        rs3=1+(rs3/reclen)
        re3=1+(re3/reclen)
c
c  Convert sample count to a pointer into an array numbered from 1.
c
        counter=counter+1
c
c  Perform file read in three zones (points/blocks/points).
c
        open(luns,file=filename,form='unformatted',status='old',
     &   access='direct',recl=reclen) 
        do recnum=rs1,re1
          read(luns,rec=recnum) buffer(counter)
          counter=counter+1
        enddo    ! recnum=rs1,re1
        close(luns)

        open(luns,file=filename,form='unformatted',status='old',
     &   access='direct',recl=blklen) 
        do recnum=rs2,re2
          read(luns,rec=recnum)
     &     (buffer(indexa),indexa=counter,counter+blkpnt-1)
          counter=counter+blkpnt
        enddo    ! recnum=rs2,re2
        close(luns)

        open(luns,file=filename,form='unformatted',status='old',
     &   access='direct',recl=reclen) 
        do recnum=rs3,re3
          read(luns,rec=recnum) buffer(counter)
          counter=counter+1
        enddo    ! recnum=rs3,re3
        close(luns)
c
c  Convert pointer back to sample count.
c
        counter=counter-1
c
c  On big-endian computer (e.g. SPARC and G4) swap data bytes.
c
        if(iendian.eq.bigendian) then
          do indexa=counter-npts+1,counter
            call r4rev(buffer(indexa))
          enddo
        endif
c
c  Done with this slice...
c
        slicind=slicind+1
      enddo  ! while (slicind.le.slicen)

      return
      end
c ===================================================================
      subroutine check_resistor_info(code)

      implicit none

      integer*4
     & code,
     & mch,        ! Maximum number of detector types
     & npgn,       ! Number of gain settings in pre-amp
     & mrci,       ! Maximum R_Config_Index (revisions of PA gains)
     & luns        ! Logical Unit Number for output file
      parameter (mch=3)   ! FIXME: this should not be repeated ; ADDED InSb
      parameter (npgn=4)
      parameter (mrci=4)
      parameter (luns=21)

      integer*4
     & resistors(npgn,mch,mrci), ! 3-D matrix to hold all R values
     & channel,    ! Channel number (1=InGaAs=slave, 2=Si=master)
     & ipgn,       ! Index into Pre-amp GaiNs
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc        ! Integer function First Non-Blank Character in string

      character
     & chan(mch)*6,! Identifiers for each channel (detector name)
     & stringa*11  ! String used to format integer display

      data resistors /1500,1820,750000,2700,    ! InGaAs, revision 1
     &                2800,3320,4700000,20000,  ! Si, revision 1
     &                0,0,0,0,                  ! InSb, none in revision 1
     &                3090,3650,750000,2700,    ! InGaAs, revision 2
     &                7680,9090,4700000,20000,  ! Si, revision 2
     &                0,0,0,0,                  ! InSb, none in revision 2
     &                3090,3650,750000,2700,    ! InGaAs, revision 3
     &                7680,9090,1500000,20000,  ! Si, revision 3
     &                0,0,0,0,                  ! InSb, none in revision 3
     &                3090,3650,750000,2700,    ! InGaAs, revision 4
     &                7680,9090,1500000,20000,  ! Si, revision 4
     &                1500,2000,3010,6800/      ! InSb, revision 4
      data chan /'InGaAs','Si    ','InSb  '/

      open(luns,file='resistors.h')
c
c  If input error, produce a header file that will cause a compilation
c  error.  Also send a message to the standard output.
c
      if((code.le.0).or.(code.gt.mrci)) then
        write(stringa,'(i11)') code
        write(luns,'(2a)')
     &   '#error Invalid resistor configuration index of ',
     &   stringa(fnbc(stringa):11)
        write(*,'(2a)')
     &   'Error: invalid resistor configuration index of ',
     &   stringa(fnbc(stringa):11)
c
c  If no error, produce header file.  We loop over the detector channels,
c  then over the resistors in each channel.
c
      else
        write(luns,'(a)')'/* resistors.h Defines resistor strings */'
        write(luns,'(a)')'#ifndef RESISTORS_H_INCLUDED'
        write(luns,'(a)')'#define RESISTORS_H_INCLUDED'
        write(luns,*)
        write(stringa,'(i11)') code
        write(luns,'(2a)')'#define R_Config_Index ',
     &   stringa(fnbc(stringa):11)
        write(luns,*)

        do channel=1,mch
          write(luns,'(3a)')'static const char *',
     &     chan(channel)(1:lnbc(chan(channel))),'_R[] = {'
          do ipgn=1,npgn
            write(stringa,'(i11)') resistors(ipgn,channel,code)
            write(luns,'(3a)')'  "',stringa(fnbc(stringa):11),'",'
          enddo
          write(luns,'(a)')'};'
          write(luns,*)
        enddo

        write(luns,'(a)')'#endif'
      endif
      close(luns)
      return
      end
c ===================================================================
      subroutine build_slice_name(inpath,year,month,day,batch,slice,
     & filename)
c
c  Input:
c    inpath    C*(*)  Directory path to interferogram slice files
c    year      I*4    Year from catalog
c    month     I*4    Month from catalog
c    day       I*4    Day from catalog
c    batch     I*4    Batch number from catalog
c    slice     I*4    Slice number from catalog
c
c  Output:
c    filename  C*(*)  Full file name for interferogram slice
c
      implicit none

      integer*4
     & year,       ! Subroutine input argument (see above)
     & month,      ! Subroutine input argument (see above)
     & day,        ! Subroutine input argument (see above)
     & batch,      ! Subroutine input argument (see above)
     & slice,      ! Subroutine input argument (see above)       
     & inlen,      ! Length of path to input slice files
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc,       ! Integer function First Non-Blank Character in string
     & shortyear,  ! Last two digits of year
     & batchind,   ! Points to start of batch number in character string
     & sliceind    ! Points to start of slice number in character string

      character
     & inpath*(*),  ! Subroutine input argument (see above)       
     & filename*(*),! Subroutine output argument (see above)
     & dirsep,     ! Directory separator
     & batchstr*11,! String used to hold batch number
     & slicestr*11 ! String used to hold slice number

      inlen=lnbc(inpath)
      shortyear=mod(year,100)
      dirsep=inpath(inlen:inlen)

      write(batchstr,'(i11)')batch
      batchind=fnbc(batchstr)

      write(slicestr,'(i11)')slice
      sliceind=fnbc(slicestr)

      write(filename,'(a,i2.2,i2.2,i2.2,8a)')inpath(1:inlen),
     & shortyear,month,day,'.',batchstr(batchind:11),
     & dirsep,'scan',dirsep,'b',slicestr(sliceind:11),'.0'

      return
      end
c ===================================================================
      subroutine build_info_name(inpath,year,month,day,batch,slice,
     & scan,filename)
c
c  Input:
c    inpath    C*(*)  Directory path to interferogram slice files
c    year      I*4    Year from catalog
c    month     I*4    Month from catalog
c    day       I*4    Day from catalog
c    batch     I*4    Batch number from catalog
c    slice     I*4    Slice number from catalog
c    scan      I*4    Scan number, starting at 0 
c
c  Output:
c    filename  C*(*)  Full file name for information file
c
      implicit none

      integer*4
     & year,       ! Subroutine input argument (see above)
     & month,      ! Subroutine input argument (see above)
     & day,        ! Subroutine input argument (see above)
     & batch,      ! Subroutine input argument (see above)
     & slice,      ! Subroutine input argument (see above)       
     & scan,       ! Subroutine input argument (see above)       
     & inlen,      ! Length of path to input slice files
     & lnbc,       ! Integer function Last Non-Blank Character in string
     & fnbc,       ! Integer function First Non-Blank Character in string
     & shortyear,  ! Last two digits of year
     & batchind,   ! Points to start of batch number in character string
     & sliceind,   ! Points to start of slice number in character string
     & scanind     ! Points to start of scan number in character string

      character
     & inpath*(*),  ! Subroutine input argument (see above)       
     & filename*(*),! Subroutine output argument (see above)
     & dirsep,     ! Directory separator
     & batchstr*11,! String used to hold batch number
     & slicestr*11,! String used to hold slice number
     & scanstr*11  ! String used to hold scan number

      inlen=lnbc(inpath)
      shortyear=mod(year,100)
      dirsep=inpath(inlen:inlen)

      write(batchstr,'(i11)')batch
      batchind=fnbc(batchstr)

      write(slicestr,'(i11)')slice
      sliceind=fnbc(slicestr)

      write(scanstr,'(i11)')scan
      scanind=fnbc(scanstr)

      write(filename,'(a,i2.2,i2.2,i2.2,10a)')inpath(1:inlen),
     & shortyear,month,day,'.',batchstr(batchind:11),
     & dirsep,'scan',dirsep,'b',slicestr(sliceind:11),'.',
     & scanstr(scanind:11),'.info'

      return
      end
c ===================================================================
      subroutine build_reject_label(year,month,day,runno,label)
c
c  Input:
c    year      I*4    Year from catalog
c    month     I*4    Month from catalog
c    day       I*4    Day from catalog
c    runno     I*4    Run number increasing throughout the day
c
c  Output:
c    label     C*(*)  String used in the rejection message
c
      implicit none

      integer*4
     & year,       ! Subroutine input argument (see above)
     & month,      ! Subroutine input argument (see above)
     & day,        ! Subroutine input argument (see above)
     & runno,      ! Subroutine input argument (see above)
     & fnbc,       ! Integer function First Non-Blank Character in string
     & runind      ! Points to start of run number in character string

      character
     & label*(*),  ! Subroutine output argument (see above)
     & runstr*11   ! String used to hold run number

      write(runstr,'(i11)')runno
      runind=fnbc(runstr)

      write(label,'(3a,i4.4,i2.2,i2.2)') 'Reject: run ',
     & runstr(runind:11),' of ',year,month,day

      return
      end
c ===================================================================
      include 'opus-comn/read_input_line.f'
      include 'opus-comn/get_opus_xx.f'
      include 'opus-comn/save_to_file.f'
      include 'opus-comn/build_cit_name.f'
      include 'opus-comn/get_opusigram_params.f'
      include 'comn/lnbc.f'
      include 'comn/fnbc.f'
      include 'comn/getendian.f'
      include 'comn/xxrev.f'
      include 'comn/julian.f'
      include '../ipp/parse_input/open_input_file.f'
      include '../ipp/parse_input/close_input_file.f'
      include '../ipp/parse_input/read_params.f'

      include 'ipp-lite.f'
c-ftnchek      include '../misc/ipp-lite-stub.f'
