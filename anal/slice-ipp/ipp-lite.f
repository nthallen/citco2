c ===================================================================
      subroutine ipplite(mip,mi4,mr8,mphz,mlfft,verbose,idir,phzprm,
     &                   rbuf,nip,i4head,r8head)
c
c  Input:
c    mip       I*4    Maximum number of input points
c    mi4       I*4    Maximum number of I*4 items in file header
c    mr8       I*4    Maximum number of R*8 items in file header
c    mphz      I*4    Maximum number of phase parameters
c    mlfft     I*4    Maximum log base 2 of half-FFT size
c    verbose   I*4    Level of verbosity for displayed messages
c    idir      I*4    Run direction 1=FWD ; (-1 or 0)=REV
c    phzprm(mphz)I*4  Phase parameters
c
c  Input/output:
c    rbuf(mip) R*4    Input interferogram and output spectrum
c    nip       I*4    Number of input and output points
c
c  Output:
c    i4head(mi4)I*4   Vector holding the I*4 header items
c    r8head(mr8)R*8   Vector holding the R*8 header items
c
      implicit none

      integer*4
     & mip,        ! Subroutine input argument (see above)
     & mi4,        ! Subroutine input argument (see above)
     & mr8,        ! Subroutine input argument (see above)
     & mphz,       ! Subroutine input argument (see above)
     & mlfft,      ! Subroutine input argument (see above)
     & verbose,    ! Subroutine input argument (see above)
     & idir,       ! Subroutine input argument (see above)
     & phzprm(mphz),! Subroutine input argument (see above)
     & nip,        ! Subroutine input/output argument (see above)
     & i4head(mi4),! Subroutine output argument (see above)
     & nfft,       ! Half size of the high-res FFT
     & izpd,       ! Point index (location) of ZPD
     & pinl,       ! Peak INterferogram Location
     & fnbc,       ! Integer function First Non-Blank Character in string
     & indexa,     ! General loop index
     & counter,    ! Number of points over which to estimate the DC-term
     & kip,
     & nphr,       ! Number of points for PHase Resolution (half size of FFT)
     & lpco,       ! Length of Phase Correction Operator
     & nburst      ! Number of interferogram points to be tested for ZPD

      parameter (
     & nphr=512,
     & lpco=1024,
     & nburst=15)

      real*4
     & rbuf(mip),  ! Subroutine input/output argument (see above)
     & pinv,       ! Peak INterferogram Value
     & dcval,      ! Interferogram DC component, measured at max OPD
     & best,
     & cr(2*nphr), ! Holds the interferogram center-burst for phase calcs
     & arraytmp(1),! Used to fix gfortran warning "Rank mismatch in argument"
     & y_nyquist,  ! Variable to save the high-resolution Nyquist value
     & thresh,     ! Phase Correction Threshold
     & phavg

      real*8
     & r8head(mr8),! Subroutine output argument (see above)
     & zpdl        ! From m4head.inc

      character
     & stringa*11  ! String used to format integer display

      include 'opus-comn/header_indices.inc'

      i4head(i_nphr)=nphr
      i4head(i_pcl)=lpco
      if(phzprm(1).lt.0) then
        thresh=0.02   !  GCT 25-Feb-2000
      else
        thresh=float(phzprm(1))/1000.0
      endif
      r8head(i_pct)=dble(thresh)

c  Set FFT size to accept the full interferogram without exceeding
c  the log-base-2 limit 'mlfft' obtained from the input file.
      nfft=1
      indexa=0
      do while((nfft.lt.nip).and.(indexa.lt.mlfft))
        nfft=nfft*2
        indexa=indexa+1
      enddo
      if(verbose.ge.4) then
        write(*,*)'NIP=',nip
        write(*,*)'NFFT=',nfft
      endif
      if((nfft*2).gt.mip) then
        write(stringa,'(i11)') (nfft*2)
        write(*,'(2a)')'Error: increase parameter MIP to ',
     &   stringa(fnbc(stringa):11)
        stop
      endif

c  Find maximum absolute interferogram value
      pinl=0
      pinv=0.0
      if(idir.le.0) then
        do indexa=max0(1,nip-300000),nip    ! reverse run
          if(abs(rbuf(indexa)).gt.abs(pinv)) then
            pinv=rbuf(indexa)
            pinl=indexa
          endif
        enddo  !  indexa=1,nip
      else
        do indexa=1,min0(nip,300000)        ! forward run
          if(abs(rbuf(indexa)).gt.abs(pinv)) then
            pinv=rbuf(indexa)
            pinl=indexa
          endif
        enddo  !  indexa=1,nip
      endif
      if(verbose.ge.3) then
        write(*,*)'Extremum value of ',pinv,' at point ',pinl
      endif

c  Reverse interferogram are reversed in-place
      if(idir.le.0) then
ct        rund='R '
        call vswap(rbuf,1,rbuf(nip),-1,nip/2)
        pinl=1+nip-pinl
      else
ct        rund='F '
      endif

c  Truncate long side of interferograms which exceed NFFT
      kip=nip
      if(nip.ge.nfft) then
        kip=nfft-1
        if(verbose.ge.3) then
          write(6,*) 'Truncating interferogram from',nip,' to',kip
        endif
      endif
c
c  Estimate the DC term from the section of the interferogram at max OPD.
c
      dcval=0.0
      counter=min0(kip,1000)
      indexa=kip
      do while (counter.gt.0)
        dcval=dcval+rbuf(indexa)
        indexa=indexa-1
        counter=counter-1
      enddo
      dcval=dcval/float(min0(kip,1000))
c
c  DC-fill the uninitialized section of rbuf up to size nfft.
c  This is necessary because the phase correction convolution is
c  performed via an FFT of size nfft.  This would not be needed
c  if we used the (slower) dot product convolution.
c
      arraytmp(1)=dcval
      call vmov(arraytmp,0,rbuf(kip+1),1,nfft-kip)

c  Find interferogram point closest to the maximum in symmetry.
      if(nphr.gt.pinl) then
        write(6,*) 'Warning: NPHR > PINL, forced PINL to 42000'
c        write(6,*) 'Error: NPHR > PINL'
        pinl=42000                      ! HABOMINABLE KLUDGE
c        stop
      endif
      call bestzpd(rbuf(pinl),lpco,nburst,best)
      zpdl=dble(pinl)+dble(best)
      izpd=nint(zpdl)
c      izpd=pinl   !  Old way of defining ZPD
      r8head(i_zpa)=dble(rbuf(izpd))
      if(verbose.ge.3) then
        write(*,*)'Best ZPD at point ',zpdl
      endif

c  Copy interferogram center burst to CR in FFT-packed wrapped-around format
      call vmov(rbuf(izpd),1,cr,1,nphr)
      call vmov(rbuf(izpd-nphr),1,cr(nphr+1),1,nphr)

c  Remove the DC-term from the center burst because it affects 'phasecor'
      dcval=0.0
      do indexa=1,2*nphr
        dcval=dcval+cr(indexa)
      enddo
      dcval=dcval/float(2*nphr)
      do indexa=1,2*nphr
        cr(indexa)=cr(indexa)-dcval
      enddo

c  Calculate phase correction operator
      call phasecor(cr,nphr,lpco,thresh,phavg)

c  Perform in-place convolution of raw interferogram in RBUF with CR
c    Before convolution, igram extends from 1 to kip
c    After, usable igram extends from 1+lpco/2 to kip-lpco/2+1 (KIP-LPCO+1 pts)
      call convol(cr,lpco,rbuf,nfft)
ct      call symmetry(rbuf(izpd),zsym,symmp,lpco)  ! corrected igram symm
ct      write(*,*) ' Corrected ZPD: Index, Value, Symmetry  =',
ct     $ izpd,rbuf(izpd),zsym
ct      zpdv=rbuf(izpd)
ct      totp=kip-izpd-lpco/2+1

c  Move corrected interferogram so that ZPD is at 1 instead of PINL
c  overwriting short side of interferogram which is lost.
      call vmov(rbuf(izpd),1,rbuf(1),1,kip-lpco/2+1-izpd) ! dg
c  Interferogram now extends from 1 to KIP-LPCO/2+1-IZPD.
c
c  Estimate the DC term from the section of the interferogram at max OPD.
c
      dcval=0.0
      indexa=kip-(lpco/2)+1-izpd
      counter=min0(indexa,1000)
      do while (counter.gt.0)
        dcval=dcval+rbuf(indexa)
        indexa=indexa-1
        counter=counter-1
      enddo
      dcval=dcval/float(min0(kip-(lpco/2)+1-izpd,1000))

c  DC-fill unused portions of RBUF beyond long side of interferogram,
c  including the NFFT+1'st point which otherwise would not subsequently
c  get initialized when the interferogram is unfolded.
      arraytmp(1)=dcval
      call vmov(arraytmp,0,rbuf(kip-lpco/2+2-izpd),1,
     $  nfft-kip+lpco/2+izpd)  ! dg

c  Unfold long side of interferogram, together with appended DC-filled points
      call vmov(rbuf(2),1,rbuf(2*nfft),-1,nfft-1)

c  Real-to-complex FFT of double-sided, perfectly symmetrical interferogram
      call ffak(rbuf,2*nfft)

c  Perform compaction (i.e. remove imaginary values which are all zero anyway)
c  and normalize spectral values to match OPUS.
      y_nyquist=rbuf(2)
      arraytmp(1)=0.125
      call vmul(rbuf,2,arraytmp,0,rbuf,1,nfft)
      rbuf(nfft+1)=arraytmp(1)*y_nyquist

      nip=nfft+1   ! Return spectrum size
      i4head(i_izpd)=izpd  ! Return ZPD position

      return
      end
c ===================================================================
      subroutine bestzpd(rbuf,np,nburst,best)
c  Scans the interferogram from -NBURST to NBURST and finds the point or half-
c  point having the best symmetry. Then fits a parabola to the symmetry at
c  this point (or half point) and its two immediate neighbors to determine the
c  fractional interferogram location at which the parabola was a maximum.
c  This location is then returned as the best symmetry
c
c  INPUTS:
c     RBUF      the raw interferogram
c     NP        number of points over which to compute symmetry
c     NBURST    number of interferogram points to be tested
c
c  OUTPUTS:
c     BEST      fractional interferogram point having highest symmetry
c
c  Note that the symmetry is evaluated every half point, and then the 3
c  half points bracketing the maximum symmetry are fitted by a parabola
c  in order to determine the fractional location of peak symmetry.
c  The best point to use would usually be NINT(BEST).

      implicit none

      integer*4 i,np,nburst
      real*4 rbuf(0:*),symmi,symmp,symiw,sympw,best,smax,eps,denom
c
      eps=1.e-37
      smax=-999.0
      best=0.0
      symiw=0.0
      sympw=0.0
      do i=-nburst,+nburst
          call symmetry(rbuf(i),symmi,symmp,np)
          if(sympw.gt.smax) then
             smax=sympw
             denom=eps+4.0*abs(2.0*sympw-symiw-symmi)
             best=float(i)-0.5+(-symiw+symmi)/denom
          endif
          if(symmi.gt.smax) then
             smax=symmi
             denom=eps+4.0*abs(2.0*symmi-sympw-symmp)
             best=float(i)+(-sympw+symmp)/denom
          endif
          symiw=symmi
          sympw=symmp
      enddo
      return
      end
c==================================================================
      subroutine symmetry(rbuf,symmi,symmp,np)
c
c  Computes the symmetry of an interferogram about the point RBUF(0)
c  and also about the mid-point between RBUF(0) and RBUF(1).
c
c  INPUTS:
c     NP                 number of points over which symmetry is to be evaluated
c     RBUF(-np/2:np/2)   section of raw interferogram
c
c  OUTPUTS:
c     SYMMI              the interferogram symmetry about RBUF(0.0)
c     SYMMP              the interferogram symmetry about RBUF(0.5)
c
c  Symmetry is evaluated by comparing the sums and differences of points
c  which are symmetrical about RBUF(0). If the interferogram is perfectly
c  symmetrical, then  SADELI = SUM{ABS[RBUF(-i)-RBUF(+i)]} = 0, whereas
c  if igram is antisymmetrical then  SASUMI = SUM{ABS[RBUF(-i)+RBUF(+i)]} = 0.
c  Therefore, symmetry can be defined by (SASUMI-SADELI)/(SASUMI-SADELI), which
c  will vary from -1 for perfect anti-symmetry, to +1 for perfect symmetry.
c
c  Similary, the symmetry can also be defined about the half point RBUF(0.5)
c  such that SADELP = SUM{ABS[RBUF(-i+1)-RBUF(+i)]} and
c            SASUMP = SUM{ABS[RBUF(-i+1)+RBUF(+i)]}

      implicit none

      integer*4 np,j
      real*4 rbuf(0:*),sasumi,sadeli,sasump,sadelp,symmi,symmp,q,x,ww
      real*4 pi

      pi = real(4.d0*datan(1.d0))
      sasumi=0.0
      sadeli=0.0
      sasump=0.0
      sadelp=0.0
      q=pi/float(np)
      do j=1,np/2
        x=q*float(j)
        ww=(5.0*cos(x)+cos(3.0*x))/6.0
        sasumi=sasumi+ww*abs(rbuf(-j)+rbuf(j))
        sadeli=sadeli+ww*abs(rbuf(-j)-rbuf(j))
        sasump=sasump+ww*abs(rbuf(-j+1)+rbuf(j))
        sadelp=sadelp+ww*abs(rbuf(-j+1)-rbuf(j))
      enddo
      symmi=(sasumi-sadeli)/(sasumi+sadeli)
      symmp=(sasump-sadelp)/(sasump+sadelp)
      return
      end
c==================================================================
      subroutine phasecor(cr,nphr,lpco,thresh,phavg)
c  Takes the interferogram center burst (CR) and calculates an operator which
c  when convolved with the raw interferogram, should make it symmetrical.
c 
c  First apodizes the raw interferogram center burst in array CR(LPCO).
c  Then computes the complex low resolution spectrum (a+i.b) where i=sqrt(-1).
c  Computes phase = arctan2(b/a), interpolating across regions with low power.
c  Then computes the complex exponential of the adjusted phase spectrum,
c  and performs a complex-to-real FFT to yield the phase correction operator.
c
c  Note, if the original phase were unadjusted, its complex exponential would
c  equal  (a-i.b)/sqrt(a^2+b^2). Therefore, multiplying the complex spectrum by 
c  it results in a spectrum = (sqrt(a^2+b^2),0) with imaginary terms all zero.
c  Thus, the complex-to-real FFT of this spectral product, the phase corrected
c  interferogram, would be perfectly symmetrical.
c
c  INPUTS:
c             CR(2*nphr)    Array containing interferogram center burst
c             nphr        Number of points for PHase Resolution (half size of FFT)
c             LPCO        Length of Phase Correction Operator
c             thresh      Phase Correction Threshold
c
c OUTPUTS:
c             CR(LPCO)    Array containing phase correction operator
c             PHAVG       The average value of the phase at ZPD

      implicit none

      integer*4 nphr,lpco,i,lastoki
      real*4 cr(2*nphr),
     & arraytmp(1),! Used to fix gfortran warning "Rank mismatch in argument"
     & ww,theta,mag,magmax,thresh,tot,toty,phavg,sum,flag
c
      flag=1.0
      if(cr(1).lt.0.0) flag=-1.
      call apodize(cr,2*nphr)
c
c  Perform small real-to-complex FFT of apodized interferogram center burst
      call ffak ( cr, 2*nphr )
c
c  Compute low resolution power and phase spectra,
      magmax=0.0
      tot=0.0
      toty=0.0
      cr(1)=abs(cr(1))     ! Set DC magnitude 
      cr(2)=0.0            ! Set DC phase 
c      open(19,file='phase1.out')
c      write(19,*)2,3
c      write(19,*)' Freq  Phase  Mag'
c      write(19,'(f9.1,f9.4,f11.2)')cr(2),cr(2),cr(1)
      do 40 i=2,nphr
        mag=sqrt(cr(2*i-1)**2+cr(2*i)**2)
        if(mag.gt.magmax) magmax=mag
        if(mag.gt.thresh*magmax) lastoki=i
        ww=flag/mag
        theta=atan2 ((-ww)*cr(2*i), ww*cr(2*i-1) )
c-MkIV   write(19,'(f9.1,f9.4,f11.2)')7899*float(i-1)/nphr,theta,mag
c-TCCON  write(19,'(f9.1,f9.4,f11.2)')15798*float(i-1)/nphr,theta,mag
        cr(2*i-1)=mag
        cr(2*i)=theta
        toty=toty+theta*mag
        tot=tot+mag
40    continue
c        close(19)
        phavg=toty/tot
c        write(*,*)toty/tot
c
c  Interpolate phase across regions with little power (mag < thresh*magmax)
      call vintrp(cr,2,nphr,lastoki,1)
      lastoki=1
      do 41 i=nphr/16,nphr
      if(cr(2*i-1).gt.thresh*magmax) then
         if(i.ne.lastoki+1) call vintrp(cr,2,nphr,lastoki,i)
         lastoki=i
      endif
41    continue
c
c      open(19,file='phase2.out')
c      write(19,*)2,3
c      write(19,*)' Freq  Phase  Mag'
c  Compute complex exponential of the adjusted phase
      do 42 i=1,nphr
c-MkIV   write(19,'(f9.1,f9.4,f11.2)')7899*float(i-1)/nphr,
c-TCCON  write(19,'(f9.1,f9.4,f11.2)')15798*float(i-1)/nphr,
c     &  cr(2*i),cr(2*i-1)
        cr(2*i-1)=cos(cr(2*i))
        cr(2*i)=sin(cr(2*i))
42    continue
c        close(19)
      cr(2)=1.0   ! insert the Nyquist frequency at cr(2)
c
c  take the inverse transform (complex-to-real)
      call ffsk ( cr, 2*nphr )
c
c  Shrink operator from size "2*nphr" to "lpco"
      if((2*nphr).gt.lpco) then
        call vmov(cr(2*nphr),-1,cr(lpco),-1,lpco/2)
      endif 
c
c  Apodize phase correction operator
      call apodize( cr, lpco )
c
c  normalize the phase-correcting function to unit area
      arraytmp(1)=1.0
      call vdot(cr,1,arraytmp,0,sum,lpco)
      arraytmp(1)=flag/abs(sum)
      call vmul(cr,1,arraytmp,0,cr,1,lpco)
c      sum=0.
c      do i=1,lpco
c        sum=sum+cr(i)
c      enddo
c      arraytmp(1)=flag/abs(sum)
c      do i=1,lpco
c        cr(i)=arraytmp(1)*cr(i)
c      enddo
      return
      end
c
      subroutine vintrp(y,incr,nn,i1,i2)
      implicit none
      integer incr,nn,i1,i2,j,k,l
      real y(incr,nn),step
      k=i2-i1
      if(k.le.0) k=k+nn   
      step=(y(2,i2)-y(2,i1))/float(k)
      do  j=1,k-1
         l=mod(i1+j-1,nn)+1
         y(2,l)=y(2,i1)+float(j)*step
      enddo
      return
      end

      subroutine apodize(y,lpco)
c   Performs in-place apodization of array Y according to the function 
c   ( 5*cos(i*pi/LPCO) + cos(3*i*pi/lpco) ) / 6
c   Assumes that Y is packed ready for FFT, i.e. with +ve part of operator
c   in Y(1:LPCO/2) and -ve half of operator in Y(1+LPCO/2:LPCO)

      implicit none

      integer*4 lpco,i
      real*4 y(lpco),x,pi,q,ww
c
      pi = real(4.d0*datan(1.d0))
      q=pi/float(lpco)
      do i=1,lpco/2-1
        x=q*float(i)
        ww=(5.0*cos(x)+cos(3.0*x))/6.0
        y(1+i)=y(1+i)*ww
        y(lpco+1-i)=y(lpco+1-i)*ww
      enddo
      y(1+lpco/2)=0.0
c  Note that Y(1) is unchanged
      return
      end
c==================================================================
      subroutine convol(cr,lpco,rbuf,nfft)
c  Performs a fast convolution of the raw interferogram in RBUF with the phase
c  correction operator CR.
c  The resulting phase corrected interferogram is written in-place in RBUF,
c  with its ZPD in the same location as that of the raw interferogram.
c
c  INPUTS:
c     LPCO           Length of Phase Correction Operator
c     NFFT           Size of FFT
c     CR(LPCO)       Phase correction operator
c     RBUF(NFFT)     Raw interferogram
c
c  OUTPUTS:
c     CR(LPCO)                    Unchanged
C     RBUF(1:LPCO/2-1)            Garbage (spectral contamination)
C     RBUF(LPCO/2:NFFT-LPCO/2)    Corrected interferogram (NFFT-LPCO+1 points)
C     RBUF(NFFT-LPCO/2+1,2*NFFT)  Garbage (spectral contamination)
c
c  Note that the declared dimension of RBUF is 2*NFFT because the upper half
c  of this array is used as workspace (to FFT the phase correction operator).
c  Also note that on exit, even the lower half of RBUF will contain garbage
c  outside the range LPCO/2 to NFFT-LPCO/2 due to wrap-around effects.
c
c  Note that there are two possible ways of doing this convolution:
c    1) In the time domain, which is simple but slow
c    2) In the spectral domain, which is messy (because it requires forward &
c       reverse FFT's), but fast (it requires only a complex multiplication).
c  This subroutine uses the latter method, but also contains the equivalent
c  code (commented) for the time domain, which has been verified to give
c  identical results.

      implicit none

      integer*4 lpco,nfft,i,j
      real*4 rbuf(2*nfft),cr(lpco),xr,
     & arraytmp(1) ! Used to fix gfortran warning "Rank mismatch in argument"
c
c================================================================
cc  Perform time-domain (slow) convolution.
cc  Note that reversal of CR places center point at LPCO/2 rather than LPCO/2+1
c      do i=1,nfft-lpco+1
c         call vdot(rbuf(i),1,cr(lpco/2),-1,x1,lpco/2)
c         call vdot(rbuf(i+lpco/2),1,cr(lpco),-1,x2,lpco/2)
c         rbuf(i)=x1+x2
c      enddo
cc  Move corrected igram to line up with raw igram
c      call vmov(rbuf(nfft-lpco+1),-1,rbuf(nfft-lpco/2),-1,nfft-lpco+1)
cc  Be aware that this leaves garbage in RBUF locations 1 to LPCO/2
cc================================================================
c  Move CR to the ends of upper half of RBUF and set middle to zero
      call vmov(cr,1,rbuf(nfft+1),1,lpco/2)
      call vmov(cr(lpco),-1,rbuf(2*nfft),-1,lpco/2)
      arraytmp(1)=0.0
      call vmov(arraytmp,0,rbuf(nfft+1+lpco/2),1,nfft-lpco)
c
c  Perform a full size FFT in order to interpolate complex phase spectrum.
      call ffak(rbuf(nfft+1),nfft)
c
c  Perform real-to-complex transform on raw interferogram in lower half of RBUF.
      call ffak(rbuf,nfft)
c
c  Multiply resulting complex spectrum by interpolated complex phase spectrum
c  in upper half of RBUF
      rbuf(1)=rbuf(1)*rbuf(nfft+1)   !  No imaginary DC term
      rbuf(2)=rbuf(2)*rbuf(nfft+2)   !  No imaginary Nyquist term
      do i=4,nfft,2
        j=nfft+i
        xr=      rbuf(i-1)*rbuf(j-1)-rbuf(i)*rbuf(j)
        rbuf(i)= rbuf(i-1)*rbuf(j)+rbuf(j-1)*rbuf(i)
        rbuf(i-1)=xr
c        cbuf(i/2)=cbuf(i/2)*cbuf(nfft/2+i)  ! equivalent to 4 previous lines
      enddo
c
c  Inverse FFT complex spectrum to yield corrected igram in lower half of RBUF.
      call ffsk(rbuf,nfft)
      return
      end
c==================================================================
c modfft.f
c
c  The original author of the FFT subroutines below is: G. D. Bergland,
c  "A Radix-Eight Fast Fourier Transform Subroutine for Real-Valued Series",
c  IEEE Trans. Audio Electroacoust., vol. AU-17, pp 138-144, June 1969
c
c  Revision 1.5  2004/05/14  JFB
c    Replaced leading tabs with spaces
c    Fixed indentation of variable declarations
c    Added "implicit none" to individual subroutines
c    Replaced two instances of "pii = 4.*datan(1.d0) with
c      "pii = real(4.d0*datan(1.d0))" to avoid ftnchek warnings
c    Removed unused label "90" in front of "call ord1 (m, b)"
c    Added parenthesis around "-p7", "-tr3", and "-t8" to avoid G77 warnings
c
c  Revision 1.4  2002/09/16  GCT
c    Extended code to support 2^24 transforms.
c
c  Revision 1.3  1998/11/05  GCT
c    Replaced most of the arrays dimensioned (*) by the actual adjustable
c    parameters so that the array bound checker can report any violations.
c    Also replace * by & as the continuation character.
c    Also defined pii = 4*datan(1.0d0) instead of 4*atan(1.)
c    On the Sun the latter was only good to 6 sig figs (3.14159).
c
c  Revision 1.2  1996/10/20  GCT
c   -Extended code to support 2^22 transforms for MkIV 120 cm InSb runs
c   -Disabled 'feature' which returned DC term in B(NFFT+1) and the
c    Nyquist term in B(NFFT+2), requiring declared dimension of B(NFFT+2).
c    Routine now requires declared dimension of B(NFFT).
c   -Removed do loops, introduced by MCA, to perform origin shift by
c    multiplying every other complex spectral point by -1.
c   -Routine I/O now corresponds exactly to original Delbouille routine
c    with the array packed in the conventional (wrapped around) FFT format.
c
c Revision 1.1  1996/04/30  23:36:20  aychang
c S0146  clean version.
c        added implicit delcarations to compile with type checking
c
c Revision 1.0  1996/01/26  00:42:47  aychang
c Initial revision
c ------------------------------------------------------------------
c Subroutine:  ffak              Fast fourier analysis subroutine
c-------------------------------------------------------------------
c  L. Delbouille, July 26, 1987; modified by JWB 4/13/91; MCA 5/11/92
c-------------------------------------------------------------------
      subroutine ffak (b, nfft)

c  This subroutine replaces the real vector b(k), (k=1,2,...,n), with 
c  its finite discrete fourier transform.  The dc term is returned in 
c  location b(1).  Thereafter, the j'th harmonic is
c  returned as a complex number stored as b(2*j+1) + i b(2*j+2).
c  Note that the n/2 harmonic (Nyquist) is returned in b(2). 
c  The subroutine is called as ffak (b,n) where n=2**m and b is an
c  n term real array.  A real-valued, radix 8 algorithm is used with
c  in-place reordering and the trig functions are computed as needed.

      implicit none
      integer*4 nfft
      real*4 b(nfft)

      integer n, i, m, nn, int, n8pow, it
      real*4 pi8

      real*4 cos, sin

      real*4 pii, p7, p7two, c22, s22, pi2
      common /con/ pii, p7, p7two, c22, s22, pi2

      pii = real(4.d0*datan(1.d0))
      pi8 = pii/8.
      p7 = 1./sqrt(2.)
      p7two = 2.*p7
      c22 = cos(pi8)
      s22 = sin(pi8)
      pi2 = 2.*pii
      n = 1
      do i=1,24
        m = i
        n = n*2
        if (n.eq.nfft) go to 20
      enddo
      write (*,*) ' nfft not a power of 2 for ffak - stopped'
      return

  20  n8pow = m/3

c  do a radix 2 or radix 4 iteration first if one is required
      if (m-n8pow*3-1) 50, 40, 30
  30  nn = 4
      int = n/nn
      call r4tr (int, b(1), b(int+1), b(2*int+1), b(3*int+1))
      go to 60
  40  nn = 2
      int = n/nn
      call r2tr (int, b(1), b(int+1))
      go to 60
  50  nn = 1
      ! perform radix 8 iterations
  60  if (n8pow .gt. 0) then
          do it=1,n8pow
              nn = nn*8
              int = n/nn
              call r8trk (int,nn,b(1),b(int+1),b(2*int+1),b(3*int+1),
     &          b(4*int+1), b(5*int+1), b(6*int+1), b(7*int+1), b(1),
     &          b(int+1),b(2*int+1),b(3*int+1),b(4*int+1),b(5*int+1),
     &          b(6*int+1), b(7*int+1))
          enddo
      end if
      ! perform in-place reordering
      call ord1 (m, b)
      call ord2k (m, b)
      return
      end
c
c-----------------------------------------------------------------------
c subroutine:  r2tr      ^I radix 2 iteration subroutine
c-----------------------------------------------------------------------
      subroutine r2tr(int, b0, b1)

      implicit none
      integer*4 int
      real*4 b0(int), b1(int)

      integer*4 k
      real*4 t

      do k=1,int
        t = b0(k) + b1(k)
        b1(k) = b0(k) - b1(k)
        b0(k) = t
      enddo
      return
      end
c-----------------------------------------------------------------------
c subroutine:  r4tr                 radix 4 iteration subroutine
c-----------------------------------------------------------------------
      subroutine r4tr (int, b0, b1, b2, b3)

      implicit none
      integer*4 int
      real*4 b0(int), b1(int), b2(int), b3(int)

      integer*4 k
      real*4 r0, r1

      do 10 k=1,int
        r0 = b0(k) + b2(k)
        r1 = b1(k) + b3(k)
        b2(k) = b0(k) - b2(k)
        b3(k) = b1(k) - b3(k)
        b0(k) = r0 + r1
        b1(k) = r0 - r1
  10  continue
      return
      end
c-----------------------------------------------------------------------
c subroutine: r8trk                   radix 8 iteration subroutine
c for a number of points greater than 32k, up to 2048k
c-----------------------------------------------------------------------
      subroutine r8trk (int, nn, br0, br1, br2, br3, br4, br5, br6,
     &    br7, bi0, bi1, bi2, bi3, bi4, bi5, bi6, bi7)

      implicit none
      integer*4 int, nn
      real*4 br0(*), br1(*), br2(*), br3(*), br4(*),
     &  br5(*), br6(*), br7(*), bi0(*), bi1(*), bi2(*),
     &  bi3(*), bi4(*), bi5(*), bi6(*), bi7(*)

      integer*4 l(24), l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,
     &  l11, l12, l13, l14, l15, l16, l17, l18, l19, l20,
     &  l21, l22, l23, l24

      integer*4 j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, 
     &  j13, j14, j15, j16, j17, j18, j19, j20, j21, j22, j23,
     &  jthet, k, ji, jr, jl, k0, kl, int8, jlast, j, j0, th2

      real*4 t0, t1, t2, t3, t4, t5, t6, t7, tr0, ti0,
     &  tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4, tr5, ti5,
     &  tr6, ti6, tr7, ti7, c1, c2, c3, c4, c5, c6, c7,
     &  s1, s2, s3, s4, s5, s6, s7, piovn, pr, pi, arg

      real*4 sin, cos

      real*4 pii, p7, p7two, c22, s22, pi2
      common /con/ pii, p7, p7two, c22, s22, pi2

      equivalence
     & (l24,l(1)),
     & (l23,l(2)),
     & (l22,l(3)),
     & (l21,l(4)),
     & (l20,l(5)),
     & (l19,l(6)),
     & (l18,l(7)),
     & (l17,l(8)),
     & (l16,l(9)),
     & (l15,l(10)),
     & (l14,l(11)),
     & (l13,l(12)),
     & (l12,l(13)),
     & (l11,l(14)),
     & (l10,l(15)),
     & (l9,l(16)),
     & (l8,l(17)),
     & (l7,l(18)),
     & (l6,l(19)),
     & (l5,l(20)),
     & (l4,l(21)),
     & (l3,l(22)),
     & (l2,l(23)),
     & (l1,l(24))

c  set up counters such that jthet steps through the arguments of w,
c  jr steps through starting locations for the real part of the
c  intermediate results and ji steps through starting locations
c  of the imaginary part of the intermediate results.

      l(1) = nn/8
      do 40 k=2,24
        if (l(k-1)-2) 10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
      piovn = pii/float(nn)
      ji = 3
      jl = 2
      jr = 2
      do 120 j1=2,l1,2
      do 120 j2=j1,l2,l1
      do 120 j3=j2,l3,l2
      do 120 j4=j3,l4,l3
      do 120 j5=j4,l5,l4
      do 120 j6=j5,l6,l5
      do 120 j7=j6,l7,l6
      do 120 j8=j7,l8,l7
      do 120 j9=j8,l9,l8
      do 120 j10=j9,l10,l9
      do 120 j11=j10,l11,l10
      do 120 j12=j11,l12,l11
      do 120 j13=j12,l13,l12
      do 120 j14=j13,l14,l13
      do 120 j15=j14,l15,l14
      do 120 j16=j15,l16,l15
      do 120 j17=j16,l17,l16
      do 120 j18=j17,l18,l17
      do 120 j19=j18,l19,l18
      do 120 j20=j19,l20,l19
      do 120 j21=j20,l21,l20
      do 120 j22=j21,l22,l21
      do 120 j23=j22,l23,l22
      do 120 jthet=j23,l24,l23
        th2 = jthet - 2
        if (th2 .gt. 0) go to 90
        do 60 k=1,int
          t0 = br0(k) + br4(k)
          t1 = br1(k) + br5(k)
          t2 = br2(k) + br6(k)
          t3 = br3(k) + br7(k)
          t4 = br0(k) - br4(k)
          t5 = br1(k) - br5(k)
          t6 = br2(k) - br6(k)
          t7 = br3(k) - br7(k)
          br2(k) = t0 - t2
          br3(k) = t1 - t3
          t0 = t0 + t2
          t1 = t1 + t3
          br0(k) = t0 + t1
          br1(k) = t0 - t1
          pr = p7*(t5-t7)
          pi = p7*(t5+t7)
          br4(k) = t4 + pr
          br7(k) = t6 + pi
          br6(k) = t4 - pr
          br5(k) = pi - t6
  60    continue
        if (nn-8 .le. 0) go to 120

        k0 = int*8 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          pr = p7*(bi2(k)-bi6(k))
          pi = p7*(bi2(k)+bi6(k))
          tr0 = bi0(k) + pr
          ti0 = bi4(k) + pi
          tr2 = bi0(k) - pr
          ti2 = bi4(k) - pi
          pr = p7*(bi3(k)-bi7(k))
          pi = p7*(bi3(k)+bi7(k))
          tr1 = bi1(k) + pr
          ti1 = bi5(k) + pi
          tr3 = bi1(k) - pr
          ti3 = bi5(k) - pi
          pr = tr1*c22 - ti1*s22
          pi = ti1*c22 + tr1*s22
          bi0(k) = tr0 + pr
          bi6(k) = tr0 - pr
          bi7(k) = ti0 + pi
          bi1(k) = pi - ti0
          pr = (-tr3)*s22 - ti3*c22
          pi = tr3*c22 - ti3*s22
          bi2(k) = tr2 + pr
          bi4(k) = tr2 - pr
          bi5(k) = ti2 + pi
          bi3(k) = pi - ti2
  80    continue
        go to 120

  90    arg = float(th2)*piovn
        c1 = cos(arg)
        s1 = sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
        c4 = c2**2 - s2**2
        s4 = c2*s2 + c2*s2
        c5 = c2*c3 - s2*s3
        s5 = c3*s2 + s3*c2
        c6 = c3**2 - s3**2
        s6 = c3*s3 + c3*s3
        c7 = c3*c4 - s3*s4
        s7 = c4*s3 + s4*c3
        int8 = int*8
        j0 = jr*int8 + 1
        k0 = ji*int8 + 1
        jlast = j0 + int - 1
        do 100 j=j0,jlast
          k = k0 + j - j0
          tr1 = br1(j)*c1 - bi1(k)*s1
          ti1 = br1(j)*s1 + bi1(k)*c1
          tr2 = br2(j)*c2 - bi2(k)*s2
          ti2 = br2(j)*s2 + bi2(k)*c2
          tr3 = br3(j)*c3 - bi3(k)*s3
          ti3 = br3(j)*s3 + bi3(k)*c3
          tr4 = br4(j)*c4 - bi4(k)*s4
          ti4 = br4(j)*s4 + bi4(k)*c4
          tr5 = br5(j)*c5 - bi5(k)*s5
          ti5 = br5(j)*s5 + bi5(k)*c5
          tr6 = br6(j)*c6 - bi6(k)*s6
          ti6 = br6(j)*s6 + bi6(k)*c6
          tr7 = br7(j)*c7 - bi7(k)*s7
          ti7 = br7(j)*s7 + bi7(k)*c7

          t0 = br0(j) + tr4
          t1 = bi0(k) + ti4
          tr4 = br0(j) - tr4
          ti4 = bi0(k) - ti4
          t2 = tr1 + tr5
          t3 = ti1 + ti5
          tr5 = tr1 - tr5
          ti5 = ti1 - ti5
          t4 = tr2 + tr6
          t5 = ti2 + ti6
          tr6 = tr2 - tr6
          ti6 = ti2 - ti6
          t6 = tr3 + tr7
          t7 = ti3 + ti7
          tr7 = tr3 - tr7
          ti7 = ti3 - ti7

          tr0 = t0 + t4
          ti0 = t1 + t5
          tr2 = t0 - t4
          ti2 = t1 - t5
          tr1 = t2 + t6
          ti1 = t3 + t7
          tr3 = t2 - t6
          ti3 = t3 - t7
          t0 = tr4 - ti6
          t1 = ti4 + tr6
          t4 = tr4 + ti6
          t5 = ti4 - tr6
          t2 = tr5 - ti7
          t3 = ti5 + tr7
          t6 = tr5 + ti7
          t7 = ti5 - tr7
          br0(j) = tr0 + tr1
          bi7(k) = ti0 + ti1
          bi6(k) = tr0 - tr1
          br1(j) = ti1 - ti0
          br2(j) = tr2 - ti3
          bi5(k) = ti2 + tr3
          bi4(k) = tr2 + ti3
          br3(j) = tr3 - ti2
          pr = p7*(t2-t3)
          pi = p7*(t2+t3)
          br4(j) = t0 + pr
          bi3(k) = t1 + pi
          bi2(k) = t0 - pr
          br5(j) = pi - t1
          pr = (-p7)*(t6+t7)
          pi = p7*(t6-t7)
          br6(j) = t4 + pr
          bi1(k) = t5 + pi
          bi0(k) = t4 - pr
          br7(j) = pi - t5
 100    continue
        jr = jr + 2
        ji = ji - 2
        if (ji-jl .gt. 0) go to 120
        ji = 2*jr - 1
        jl = jr
 120  continue

      return
      end
c-----------------------------------------------------------------------
c subroutine:  ord1
c-----------------------------------------------------------------------
      subroutine ord1 (m,b)

      implicit none
      integer*4 m
      real*4 b(2**m)

      integer*4 k, kl, j, n
      real*4 t

      k = 4
      kl = 2
      n = 2**m
      do 40 j=4,n,2
        if (k-j) 20, 20, 10
  10    t = b(j)
        b(j) = b(k)
        b(k) = t
  20    k = k - 2
        if (k-kl) 30, 30, 40
  30    k = 2*j
        kl = j
  40  continue
      return
      end
c-----------------------------------------------------------------------
c subroutine:  ord2k
c in-place reordering subroutine, for up to 2 million points
c-----------------------------------------------------------------------
      subroutine ord2k (m, b)

      implicit none
      integer*4 m
      real*4 b(2**m)

      integer*4 l(24), l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,
     &  l11, l12, l13, l14, l15, l16, l17, l18, l19, l20,
     &  l21, l22, l23, l24

      integer*4 j1, j2, j3, j4, j5, j6, j7, j8, j9, j10,
     &  j11, j12, j13, j14, j15, j16, j17, j18, j19, j20,
     &  j21, j22, j23, n, k, ji, ij

      real*4 t

      equivalence
     & (l24,l(1)),
     & (l23,l(2)),
     & (l22,l(3)),
     & (l21,l(4)),
     & (l20,l(5)),
     & (l19,l(6)),
     & (l18,l(7)),
     & (l17,l(8)),
     & (l16,l(9)),
     & (l15,l(10)),
     & (l14,l(11)),
     & (l13,l(12)),
     & (l12,l(13)),
     & (l11,l(14)),
     & (l10,l(15)),
     & (l9,l(16)),
     & (l8,l(17)),
     & (l7,l(18)),
     & (l6,l(19)),
     & (l5,l(20)),
     & (l4,l(21)),
     & (l3,l(22)),
     & (l2,l(23)),
     & (l1,l(24))

      n = 2**m
      l(1) = n
      do k=2,m
        l(k) = l(k-1)/2
      enddo
      do k=m,23
        l(k+1) = 2
      enddo
      ij = 2
      do 40 j1=2,l1,2
      do 40 j2=j1,l2,l1
      do 40 j3=j2,l3,l2
      do 40 j4=j3,l4,l3
      do 40 j5=j4,l5,l4
      do 40 j6=j5,l6,l5
      do 40 j7=j6,l7,l6
      do 40 j8=j7,l8,l7
      do 40 j9=j8,l9,l8
      do 40 j10=j9,l10,l9
      do 40 j11=j10,l11,l10
      do 40 j12=j11,l12,l11
      do 40 j13=j12,l13,l12
      do 40 j14=j13,l14,l13
      do 40 j15=j14,l15,l14
      do 40 j16=j15,l16,l15
      do 40 j17=j16,l17,l16
      do 40 j18=j17,l18,l17
      do 40 j19=j18,l19,l18
      do 40 j20=j19,l20,l19
      do 40 j21=j20,l21,l20
      do 40 j22=j21,l22,l21
      do 40 j23=j22,l23,l22
      do 40 ji=j23,l24,l23
        if (ij-ji) 30, 40, 40
  30    t = b(ij-1)
        b(ij-1) = b(ji-1)
        b(ji-1) = t
        t = b(ij)
        b(ij) = b(ji)
        b(ji) = t
  40    ij = ij + 2
      return
      end

c-----------------------------------------------------------------------
c subroutine:  ffsk
c fast fourier synthesis subroutine, radix 8-4-2
c  modified by JWB 8/10/91 for 2**21 points
c-----------------------------------------------------------------------
      subroutine ffsk(b, nfft)

c This subroutine synthesizes the real vector b(k), where k=1,2,...,n. 
c The initial fourier coefficients are placed in the b array of size n.  
c The jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c The dc term is in b(1)
c The n/2 harmonic is in b(2).
c The subroutine is called as ffsk(b,n) where n=2**m and b is the n term 
c real array discussed above.

      implicit none
      integer*4 nfft
      real*4 b(nfft)

      integer*4 i, n, nn, n8pow, it, int, m
      real*4 con, pi8

      real*4 cos, sin

      real*4 pii, p7, p7two, c22, s22, pi2
      common /con1/ pii, p7, p7two, c22, s22, pi2

      pii = real(4.d0*datan(1.d0))
      pi8 = pii/8.
      p7 = 1./sqrt(2.)
      p7two = 2.*p7
      c22 = cos(pi8)
      s22 = sin(pi8)
      pi2 = 2.*pii
      n = 1
      do i=1,24
          m = i
          n = n*2
          if (n.eq.nfft) go to 20
      enddo
        write (*,*) ' nfft not a power of 2 < 24 for ffsk'
        return

20      continue
      con = 1.0/float(nfft)
      do i=1,nfft
          b(i) = b(i)*con
      enddo

      n8pow = m/3

c reorder the input Fourier coefficients
      call ord2k(m, b)
      call ord1(m, b)


c perform the radix 8 iterations
      if (n8pow .gt. 0) then
        nn = n
        do it=1,n8pow
c            write (*,'(a1,i1,$)') '^H',it
          int = n/nn
          call r8synk(int, nn, b, b(int+1), b(2*int+1), b(3*int+1),
     &        b(4*int+1), b(5*int+1), b(6*int+1), b(7*int+1), b(1),
     &        b(int+1), b(2*int+1), b(3*int+1), b(4*int+1), b(5*int+1),
     &        b(6*int+1), b(7*int+1))
          nn = nn/8
        enddo

      end if

c do a radix 2 or radix 4 iteration if one is required
      if (m-n8pow*3-1) 90, 80, 70
  70  int = n/4
      call r4syn(int, b(1), b(int+1), b(2*int+1), b(3*int+1))
      go to 90
  80  int = n/2
      call r2tr(int, b(1), b(int+1))
  90  return
      end
c
c-----------------------------------------------------------------------
c subroutine:  r8synk
c radix 8 synthesis subroutine
c-----------------------------------------------------------------------
      subroutine r8synk(int, nn, br0,br1,br2,br3,br4,br5,br6,br7,
     &    bi0, bi1, bi2, bi3, bi4, bi5, bi6, bi7)

      implicit none
      integer*4 int, nn
      real*4 br0(*), br1(*), br2(*), br3(*), br4(*),
     &  br5(*), br6(*), br7(*), bi0(*), bi1(*), bi2(*),
     &  bi3(*), bi4(*), bi5(*), bi6(*), bi7(*)

      integer*4 l(24), l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,
     &  l11, l12, l13, l14, l15, l16, l17, l18, l19, l20,
     &  l21, l22, l23, l24

      integer*4 j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12,
     &  j13, j14, j15, j16, j17, j18, j19, j20, j21, j22, j23,
     &  jthet, k, ji, jr, jl, k0, kl, int8, jlast, j, j0, th2 

      real*4 t0, t1, t2, t3, t4, t5, t6, t7, t8, tr0, ti0,
     &  tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4, tr5, ti5,
     &  tr6, ti6, tr7, ti7, c1, c2, c3, c4, c5, c6, c7,
     &  s1, s2, s3, s4, s5, s6, s7,
     &  piovn, pr, pi, tt0, tt1, rr, ri, ttr6, ttr7, arg

      real*4 sin, cos

      real*4 pii, p7, p7two, c22, s22, pi2
      common /con1/ pii, p7, p7two, c22, s22, pi2

      equivalence
     & (l24,l(1)),
     & (l23,l(2)),
     & (l22,l(3)),
     & (l21,l(4)),
     & (l20,l(5)),
     & (l19,l(6)),
     & (l18,l(7)),
     & (l17,l(8)),
     & (l16,l(9)),
     & (l15,l(10)),
     & (l14,l(11)),
     & (l13,l(12)),
     & (l12,l(13)),
     & (l11,l(14)),
     & (l10,l(15)),
     & (l9,l(16)),
     & (l8,l(17)),
     & (l7,l(18)),
     & (l6,l(19)),
     & (l5,l(20)),
     & (l4,l(21)),
     & (l3,l(22)),
     & (l2,l(23)),
     & (l1,l(24))

      l(1) = nn/8
      do 40 k=2,24
        if (l(k-1)-2) 10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
      piovn = pii/float(nn)
      ji = 3
      jl = 2
      jr = 2

      do 120 j1=2,l1,2
      do 120 j2=j1,l2,l1
      do 120 j3=j2,l3,l2
      do 120 j4=j3,l4,l3
      do 120 j5=j4,l5,l4
      do 120 j6=j5,l6,l5
      do 120 j7=j6,l7,l6
      do 120 j8=j7,l8,l7
      do 120 j9=j8,l9,l8
      do 120 j10=j9,l10,l9
      do 120 j11=j10,l11,l10
      do 120 j12=j11,l12,l11
      do 120 j13=j12,l13,l12
      do 120 j14=j13,l14,l13
      do 120 j15=j14,l15,l14
      do 120 j16=j15,l16,l15
      do 120 j17=j16,l17,l16
      do 120 j18=j17,l18,l17
      do 120 j19=j18,l19,l18
      do 120 j20=j19,l20,l19
      do 120 j21=j20,l21,l20
      do 120 j22=j21,l22,l21
      do 120 j23=j22,l23,l22
      do 120 jthet=j23,l24,l23
        th2 = jthet - 2
        if (th2) 50, 50, 90
  50    do 60 k=1,int
          t0 = br0(k) + br1(k)
          t1 = br0(k) - br1(k)
          t2 = br2(k) + br2(k)
          t3 = br3(k) + br3(k)
          t4 = br4(k) + br6(k)
          t6 = br7(k) - br5(k)
          t5 = br4(k) - br6(k)
          t7 = br7(k) + br5(k)
          pr = p7*(t7+t5)
          pi = p7*(t7-t5)
          tt0 = t0 + t2
          tt1 = t1 + t3
          t2 = t0 - t2
          t3 = t1 - t3
          t4 = t4 + t4
          t5 = pr + pr
          t6 = t6 + t6
          t7 = pi + pi
          br0(k) = tt0 + t4
          br1(k) = tt1 + t5
          br2(k) = t2 + t6
          br3(k) = t3 + t7
          br4(k) = tt0 - t4
          br5(k) = tt1 - t5
          br6(k) = t2 - t6
          br7(k) = t3 - t7
  60    continue
        if (nn-8) 120, 120, 70
  70    k0 = int*8 + 1
        kl = k0 + int - 1
        do 80 k=k0,kl
          t1 = bi0(k) + bi6(k)
          t2 = bi7(k) - bi1(k)
          t3 = bi0(k) - bi6(k)
          t4 = bi7(k) + bi1(k)
          pr = t3*c22 + t4*s22
          pi = t4*c22 - t3*s22
          t5 = bi2(k) + bi4(k)
          t6 = bi5(k) - bi3(k)
          t7 = bi2(k) - bi4(k)
          t8 = bi5(k) + bi3(k)
          rr = t8*c22 - t7*s22
          ri = (-t8)*s22 - t7*c22
          bi0(k) = (t1+t5) + (t1+t5)
          bi4(k) = (t2+t6) + (t2+t6)
          bi1(k) = (pr+rr) + (pr+rr)
          bi5(k) = (pi+ri) + (pi+ri)
          t5 = t1 - t5
          t6 = t2 - t6
          bi2(k) = p7two*(t6+t5)
          bi6(k) = p7two*(t6-t5)
          rr = pr - rr
          ri = pi - ri
          bi3(k) = p7two*(ri+rr)
          bi7(k) = p7two*(ri-rr)
  80    continue
        go to 120
  90    arg = float(th2)*piovn
        c1 = cos(arg)
        s1 = -sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
        c4 = c2**2 - s2**2
        s4 = c2*s2 + c2*s2
        c5 = c2*c3 - s2*s3
        s5 = c3*s2 + s3*c2
        c6 = c3**2 - s3**2
        s6 = c3*s3 + c3*s3
        c7 = c3*c4 - s3*s4
        s7 = c4*s3 + s4*c3
        int8 = int*8
        j0 = jr*int8 + 1
        k0 = ji*int8 + 1
        jlast = j0 + int - 1
        do 100 j=j0,jlast
          k = k0 + j - j0
          tr0 = br0(j) + bi6(k)
          ti0 = bi7(k) - br1(j)
          tr1 = br0(j) - bi6(k)
          ti1 = bi7(k) + br1(j)
          tr2 = br2(j) + bi4(k)
          ti2 = bi5(k) - br3(j)
          tr3 = bi5(k) + br3(j)
          ti3 = bi4(k) - br2(j)
          tr4 = br4(j) + bi2(k)
          ti4 = bi3(k) - br5(j)
          t0 = br4(j) - bi2(k)
          t1 = bi3(k) + br5(j)
          tr5 = p7*(t0+t1)
          ti5 = p7*(t1-t0)
          tr6 = br6(j) + bi0(k)
          ti6 = bi1(k) - br7(j)
          t0 = br6(j) - bi0(k)
          t1 = bi1(k) + br7(j)
          tr7 = (-p7)*(t0-t1)
          ti7 = (-p7)*(t1+t0)
          t0 = tr0 + tr2
          t1 = ti0 + ti2
          t2 = tr1 + tr3
          t3 = ti1 + ti3
          tr2 = tr0 - tr2
          ti2 = ti0 - ti2
          tr3 = tr1 - tr3
          ti3 = ti1 - ti3
          t4 = tr4 + tr6
          t5 = ti4 + ti6
          t6 = tr5 + tr7
          t7 = ti5 + ti7
          ttr6 = ti4 - ti6
          ti6 = tr6 - tr4
          ttr7 = ti5 - ti7
          ti7 = tr7 - tr5
          br0(j) = t0 + t4
          bi0(k) = t1 + t5
          br1(j) = c1*(t2+t6) - s1*(t3+t7)
          bi1(k) = c1*(t3+t7) + s1*(t2+t6)
          br2(j) = c2*(tr2+ttr6) - s2*(ti2+ti6)
          bi2(k) = c2*(ti2+ti6) + s2*(tr2+ttr6)
          br3(j) = c3*(tr3+ttr7) - s3*(ti3+ti7)
          bi3(k) = c3*(ti3+ti7) + s3*(tr3+ttr7)
          br4(j) = c4*(t0-t4) - s4*(t1-t5)
          bi4(k) = c4*(t1-t5) + s4*(t0-t4)
          br5(j) = c5*(t2-t6) - s5*(t3-t7)
          bi5(k) = c5*(t3-t7) + s5*(t2-t6)
          br6(j) = c6*(tr2-ttr6) - s6*(ti2-ti6)
          bi6(k) = c6*(ti2-ti6) + s6*(tr2-ttr6)
          br7(j) = c7*(tr3-ttr7) - s7*(ti3-ti7)
          bi7(k) = c7*(ti3-ti7) + s7*(tr3-ttr7)
 100    continue
        jr = jr + 2
        ji = ji - 2
        if (ji-jl) 110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end

c-----------------------------------------------------------------------
c subroutine:  r4syn
c radix 4 synthesis
c-----------------------------------------------------------------------
      subroutine r4syn(int, b0, b1, b2, b3)

      implicit none
      integer*4 int
      real*4 b0(int), b1(int), b2(int), b3(int)

      integer*4 k
      real*4 t0, t1, t2, t3

      do 10 k=1,int
        t0 = b0(k) + b1(k)
        t1 = b0(k) - b1(k)
        t2 = b2(k) + b2(k)
        t3 = b3(k) + b3(k)
        b0(k) = t0 + t2
        b2(k) = t0 - t2
        b1(k) = t1 + t3
        b3(k) = t1 - t3
  10  continue
      return
      end
c-----------------------------------------------------------------------
c==================================================================
c  VMOV:  moves the contents of a vector "vecin" to the vector "vecout".
c
c     calling sequence:
c     call vmov ( vecin,incrin,vecout,incrout,nele )
c
c     input parameters:
c     vecin : real*4 vector to move FROM
c     incrin : integer array index increment for "vecin"
c     incrout : integer array index increment for "vecout"
c     nele : integer number of elements on which to perform
c            the operation
c
c     output parameters:
c     vecout : real*4 vector to contain the values from "vecin"
c     vecout(1+m*incrout)=vecin(1+m*incrin)  for m=0 to nele-1
c
      subroutine vmov(vecin,incrin,vecout,incrout,nele)

      implicit none
      integer i,j,k,incrin,incrout,nele
      real*4 vecin(*),vecout(*)
      j=1
      k=1
      do 100 i=1,nele
        vecout(k)=vecin(j)
        j=j+incrin
        k=k+incrout
100   continue
      return
      end
c==========================================================================
c  VSWAP:  swaps the contents of two vectors.
c
c     calling sequence:
c     call vswap ( v1,inc1,v2,inc2,nele )
c
c     input parameters:
c     v1    : real*4 vector 
c     inc1  : integer array index increment for V1
c     v2    : real*4 vector 
c     inc2  : integer array index increment for V2
c     nele : integer number of elements to swap
c
c     output parameters:
c     v1    : real*4 vector 
c     v2    : real*4 vector 
c
c     v2(1+m*inc2)=v1(1+m*inc1)  for m=0 to nele-1
c     v1(1+m*inc1)=v2(1+m*inc2)  for m=0 to nele-1
c
c  Note:  VSWAP may be used to reverse a vector in place by calling
c     CALL VSWAP(V1,INC1,V1(1+INC1*(NELE-1)),-INC1,NELE/2)
c
      subroutine vswap(v1,inc1,v2,inc2,nele)

      implicit none
      integer i,j1,j2,inc1,inc2,nele
      real*4 v1(*),v2(*),dum
      j1=1
      j2=1
      do 100 i=1,nele
         dum=v1(j1)
         v1(j1)=v2(j2)
         v2(j2)=dum
         j1=j1+inc1
         j2=j2+inc2
100   continue
      return
      end
c==================================================================
c  VMUL:  multiplies the elements of a vector "v1" by the corresponding
c     elements of the vector "v2", and return the resulting vector in "v3".
c 
c     calling sequence:
c     call vmul (v1,i1,v2,i2,v3,i3,nele)
c
c     input parameters:
c     v1, v2 : real*4 vectors to multiply
c     i1, i2 : integer array index increments for "v1" and "v2" respectively
c     i3 : integer array index increment for "v3"
c     nele : integer number of elements on which to perform the operation
c
c     Output parameters:
c     v3(1+m*i3)=v1(1+m*i1)*v2(1+m*i2) for m=0 to nele-1
c
      subroutine vmul(v1,i1,v2,i2,vec3,i3,nele)

      implicit none
      integer i,j,k,l,i1,i2,i3,nele
      real*4 v1(1+i1*(nele-1)),v2(1+i2*(nele-1)),vec3(1+i3*(nele-1))
      j=1
      k=1
      l=1
      do 100 i=1,nele
        vec3(l)=v1(j)*v2(k)
        j=j+i1
        k=k+i2
        l=l+i3
100   continue
      return
      end
c===================================================================
c  VDOT:  performs the dot-product of a vector "v1" with the vector "v2",
c     and returns the resulting scalar in "prod".
c
c     calling sequence:
c     call vdot (v1,incr1,v2,incr2,prod,nele)
c
c     input parameters:
c     v1, v2 : real*4 vectors to take dot product of
c     incr1, incr2 : integer array index increments for "v1" and "v2",
c                    respectively
c     nele : integer number of elements on which to perform
c            the operation
c
c     output parameters:
c     prod : real*4 dot product result
c     prod = sum_over_i {v1(1+i*incr1)*v2(1+i*incr2)} for i=0 to nele-1
c
      subroutine vdot(v1,incr1,v2,incr2,prod,nele)

      implicit none
      integer i,j,k,incr1,incr2,nele
      real*8 dp
      real*4 prod,v1(*),v2(*)
      j=1
      k=1
      dp=0.0d0
      do 100 i=1,nele
        dp = dp + dprod(v1(j),v2(k))
        j=j+incr1
        k=k+incr2
100   continue
      prod=sngl(dp)
      return
      end
c================================================================
