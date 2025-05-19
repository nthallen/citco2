c
c  Here we define a version string and a program release date.
c  Please remember to update those when you modify this program.
c
      character
     & verstr*(*)  ! Version string displayed at program start
      real*8
     & progver     ! Program version (date) to be stored in OPUS header

      parameter (verstr=
     & 'slice-ipp    version 1.1.0pre26 15-May-2019  jfb')
      parameter (progver=20190515.d0)
