package IFSctrl;
use strict;
our ( %options, %directopts, %choices );

#------------------------------------------------
# Acquisition Options:
#------------------------------------------------
%options = (
   AMD => "3",           # do sliced data format
   CNM => "Anonymous Chemist",      # free text 
   SFM => "Default",     # free text
   SNM => "",   # free text
   DEL => "0",           # Delay Before Measurement
   NSS => "2",           # Number of scan
   RES => "1.000000",    # Resolution
   DTC => "0x4042",       # Detector Setting
      # 0x4020: LN-InSb AC [IP1]
      # 0x4021: RT-InGaAs AC [IP1]
      # 0x4022: LN-InSb AC + RT-InGaAs AC [IP1]
      # 0x4040: LN-InSb DC [IP2]
      # 0x4041: RT-InGaAs DC [IP2]
      # 0x4042: LN-InSb DC + RT-InGaAs DC [IP2]

   AP2 => "1000", # Aperture at sample compartment 
      # 500 => 0.5 mm
      # 800 => 0.8 mm
      # 1000 => 1 mm
      # 1150 => 1.15 mm
      # 1300 => 1.3 mm
      # 1500 => 1.5 mm
      # 1700 => 1.7 mm
      # 2000 => 2 mm
      # 2500 => 2.5 mm
      # 3150 => 3.15 mm
      # 4000 => 4 mm
      # 5000 => 5 mm
      # 6300 => 6.3 mm
      # 8000 => 8 mm
      # 10000 => 10 mm
      # 12500 => 12.5 mm
   APT => "1000", # Aperture 
      # 500 => 0.5 mm
      # 800 => 0.8 mm
      # 1000 => 1 mm
      # 1150 => 1.15 mm
      # 1300 => 1.3 mm
      # 1500 => 1.5 mm
      # 1700 => 1.7 mm
      # 2000 => 2 mm
      # 2500 => 2.5 mm
      # 3150 => 3.15 mm
      # 4000 => 4 mm
      # 5000 => 5 mm
      # 6300 => 6.3 mm
      # 8000 => 8 mm
      # 10000 => 10 mm
      # 12500 => 12.5 mm
   CHN => "1",   # Measurement Channel
      # 1 => Front sample compartment
#   OF1 => "7",   # Optical filter at det. pos. 1 and 2
      # 7 => Filter 7
#   OF2 => "3",   # Optical filter at det. pos. 3 and 4
      # 3 => Filter 3
   SRC => "201",  # Source
      # 0 => Off All
      # -104 => NIR Off
      # 104 => NIR
      # -105 => MIR Off
      # 105 => MIR
      # -106 => FIR Off
      # 106 => FIR
      # 201 => Emission back parallel input
   GNS => "1",    # Signal Gain
      # 1 => x1
      # 2 => x2
      # 4 => x4
      # 8 => x8
      # 16 => x16
   SG2 => "1",     # Signal 2 Gain
      # 1 => x1
      # 2 => x2
      # 4 => x4
      # 8 => x8
      # 16 => x16
   VEL => "10000",  # Velocity
      # 5000 => 5 kHz
      # 7500 => 7.5 kHz
      # 10000 => 10 kHz
      # 15000 => 15 kHz
      # 20000 => 20 kHz
      # 30000 => 30 kHz
      # 40000 => 40 kHz
      # 60000 => 60 kHz
      # 80000 => 80 kHz
   BMS => "1",   # Beam Splitter
      # 1 => CaF2 
   DLY => "0",  # Stabilization Delay
   SON => "0",  # External Trigger
      # 0 => Off
      # 1 => On
   AQM => "SD",  # Acquisition Mode
      # DD => Double Sided,Forward-Backward
      # SD => Single Sided,Forward-Backward
      # DN => Double sided
      # SN => Single sided
   COR => "0",   # Correlation
      # 0 => OFF
      # 1 => ON
   HFW => "15798.000000", # Wanted High Frequency Limit [cm-1]
   LFW => "0.000000", # Wanted Low Frequency Limit [cm-1]
   LPF => "10000", # Low Pass Filter
      # 10000 => 10 KHz
      # 40000 => Open
   HPF => "0", # High Pass Filter
      # 0 => OPEN (OFF)
      # 1 => ON
   PHR => "0.400000", # Phase Resolution
   SOT => "0",
   WRK => "Start Measurement"
);
%directopts = (
   PGN => 0, # 0, 1, 2 or 3
   VAC => 0
     # 0 Standby
     # 1 Evacuate Instrument
     # 2 Vent Instrument
     # 3 Evacuate Interferometer
     # 4 Evacuate Sample
     # 5 Vent Sample Compartment
);
%choices = (
  ShortSolar => {
    SFM => 'ShortSolar'
  },
  Solar => {
    SFM => 'Solar',
    RES => "0.020000",
    APT => "800",
    AP2 => "800"
  },
  SolarLR => {
    SFM => 'SolarLR',
    RES => '0.490000',
    APT => "800",
    AP2 => "800",
    PHR => '0.490000',
    NSS => '2'
  },
  # SolarInSb => {
    # SFM => 'SolarInSb',
    # DTC => "16577",
    # RES => "0.010000",
    # NSS => "2",
    # APT => "1000",
    # AP2 => "1000",
    # VEL => "10000",
    # LPF => "10000",
    # GNS => "1"
# #    PHR => "1.0",
# #    HFW => "7898.0000"
  # },
  SolarInGaAs => {
    SFM => 'SolarInGaAs',
    DTC => "0x4041",
    RES => "0.020000",
    APT => "800",
    AP2 => "800"
  },
  SolarInGaAsLR => {
    SFM => 'SolarInGaAsLR',
    DTC => "0x4041",
    RES => "0.490000",
    PHR => '0.490000',
    NSS => '2'
  },
  # SolarInGaAsPos4 => {
    # SFM => 'SolarInGaAsPos4',
    # DTC => "16577", 
    # RES => "0.020000",
    # APT => "500",
    # GNS => "1"
  # },
  # Aerosol => {
   #  SFM => 'Aerosol',
   #  RES => "0.1",
   #  AP2 => "3150",
   #  APT => "3150",
   #  DTC => "16450",
   #  VEL => "10000",
   #  LPF => "10000",
   #  GNS => "4",
   #  SG2 => "4",
   #  PHR => "0.4"
  # },
  Cell => {
    SFM => 'Cell',
    RES => "0.012000",
    AP2 => "1300",
    APT => "1300",
    SRC => "104",
    NSS => "4"
  },
  CellLR => {
    SFM => 'Cell',
    RES => "0.490000",
    PHR => "0.490000",
    AP2 => "1300",
    APT => "1300",
    SRC => "104",
    NSS => "2"
  },
  Cellres02 => {
    SFM => 'Cellres02',
    RES => "0.020000",
    SRC => "104",
    NSS => "16"
  },
  # InSbCell => {
    # SFM => 'InSbCell',
    # RES => "0.010000",
    # DTC => "16577",
    # AP2 => "1000",
    # APT => "1000",
    # SRC => "104",
    # NSS => "8"
  # },
  # InSbCell02 => {
    # SFM => 'InSbCell02',
    # RES => "0.020000",
    # DTC => "16577",
    # AP2 => "1000",
    # APT => "1000",
    # SRC => "104",
    # NSS => "32"
  # }, 
  InGaAsCell => {
    SFM => 'InGaAsCell',
    RES => "0.010000",
    DTC => "0x4041",
    AP2 => "1000",
    APT => "1000",
    SRC => "104",
    NSS => "8"
  },
  InGaAsCellLR => {
    SFM => 'InSbCell',
    RES => "0.490000",
    PHR => "0.490000",
    DTC => "0x4041",
    AP2 => "1000",
    APT => "1000",
    SRC => "104",
    NSS => "2"
  },
  InGaAsCellres02 => {
    SFM => 'InGaAsCell02',
    RES => "0.020000",
    DTC => "0x4041",
    AP2 => "1000",
    APT => "1000",
    SRC => "104",
    NSS => "32"
  }, 
  ShortCell => {
    SFM => 'Cell for turnon transient',
    RES => "4.000000",
    AP2 => "1300",
    APT => "1300",
    SRC => "104"
  },
  CheckInSbSignal => {
    SFM => 'Check InSb Signal',
    DTC => '0x4040',
    RES => '3.020000',
    PHR => '3.150000', # Phase Resolution
    SNM => '',
    NSS => 1
  },
  IdleScan => {
    RES => '3.020000',
    PHR => "3.150000", # Phase Resolution
    SFM => 'Short Scan for inactivity',
    SNM => '',
#    SRC => 0, # Everything Off
    NSS => 1 # We're discarding it anyway!
  },
  # InSbIdleScan => {
    # RES => '3.020000',
    # PHR => "3.150000", # Phase Resolution
    # SFM => 'Short InSb Scan for inactivity',
    # SNM => '',
    # NSS => "1",
    # DTC => "16577"
  # },
  InGaAsIdleScan => {
    RES => '3.020000',
    DTC => "0x4041",
    PHR => "3.150000", # Phase Resolution
    SFM => 'Short InGaAs Scan for inactivity',
    SNM => '',
    NSS => 1 # Comment goes here!
  }
);

1;
