#ifndef PREAMPS_H_INCLUDED
#define PREAMPS_H_INCLUDED

/* Here we document IFS parameters that differ between
   instruments. [These values are for SN #160, aka IFS12]
   These values are referenced by the function
   Set_Preamp_Gains() in IFS.cmd. This function is invoked
   without regard to the state of the LN2 dewar, so these
   detectors must be room temperature detectors of the Bruker
   will complain when we try to select them.
   
   Values as of 1/1/2025: There is no Si detector installed,
   so we'll address the InGaAs detector twice. The corresponding
   calls to Set_Preamp_Gains() had better specify the same
   gain for both parameters.
*/

#define IFS_SELECT_InGaAs "DTC=0x4041"
#define IFS_SELECT_Si     "DTC=0x4041"

#endif
