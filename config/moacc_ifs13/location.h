/* location.h defines instrument location
   LATITUDE() is N. Latitude
   LONGITUDE() is E. Longitude
   ALTITUDE() is Altitude in meters

  This is the approximate location of the rooftop enclosure on
  the container at CHARS, Cambridge Bay, Nunavut, CA.
  (added 3 meters to site altitude for height of the building)
  Altitude is a WAG
*/

#ifndef LOCATION_H_INCLUDED
#define LOCATION_H_INCLUDED

#define LATITUDE() (69.121154)
#define LONGITUDE() (-105.039675)
#define ALTITUDE() (8)

/* RADIANCE_THRESHOLD is referenced in Radiance.tmc. It is
   a scale factor by which the standard radiance model is
   multiplied to calculate the 'model_intensity' value
   displayed on the screen that sets the threshold for
   deciding whether to open the enclosure (ok_to_open).
   The original value was 0.85.

   Prior to late August 2018, model_intensity displayed
   the standard model, not the scaled value.
   
   RADIANCE_THRESHOLD is no longer referenced in Radiance.tmc.
   The value has been replaced with Rad_Open_Pct which can
   be set from the TMA or the command line, along with
   Rad_Close_Pct.
*/

#endif
