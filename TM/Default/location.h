/* location.h defines instrument location
   LATITUDE() is N. Latitude
   LONGITUDE() is E. Longitude
   ALTITUDE() is Altitude in meters

  This is the location for rooftop enclosure of ETL building
  (added 3 meters to site altitude for height of the building)
*/

#ifndef LOCATION_H_INCLUDED
#define LOCATION_H_INCLUDED

#define LATITUDE() (54.353738)
#define LONGITUDE() (-104.986667)
#define ALTITUDE() (502)

/* RADIANCE_THRESHOLD is referenced in Radiance.tmc. It is
   a scale factor by which the standard radiance model is
   multiplied to calculate the 'model_intensity' value
   displayed on the screen that sets the threshold for
   deciding whether to open the enclosure (ok_to_open).
   The original value was 0.85.

   Prior to late August 2018, model_intensity displayed
   the standard model, not the scaled value.
*/
#define RADIANCE_THRESHOLD 0.7

#endif
