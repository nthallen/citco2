/* sza_calc.h */
#ifndef SZA_CALC_H_INCLUDED
#define SZA_CALC_H_INCLUDED
#include <time.h>

#ifdef __cplusplus
  extern "C" {
#endif

double sza_calc( double N_Lat, double E_Long, time_t dtime, int year, int month, int day );

#ifdef __cplusplus
  };
#endif

#endif
