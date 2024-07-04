#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define PI (3.14159265359)
/*
% sza = sza_calc( Lat, Long, time, month, day, year );
%   Lat = North Latitude in degrees
%   Long = East Longitude in degrees
%   time = Seconds since midnight GMT
%   month = 1 => Jan, 12 => Dec
%   day = Day of Month
%   year = Full year.
%
%  This program calculates solar zenith angle for any time and place
%  on earth. The required input is date, time and position of the observer.
%  This routine was originally transcribed from a Fortran program which
%  was written by Steve Lloyd. No other references to sources is known.
%  It has been tweaked minimally by Norton Allen to eliminate loops where
%  Matlab syntax permits.
*/
static double fix( double val ) {
  return val < 0 ? ceil(val) : floor(val);
}

double sza_calc( double N_Lat, double E_Long, time_t dtime, int year, int month, int day ) {
  int ak, ai, am;
  double ut, phi, rphi, alambda;
  double ajd0, ajd, t0, t, d;
  double rd, al, nu, rl, rra, ra, nub;
  double rta, rdec, dec, gmst, omega, e, rlha, rsza, sza;
  if ( year == 0 ) {
    time_t now = time(0);
    struct tm *tms = gmtime(&now);
    ak = tms->tm_year + 1900;
    ai = tms->tm_mday;
    am = tms->tm_mon+1;
    ut = tms->tm_hour + tms->tm_min/60. + tms->tm_sec/3600.; /* GMT in hours */
  } else {
    ak = year;
    ai = month;
    am = day;
    ut = dtime/3600.;
  }
  phi = N_Lat; /* 34.134 N Latitude */
  alambda = E_Long; /* -118.126 E Longitude */

  /* Julian date at midnight gmt minus 1721013.5 */
  ajd0=(367.*ak)-floor((7./4)*(ak+floor((am+9)/12.)))+(floor((275*am)/9.))+ai;
  /* Julian date minus 1721013.5 */
  ajd = (367.*ak)-floor((7./4)*(ak+floor((am+9)/12.)))+(floor((275*am)/9.))+ai+(ut/24.);
  t0 = (ajd0-730531.5)/36525;
  t = (ajd-730531.5)/36525;
  d = 357.528+(35999.05*t);
  d = fmod(d, 360 );

  rd = d*PI/180;

  /* True geocentric longitude of the sun */
  al = fmod((280.46+(36000.772*t)+(1.916*sin(rd))+(0.02*sin(2.*rd))), 360);
  if ( al < 0 ) al += 360;

  /*  Nu is the quadrant in which the sun resides */
  nu = floor(al/90) + 1;
  rl = al*PI/180;
  rra = atan(0.91747*(tan(rl)));

  /* Apparent right ascention of the sun */
  ra = fmod((rra*180/PI), 360);
  if ( ra < 0 ) ra += 360;

  /* Nub is the quadrant of the right ascension */
  /* Since al and ra must be in the same quadrant, */
  /* We will now check and make sure this is so. */
  nub = fix(ra/90) + 1;
  rta = ra + (nu - nub) * 90;

  rdec = asin(0.3978*sin(rl));
  /* Declination of the sun, north is positive, south is negative. */
  dec = rdec*180/PI;
  rphi = phi*PI/180;
  /* Greenwich mean sideral time in hours */
  gmst = fmod((6.69737456+(2400.051336*t0)+(0.0000258622*t0*t0)+(1.002737909*ut)),
              24);
  if ( gmst < 0 ) gmst += 24;

  /* Mean longitude of the ascending node of the moon's orbit ??? */
  omega = fmod((125.04452-(1934.13626*t)+(0.002071*t*t)), 360);
  if ( omega < 0 ) omega += 360;
  
  /* Equation of the equinoxes in hours */
  e = -0.00029*sin(omega*PI/180);

  /* Local hour angle */
  rlha = (15. *(gmst+e-(rta/15))+alambda)*PI/180;
  rsza = acos((sin(rphi)*sin(rdec))+(cos(rphi)*cos(rdec)*cos(rlha)));
  /* Solar zenith angle */
  sza = rsza*180/PI;
  return sza;
}
