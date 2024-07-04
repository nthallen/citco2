#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include "SunTracker.h"
#include "nl.h"
// #include "collect.h"

/**
 * We need to force the timezone to UTC so that mktime(), referenced
 * in ST_Pkt::report(), will interpret it's input as UTC.
 */
ST_Pkt::ST_Pkt() {
  ready = 0;
  stale = 0;
  strmessage[0] = '\0';
  wantedmode[0] = '\0';
  currentmode[0] = '\0';
  flipstate[0] = '\0';
  moving[0] = '\0';
  datetime[0] = '\0';
  Cloud_Detector = '\0';
  setenv("TZ","UTC00",1);
  ST_SSR.ST_Tdrift = 0;
  ST_SSR.ST_tpg_azi = 0;
  ST_SSR.ST_tpg_ele = 0;
  ST_SSR.ST_t_int = 0;
  ST_SSR.ST_flip = 0;
  ST_SSR.ST_modus = ST_MODE_TTM;
  // ST_SSR_id = Col_send_init("ST_SSR", &ST_SSR, sizeof(ST_SSR),0);
}

ST_Pkt_sndr *ST_Pkt::get_ST_SSR_sndr()
{
  ST_SSR_sndr =
    new ST_Pkt_sndr("ST_SSR", &ST_SSR, sizeof(ST_SSR));
  return ST_SSR_sndr;
}

bool ST_Pkt_sndr::app_input()
{
  report_ok(nc);
  return false;
}

/**
 * \return non-zero if the time string has changed. This effectively
 * limits data reporting to 1 Hz, although packets actually come in
 * faster than that.
 */
void ST_Pkt::new_data(unsigned char *data) {
  memcpy(&pkt, data, sizeof(pkt));
  check_string("strmessage", &strmessage[0], &pkt.strmessage[0], 41, 1);
  check_string("wantedmode", &wantedmode[0], &pkt.wantedmode[0], 20, 1);
  check_string("currentmode", &currentmode[0], &pkt.currentmode[0], 36, 1);
  check_string("flipstate", &flipstate[0], &pkt.flipstate[0], 20, 1);
  // check_string("moving", &moving[0], &pkt.moving[0], 9, 0);
  ready = 1;
  stale = 0;
  check_string("datetime", &datetime[0], &pkt.datetime[0], 41, 0);
  if (pkt.Cloud_Detector != Cloud_Detector) {
    switch (pkt.Cloud_Detector) {
      case 'C': msg(0, "Cloud Detector On"); break;
      case 'N': msg(0, "Cloud Detector Off"); break;
      default: msg(1, "Invalid Cloud Detector State"); break;
    }
    Cloud_Detector = pkt.Cloud_Detector;
  }
}

/**
 * Copies source into destination, providing a terminating NUL if necessary
 * \return true if string has changed
 */
int ST_Pkt::check_string(const char *name, char *dest, const char *src,
        int maxlen, int verbose) {
  int len, rv;
  for (len = 0; len < maxlen; ++len) {
    if (src[len] == '\0') break;
  }
  rv = strncmp(dest, src, len);
  if (rv != 0) {
    strncpy(dest, src, len);
    dest[len] = '\0';
    if (verbose)
      msg(0, "%s: %s", name, dest);
  }
  return rv;
}

float ST_Pkt::swap(float in) {
  float out;
  char *ip = (char *)(&in);
  char *op = (char *)(&out);
  unsigned int i;
  for (i = 0; i < sizeof(float); ++i) {
    op[sizeof(float)-i-1] = ip[i];
  }
  return out;
}

/* I need to use mktime to convert from y/m/d h:m:s to time_t, but
 * mktime() uses local time, so I need to make sure the timezone is
 * set to UTC
 */
/**
 * Responsible for forwarding data to wherever it needs to go.
 *
 * This data looks like this currently:
ST 2011/09/21 01:13:37 internal: 266.173 7.3132
ST 2011/09/21 01:13:38 internal: 266.174 7.31602
 * The date is UTC.
 */
void ST_Pkt::report() {
  if ( ready ) {
    struct tm st_new;
    time_t st_time, our_time;
    short int deltaT;
    if ( sscanf(datetime, "%d/%d/%d %d:%d:%d",
          &st_new.tm_year, &st_new.tm_mon, &st_new.tm_mday,
          &st_new.tm_hour, &st_new.tm_min, &st_new.tm_sec ) == 6 ) {
      double dT;
      st_new.tm_year -= 1900;
      --st_new.tm_mon;
      st_time = mktime(&st_new);
      our_time = time(NULL);
      dT = difftime(st_time, our_time);
      if ( dT < SHRT_MIN ) deltaT = SHRT_MIN;
      else if ( dT > SHRT_MAX ) deltaT = SHRT_MAX;
      else deltaT = (short)dT;
    } else {
      msg( 1, "Invalid datetime string: '%s'", datetime );
      deltaT = SHRT_MIN;
    }
    ST_SSR.ST_Tdrift = deltaT;
    ST_SSR.ST_tpg_azi = (unsigned short)(swap(pkt.Geo_Position_Azimuth)*100);
    ST_SSR.ST_tpg_ele = (unsigned short)(swap(pkt.Geo_Position_Elevation)*100);
    if (strcmp(currentmode, "Position Local") == 0) {
      ST_SSR.ST_modus = ST_MODE_TPL;
    } else if (strncmp(currentmode, "Position Sun", 12) == 0) {
      ST_SSR.ST_modus = ST_MODE_TPS;
    } else if (strncmp(currentmode, "Tracking Mode", 13) == 0) {
      ST_SSR.ST_modus = ST_MODE_TTM;
    } else {
      ST_SSR.ST_modus = ST_MODE_INIT;
    }

    if (strcmp(flipstate, "Left Flip Position") == 0) {
      ST_SSR.ST_flip = 0;
    } else if (strcmp(flipstate, "Right Flip Position") == 0) {
      ST_SSR.ST_flip = 1;
    } else {
      ST_SSR.ST_flip = 2;
    }

    { float t_int = swap(pkt.Counter_Total_Intensity);
      if (t_int < 0 || t_int > 65535) ST_SSR.ST_t_int = 65535;
      else ST_SSR.ST_t_int = (unsigned short) t_int;
    }

    msg( -2, "%s: %g %g %g %g %g %g %g %g", datetime,
      swap(pkt.Counter_Azimuth),
      swap(pkt.Counter_Elevation),
      swap(pkt.Tracker_Diode_Intensity_x1),
      swap(pkt.Tracker_Diode_Intensity_x2),
      swap(pkt.Tracker_Diode_Intensity_y1),
      swap(pkt.Tracker_Diode_Intensity_y2),
      swap(pkt.Intensity_Dx),
      swap(pkt.Intensity_Dy));
    ST_SSR_sndr->Send();
    ready = 0;
  }
}
