/* SunTrack.h Defines command message structure
   and data structures for SunTracker */
#ifndef SUNTRACK_H_INCLUDED
#define SUNTRACK_H_INCLUDED

/* cmd codes are:
  TS tps Track Sun by programmed position
  TM tpm Track Moon
  TD ttm Track by diode
  // PG tpg Tracker Position Geo
  // C0 cdt0 Turn Cloud Detector Off
  // C1 cdt1 Turn Cloud Detector On
  // IN init
  // SL sleep
  // ST settime
  R  Read top HTML pages
  T  Synchronize Time
  Q  Exit
*/

typedef struct __attribute__((__packed__)) {
  unsigned short ST_status; // Unused with HTML-based tracker
  signed   short ST_Tdrift;
  unsigned short ST_tpg_azi;
  signed   short ST_tpg_ele;
  unsigned short ST_t_int;
  unsigned char  ST_flip;  // 0 => Left, 1 => Right, 2 => Unknown
  unsigned char  ST_modus; // ST_MODE_* below
  // unsigned short ST_tdg;
  // unsigned short ST_min_int;
  // unsigned short ST_max_int;
  // unsigned char  ST_cdt;
} ST_SSR_t;

typedef struct __attribute__((__packed__)) {
  signed   short ST_off_azi;
  signed   short ST_off_ele;
} ST_ROA_t;

/* Modes:
 * TPL Track Position Local
 * TPS Track to Programmed Sun Position
 * TPM Track to Programmed Moon Position
 * TMM Manual Position 
 * TPG Track to Programmed Geo Position
 * TTM Track by Diode
 * INIT 
 */
#define ST_MODE_TPL 0
#define ST_MODE_TPS 1
#define ST_MODE_TPM 2
#define ST_MODE_TMM 3
#define ST_MODE_TPG 4
#define ST_MODE_TTM 5
#define ST_MODE_INIT 6

#endif
