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
  // uint16_t ST_status; // Unused with HTML-based tracker
  int16_t  ST_Tdrift;
  uint16_t ST_tpg_azi;
  int16_t  ST_tpg_ele;
  uint16_t ST_t_int;
  uint8_t  ST_flip;  // 0 => Left, 1 => Right, 2 => Unknown
  uint8_t  ST_modus; // ST_MODE_* below
  // uint16_t ST_tdg;
  // uint16_t ST_min_int;
  // uint16_t ST_max_int;
  // uint8_t  ST_cdt;
} ST_SSR_t;

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
#define ST_MODE_AOA 7
#define ST_MODE_TMR 8

#endif
