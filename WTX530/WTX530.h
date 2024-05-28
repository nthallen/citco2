#ifndef WTX530_H_INCLUDED
#define WTX530_H_INCLUDED
#include <cstdint>

typedef struct {
  int16_t RH; // SF=10 0-1000
  int16_t WindDir; // SF=10 0-3599 deg
  int16_t PrecipType; // 0=none, 40=precip, 90=Hail
  uint16_t WindQ; // Wind Measurement Quality: 0-100
  int16_t SolRad; // SF=10 0-20000 W/m^2
  int16_t AirT; // SF=10 -500 to 600 C
  int16_t DewPt; // SF=10 -500 to 600 C
  int16_t WindSpd; // SF=10 0 to 750 m/s
  uint16_t PrecipAcc; // SF=100 0-65530 (25.8") mm
  uint16_t PrecipInt; // SF=100 0-20000 (7.87") mm/h
  int16_t AbsAirP; // SF=10 3000-12000 hPa
  uint8_t Fresh; // bit-mapped fresh status
  uint8_t stale;
} WTX530_t;

extern WTX530_t WTX530;
extern int WTX530_ID;

#endif