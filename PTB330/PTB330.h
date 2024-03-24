#ifndef PTB330_H_INCLUDED
#define PTB330_H_INCLUDED

typedef struct {
  float P; // mbar
  float TP1; // deg C
  uint8_t stale;
} PTB330_t;

extern PTB330_t PTB330;

#endif