#ifndef WEBPOWER_H_INCLUDED
#define WEBPOWER_H_INCLUDED

#include <stdint.h>

typedef struct {
  uint8_t status;
} webpower_t;

#define WP_PUMP 1
#define WP_PDISO8 2
#define WP_MKS925 4
#define WP_AXIS_P1378 8

extern webpower_t webpower;

#endif

