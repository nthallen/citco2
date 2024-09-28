#ifndef STENC_H_INCLUDED
#define STENC_H_INCLUDED

#include <cstdint>

/*
 * status:
 *   0: Enclosure open limit status
 *   1: Enclosure closed limit status
 *   2: Enclosure power status
 *   3: Enclosure open command status
 *   4: Enclosure closed command status
 */
typedef struct __attribute__((__packed__)) {
  uint16_t STEnc_status;
} STEnc_TM_t;

#endif // STENC_H_INCLUDED
