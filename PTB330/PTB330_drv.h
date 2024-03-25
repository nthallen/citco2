#ifndef PTB330_DRV_H_INCLUDED
#define PTB330_DRV_H_INCLUDED
#include <fcntl.h>
#include "dasio/serial.h"
#include "dasio/tm_data_sndr.h"
#include "PTB330.h"

using namespace DAS_IO;

extern const char *PTB330_port;

class PTB330_dev : public Serial {
  public:
    PTB330_dev(const char *port, TM_data_sndr *TM);
  protected:
    bool tm_sync();
    bool protocol_input();
    bool skip_whitespace();
    TM_data_sndr *TM;
};

#endif
