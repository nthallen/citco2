#ifndef MKS925_INT_H_INCLUDED
#define MKS925_INT_H_INCLUDED

#include "dasio/serial.h"
#include "mks925.h"

using namespace DAS_IO;

class mks925_dev : public Serial
{
  public:
    mks925_dev();
    ~mks925_dev();
    static const char *port;
  protected:
    enum state_t {s_init,s_pres,s_temp};
    state_t state;
    bool protocol_input() override;
    bool protocol_timeout() override;
    bool tm_sync() override;
};

#endif
