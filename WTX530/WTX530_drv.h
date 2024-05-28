#ifndef WTX530_DRV_H_INCLUDED
#define WTX530_DRV_H_INCLUDED
#include <fcntl.h>
#include "dasio/modbus_rtu.h"
#include "dasio/tm_data_sndr.h"
#include "WTX530.h"

namespace DAS_IO { namespace Modbus {

  class WTX530_dev : public Modbus::RTU::modbus_device {
    public:
      inline WTX530_dev(const char *dev_name, uint8_t dev_ID,
                  WTX530_t *TM) :
        RTU::modbus_device(dev_name, dev_ID), TM(TM) {}
      void enqueue_polls() override;
      void protocol_gflag(int flag) override;
      static void RH_WTX530_cfg(RTU::modbus_req *req,
          RTU::modbus_device *dev, RTU *MB);
    protected:
      WTX530_t *TM;
      uint16_t dev_cfg[7];
  };

} }

extern const char *WTX530_port;

#endif
