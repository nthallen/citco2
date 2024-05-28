#include "dasio/quit.h"
#include "dasio/appid.h"
#include "WTX530_drv.h"
#include "oui.h"

namespace DAS_IO { namespace Modbus {
  
  void WTX530_dev::enqueue_polls() {
    RTU::modbus_req *req;

    req = MB->new_modbus_req();
    req->setup(this, 4, 0x0, 1, &dev_cfg[0]);
    MB->enqueue_command(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x2, 6, &dev_cfg[1], RH_WTX530_cfg);
    MB->enqueue_command(req);
    
    req = MB->new_modbus_req();
    req->setup(this, 4, 0xA, 1, &TM->RH);
    req->setup_fresh_bit(&TM->Fresh, 0);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x12, 1, &TM->WindDir);
    req->setup_fresh_bit(&TM->Fresh, 1);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x19, 3, &TM->PrecipType);
    req->setup_fresh_bit(&TM->Fresh, 2);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x1F, 1, &TM->AirT);
    req->setup_fresh_bit(&TM->Fresh, 3);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x23, 1, &TM->DewPt);
    req->setup_fresh_bit(&TM->Fresh, 4);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x2F, 4, &TM->WindSpd);
    req->setup_fresh_bit(&TM->Fresh, 5);
    MB->enqueue_poll(req);
    req = MB->new_modbus_req();
    req->setup(this, 4, 0x4F, 1, &TM->AbsAirP);
    req->setup_fresh_bit(&TM->Fresh, 6);
    MB->enqueue_poll(req);
  }
  
  void WTX530_dev::protocol_gflag(int flag) {
    TM->stale = TM->Fresh ? 0
        : TM->stale < 255 ? TM->stale+1 : 255;
    TM->Fresh = 0;
  }

  void WTX530_dev::RH_WTX530_cfg(RTU::modbus_req *req,
          RTU::modbus_device *dev, RTU *MB) {
    RH_uint16(req, dev, MB);
    msg(MSG, "Received configuration data");
  }

} } // end of namespaces

using namespace DAS_IO;

WTX530_t WTX530;
const char *WTX530_port = "/dev/ttyS0";
int WTX530_ID = 1;

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  Loop ELoop;
  Modbus::RTU *MB = new Modbus::RTU("RTU", 80, WTX530_port);
  MB->setup(19200, 8, 'e', 1, 5, 1);
  MB->flush_input();

  MB->add_device(new Modbus::WTX530_dev(
    "WTX530", WTX530_ID, &WTX530));

  Quit *Q = new Quit();
  Q->connect();
  ELoop.add_child(Q);

  TM_data_sndr *TM =
    new TM_data_sndr("TM", 0, AppID.name, (void *)&WTX530,
      sizeof(WTX530_t));
  TM->connect();
  ELoop.add_child(TM);

  ELoop.add_child(MB);
  AppID.report_startup();
  ELoop.event_loop();
  ELoop.delete_children();
  ELoop.clear_delete_queue();
  AppID.report_shutdown();
  return 0;
}
