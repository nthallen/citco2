/* STEnc_main.cc
 * Driver for East Trout Lake Sun Tracker Enclosure
 */
#include "STEnc.h"
#include "oui.h"
#include "nl.h"
#include "dasio/appid.h"

char *STEnc_path;

void enqueue_polls(STEnc *STE, STEnc_TM_t *STEnc_TM) {
  STE->enqueue_poll("$016", 0x07, 0);
  STE->enqueue_poll("$006", 0x03, 3);
}

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  AppID.report_startup();
  { ELoop Loop;
    STEnc_TM_t STEnc_TM;
    STEnc_TM.STEnc_status = 0;
    STEnc *STE = new STEnc(&STEnc_TM);
    if (!STE->connect()) {
      TM_data_sndr *TM = new TM_data_sndr("STEnc", &STEnc_TM, sizeof(STEnc_TM));
      STEnc_cmd *Cmd = new STEnd_cmd(&STE);
      msg(MSG_DBG(2), "Finished constructors");
      Loop.add_child(&TM);
      Loop.add_child(&Cmd);
      msg(MSG_DBG(2), "Entering event_loop");
      Loop.event_loop();
    }
  }
  AppID.report_shutdown();
}
