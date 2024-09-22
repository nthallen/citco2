/* STEnc_main.cc
 * Driver for East Trout Lake Sun Tracker Enclosure
 */
#include "STEnc.h"
#include "oui.h"
#include "nl.h"
#include "dasio/appid.h"
#include "dasio/loop.h"
#include "dasio/tm_data_sndr.h"

using namespace DAS_IO;

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  AppID.report_startup();
  { Loop ELoop;
    STEnc_TM_t STEnc_TM;
    STEnc_TM.STEnc_status = 0;
    STEnc *STE = new STEnc(&STEnc_TM);
    if (!STE->connect()) {
      msg(MSG_DEBUG, "Connected");
      TM_data_sndr *TM = new TM_data_sndr("STEnc", 0,
          "STEnc", &STEnc_TM, sizeof(STEnc_TM));
      TM->connect();
      ELoop.add_child(TM);
      STEnc_cmd *Cmd = new STEnc_cmd(STE);
      Cmd->connect();
      ELoop.add_child(Cmd);
      msg(MSG_DEBUG, "Entering event_loop");
      ELoop.event_loop();
    }
  }
  AppID.report_shutdown();
}
