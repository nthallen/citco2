#include "SunTracker.h"
#include "curllog/curl_select.h"
#include "dasio/appid.h"
#include "nl.h"
#include "oui.h"

const char *hostname = HOST;
int ST_Revision = 1;

int main(int argc, char **argv ) {
  oui_init_options(argc, argv);
  {
    curl_multi_obj *co = new curl_multi_obj();
    curl_multi *Sr = curl_multi::getInstance();
    co->set_log_level(CT_LOG_BODIES);
    STPort *Port = new STPort(hostname, ST_Revision);
    STcmd *Cmd = new STcmd("SunTrack", co);
    Sr->add_child(Port);
    Sr->add_child(Cmd);
    AppID.report_startup();
    Port->transmit(true);
    Sr->event_loop();
    AppID.report_shutdown();
  }
  return 0;
}
