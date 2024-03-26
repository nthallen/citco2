/* PTB330.cc */
#include "dasio/quit.h"
#include "dasio/appid.h"
#include "PTB330_drv.h"
#include "oui.h"

using namespace DAS_IO;

const char *PTB330_port = "/dev/ttyS1";
PTB330_t PTB330;

PTB330_dev::PTB330_dev(const char *port, TM_data_sndr *TM)
    : Serial("PTB", 80, port, O_RDWR|O_NONBLOCK),
      TM(TM) {
  setup(4800, 7, 'e', 1, -1, 0); // -1,0 is canonical mode
  flags = Fl_Read | gflag(0);
}

bool PTB330_dev::tm_sync() {
  ++PTB330.stale;
  return iwrite("SEND\r");
}

bool PTB330_dev::skip_whitespace() {
  while (cp < nc && isspace(buf[cp]))
    ++cp;
  return false;
}

bool PTB330_dev::protocol_input() {
  float P, TP;
  if (skip_whitespace() ||
      not_float(P) ||
      not_str("\t") ||
      skip_whitespace() ||
      not_float(TP) ||
      not_str("\r\n")) {
    if (cp >= nc) {
      report_err("%s: unfinished input\n", iname);
      return false;
    }
    consume(nc);
  } else {
    PTB330.P = P;
    PTB330.TP1 = TP;
    PTB330.stale = 0;
    report_ok(cp);
  }
  return false;
}

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  
  { Loop ELoop;

    AppID.report_startup();
    
    TM_data_sndr *TM =
      new TM_data_sndr("TM", 0, "PTB330", (void *)&PTB330, sizeof(PTB330));
    ELoop.add_child(TM);
    TM->connect();
    
    Quit *TM_Quit = new Quit();
    ELoop.add_child(TM_Quit);
    TM_Quit->connect();
    
    PTB330_dev *PTB = new PTB330_dev(PTB330_port, TM);
    ELoop.add_child(PTB);
    
    ELoop.event_loop();
    
    AppID.report_shutdown();
  }
  return 0;
}
