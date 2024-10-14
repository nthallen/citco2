#include <fcntl.h>
#include "dasio/appid.h"
#include "dasio/tm_data_sndr.h"
#include "dasio/loop.h"
#include "dasio/quit.h"
#include "mks925_int.h"
#include "oui.h"
#include "nl.h"

const char *mks925_dev::port = "/dev/ttyS3";
MKS925_t MKS925;

using namespace DAS_IO;

mks925_dev::mks925_dev()
  : Serial("MKS", 80, port, O_RDWR | O_NONBLOCK),
    state(s_init)
{
  setup(9600, 8, 'n', 1, 10, 1);
  flags |= gflag(0); // enable tm_sync()
}

mks925_dev::~mks925_dev()
{
}

bool mks925_dev::protocol_input()
{
  uint8_t addr, code;
  float val;
  
  if (state == s_init)
  {
    report_err("%s: unexpected input", iname);
    consume(nc);
    return false;
  }
  if (not_found('@') ||
      not_uint8(addr))
  { // syntax problem
    if (cp < nc)
      consume(nc);
  }
  else if (buf[cp] == 'A')
  {
    if (not_str("ACK",3) ||
        not_float(val) ||
        not_str(";FF"))
    { // syntax
      if (cp < nc)
        consume(nc);
    }
    else
    { // good value read
      report_ok(nc);
      switch (state)
      {
        case s_pres:
          MKS925.P = val;
          if (obuf_empty())
          {
            iwrite("@253TEM?;FF");
            state = s_temp;
            TO.Set(0,300);
          }
          break;
        case s_temp:
          MKS925.T = val;
          TO.Clear();
          state = s_init;
          break;
        default:
          msg(MSG_FATAL, "Invalid state value: %d", state);
      }
    }
  }
  else if (not_str("NAK",3) ||
           not_uint8(code) ||
           not_str(";FF"))
  { // syntax problem
    if (cp < nc)
      consume(nc);
  }
  else
  { // Report NAK value
    report_err("%s: NAK %d", iname, code);
    consume(nc);
    TO.Clear();
    state = s_init;
  }
  return false;
}

bool mks925_dev::protocol_timeout()
{
  report_err("%s: Timeout", iname);
  consume(nc);
  state = s_init;
  return false;
}

bool mks925_dev::tm_sync()
{
  if (state == s_init && obuf_empty()) {
    iwrite("@253PR4?;FF");
    state = s_pres;
    TO.Set(0,400);
  }
  return false;
}

int main(int argc, char **argv)
{
  oui_init_options(argc, argv);
  Loop ELoop;
  {
    mks925_dev *MKS = new mks925_dev();
    ELoop.add_child(MKS);

    TM_data_sndr *TM =
      new TM_data_sndr("TM", 0, "MKS925", &MKS925, sizeof(MKS925));
    TM->connect();
    ELoop.add_child(TM);

    Quit *Q = new Quit();
    Q->connect();
    ELoop.add_child(Q);
    
    AppID.report_startup();
    ELoop.event_loop();
    AppID.report_shutdown();
  }
  return 0;
}
