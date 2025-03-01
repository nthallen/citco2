/* LN2.cc */
// #include <termios.h>
#include "dasio/ascii_escape.h"
#include "dasio/appid.h"
#include "LN2.h"
#include "nl.h"
#include "nl_assert.h"
#include "oui.h"

const char *ln2_port = "/dev/ser1";
const uint16_t LN2::CalVal[6] =
  // { 0xed, 0x1a0, 0x27d, 0x8f, 0x1dd, 0x23 };
  { 237, 288, 637, 137, 769, 41 };

LN2Request::LN2Request(const char *cmd) {
  cmdtxt = cmd;
  result = 0;
  n_hex_responses = 0;
  cmdlen = strlen(cmdtxt);
  flagval = 0;
}

LN2Request::LN2Request(const char *cmd, uint8_t *res, uint8_t flag) {
  cmdtxt = cmd;
  result = res;
  n_hex_responses = 1;
  cmdlen = strlen(cmdtxt);
  flagval = flag;
}

LN2Request::LN2Request(const char *cmd, uint16_t *res, uint8_t flag) {
  cmdtxt = cmd;
  result = res;
  n_hex_responses = 2;
  cmdlen = strlen(cmdtxt);
  flagval = flag;
}

LN2TM::LN2TM(LN2_t *data)
  : TM_data_sndr("TM", 0, "LN2", data, sizeof(LN2_t))
{
  TMdata = data;
  TMdata->LN2DrvStat = 0;
}

LN2TM::~LN2TM() {}

bool LN2TM::app_input()
{
  bool rv = TM_data_sndr::app_input();
  TMdata->LN2DrvStat &= ~LN2_DS_CRNT;
  return rv;
}

Cmd_rdr::Cmd_rdr(LN2Driver *top)
  : Cmd_reader("cmd", 20, "LN2"),
    top(top),
    PwrStat(0)
{
  Cmds.push_back(LN2Request("pof\r"));
  Cmds.push_back(LN2Request("pon\r"));
  // flush_input();
}

Cmd_rdr::~Cmd_rdr() {}

/**
 * Command syntax from command server (cmd/LN2)
 *  P0: Pump Off
 *  P1: Pump On
 *  Q: Driver Quit
 */
bool Cmd_rdr::app_input() {
  nl_assert(top != 0);
  switch (buf[cp]) {
    case 'P':
      switch (buf[++cp]) {
        case '0': top->LN2Command(&Cmds[0]); break;
        case '1': top->LN2Command(&Cmds[1]); break;
        default: report_err("Invalid Power Option"); break;
      }
      break;
    case 'Q': return 1;
    default: report_err("Invalid Command"); break;
  }
  consume(nc);
  return 0;
}

LN2::LN2(const char *port, LN2_t *data)
  : Serial("LN2", 50, port, O_RDWR | O_NONBLOCK),
    TMdata(data),
    TM_reported(false),
    Initialized(false),
    WrongCmdCount(0),
    CmdReq(0),
    CurReq(0)
{
  Req = Reqs.end();
  setup(19200, 8, 'n', 1, -1, '\r'); // canonical mode
  // Vessel = big tank. Main = InSb Dewar
  InitReqs.push_back(LN2Request("re 010 2\r", &CalValRpt[0])); // Vessel cold (-196)
  InitReqs.push_back(LN2Request("re 018 2\r", &CalValRpt[1])); // Vessel room (-100)
  InitReqs.push_back(LN2Request("re 090 2\r", &CalValRpt[2])); // Vessel empty (30)
  InitReqs.push_back(LN2Request("re 016 2\r", &CalValRpt[3])); // Main cold (-196)
  InitReqs.push_back(LN2Request("re 095 2\r", &CalValRpt[4])); // Main +30
  InitReqs.push_back(LN2Request("rm 0c8 2\r", &CalValRpt[5])); // Pressure zero
  InitReq = InitReqs.begin();
  Reqs.push_back(LN2Request("rm 080 2\r", &TMdata->LN2TankT, LN2_DS_TankT_CRNT));
  Reqs.push_back(LN2Request("rm 086 2\r", &TMdata->InSbT, LN2_DS_InSbT_CRNT));
  Reqs.push_back(LN2Request("rm 088 2\r", &TMdata->LN2P, LN2_DS_LN2P_CRNT));
  Reqs.push_back(LN2Request("rm 0ce 2\r", &TMdata->LN2Depth, LN2_DS_LN2Depth_CRNT));
  Reqs.push_back(LN2Request("rm 019\r",   &TMdata->LN2Stat, LN2_DS_LN2Stat_CRNT));
  Req = Reqs.end();
  flush_input();
  flags = Fl_Read | Fl_Timeout | gflag(0);
}

LN2::~LN2() {
  if (WrongCmdCount)
    msg(0, "%s: Wrong Command Count: %ld", iname, WrongCmdCount);
}

void LN2::LN2Command(LN2Request *cmd) {
  CmdReq = cmd;
}

bool LN2::protocol_input() {
  while (cp < nc && isspace(buf[cp])) ++cp;
  if (CurReq == 0) {
    if (cp < nc)
      report_err("Unexpected input:");
    consume(nc);
  } else {
    uint16_t value;
    if (not_str(CurReq->cmdtxt, CurReq->cmdlen)) {
      if (cp < nc) {
        consume(nc);
        return false;
      }
    }
    while (cp < nc && isspace(buf[cp])) ++cp;
    if (cp >= nc) return false;
    if (CurReq->result) {
      if (CurReq->n_hex_responses && isxdigit(buf[cp])) {
        value = 0;
        int i;
        for (i = 0; i < CurReq->n_hex_responses; ++i) {
          uint16_t byte;
          if (not_hex(byte)) {
            if (cp < nc) consume(nc);
            return false;
          }
          value += byte<<(8*i);
        }
      }
    } else if (buf[cp] != 'W') {
      if (not_str(CurReq->cmdtxt, CurReq->cmdlen)) {
        if (cp < nc) consume(nc);
        return false;
      }
    }
    if (cp >= nc) return false;
    if (buf[cp] == 'W') {
      if (not_str("Wrong command!\r\n\r\nReady\r\n")) {
        if (cp < nc) {
          consume(nc);
          CurReq = 0;
        }
      } else {
        msg(1, "%s: 'Wrong command' reported for %s",
                iname, ascii_esc(CurReq->cmdtxt));
        consume(nc);
        ++WrongCmdCount;
        CurReq = 0;
      }
      return false;
    }
    while (cp < nc && isspace(buf[cp])) ++cp;
    if (not_str("Ready\r\n")) {
      if (cp >= nc) return false;
      // else consume(nc); // error was reported, but we'll clear below anyway.
    }
    if (CurReq && CurReq->result) {
      switch (CurReq->n_hex_responses) {
        case 1: *((uint8_t *)CurReq->result) = value; break;
        case 2: *((uint16_t *)CurReq->result) = value & 0x3FF; break;
        default: msg(4, "%s: Unexpected n_hex_responses: %d",
              iname, CurReq->n_hex_responses);
      }
      TMdata->LN2DrvStat |= CurReq->flagval;
    }
    CurReq = 0;
    consume(nc);
    report_ok();
    // next_request(); // wait for timeout or TM
  }
  return false;
}

bool LN2::tm_sync()
{
  TM_reported = true;
  if (CurReq == 0 && !Initialized)
    next_request();
  return false;
}

bool LN2::protocol_timeout()
{
  if (CurReq) {
    report_err("Timeout on query: '%s'",
      ::ascii_escape(CurReq->cmdtxt, CurReq->cmdlen));
    CurReq = 0;
  }
  TO.Clear();
  if (Initialized)
    next_request();
  return false;
}

void LN2::next_request() {
  if (CurReq == 0) {
    if (CmdReq != 0) {
      CurReq = CmdReq;
      CmdReq = 0;
    } else {
      if (InitReq != InitReqs.end()) {
        CurReq = &(*InitReq);
        ++InitReq;
        TM_reported = false;
      } else {
        if (!Initialized) {
          int i;
          for (i = 0; i < 6; ++i) {
            if (CalVal[i] != CalValRpt[i])
              msg(1, "%s: CalVal[%d] expected %u, received %u",
                iname, i, CalVal[i], CalValRpt[i]);
            Initialized = true;
            TMdata->LN2DrvStat |= LN2_DS_INIT;
          }
        }
        if (Req == Reqs.end()) {
          Req = Reqs.begin();
        }
        CurReq = &(*Req);
        ++Req;
      }
    }
    if (CurReq != 0) {
      iwrite(CurReq->cmdtxt, CurReq->cmdlen);
      TO.Set(0, 400); // 400 msecs
    }
  }
}

LN2Driver::LN2Driver(const char *serial_port) {
  TMdata.LN2TankT = 0;
  TMdata.InSbT = 0;
  TMdata.LN2P = 0;
  TMdata.LN2Depth = 0;
  TMdata.LN2Stat = 0;
  TMdata.LN2DrvStat = 0;
  msg(-2, "LN2Driver: constructor");
  
  TM_if = new LN2TM(&TMdata);
  TM_if->connect();
  ELoop.add_child(TM_if);

  Cmd_if = new Cmd_rdr(this);
  Cmd_if->connect();
  ELoop.add_child(Cmd_if);

  LN2_if = new LN2(serial_port, &TMdata);
  ELoop.add_child(LN2_if);

  msg(-2, "LN2Driver: leaving constructor");
}

LN2Driver::~LN2Driver() {}

void LN2Driver::LN2Command(LN2Request *cmd) {
  LN2_if->LN2Command(cmd);
}

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  AppID.report_startup();
  msg(-2, "In debug mode: serial port is %s", ln2_port);
  { LN2Driver LN2D(ln2_port);
    msg(-2, "LN2Driver should be initialized");
    LN2D.event_loop();
  }
  AppID.report_shutdown();
  return 0;
}
