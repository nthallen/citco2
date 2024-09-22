/*
 * STEnc_cmd.cc
 */
#include <string.h>
#include "STEnc.h"
#include "nl.h"
// #include "nl_assert.h"

STEnc_cmd::STEnc_cmd(STEnc *STE)
    : Cmd_reader("cmd", 20, "STEnc"),
      STE(STE)
{
}

STEnc_cmd::~STEnc_cmd()
{
}

/**
 * Command format:
 *   A:\d+\n Goto Azimuth: Not supported
 *   H\n Goto Home: Not supported
 *   Q\n Quit
 *   S:[012]\n Close/Open/Neither
 */
bool STEnc_cmd::protocol_input()
{
  int cmd, relay_cmd;
  switch (buf[cp]) {
    case 'A':
    case 'H':
      // report_err(2, "Command not supported: '%s'", ascii_esc((char*)buf));
      break;
    case 'Q':
      return true;
    case 'S':
      if (not_str("S:") || not_int(cmd)) {
        report_err("%s: Invalid syntax", iname);
        return false;
      }
      switch(cmd) {
        case 0: relay_cmd = STEnc::RELAY_OPEN; break;
        case 1: relay_cmd = STEnc::RELAY_CLOSE; break;
        case 2: relay_cmd = STEnc::RELAY_NONE; break;
        default:
          report_err("%s: S command value out of range: '%d'",
            iname, cmd);
          return false;
      }
      STE->set_relays(relay_cmd);
      break;
    default:
      report_err("%s: Invalid command", iname);
      break;
  }
  consume(nc);
  return false;
}

bool tm_sync()
{
  uint8_t raw_status = STE->read_both();
}
