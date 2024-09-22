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
  flags |= gflag(0);
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
bool STEnc_cmd::app_input()
{
  int relay_cmd;
  uint8_t cmd;
  switch (buf[cp]) {
    case 'A':
    case 'H':
      // report_err(2, "Command not supported: '%s'", ascii_esc((char*)buf));
      break;
    case 'Q':
      return true;
    case 'S':
      if (not_str("S:") || not_uint8(cmd)) {
        report_err("%s: Invalid syntax", iname);
        return false;
      }
      switch(cmd) {
        case 0: relay_cmd = STEnc::RELAY_CLOSE; break;
        case 1: relay_cmd = STEnc::RELAY_OPEN; break;
        case 2: relay_cmd = STEnc::RELAY_NONE; break;
        default:
          report_err("%s: S command value out of range: '%d'",
            iname, cmd);
          return false;
      }
      msg(MSG_DEBUG, "Sending relay command %d", relay_cmd);
      STE->set_relays(relay_cmd);
      break;
    default:
      report_err("%s: Invalid command", iname);
      break;
  }
  consume(nc);
  return false;
}

bool STEnc_cmd::tm_sync()
{
  STE->read_both();
  return false;
}
