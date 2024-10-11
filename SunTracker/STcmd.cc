/** \file STcmd.cc
 */
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "SunTracker.h"
#include "nl.h"

STcmd::STcmd(const char *name, curl_multi_obj *co_in)
      : Cmd_reader("cmd", 80, name)
{
  co = co_in;
}

/**
 * Read commands from the command channel.
 * @return non-zero if we should quit
 */
bool STcmd::app_input()
{
  bool rv = execute_cmd();
  report_ok(nc);
  return rv;
}

bool STcmd::execute_cmd()
{
  int delta = 0;
  switch (buf[0]) {
    case 'R':
      co->enqueue_transaction(new ST_Read_Top(co, "Read Top to Connect"));
      return false;
    case 'I':
      switch (buf[1]) {
        case 'N': // Init Mode
          co->enqueue_transaction(new ST_Set_Mode(co, "Init Mode", 5));
          return false;
        default: break;
      }
      break;
    case 'T':
      switch (buf[1]) {
        case 'Y':
          if ( ( buf[2] == '+' || buf[2] == '-' ) && isdigit(buf[3]) ) {
            delta = atoi((const char*)(buf+3));
            if ( buf[2] == '-' ) delta = -delta;
          }
          msg( 0, "Received Time Set %d", delta );
          co->enqueue_transaction(new ST_Set_Time( co, "Set Time", delta));
          return false;
        case 'S': // track to programmed sun
          co->enqueue_transaction(new ST_Set_Mode(co, "TPS Mode", 1));
          return false;
        case 'M': // track to programmed moon
          co->enqueue_transaction(new ST_Set_Mode(co, "TPM Mode", 2));
          return false;
        case 'D': // track mode
          co->enqueue_transaction(new ST_Set_Mode(co, "TTM Mode", 4));
          return false;
        case 'R': // CamTracker mode
          co->enqueue_transaction(new ST_Set_Mode(co, "TMR Mode", 7));
          return false;
        default:
          break;
      }
      break;
    case 'Q':
      return true;
    default:
      break;
  }
  msg( 2, "Invalid command: '%s'", ascii_escape());
  return false;
}
