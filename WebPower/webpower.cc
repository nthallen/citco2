#include <string.h>
#include <unistd.h>
#include "nl.h"
#include "nl_assert.h"
#include "oui.h"
#include "webpower_drv.h"
#include "exec_child.h"
#include "dasio/ascii_escape.h"
#include "dasio/loop.h"
#include "dasio/tm_data_sndr.h"
#include "dasio/appid.h"
#include "dasio/msg.h"

extern char **environ;

using namespace DAS_IO;

webpower_t webpower;

webpower_dev::webpower_dev()
  : Interface("webpwr", 80),
    w_fd(0),
    cmd_is_pending(false),
    status_pending(0),
    status_is_pending(false),
    state(s_idle)
{
  pending[0] = '\0';
  flags |= gflag(0); // for tm_sync()
  set_qerr_threshold(-1);
}

// webpower_dev::~webpower_dev();

void webpower_dev::connect()
{
  const char * argv[5] = {
    "/usr/bin/ssh",
    "-t",
    "webpower",
    "./webpower_cmds.ash",
    0
  };
  int rv;
  rv = exec_child(&w_fd, &fd, argv);
  if (rv <= 0)
    msg(MSG_FATAL,
      "%s: ssh to webpower failed: %d:%s", iname,
      errno, strerror(errno));
  flags |= Fl_Read;
}

bool webpower_dev::serialized_signal_handler(uint32_t sigs_seen)
{
  if (sigs_seen & (1<<SIGCHLD)) {
    if (child_pid)
    {
      msg(MSG, "%s: Child terminated", iname);
      child_pid = 0;
    }
    tear_down();
    queue_retry();
    return true;
  }
  return false;
}

/**
 * We are looking for response to commands and status.
 * If state is s_cmd, we expect command responses
 * If state is s_status, we expect status response
 * All successful responses begin with 'OK'
 * All errors begin with 'NOK'
 * Quit response is 'OK Quit\n', so if we see that,
 * we should tear_down and return true.
 * Status reponse looks like:
 *   "OK  false true true true" (in reverse bit order)
 */
bool webpower_dev::protocol_input()
{
  const char *context;
  int matched, matched2;
  switch (state) {
    case s_idle:
      context = "while idle";
      break;
    case s_cmd:
      context = "command response";
      break;
    case s_status:
      context = "status response";
      break;
    default:
      context = "while state was invalid";
      break;
  }
  msg(MSG_DEBUG,"%s: %s: Input is '%s'", iname, context, buf);
  if (not_alt("OK", "NOK", matched, context)) {
    if (matched==0) return false;
    // bad response already reported
  } else {
    switch (matched) {
      case 0:
        return false; // incomplete response
      case 2: // NOK
        report_err("%s: Error from script", iname);
        break;
      case 1: // OK, parse for Quit or Status
        switch (state) {
          case s_cmd:
            if (pending[0] == 'Q') {
              if (not_spaces() || not_str("Quit")) {
                report_err("%s: Bad quit response", iname);
              }
              tear_down();
              state = s_idle;
              cmd_is_pending = false;
              consume(nc);
              return true;
            }
            break;
          case s_status:
            webpower.status = 0;
            for (int i = 0, bit = 1; i < 4; ++i, bit <<= 1) {
              if (not_spaces() || not_alt("true", "false", matched2, "status")) {
                if (matched2 == 0) return false;
                break;
              } else {
                switch (matched2) {
                  case 1:
                    webpower.status |= bit;
                    break;
                  case 2:
                    break;
                }
              }
            }
            break;
          default:
            report_err("%s: unexpected response", iname);
            return false;
        }
        break;
      default:
        report_err("%s: Unexpected matched value: %d",
          iname, matched);
    }
  }
  // Assume any response resolves a request,
  // for better or worse.
  TO.Clear();
  switch (state) {
    case s_cmd:
      cmd_is_pending = false;
      msg(MSG_DEBUG, "%s: cmd not pending", iname);
      break;
    case s_status:
      status_is_pending = false;
      msg(MSG_DEBUG, "%s: status not pending", iname);
      break;
    case s_idle:
      break;
  }
  state = s_idle;
  consume(nc);
  return false;
}

bool webpower_dev::protocol_timeout()
{
  switch (state) {
    case s_idle:
      if (fd > 0) {
        msg(MSG_ERROR, "%s: Unexpected timeout", iname);
      } else {
        msg(MSG, "%s: reconnecting");
        connect();
        return false;
      }
      break;
    case s_cmd:
      msg(MSG_ERROR, "%s: Timeout on cmd '%s'",
        iname, ::ascii_escape(pending));
      cmd_is_pending = false;
      state = s_idle;
      break;
    case s_status:
      msg(MSG_ERROR, "%s: Timeout on status '%s'",
        iname, ::ascii_escape(pending));
      status_is_pending = false;
      state = s_idle;
      break;
    default:
      msg(MSG_ERROR, "%s: Invalid state: %d", iname, state);
      break;
  }
  TO.Clear();
  process_commands();
  return false; 
}

bool webpower_dev::tm_sync() {
  status_pending = "S:0,1,2,3\n";
  if (state != s_status) {
    status_is_pending = true;
    process_commands();
  } else {
    msg(MSG_DEBUG, "%s: suppressed status request");
  }
  return false;
}

bool webpower_dev::queue_command(const char *cmd, int len) {
  if (len > 0) {
    if (cmd_is_pending) {
      report_err("%s: Busy", iname);
    } else {
      nl_assert(state != s_cmd);
      nl_assert(len < 20);
      strncpy(cmd_pending, cmd, len);
      cmd_pending[len] = '\0';
      cmd_is_pending = true;
      process_commands();
    }
  }
  return false;
}

bool webpower_dev::issue_command(const char *cmd)
{
  int len = strlen(cmd);
  strncpy(pending, cmd, 20);
  msg(MSG_DEBUG, "%s: Sending '%s'", iname, pending);
  int rv = ::write(w_fd, pending, len);
  if (rv != len)
  {
    if (!werr_reported)
    {
      if (rv < 0)
      {
        msg(MSG_ERROR, "%s: Write returned %d:%s",
          iname, errno, strerror(errno));
      }
      else
      {
        msg(MSG_ERROR, "%s: Write(%d) returned %d",
          iname, len, rv);
      }
      tear_down();
      queue_retry();
      werr_reported = true;
    }
  }
  else
  {
    werr_reported = false;
  }
  TO.Set(2, 0);
  flags |= Fl_Timeout;
  return !werr_reported;
}

void webpower_dev::process_commands()
{
  if (state == s_idle)
  {
    if (status_is_pending)
    {
      if (issue_command(status_pending))
        state = s_status;
    }
    else if (cmd_is_pending)
    {
      if (issue_command(cmd_pending))
        state = s_cmd;
    }
  }
}

void webpower_dev::tear_down()
{
  if (w_fd > 0)
  {
    ::close(w_fd);
    w_fd = -1;
  }
  if (child_pid > 0)
  {
    kill(child_pid, SIGINT);
    child_pid = 0;
  }
  close();
}

void webpower_dev::queue_retry()
{
  TO.Set(5,0);
  flags |= Fl_Timeout;
}

webpower_cmd::webpower_cmd(webpower_dev *wp)
    : Cmd_reader("cmd", 20, "webpower"),
      wp(wp)
{
}

bool webpower_cmd::app_input() {
  if (nc > 0) {
    int relay;
    switch (buf[0]) {
      case 'N':
      case 'F':
        ++cp;
        if (not_str(":") || not_ndigits(1, relay) ||
            not_str("\n")) {
          report_err("%s: Invalid command", iname);
        } else if (relay < 0 || relay > 3) {
          report_err("%s: Invalid relay offset %d", iname, relay);
        } else {
          wp->queue_command((const char *)buf, cp);
        }
        break;
      case 'Q':
        wp->queue_command("Q\n", 2);
        break;
      default:
        msg(MSG_ERROR, "%s: Invalid command '%s'",
          iname, ::ascii_escape((const char *)buf));
    }
  }
  consume(nc);
  return false;
}

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  Loop ELoop;
  {
    TM_data_sndr *TM =
      new TM_data_sndr("TM", 0, "webpower", &webpower, sizeof(webpower));
    TM->connect();
    ELoop.add_child(TM);

    webpower_dev *wp = new webpower_dev();
    wp->connect();
    ELoop.add_child(wp);

    webpower_cmd *wpcmd = new webpower_cmd(wp);
    wpcmd->connect();
    ELoop.add_child(wpcmd);

    AppID.report_startup();
    ELoop.event_loop();
    AppID.report_shutdown();
  }
  return 0;
}
