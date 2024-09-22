/*
 * CmdReq.cc
 * command_request object
 */
#include <string.h>
#include <stdlib.h>
#include "STEnc.h"
#include "nortlib.h"
#include "nl_assert.h"

/*
 * The commands we support are either status read commands for telemetry,
 * which are persistent, or commands to switch relays, which are not.
 * Status reads expect a reply of format ![0-9A-F]{6}\r. We really
 * only care about the first two digits.
 * Relay commands expect a response of '>\r'
 */
command_request::command_request() {
  active = false;
  persistent = false;
  TO_msecs = 250;
  rep_sz = 8;
}

/**
 * @return true if command is not valid. Caller should return object to
 * the free queue.
 */
bool command_request::init(const char *cmdstr, int rep_len, uint16_t src_mask, int dest_bit_offset) {
  active = false;
  TO_msecs = 250;
  persistent = false;
  nl_assert(cmdstr != 0);
  unsigned cmdlen = strlen(cmdstr);
  nl_assert(cmdlen+3 < max_cmd_bytes);
  strncpy((char *)req_buf, cmdstr, max_cmd_bytes);
  //if (using_chksum) {
    // append the chksum
  //}
  req_buf[cmdlen++] = '\r';
  req_buf[cmdlen] = '\0';
  req_sz = cmdlen;
  this->src_mask = src_mask;
  if (src_mask) {
    this->dest_bit_offset = dest_bit_offset;
    if (dest_bit_offset < 0) {
      dest_mask = src_mask >> (-dest_bit_offset);
    } else {
      dest_mask = src_mask << dest_bit_offset;
    }
    dest_mask = ~dest_mask;
  }
  // nl_error(MSG_DBG(2), "command_request::init #1");
  rep_sz = rep_len; // + (using_chksum ? 2 : 0);
  return false;
}

/*
 * Need enough information in the command request to know what to
 * do with the reply.
 * If it's persistent, then we expect ![0-9A-F]{6}\r
 */
// STE_rep_ok, STE_rep_incomplete, STE_rep_error
STEnc_rep_status_t STEnc::process_reply() {
  if (nc-cp < pending->rep_sz) return STEnc_rep_incomplete;
  if (pending->persistent) {
    if (not_str("!")) {
      report_err("Unexpected response");
      return STEnc_rep_error;
    }
    if (isxdigit(buf[cp]) && isxdigit(buf[cp+1])) {
      uint16_t val =
        ((isdigit(buf[cp]) ? (buf[cp]-'0') : (buf[cp]-'A'+10)) * 16 +
         (isdigit(buf[cp+1]) ? (buf[cp+1]-'0') : (buf[cp+1]-'A'+10)))
         & pending->src_mask;
      STEnc_TM_p->STEnc_status =
        (STEnc_TM_p->STEnc_status & pending->dest_mask) |
        (pending->dest_bit_offset < 0 ?
          val >> (-pending->dest_bit_offset) :
          val << pending->dest_bit_offset);
      return STEnc_rep_ok;
    }
    report_err("Expected hex digits");
    return STEnc_rep_error;
  } else {
    if (buf[cp] != '>') {
      report_err("Unexpected repsonse");
      return STEnc_rep_error;
    } else return STEnc_rep_ok;
  }
}

/**
 * Invokes ascii_escape on the request buffer
 */
const char *command_request::ascii_escape() {
  return ::ascii_escape((const char *)req_buf, req_sz);
}

/**
 * Writes the request to the specified file descriptor.
 * @return -1 if write returns anything but req_sz. 0 otherwise.
 */
int command_request::write(int fd) {
  if (nl_debug_level <= MSG_DBG(1)) {
    nl_error(MSG_DBG(1), "Sending: '%s'", ascii_escape());
  }
  int nb = ::write(fd, req_buf, req_sz);
  return (nb != (int)req_sz) ? -1 : 0;
}
