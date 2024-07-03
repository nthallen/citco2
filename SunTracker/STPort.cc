/**
 * @file STPort.cc
 */
#include "SunTracker.h"
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include "nl.h"

// magic number to identify the Applet at the controller
const char STPort::magic[64] = {25, 67, 77, 47};

/**
 * ip should be a numerical IPV4 address.
 */
STPort::STPort(const char *ip, int revision)
    : Interface("UDP", 300)
{
  // Fl_Read|Fl_Timeout) {
  struct addrinfo hints, *res, *p;
  int rv, ioflags;
  const char *HOST_PORT = "3003";
  struct sockaddr_in local;
  struct sockaddr *localp = (struct sockaddr *)&local;
  
  hostname = ip;
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_family = PF_INET;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_protocol = IPPROTO_UDP;
  hints.ai_addrlen = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;
  
  // Set up the local address
  switch (revision) {
    case 1:
      rv = getaddrinfo(NULL, HOST_PORT, &hints, &res);
      if ( rv != 0 )
        msg(3, "getaddrinfo(NULL) returned error '%s'",
          gai_strerror(rv) );
      for (p = res; p != NULL; p = p->ai_next) {
        fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol); // AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if ( fd < 0 ) {
          msg(2, "socket returned error '%s'", strerror(errno));
        } else {
          rv = bind(fd, p->ai_addr, p->ai_addrlen);
          if ( rv != 0 ) {
            msg(2, "bind() returned error '%s'", strerror(errno) );
            close();
          } else break;
        }
      }
      break;
    case 2:
      HOST_PORT = "3000";
      local.sin_family = AF_INET;
      local.sin_port = 0;
      local.sin_addr.s_addr = htonl(INADDR_ANY);
      fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
      if ( fd < 0 ) {
        msg(2, "socket returned error '%s'", strerror(errno));
      } else {
        rv = bind(fd, localp, sizeof(local));
        if ( rv != 0 ) {
          msg(3, "bind() returned error '%s'", strerror(errno) );
        }
      }
      break;
    default:
      msg(3, "Invalid revision number: %d", revision);
  }
  if (fd < 0)
    msg(3, "Unable to bind UDP socket");

  ioflags = fcntl(fd, F_GETFL, 0);
  msg(-2, "ioflags: 0x%X", ioflags);
  if (ioflags != -1)
    ioflags = fcntl(fd, F_SETFL, ioflags | O_NONBLOCK);
  if (ioflags == -1)
    msg( 3, "Error setting O_NONBLOCK on UDP socket: %s",
      strerror(errno));
  
  // Now set up the destination address:
  hints.ai_flags = AI_NUMERICHOST;
  rv = getaddrinfo( ip, HOST_PORT, &hints, &res);
  if ( rv != 0)
    msg(3, "getaddrinfo(ip) returned error '%s'",
      gai_strerror(rv) );
  
  // Now we could use connect(), but it's not entirely clear that
  // the destination port will be the source port for inbound
  // datagrams. If I'm not going to connect, I'd
  // better copy the address info for later use.
  namelen = res->ai_addrlen;
  name = (sockaddr *)new_memory(namelen);
  memcpy(name, res->ai_addr, namelen);
}

/**
 * Send the magic packet to the destination address and set the timeout interval
 */
void STPort::transmit(bool first) {
  int rv;

  msg(MSG_DEBUG, "Transmitting");
  rv = sendto(fd, magic, 64, 0, name, namelen);
  if ( rv == -1 )
    msg(MSG_ERROR, "%s: Error from sendto: '%s'", iname, strerror(errno) );
  TO.Set(1, 975);
}

void STPort::adopted()
{
  ST_Pkt_sndr *ST_SSR_sndr = Packet.get_ST_SSR_sndr();
  ELoop->add_child(ST_SSR_sndr);
}

bool STPort::protocol_input()
{
  if (nc != 256) {
    report_err("%s: Received %d bytes", iname, nc);
    consume(nc);
  } else {
    Packet.new_data(buf);
    Packet.report();
    report_ok(nc);
  }
  return false;
}

bool STPort::protocol_timeout()
{
  transmit(false);
  return false;
}

// read and process incoming UDP packet
// int STPort::ProcessData(int flag) {
//   if (flag & Fl_Read) {
//     struct sockaddr_storage from;
//     socklen_t fromlen = sizeof(from);
//     int rv = recvfrom(fd, &ibuf[0], 256, 0,
//                     (struct sockaddr*)&from, &fromlen);
// 
//     msg(-3, "recvfrom returned %d", rv);
//     if (rv == -1) {
//       if ( errno == EWOULDBLOCK ) {
//         //++n_eagain;
//       } else if (errno == EINTR) {
//         // ++n_eintr;
//       } else {
//         msg( 2, "STPort: recvfrom error: %s", strerror(errno));
//         return 1;
//       }
//       return 0;
//     } else {
//       Packet.new_data(ibuf);
//       Packet.report();
//     }
//   }
//   if (flag & Fl_Timeout) {
//     transmit(false);
//   }
//   return 0;
// }
