/** \file Set_Mode.cc
 */
#include <time.h>
#include "SunTracker.h"
#include "nl_assert.h"

ST_Set_Mode::ST_Set_Mode(curl_multi_obj *co, const char *trans_desc, int Mode_in)
    : Transaction( co, trans_desc ) {
  mode = Mode_in;
  next_step = &ST_Set_Mode::Submit_req;
}

ST_Set_Mode::~ST_Set_Mode() {}

int ST_Set_Mode::take_next_step(CURLcode code) {
  return (this->*next_step)(code);
}

int ST_Set_Mode::Submit_req( CURLcode code ) {
  char URL[80];
  nl_assert( code == 0 );
  snprintf(URL,80,"http://%s/tracker/toptrack.htm?sub=Send&TRM=%d",
    hostname, mode);
  co->set_url(URL);
  next_step = &ST_Set_Mode::End;
  msg(0, "ST_Set_Mode::Submit_req: %s", URL);
  co->multi_add( "Submit mode request" );  
  return 0;
}

int ST_Set_Mode::End( CURLcode code ) {
  if ( code != 0 ) {
    msg( 2, "Mode request failed" );
  }
  co->dequeue_transaction();
  return 0;
}

ST_Sleep::ST_Sleep(curl_multi_obj *co)
    : Transaction(co, "Sleep") {
  next_step = &ST_Sleep::Submit_req;
}

ST_Sleep::~ST_Sleep() {}

int ST_Sleep::take_next_step(CURLcode code) {
  return (this->*next_step)(code);
}

int ST_Sleep::Submit_req( CURLcode code ) {
  char URL[80];
  nl_assert( code == 0 );
  snprintf(URL,80,"http://%s/tracker/toptrack.htm?sub=Send&TP1",
    hostname);
  co->set_url(URL);
  next_step = &ST_Sleep::End;
  msg(0, "ST_Sleep::Submit_req: %s", URL);
  co->multi_add( "Submit sleep request" );  
  return 0;
}

int ST_Sleep::End( CURLcode code ) {
  if ( code != 0 ) {
    msg( 2, "Sleep request failed" );
  }
  co->dequeue_transaction();
  return 0;
}
