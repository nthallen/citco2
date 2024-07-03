/** \file Set_Time.cc
 */
#include <time.h>
#include "SunTracker.h"
#include "nl_assert.h"

ST_Read_Top::ST_Read_Top(curl_multi_obj *co, const char *trans_desc)
    : Transaction( co, trans_desc ) {
  next_step = &ST_Read_Top::Get_top;
}

ST_Read_Top::~ST_Read_Top() {}

int ST_Read_Top::take_next_step(CURLcode code) {
  return (this->*next_step)(code);
}

int ST_Read_Top::Get_top( CURLcode code ) {
  char URL[80];
  nl_assert( code == 0 );
  snprintf(URL,80,"http://%s/", hostname);
  co->set_url(URL);
  next_step = &ST_Read_Top::Got_top;
  msg(0, "ST_Read_Top::Get_top: %s", URL );
  co->multi_add( "Get /" );
  return 0;
}

int ST_Read_Top::Got_top( CURLcode code ) {
  if ( code == 0 ) {
    msg( 0, "Loaded sun tracker home page successfully" );
  } else {
    msg( 2, "Read of sun tracker home page failed" );
  }
  co->dequeue_transaction();
  return 0;
}
