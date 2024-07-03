/** \file Set_Time.cc
 */
#include <time.h>
#include "SunTracker.h"
#include "nl_assert.h"

/**
 * Should clean this up atexit().
 */
curl_form *ST_Set_Time::Set_Time_Form = 0;

ST_Set_Time::ST_Set_Time(curl_multi_obj *co, const char *trans_desc, int delta_T_in)
    : Transaction( co, trans_desc ) {
  delta_T = delta_T_in;
  next_step = Set_Time_Form ? &ST_Set_Time::Submit_Time_Form : &ST_Set_Time::Get_Time_Form;
}

ST_Set_Time::~ST_Set_Time() {}

int ST_Set_Time::take_next_step(CURLcode code) {
  return (this->*next_step)(code);
}

int ST_Set_Time::Get_Time_Form( CURLcode code ) {
  char URL[80];
  nl_assert( code == 0 );
  snprintf(URL,80,"http://%s/config/hardware/settime.htm", hostname);
  co->set_url(URL);
  next_step = &ST_Set_Time::Submit_Time_Form;
  msg(0, "ST_Set_Time::Get_Time_Form: %s", URL);
  co->multi_add( "Get empty time form" );  
  return 0;
}

int ST_Set_Time::Submit_Time_Form( CURLcode code ) {
  if ( code == 0 ) {
    if ( Set_Time_Form == 0 ) {
      Set_Time_Form = co->find_form(1);
    }
    msg(0, "ST_Set_Time::Submit_Time_Form" );
    if ( Set_Time_Form ) {
      time_t now;
      struct tm *tms;
      char tbuf[11];
      now = time(NULL) + delta_T;
      tms = gmtime(&now);
      // Now set date and time
      snprintf( tbuf, 11, "%02d/%02d/%04d", tms->tm_mday, tms->tm_mon+1, tms->tm_year+1900 );
      Set_Time_Form->set( "DATE", tbuf );
      snprintf( tbuf, 11, "%02d:%02d:%02d", tms->tm_hour, tms->tm_min, tms->tm_sec );
      Set_Time_Form->set( "TIME", tbuf ); // plus delta_T, UTC
      if ( ! Set_Time_Form->select_single( "TZH", "0" ) )
        msg(2, "Did not find TZH to select" );
      if ( ! Set_Time_Form->select_single( "TZM", "0" ) )
        msg(2, "Did not find TZM to select" );
      Set_Time_Form->checkbox( "DAYLS", "1", 0 );
      Set_Time_Form->submit_setup( "WRK", "Save" );
      next_step = &ST_Set_Time::Report_Time_Form;
      co->multi_add( "Submit new time setting" );
      return 0;
    }
  }
  msg(2, "Form retrieval failed");
  co->dequeue_transaction();
  return 0;
}

int ST_Set_Time::Report_Time_Form( CURLcode code ) {
  if ( code == 0 ) {
    msg( 0, "Time set successfully" );
  } else {
    msg( 2, "Time submit failed" );
  }
  co->dequeue_transaction();
  return 0;
}
