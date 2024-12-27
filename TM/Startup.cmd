%{
  #ifdef SERVER

  void write_startup_file(const char *filename, const char *s) {
    if (s == 0) {
      if (unlink(filename))
        msg(2, "Unable to delete %s", filename);
    } else {
      FILE *fp = fopen(filename, "w");
      if ( fp == 0 ) msg( 2, "Unable to write to %s", filename);
      else {
        fprintf( fp, "%s\n", s );
        fclose( fp );
      }
    }
  }
  
  void write_startup(const char *s) {
    write_startup_file("Startup.tmas", s);
  }
  
  void write_unexpected(const char *s) {
    write_startup_file("Unexpected.tmas", s);
  }
  
  void write_wakeup(const char *s) {
  }

  #endif /* SERVER */
%}
&command
  : Startup Define %s (Enter Startup Command) * { write_startup($3); }
  : Startup Clear * { write_startup(0); }
  : Startup Unexpected Set * { write_unexpected("Validate Unexpected_Restart;"); }
  : Startup Unexpected Clear * { write_unexpected(0); }
  : Startup Wakeup Set * { write_wakeup("Validate Up_After_Bedtime;"); }
  : Startup Wakeup Clear * { write_wakeup(0); }
  ;
