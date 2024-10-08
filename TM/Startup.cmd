%{
  #ifdef SERVER

  void write_startup( const char *s ) {
    if (s == NULL) {
      if (unlink("Startup.tmas"))
        msg(2, "Unable to delete Startup.tmas");
    } else {
      FILE *fp = fopen( "Startup.tmas", "w" );
      if ( fp == 0 ) msg( 2, "Unable to write to Startup.tmas" );
      else {
        fprintf( fp, "%s\n", s );
        fclose( fp );
      }
    }
  }

  #endif /* SERVER */
%}
&command
  : Startup Define %s (Enter Startup Command) * { write_startup($3); }
  : Startup Clear * { write_startup(NULL); }
  ;
