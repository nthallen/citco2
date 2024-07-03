This directory contains the SunTracker driver for the
Bruker Sun Tracker with web interface.

Most of the sun tracker data is trasmitted via UDP packets.
There is no documentation for the UDP packet format, but
we were granted source code access to the Java source for
the web applet that displays the status, and from that
we were able to reconstruct the format.

The sun tracker only supports one client at a time. If you
are not the current client, you will not receive any UDP
packets. The client must also transmit keep-alive UDP
packets back to the sun tracker to maintain communication.
Although we had considerable difficulty figuring out
how things worked originally, getting the flow of packets
started is really quite simple: Make sure you are sending
the UDP keep alive packets and then access the sun tracker's
home page. This latest step is now implemented with the
command "Sun Tracker Connect".

Notes:

Currently the only command implemented is to set the time
on the Sun Tracker. The syntax is:

	T([-+]\d+)?

which is to say "T" followed by an optional signed integer.
The integer value is an offset that should be added to the
current time before sending the time to the SunTracker.

To initialize the suntracker, we need to issue the following commands:
  Connect: http://suntracker/
    Wait for some sort of activity.
  Init: http://suntracker/tracker/toptrack.htm?sub=Send&TRM=5
    Wait for modus to switch to INIT and then to TPL
  TPS: http://suntracker/tracker/toptrack.htm?sub=Send&TRM=1
    Wait for position to stop changing and hopefully agree with Sol_ele
  TTM: http://suntracker/tracker/toptrack.htm?sub=Send&TRM=4

Since these are discrete, it may make sense to implement these as
discrete commands orchestrated from the algo.
