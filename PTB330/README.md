# Vaisala PTB330 Baracap Digital Barometer
We will connect the PTB330 user port to COM2 aka /dev/ttyS1 RS232.
The default configuration is 4800,7,E,1. At the moment, I don't see
any reason to change that as we expect to have low frequency updates.

## Configuration

Commands are terminated by carriage return.

 - FORM P,TP1#RN
 - UNIT
   - to see current settings
 - UNIT ??
   - to see choices, then probably
 - UNIT hPa
 - UNIT C
 - SMODE
   - to view current mode
 - SMODE POLL
   - Set to POLL protocol
 - SEND
   - To request a measurement

## Driver operation

We will assume the device has been configured for POLL mode, unless
that proves to be a bad assumption (i.e. it is not sustained through
a power cycle.) Then we simply need to send a 'SEND' command whenever
we want a new pressure reading.

