# Sun Tracker Enclosure README

This driver is a replacement for the QNX6 version. For this
version, we are using a Measurement Computing USB-PDISO8
device, which includes an output port with 8 relays and
an input port with 8 isolated digital inputs. Much of what
follows is relevant context. The specific output values
used on the PDISO8 are different (see STEnc.h)

## Intro to the QNX6 Version

This driver is a replacement for the RoboDome driver.
The initial goal will be to mimic the interface to the data
acquisition system so that commands and data can be handled
without modifications to, say, IFS.tma.

That said, this is an obvious candidate for the new acquisition
module system that would allow us to customize the display for
similar systems with different data.

This is an open/close enclosure like that used at Four Corners and Manaus.
That means we don't need to support rotation as with the RoboDome.
Unlike the Four Corners enclosure, we do not have any positional
readback besides the limit switches, so we probably will not use
the Dome_azi channel.

## Commands:
  
The existing dome commands from TM/Dome.cmd are:
```
    &command
      : Dome &DOME_Command * { if_Dome.Turf("%s\n", $2); }
      : Dome GoTo Azimuth %d (Enter Azimuth in degrees) *
        { if_Dome.Turf("A:%d\n", $4 ); }
      ;
    &DOME_Command <const char *>
      : Open Shutter { $0 = "S:1"; }
      : Close Shutter { $0 = "S:0"; }
      : Home { $0 = "H"; }
      : Exit { $0 = "Q"; }
      ;
```

  We will complain about Azimuth and Home, but we should at least
  recognize them to allow for compatibility.
  
  On the device side:
    Open Cover = #000001 (Relay 1 on, all others off) Response = >
    Close Cover = #000002 (Relay 2 on, all others off) Response = >
    All relays off = #000000 Response = >

Device Communications:
  The protocol for communication with the 7052 and 7053B are described in
  CB7052_manual.pdf. I would like to use the checksum capability, but
  the manual is a bit incomplete with regard to this. Specifically, it
  is not clear what code to use for the 7063B. However, we can read
  the current configuration.
  
  One question I would like answered is what happens if checksums are
  not enabled but you include one with a command. I believe both questions
  can be resolved by issuing the following requests:
    $002B6 (read configuration of address 00 with checksum)
    $002   (read configuration of address 00 without checksum)
    $012B7 (read configuration of address 01 with checksum)
    $012   (read configuration of address 01 without checksum)
  If any errors are reported, then we need to be sensitive when
  issuing commands. Probably start by requesting status with
  checksum. If an error is returned, then attempt to set configuration
  (without checksum). Then reiterate.