#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include "STEnc_int.h"
#include "nl.h"
#include "uldaq.h"

#define MAX_STR_LENGTH 64

STEnc::STEnc(STEnc_TM_t *TM)
     : TM(TM),
       daqDeviceHandle(0)
{
}


STEnc::~STEnc()
{
  if (daqDeviceHandle)
  {
    ulReleaseDaqDevice(daqDeviceHandle);
    daqDeviceHandle = 0;
  }
}

bool STEnc::connect()
{
  int MAX_DEV_COUNT = 1;
  DaqDeviceDescriptor devDescriptors[MAX_DEV_COUNT];
  DaqDeviceInterface interfaceType = USB_IFC;
  unsigned int numDevs = MAX_DEV_COUNT;
  UlError err = ERR_NO_ERROR;
  // Get descriptors for all of the available DAQ devices
  err = ulGetDaqDeviceInventory(interfaceType, devDescriptors,
      &numDevs);
  if (check_ulerror(err, "ulGetDaqDeviceInventory"))
    return true;

  // verify at least one DAQ device is detected
  if (numDevs == 0)
  {
    msg(MSG_ERROR, "No DAQ device is detected");
    return true;
  }

  if (numDevs != 1)
  {
    msg(MSG_ERROR, "%d DAQ devices detected", numDevs);
    return true;
  }
  
  if (strcmp(devDescriptors[0].productName, "USB-PDISO8"))
  {
    msg(MSG_ERROR, "Unrecognized DAQ device: '%s'",
      devDescriptors[0].productName);
    return true;
  }

  // get a handle to the DAQ device associated with the first descriptor
  daqDeviceHandle = ulCreateDaqDevice(devDescriptors[0]);

  if (daqDeviceHandle == 0)
  {
    msg(MSG_ERROR, "Unable to create a handle to USB-PDISO8");
    return true;
  }
  // establish a connection to the DAQ device
  err = ulConnectDaqDevice(daqDeviceHandle);
  if (check_ulerror(err, "ulGetDaqDeviceInventory"))
    return true;
  msg(MSG, "%s ready\n", devDescriptors[0].devString);
  
  TM->STEnc_status = 0;
  // read_both();
  return false;
}

bool STEnc::disconnect()
{
  if (daqDeviceHandle)
  {
    ulDisconnectDaqDevice(daqDeviceHandle);
    ulReleaseDaqDevice(daqDeviceHandle);
    daqDeviceHandle = 0;
  }
  return false;
}

bool STEnc::reconnect()
{
  disconnect();
  connect();
  return false;
}

bool STEnc::set_relays(uint8_t val)
{
  unsigned long long data = val;
  UlError err = ulDOut(daqDeviceHandle, AUXPORT0, data);
  return check_ulerror(err, "ulDOut(PORT0)");
}

bool STEnc::read(DigitalPortType ptype, const char *desc, uint16_t &rval)
{
  unsigned long long data;
	UlError err = ulDIn(daqDeviceHandle, ptype, &data);
  if (check_ulerror(err, desc))
    return true;
  rval = (uint16_t)(data & 0xFF);
  return false;
}

bool STEnc::read_both()
{
  if (TM->STEnc_status & (1<<7)) {
    reconnect();
  } else {
    uint16_t relays, status;
    if (read(AUXPORT0, "ulDIn(PORT0)", relays) ||
        read(AUXPORT1, "ulDIn(PORT1)", status)) {
      TM->STEnc_status |= 1<<7;
      return true;
    }
    TM->STEnc_status = status | (relays << 8);
  }
  return false;
//  ((relays & RELAY_OPEN) ? S_OPEN_RELAY : 0) |
//  ((relays & RELAY_CLOSE) ? S_OPEN_RELAY : 0) |
//  ((status & SW_OPEN) ? S_OPEN_LIMIT : 0) |
//  ((status & SW_CLOSE) ? S_CLOSE_LIMIT : 0);
}

bool STEnc::check_ulerror(UlError err, const char *desc)
{
	if(err == ERR_NO_ERROR) return false;
  char errMsg[ERR_MSG_LEN];
  ulGetErrMsg(err, errMsg);
	msg(MSG_ERROR, "%s: ulError: %d: %s",
      desc, err, errMsg);
  return true;
}
