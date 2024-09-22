#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include "STEnc.h"
#include "nortlib.h"
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
	int hasDIO = 0;
	DigitalPortType portType;
	DigitalPortIoType portIoType;

	char portTypeStr[MAX_STR_LENGTH];
	char portIoTypeStr[MAX_STR_LENGTH];

	unsigned long long data = 0;
	UlError err = ERR_NO_ERROR;

	// Get descriptors for all of the available DAQ devices
	err = ulGetDaqDeviceInventory(interfaceType, devDescriptors,
            &numDevs);
  if (check_ulerr(err, "ulGetDaqDeviceInventory"))
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
  
  if (!strcmp(devDescriptors[0].productName, "USB-PDISO8"))
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
  if (check_ulerr(err, "ulGetDaqDeviceInventory"))
    return true;
	msg(MSG, "%s ready\n", devDescriptors[0].devString);
  read_both();
}

bool STEnc::set_relays(uint8_t val)
{
  unsigned long long data = val;
  UlError err = ulDOut(daqDeviceHandle, AUXPORT0, data);
  return check_ulerr(err, "ulDOut(PORT0)");
}

uint8_t STEnc::read(DigitalPortType ptype, const char *desc)
{
  unsigned long long data;
	UlError err = ulDIn(daqDeviceHandle, ptype, &data);
  check_ulerr(err, desc);
  uint8_t rval = (uint8_t)data;
  return rval;
}

void STEnc::read_both()
{
  uint8_t relays = read(AUXPORT0, "ulDIn(PORT0)");
  uint8_t status = read(AUXPORT1, "ulDIn(PORT1)");
  TM->STEnc_status =
    ((relays & RELAY_OPEN) ? S_OPEN_RELAY : 0) |
    ((relays & RELAY_CLOSE) ? S_OPEN_RELAY : 0) |
    ((status & SW_OPEN) ? S_OPEN_LIMIT : 0) |
    ((status & SW_CLOSE) ? S_CLOSE_LIMIT : 0);
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
