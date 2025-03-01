#ifndef LN2_H_INCLUDED
#define LN2_H_INCLUDED
#include <stdint.h>

typedef struct {
  uint16_t LN2TankT;
  uint16_t InSbT;
  uint16_t LN2P;
  uint16_t LN2Depth;
  uint8_t  LN2Stat;
  uint8_t  LN2DrvStat;
} LN2_t;

/* LN2Stat bits:
  0: rs_recv  RS232 character received
  1: Pumping  Pump request
  2: Valve_on Valve closed
  3: TMB_on   TMB in heating mode
  4: Active   Pump status : active
  5: Sleeep   Pump status : sleep
  6: Warn_on  Pump status : warning
  7: Alarm_on pump status : alarm
*/

#define LN2_DS_INIT 1 // Set after calibration values have been queried
#define LN2_DS_POWER 2 // Not implemented
#define LN2_DS_CRNT 0x7C
#define LN2_DS_TankT_CRNT 4
#define LN2_DS_InSbT_CRNT 8
#define LN2_DS_LN2P_CRNT 0x10
#define LN2_DS_LN2Depth_CRNT 0x20
#define LN2_DS_LN2Stat_CRNT 0x40

extern const char *ln2_port;

#ifdef __cplusplus

#include <string.h>
#include <vector>
#include "dasio/tm_data_sndr.h"
#include "dasio/cmd_reader.h"
#include "dasio/serial.h"
#include "dasio/loop.h"

class LN2Driver;

using namespace DAS_IO;

// inherit from TM_data_sndr and override app_input()
class LN2TM : public TM_data_sndr {
  public:
    LN2TM(LN2_t *data);
    ~LN2TM();
    bool app_input() override;
  private:
    LN2_t *TMdata;
};

class LN2Request {
  public:
    LN2Request(const char *cmd);
    LN2Request(const char *cmd, uint8_t *res, uint8_t flag = 0);
    LN2Request(const char *cmd, uint16_t *res, uint8_t flag = 0);
    const char *cmdtxt;
    int cmdlen;
    int n_hex_responses;
    void *result;
    uint8_t flagval;
  private:
};

class Cmd_rdr : public Cmd_reader {
  public:
    Cmd_rdr(LN2Driver *top);
    ~Cmd_rdr();
    bool app_input();
    // int ProcessData(int flag);
  private:
    LN2Driver *top;
    std::vector<LN2Request> Cmds;
    uint8_t PwrStat;
};

class LN2 : public Serial {
  public:
    LN2(const char *port, LN2_t *data);
    ~LN2();
    void LN2Command(LN2Request *cmd);
  protected:
    bool tm_sync() override;
    bool protocol_input() override;
    bool protocol_timeout() override;
  private:
    void next_request();
    LN2_t *TMdata;
    bool TM_reported;
    bool Initialized;
    int WrongCmdCount;
    const LN2Request *CmdReq; // Command stored here when queued
    const LN2Request *CurReq; // Then moved here...
    // CalVals:
    //  0: Vessel sensor cold calibration point (-196C): re 010 2    (ed 00)
    //  1: Vessel sensor room (-100C): re 018 2    (a0 01)
    //  2: Vessel empty (30C): re 090 2    (7d 02)
    //  3: Main sensor cold calibration point (-196C): re 018 2    (8f 00)
    //  4: Main sensor warm calibration point (30C): re 095 2    (dd 01)
    //  5: Pressure Sensor 0mBar LN2P0: rm 0c8 2    (23 00)
    uint16_t CalValRpt[6];
    static const uint16_t CalVal[6];
    std::vector<LN2Request> InitReqs;
    std::vector<LN2Request>::const_iterator InitReq;
    std::vector<LN2Request> Reqs;
    std::vector<LN2Request>::const_iterator Req;
};

class LN2Driver {
  public:
    LN2Driver(const char *serial_port);
    ~LN2Driver();
    void LN2Command(LN2Request *cmd);
    inline void event_loop() { ELoop.event_loop(); }
  private:
    Loop ELoop;
    LN2_t TMdata;
    LN2TM *TM_if;
    Cmd_rdr *Cmd_if;
    LN2 *LN2_if;
};

#endif
#endif
