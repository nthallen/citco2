/**
 * \file SunTracker.h
 */
#ifndef SUNTRACKER_H_INCLUDED
#define SUNTRACKER_H_INCLUDED

#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "dasio/tm_data_sndr.h"
#include "dasio/cmd_reader.h"
#include "SunTrack.h"
#define HOST "10.10.0.2"
// #define HOST "128.100.78.55"

extern const char *hostname;
extern int ST_Revision;

#ifdef __cplusplus

#include "curllog/curl_select.h"

using namespace DAS_IO;

typedef struct {
    float Geo_Position_Azimuth; // Degrees
    float Geo_Position_Elevation; // Degrees
    float Local_Position_Azimuth; // Degrees
    float Local_Position_Elevation; // Degrees
    float Counter_Azimuth; // Integer
    float Counter_Elevation; // Integer
    float Tracker_Diode_Intensity_x1; // Integer
    float Tracker_Diode_Intensity_x2; // Integer
    float Tracker_Diode_Intensity_y1; // Integer
    float Tracker_Diode_Intensity_y2; // Integer
    float Intensity_Dx; // Integer
    float Intensity_Dy; // Integer
    float Counter_Total_Intensity; // Integer
    float Counter_Clouds; // Integer
    float Counter_Differ_Azimuth; // Integer
    float Counter_Differ_Elevation; // Integer
    float AZI_Speed_DAC; // Integer
    float ELE_Speed_DAC; // Integer
    float Cloud_Detector_Threshold; // Integer
    float AZI_Voltage; // Degrees
    float ELE_Voltage; // (TRACKER DIODE??)   Degrees
    unsigned char Spacer_0;
    unsigned char Cloud_State; // 'C' cloud detected, 'N' no cloud detected
    unsigned char Tracker_Diode_Gain;
    unsigned char Cloud_Detector; // 'C'="ON" 'N'="OFF"
    char strmessage[41];
    char wantedmode[20];
    char currentmode[36];
    char flipstate[20];
    char moving[9];
    char datetime[41];
} __attribute__((packed)) ST_Packet;

class ST_Pkt_sndr : public TM_data_sndr {
  public:
    inline ST_Pkt_sndr(const char *datum, const void *data, uint16_t size)
      : TM_data_sndr("TM_SSR", 0, datum, data, size) {}
    bool app_input() override;
};

class ST_Pkt {
  public:
    ST_Pkt();
    void new_data(unsigned char *data);
    void report();
    ST_Pkt_sndr *get_ST_SSR_sndr();
  private:
    ST_Packet pkt;
    ST_SSR_t ST_SSR;
    ST_Pkt_sndr *ST_SSR_sndr;
    //send_id ST_SSR_id;
    int ready;
    int stale;
    bool dT_reported;
    char strmessage[42];
    char wantedmode[21];
    char currentmode[37];
    char flipstate[21];
    char moving[10];
    char datetime[42];
    unsigned char Cloud_Detector;
    int check_string(const char *name, char *dest, const char *src,
          int maxlen, int verbose);
    float swap(float in);
};

class STPort : public Interface {
  public:
    STPort(const char *ip, int revision); // create UDP socket
    void transmit(bool first); // send magic packet, update next_transmit_time
    void adopted() override;
    bool protocol_input() override;
    bool protocol_timeout() override;
    // int ProcessData(int flag); // read and process incoming UDP packet
  private:
    const char *hostname;
    struct sockaddr *name;
    socklen_t namelen;
    // Timeout TO;
    // struct timespec next_transmit_time;
    // struct timeval timeout;
    static const char magic[64];
    // char ibuf[256];
    ST_Pkt Packet;
};

class STcmd : public Cmd_reader {
  public:
    STcmd(const char *name, curl_multi_obj *co_in);
    bool app_input() override;
    // int ProcessData(int flag);
  private:
    //static const unsigned ibufsize = 256;
    //char ibuf[ibufsize];
    //unsigned bufidx;
    curl_multi_obj *co;
    bool execute_cmd();
};

class ST_Set_Time;
typedef int (ST_Set_Time::*ST_Set_TimeReq)(CURLcode);

class ST_Set_Time : public Transaction {
  public:
    ST_Set_Time(curl_multi_obj *co, const char *trans_desc, int delta_T);
    ~ST_Set_Time();
    int take_next_step(CURLcode);
  private:
    static curl_form *Set_Time_Form;
    int delta_T;
    ST_Set_TimeReq next_step;
    int Get_Time_Form(CURLcode);
    int Submit_Time_Form(CURLcode);
    int Report_Time_Form(CURLcode);
};

class ST_Set_Mode;
typedef int (ST_Set_Mode::*ST_Set_ModeReq)(CURLcode);

class ST_Set_Mode : public Transaction {
  public:
    ST_Set_Mode(curl_multi_obj *co, const char *trans_desc, int mode);
    ~ST_Set_Mode();
    int take_next_step(CURLcode);
  private:
    int mode;
    ST_Set_ModeReq next_step;
    int Submit_req(CURLcode);
    int End(CURLcode);
};

class ST_Read_Top;
typedef int (ST_Read_Top::*ST_Read_TopReq)(CURLcode);

class ST_Read_Top : public Transaction {
  public:
    ST_Read_Top(curl_multi_obj *co, const char *trans_desc);
    ~ST_Read_Top();
    int take_next_step(CURLcode);
  private:
    ST_Read_TopReq next_step;
    int Get_top(CURLcode);
    int Got_top(CURLcode);
};

#endif // __cplusplus
#endif
