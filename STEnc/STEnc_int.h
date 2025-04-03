#ifndef STENC_INT_H_INCLUDED
#define STENC_INT_H_INCLUDED

  #include "STEnc.h"
  #include "dasio/cmd_reader.h"
  #include "uldaq.h"

  using namespace DAS_IO;

  #ifndef STENC_HIDE_INTERNALS
  #ifdef __cplusplus
   
    class STEnc {
      public:
        STEnc(STEnc_TM_t *STEnc_TM);
        ~STEnc();
        bool connect();
        bool disconnect();
        bool reconnect();
        
        /**
         * @param val Value to be written to the relay output
         * @return true if there is an error (reported)
         */
        bool set_relays(uint8_t val);
        bool read_both();
        // Command bit definitions
        static const uint8_t RELAY_OPEN = 2;
        static const uint8_t RELAY_CLOSE = 1;
        static const uint8_t RELAY_NONE = 0;
        static const uint8_t RELAY_ENC_MASK = 3;
        static const uint8_t RELAY_ASE_DS_2C_MAN = 0x20;
        static const uint8_t RELAY_ASE_DS_2C_STBY = 0x10;
        static const uint8_t RELAY_ASE_DS_2C_NONE = 0x00;
        static const uint8_t RELAY_ASE_DS_2C_MASK = 0x30;
        // Status bit definitions for telemetry:
        static const uint16_t S_OPEN_LIMIT = 1;
        static const uint16_t S_CLOSE_LIMIT = 2;
        static const uint16_t S_OPERATING = 4;
        static const uint16_t S_WEATHER = 8;
        static const uint16_t S_POWER = 0x10;
        static const uint16_t S_SPARE = 0x20;
        static const uint16_t S_CLOSE_RELAY = 0x100;
        static const uint16_t S_OPEN_RELAY = 0x200;
      private:
        /**
         * @param err Error return from library function
         * @param desc Description of the context
         * @return true if there is an error
         */
        bool check_ulerror(UlError err, const char *desc);
        
        /**
         * @param ptype The DigitalPortType to read from
         * @param desc The context for error reporting
         * @param rval The returned value
         * @return true on error
         */
        bool read(DigitalPortType ptype, const char *desc, uint16_t &rval);
        
        STEnc_TM_t *TM;
        DaqDeviceHandle daqDeviceHandle;
    };
    
    class STEnc_cmd : public Cmd_reader {
      public:
        STEnc_cmd(STEnc *STE);
        ~STEnc_cmd();
        bool app_input();
      protected:
        bool tm_sync() override;
      private:
        STEnc *STE;
        uint8_t relay_cmd;
    };

  #endif // __cplusplus
  #endif // STENC_HIDE_INTERNALS
#endif // STENC_H_INCLUDED
