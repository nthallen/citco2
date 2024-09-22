#ifndef STENC_H_INCLUDED
#define STENC_H_INCLUDED

  #include "dasio/cmd_reader.h"
  #include "uldaq.h"

  using namespace DAS_IO;

  /*
   * status:
   *   0: Enclosure open limit status
   *   1: Enclosure closed limit status
   *   2: Enclosure power status
   *   3: Enclosure open command status
   *   4: Enclosure closed command status
   */
  typedef struct __attribute__((__packed__)) {
    uint16_t STEnc_status;
  } STEnc_TM_t;

  #ifndef STENC_HIDE_INTERNALS
  #ifdef __cplusplus
   
    class STEnc {
      public:
        STEnc(STEnc_TM_t *STEnc_TM);
        ~STEnc();
        bool connect();
        
        /**
         * @param val Value to be written to the relay output
         * @return true if there is an error (reported)
         */
        bool set_relays(uint8_t val);
        void read_both();
        // Command bit definitions
        static const uint8_t RELAY_OPEN = 2;
        static const uint8_t RELAY_CLOSE = 1;
        static const uint8_t RELAY_NONE = 0;
        // Status bit definitions for telemetry:
        static const uint16_t S_OPEN_LIMIT = 1;
        static const uint16_t S_CLOSE_LIMIT = 2;
        static const uint16_t S_OPERATING = 4;
        static const uint16_t S_WEATHER = 8;
        static const uint16_t S_POWER = 0x10;
        static const uint16_t S_SPARE = 0x20;
        static const uint16_t S_CLOSE_RELAY = 0x100;
        static const uint16_t S_OPEN_RELAY = 0x200;
        // Status switch bit definitions:
        static const uint8_t SW_OPEN = 1;
        static const uint8_t SW_CLOSE = 2;
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
         * @return The value read
         */
        uint8_t read(DigitalPortType ptype, const char *desc);
        
        STEnc_TM_t *TM;
        DaqDeviceHandle daqDeviceHandle;
    };
    
    class STEnc_cmd : public Cmd_reader {
      public:
        STEnc_cmd(STEnc *STE);
        ~STEnc_cmd();
        bool protocol_input();
      protected:
        bool tm_sync() override;
      private:
        STEnc *STE;
    };

  #endif // __cplusplus
  #endif // STENC_HIDE_INTERNALS
#endif // STENC_H_INCLUDED
