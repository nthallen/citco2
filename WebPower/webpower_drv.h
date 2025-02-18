#ifndef WEBPOWER_DRV_H_INCLUDED
#define WEBPOWER_DRV_H_INCLUDED
#include "dasio/cmd_reader.h"
#include "webpower.h"

using namespace DAS_IO;

/**
 * Class to talk to the webpower_cmds.ash script running
 * on the webpower device. We need to open an ssh
 * connection invoking the script, the write and read
 * from it.
 */
class webpower_dev : public Interface {
  public:
    webpower_dev();
    void connect();
    bool queue_command(const char *cmd, int len);
    bool serialized_signal_handler(uint32_t sigs_seen) override;
  protected:
    // ~webpower_dev();
    bool protocol_input() override;
    bool protocol_timeout() override;
    bool tm_sync() override;
    void process_commands();
    /**
     * @param cmd Command string to send
     * @returns true on success
     */
    bool issue_command(const char *cmd);
    void tear_down();
    void queue_retry();

    int child_pid;
    int w_fd; //< send commands here
    bool werr_reported;
    char cmd_pending[20];
    bool cmd_is_pending;
    const char *status_pending;
    bool status_is_pending;
    const char *pending; //< pending command
    enum { s_idle, s_cmd, s_status } state;
};

class webpower_cmd : public Cmd_reader {
  public:
    webpower_cmd(webpower_dev *wp);
    // ~webpower_cmd();
  protected:
    bool app_input() override;
    webpower_dev *wp;
};

#endif

