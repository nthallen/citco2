#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#define PIPE_READ 0
#define PIPE_WRITE 1

I need to execute: /usr/bin/ssh -t webpower ./webpower_cmds.ash
so:
  int fd_in, fd_out;
  const char *argv[5] = {
    "/usr/bin/ssh",
    "-t",
    "webpower",
    "./webpower_cmds.ash",
    0
  }
  exec_child(&fd_in, &fd_out, argv[0], argv, environ);
  Now write to fd_in, read from fd_out

/**
 * Values are returned to *fd_in and *fd_out
 * @param fd_in pointer to var set to pipe connected to child's stdin
 * @param fd_out pointer to var set to pipe connected to child's stdout
 * @param Command The command to be exec()ed
 * @param Arguments The command arguments
 * @param Environment The environment vector
 * @return PID of the child process or -1 on error
 */
int exec_child(int *fd_in, int *fd_out, const char* Command, char* const Arguments[], char* const Environment[]) {
  int StdinPipe[2];
  int StdoutPipe[2];
  int nChild;
  char nChar;
  int nResult;

  if (pipe(StdinPipe) < 0) {
    perror("allocating pipe for child input redirect");
    return -1;
  }
  if (pipe(StdoutPipe) < 0) {
    close(StdinPipe[PIPE_READ]);
    close(StdinPipe[PIPE_WRITE]);
    perror("allocating pipe for child output redirect");
    return -1;
  }

  nChild = fork();
  if (0 == nChild) {
    // child continues here

    // redirect stdin
    if ((dup2(StdinPipe[PIPE_READ], STDIN_FILENO) == -1) ||
        (dup2(StdoutPipe[PIPE_WRITE], STDOUT_FILENO) == -1) ||
        (dup2(StdoutPipe[PIPE_WRITE], STDERR_FILENO) == -1)) {
      exit(errno);
    }

    // all these are for use by parent only
    close(StdinPipe[PIPE_READ]);
    close(StdinPipe[PIPE_WRITE]);
    close(StdoutPipe[PIPE_READ]);
    close(StdoutPipe[PIPE_WRITE]); 

    // run child process image
    // replace this with any exec* function find easier to use ("man exec")
    nResult = execve(Command, Arguments, Environment);

    // if we get here at all, an error occurred, but we are in the child
    // process, so just exit
    exit(nResult);
  } else if (nChild > 0) {
    // parent continues here

    // close unused file descriptors, these are for child only
    close(StdinPipe[PIPE_READ]);
    close(StdoutPipe[PIPE_WRITE]); 

    *fd_in = StdinPipe[PIPE_WRITE];
    *fd_out = StdoutPipe[PIPE_READ];

    return nChild;
  } else {
    // failed to create child
    close(StdinPipe[PIPE_READ]);
    close(StdinPipe[PIPE_WRITE]);
    close(StdoutPipe[PIPE_READ]);
    close(StdoutPipe[PIPE_WRITE]);
  }
  return nChild;
}
