#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

void failwith (char *msg) {
  perror (msg);
  exit (1);
}

int readFd (int fd, char *buf, int size){
  return read (fd, buf, size);
}

void closeFd (int fd) {
  close (fd);
}

int waitForExit (int pid){
  int status;
  while (waitpid (pid, &status, 0) != pid) ;
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  else if (WIFSIGNALED(status)) return -WTERMSIG(status);
  else failwith ("waitForExit fail");
}

void exec (int argc, char **arg, char *dir, int timeout, int *ret) {
  int i, pid;
  char **argv = malloc (sizeof(char*) * (argc + 1));
  int stdoutPipe[2] = {-1, -1};
  int stderrPipe[2] = {-1, -1};

  for (i = 0; i< argc; i++) argv[i] = arg[i];
  argv[argc] = NULL;

  if (pipe (stdoutPipe) < 0 || pipe (stderrPipe) < 0)
    failwith ("pipe fail");

  pid = vfork ();
  
  if (pid < 0) failwith ("vfork fail");
  else if (pid == 0) {
    close (stdoutPipe[0]);
    close (stderrPipe[0]);

    dup2 (stdoutPipe[1], 1); close(stdoutPipe[1]);
    dup2 (stderrPipe[1], 2); close(stderrPipe[1]);

    alarm (timeout);
    if (chdir (dir) < 0) failwith ("chdir fail");
    execv (argv[0], argv);
  }
  
  close (stdoutPipe[1]);
  close (stderrPipe[1]);

  ret[0] = pid;
  ret[1] = stdoutPipe[0];
  ret[2] = stderrPipe[0];

}
