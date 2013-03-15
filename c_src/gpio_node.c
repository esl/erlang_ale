#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

/* this node will be known as c1@<hostname> */



/* the erlang node will send messages using this format:
 * {call, Pid, Msg}
 * (others might be added later)
 * call has to send a response back to the Pid.
 */

void get_hostname(char*);
void strstrip(char * );

int main(int argc, char **argv) {
  int fd;                                  /* fd to Erlang node */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *msg_type, *fromp, *tuplep, *fnp, *arg1p, *arg2p, *resp;
  int res;

  char hostname[255];
  char erlang_nodename[255] = "e1@";
  
  erl_init(NULL, 0);

  if (erl_connect_init(1, "secretcookie", 0) == -1)
    erl_err_quit("erl_connect_init");

  /* now we figure out where to connect to */
  get_hostname(hostname);
  strcat(erlang_nodename, hostname);
  strstrip(erlang_nodename);
  
  if ((fd = erl_connect(erlang_nodename)) < 0)
    erl_err_quit("erl_connect");
  fprintf(stderr, "Connected to %s\n\r",erlang_nodename);

  while (loop) {

     got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
     if (got == ERL_TICK) {
        /* ignore */
     } else if (got == ERL_ERROR) {
        loop = 0;
     } else {
        
        if (emsg.type == ERL_REG_SEND) {
           msg_type = erl_element(1, emsg.msg);
           fromp = erl_element(2, emsg.msg);
           tuplep = erl_element(3, emsg.msg);
           fnp = erl_element(1, tuplep);


           if (strncmp(ERL_ATOM_PTR(msg_type), "call", 4) == 0) {
              /* call expects a msg back */
              /* always at least one argument so we get that out first*/
              arg1p = erl_element(2, tuplep);              

              if (strncmp(ERL_ATOM_PTR(fnp), "init", 4) == 0) {
                 arg2p = erl_element(3, tuplep);
                 /* @todo implement the real impl here */
                 resp = erl_format("ok");
              } else if (strncmp(ERL_ATOM_PTR(fnp), "write", 5) == 0) {
                    arg2p = erl_element(3, tuplep);
                    /* @todo implement the real impl here */
                    resp = erl_format("ok");
              } else if (strncmp(ERL_ATOM_PTR(fnp), "read", 4) == 0) {
                 /* @todo implement the real impl here */
                 resp = erl_format("ok");
              } else if (strncmp(ERL_ATOM_PTR(fnp), "set_int", 7) == 0) {
                 arg2p = erl_element(3, tuplep);
                 /* @todo implement the real impl here */
                 resp = erl_format("ok");
              }

              erl_send(fd, fromp, resp);
           } else if (strncmp(ERL_ATOM_PTR(msg_type), "cast", 4) == 0) {
              /*  cast does not expect a msg back */
           }
           
	erl_free_term(emsg.from); erl_free_term(emsg.msg);
	erl_free_term(fromp); erl_free_term(tuplep);
	erl_free_term(fnp); erl_free_term(arg1p);
	erl_free_term(arg2p); erl_free_term(resp);
      }
    }
  }
}

void
get_hostname(char* name)
{
   FILE *f;
   
   if( !(f = popen("hostname -s", "r")) ){
      perror("Can't execute cmd");
   }
   
   fgets(name, 255, f);

}

void  strstrip( char *s )
{
  char *start;
  char *end;

  // Exit if param is NULL pointer
  if (s == NULL)
    return;

  // Skip over leading whitespace
  start = s;
  while ((*start) && isspace(*start))
    start++;      

  // Is string just whitespace?
  if (!(*start)) 
  {         
    *s = 0x00; // Truncate entire string
    return;     
  }     

  // Find end of string
  end = start;
  while (*end)         
    end++;     

  // Step back from NUL
  end--;      

  // Step backward until first non-whitespace
  while ((end != start) && isspace(*end))         
    end--;     

  // Chop off trailing whitespace
  *(end + 1) = 0x00;

  // If had leading whitespace, then move entire string back to beginning
  if (s != start)
    memmove(s, start, end-start+1);      

  return; 
} 
