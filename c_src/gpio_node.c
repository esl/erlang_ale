#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <inttypes.h>
#include <fcntl.h>
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

/* gpio functions */
ETERM* gpio_init(ETERM*, ETERM*);
ETERM* gpio_release(ETERM*);
ETERM* gpio_write(ETERM*, ETERM*);

/* helper functions */

static int gpio_valfd (int);
void get_hostname(char*);
void strstrip(char * );

int main(int argc, char **argv) {
  int fd;                                  /* fd to Erlang node */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *msg_type, *fromp, *ref, *tuplep, *fnp, *arg1p, *arg2p, *resp;

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
           ref = erl_element(3, emsg.msg);
           tuplep = erl_element(4, emsg.msg);
           fnp = erl_element(1, tuplep);


           if (strncmp(ERL_ATOM_PTR(msg_type), "call", 4) == 0) {
              /* call expects a msg back */
              /* always at least one argument so we get that out first*/
              arg1p = erl_element(2, tuplep);              

              if (strncmp(ERL_ATOM_PTR(fnp), "init", 4) == 0) {
                 printf("init requested\n");
                 arg2p = erl_element(3, tuplep);
                 resp = gpio_init(arg1p, arg2p);
              } else if (strncmp(ERL_ATOM_PTR(fnp), "write", 5) == 0) {
                    arg2p = erl_element(3, tuplep);
                    /* @todo implement the real impl here */
                    resp = gpio_write(arg1p, arg2p);
              } else if (strncmp(ERL_ATOM_PTR(fnp), "read", 4) == 0) {
                 /* @todo implement the real impl here */
                 resp = erl_format("ok");
              } else if (strncmp(ERL_ATOM_PTR(fnp), "set_int", 7) == 0) {
                 arg2p = erl_element(3, tuplep);
                 /* @todo implement the real impl here */
                 resp = erl_format("ok");
              } else if (strncmp(ERL_ATOM_PTR(fnp), "release", 7) == 0) {
                 resp = gpio_release(arg1p);
              }

              printf("going to send resp: %s\n", ERL_ATOM_PTR(resp));
              erl_send(fd, fromp, erl_format("{~w,~w}", ref, resp));
           } else if (strncmp(ERL_ATOM_PTR(msg_type), "cast", 4) == 0) {
              /*  cast does not expect a msg back */
           }
           
	erl_free_term(emsg.from); erl_free_term(emsg.msg);
	erl_free_term(fromp); erl_free_term(ref);
        erl_free_term(tuplep);
	erl_free_term(fnp); erl_free_term(arg1p);
	erl_free_term(arg2p); erl_free_term(resp);
      }
    }
  }
}

ETERM*
gpio_init(ETERM* pin_t, ETERM* direction_t) {
   int pin, mode;
   char dirname[80], pinStr[2];
   FILE *export, *direction;

   /* convert erlang terms to usable values */
   pin = ERL_INT_VALUE(pin_t);
   if (strncmp(ERL_ATOM_PTR(direction_t), "input", 5) == 0) {
      mode = 0;
   } else if (strncmp(ERL_ATOM_PTR(direction_t), "output", 5) == 0) {
      mode = 1;
   } else {
      return erl_format("{error, wrong_pin_mode}");
   }

   printf("pin=%d, mode=%d\n", pin, mode);
   
   /* Export the pin for use. */

   export = fopen("/sys/class/gpio/export", "w");
   if (export == NULL) {
      return erl_format("{error, unable_to_export_pin}");
   }

         
   sprintf(pinStr, "%d", pin);
   fwrite(pinStr, sizeof (char), strlen(pinStr), export);
   
   fclose(export);     
   printf("exported pin\n");
   
   /* Open the direction file and write direction */
   sprintf(dirname, "/sys/class/gpio/gpio%d/direction", pin);
   direction = fopen(dirname, "w");
   printf("direction for pin %d opened\n", pin);
   
   if (direction != NULL) {
      if ( mode == 0) {
         fwrite("in", sizeof (char), 3, direction);
      } else if ( mode == 1) {
         fwrite("out", sizeof (char), 4, direction);
      }
   } else {
      return erl_format("{error, unable_to_set_pin_direction}");
   }

   fclose(direction);

   printf("wrote mode %d to pin %d\n", mode, pin);
  return erl_format("ok");         
   
}


ETERM*
gpio_release (ETERM* pin_t)
{
   unsigned int pin;
   FILE *file;
   char pinStr[3];

   /* convert Erlang args to usable values */
   pin = ERL_INT_VALUE(pin_t);

   file = fopen ("/sys/class/gpio/unexport", "w");
   if (file == NULL)
   {
      /* debug ("[%s] Can't open file (unexport)\n", __func__); */
      return erl_format("{error, cannot_open_file_for_unexport}");
   }

   sprintf (pinStr, "%d", pin);
   fwrite (pinStr, sizeof (char), strlen (pinStr), file);
   
   fclose (file);
  
   return erl_format("ok");
}

ETERM*
gpio_write(ETERM* pin_t, ETERM* value_t) {
   unsigned int pin;
   unsigned int val;
   FILE *file;
   /* int file; */
   char filename[35];
   char str_val[2];

   
   pin = ERL_INT_VALUE(pin_t);
   val = ERL_INT_VALUE(value_t);

   sprintf (filename, "/sys/class/gpio/gpio%d/value", pin);

   file = fopen (filename, "w");

   if ( file == NULL ) {
      return erl_format("{error, unable_to_open_value_file}");
   }

   if ((val!=0)&&(val!=1))
   {
      fprintf(stderr, "ERROR: Invalid value!\nValue must be 0 or 1\n");
      return erl_format("{error, wrong_value_for_gpio_pin}");
   }

   snprintf (str_val, (2*sizeof(char)), "%d", val);

   if ( fwrite (str_val, sizeof(char), 2, file) != 2 ){
         return erl_format("{error, cannot_write_to_gpio_pin}");
      }
  
   fclose (file);

   /* if ( (file = gpio_valfd(pin)) == -1)  */
   /*    return erl_format("{error, unable_to_open_value_file}"); */
   
   /* if ( val == 0) { */
   /*    if ( write(file, "0", (sizeof(char)*(1+1))) == -1 ) { */
   /*       return erl_format("{error, cannot_write_to_gpio_pin}"); */
   /*    } */
   /* } else if ( val == 1 ) { */
   /*    if ( write(file, "1", (sizeof(char)*(1+1))) == -1) { */
   /*          return erl_format("{error, cannot_write_to_gpio_pin}"); */
   /*    } */
   /* } else {    */
   /*    return erl_format("{error, wrong_value_for_gpio_pin}"); */
   /* } */

   /* close(file); */
   
   return erl_format("ok");
}   
   


static int
gpio_valfd (int pin)
{
  int file;
  char filename[35];

  sprintf (filename, "/sys/class/gpio/gpio%d/value", pin);
  file = open (filename, O_RDWR | O_NONBLOCK);
  if (file < 0)
    {
      /* debug ("[%s] Can't open file (value): %s\n", __func__, filename); */
      return -1;
    }
  else
    {
      return file;
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
