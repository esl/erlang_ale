#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <inttypes.h>
#include <fcntl.h>
#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

#define DEBUG 1

#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...) ;
#endif

static pthread_t isr_handler_thread;
static int isr_handler_flag;
static int fd_erlang_node;


typedef struct {
   ETERM* pinp;
   void (*isr) (ETERM* pinp, ETERM* pidp, ETERM* modep);
   ETERM* pidp;
   ETERM* modep;
} isr_t;

/* this node will be known as c1@<hostname> */



/* the erlang node will send messages using this format:
 * {call, Pid, Ref, Msg}
 * (others might be added later)
 * call has to send a response back to the Pid and it tags the
 * response with the unique reference Ref sent to it as part of the
 * call message.
 */

/* gpio functions */
ETERM* gpio_init(ETERM*, ETERM*);
ETERM* gpio_release(ETERM*);
ETERM* gpio_write(ETERM*, ETERM*);
ETERM* gpio_read(ETERM*);
ETERM*
gpio_set_int (ETERM* pinp, ETERM* pidp, 
              void (*isr) (ETERM*, ETERM*, ETERM*), ETERM* modep);


void
handle_gpio_interrupt (ETERM* pinp, ETERM* pidp, ETERM* modep);
static int gpio_edge (int, char* );

/* helper functions */

static int gpio_valfd (int);
void get_hostname(char*);
void strstrip(char * );

int
main(int argc, char **argv) {
   /* int fd;                                  /\* fd to Erlang node *\/ */

   int loop = 1;                            /* Loop flag */
   int got;                                 /* Result of receive */
   unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
   ErlMessage emsg;                         /* Incoming message */

   ETERM *msg_type, *fromp, *refp, *tuplep, *fnp, *arg1p, *arg2p, *resp;

   char hostname[255];
   char erlang_nodename[255] = "e1@";
  
   erl_init(NULL, 0);

   if (erl_connect_init(1, "secretcookie", 0) == -1)
      erl_err_quit("erl_connect_init");

   /* now we figure out where to connect to */
   get_hostname(hostname);
   strcat(erlang_nodename, hostname);
   strstrip(erlang_nodename);
  
   if ((fd_erlang_node = erl_connect(erlang_nodename)) < 0)
      erl_err_quit("erl_connect");
   fprintf(stderr, "Connected to %s\n\r",erlang_nodename);

   while (loop) {

      got = erl_receive_msg(fd_erlang_node, buf, BUFSIZE, &emsg);
      if (got == ERL_TICK) {
         /* ignore */
      } else if (got == ERL_ERROR) {
         loop = 0;
      } else {
        
         if (emsg.type == ERL_REG_SEND) {
            msg_type = erl_element(1, emsg.msg);
            fromp = erl_element(2, emsg.msg);
            refp = erl_element(3, emsg.msg);
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
                  resp = gpio_read(arg1p);
               } else if (strncmp(ERL_ATOM_PTR(fnp), "set_int", 7) == 0) {
                  arg2p = erl_element(3, tuplep);
                  /* @todo implement the real impl here */
                  resp = gpio_set_int(arg1p, fromp, 
                                      handle_gpio_interrupt, arg2p);
               } else if (strncmp(ERL_ATOM_PTR(fnp), "release", 7) == 0) {
                  resp = gpio_release(arg1p);
               }

               printf("going to send resp: %s\n", ERL_ATOM_PTR(resp));
               erl_send(fd_erlang_node, fromp, erl_format("{~w,~w}", refp, resp));
            } else if (strncmp(ERL_ATOM_PTR(msg_type), "cast", 4) == 0) {
               /*  cast does not expect a msg back */
            }
           
            erl_free_term(emsg.from); erl_free_term(emsg.msg);
            erl_free_term(fromp); erl_free_term(refp);
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

ETERM*
gpio_read(ETERM* pin_t) {

   unsigned int pin;
   unsigned int val;
   FILE *file;
   /* int file; */
   char filename[35];
   char str_val[2];

   pin = ERL_INT_VALUE(pin_t);

   sprintf (filename, "/sys/class/gpio/gpio%d/value", pin);

   file = fopen (filename, "r");

   if ( file == NULL ) {
      return erl_format("{error, unable_to_open_value_file}");
   }

   if ( fread(str_val, sizeof(char), 1, file) != 1 ){
      return erl_format("{error, unable_to_read_value_file}");
   }

   val = atoi(str_val);

   fclose(file);
   
   return erl_format("~i",val);
}



static int
gpio_edge (int pin, char *edge) {
   FILE *file;
   char filename[35];

   sprintf (filename, "/sys/class/gpio/gpio%d/edge", pin);
   file = fopen (filename, "w");
   if (file == NULL)
   {
      debug ("[%s] Can't open file (edge): %s\n", __func__, filename); 
      return -1;
   }
   fwrite (edge, sizeof (char), strlen (edge) + 1, file);
              
   fclose (file);
              
   return 1;
}


void
handle_gpio_interrupt (ETERM* pinp, ETERM* pidp, ETERM* modep) {
   debug("inside handle_gpio_interrupt\n\r");
   debug("pin: %d, pid_number: %d, mode: %s\n\r",
         ERL_INT_VALUE(pinp), ERL_PID_NUMBER(pidp), ERL_ATOM_PTR(modep));
   
   ETERM* resp = erl_format("{gpio_interrupt, ~w, ~w}", pinp, modep);

   erl_send(fd_erlang_node, pidp, resp);
}

/* taken from https://github.com/omerk/pihwm/blob/master/lib/pi_gpio.c */              
static void *
isr_handler (void *isr) {
   struct pollfd fdset[2];
   int nfds = 2, gpio_fd, rc;
   char *buf[64];

   isr_t i = *(isr_t *) isr;

   if (isr_handler_flag)
   {
      printf ("isr_handler running\n");

      /* Get /value fd
         TODO: Add check here */
      gpio_fd = gpio_valfd (ERL_INT_VALUE(i.pinp));

      if ( gpio_fd == -1) {
         fprintf(stderr, "Unable to open gpio fd\n\r");
         return NULL;
      }

      while (1)
      {
         memset ((void *) fdset, 0, sizeof (fdset));

         fdset[0].fd = STDIN_FILENO;
         fdset[0].events = POLLIN;

         fdset[1].fd = gpio_fd;
         fdset[1].events = POLLPRI;

         rc = poll (fdset, nfds, 1000);	/* Timeout in ms */

         if (rc < 0)
         {
            debug ("\npoll() failed!\n"); 
            return (void *) -1;
         }

         if (rc == 0)
         {
            /* debug ("poll() timeout.\n"); */
            if (isr_handler_flag == 0)
            {
               debug ("exiting isr_handler (timeout)"); 
               pthread_exit (NULL);
            }
         }

         if (fdset[1].revents & POLLPRI)
         {
/* We have an interrupt! */
            if (-1 == read (fdset[1].fd, buf, 64))
            {
               debug ("read failed for interrupt"); 
               return (void *) -1;
            }

            (*i.isr) (i.pinp, i.pidp, i.modep);	/* Call the ISR */
         }

         if (fdset[0].revents & POLLIN)
         {
            if (-1 == read (fdset[0].fd, buf, 1))
            {
               debug ("read failed for stdin read"); 
               return (void *) -1;
            }

            printf ("\npoll() stdin read 0x%2.2X\n", (unsigned int) buf[0]);
         }

         fflush (stdout);
      }
   }
   else
   {
      debug ("exiting isr_handler (flag)"); 
      pthread_exit (NULL);
   }

}


ETERM*
gpio_set_int (ETERM* pinp, ETERM* pidp,
              void (*isr) (ETERM*, ETERM*, ETERM*), ETERM* modep)
{
  /* Details of the ISR */
  isr_t *i = (isr_t *) malloc (sizeof (isr_t));
  i->pinp = erl_format("~w",pinp);
  i->isr = isr;
  i->pidp = erl_format("~w", pidp);
  i->modep = erl_format("~w", modep);
  char mode[10];
  unsigned int pin;
  
  sprintf(mode, "%s", ERL_ATOM_PTR(modep));
  pin = ERL_INT_VALUE(pinp);
  
  /* Set up interrupt */
  if ( gpio_edge (pin, mode) == -1) {
     return erl_format("{error, unable_to_set_gpio_edge}");
  }

  /* Set isr_handler flag and create thread
TODO: check for errors using retval */
  isr_handler_flag = 1;
  pthread_create (&isr_handler_thread, NULL, isr_handler, (void *) i);
  pthread_tryjoin_np (isr_handler_thread, NULL);
  fprintf(stderr, "pthtread created\n");
  
  return erl_format("ok");
}


int
gpio_clear_int (unsigned int pin)
{
  /* this will terminate isr_handler thread */
  isr_handler_flag = 0;

  /* TODO: Reset "edge", release pin? */
  return 0;	/* Is this a correct return result. */
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

void
strstrip( char *s )
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
