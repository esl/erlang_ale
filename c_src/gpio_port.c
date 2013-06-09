#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include <erl_interface.h>
#include <ei.h>
#include <port_comms.h>
#include <pihwm.h>
#include <pi_gpio.h>

#define BUF_SIZE 1024

// to store file descriptors
//int gpio_fd[64] = { -1 };

static int my_pin;

// GPIO functions
int
port_gpio_init (ETERM *pin_t, ETERM *direction_t)
{
   int pin, dir;
   /* convert erlang terms to usable values */
   pin = ERL_INT_VALUE(pin_t);
   if (strncmp(ERL_ATOM_PTR(direction_t), "input", 5) == 0)
   {
      dir = 0;
   } else if (strncmp(ERL_ATOM_PTR(direction_t), "output", 6) == 0)
   {
      dir = 1;
   } else
   {
      return -1;
   }

   my_pin = pin;
      
   return gpio_init(pin, dir);
}

int
port_gpio_release (int pin)
{
	return gpio_release(pin);
}

int
port_gpio_write (int pin, int value)
{
	return gpio_write(pin, value);
}

int
port_gpio_read (int pin)
{
	return gpio_read(pin);
}

int
port_gpio_set_int (int pin, int condition)
{
	// TODO: Implement a pthread listener
	return -1;
}



/* MAIN */
int main() {
  unsigned char buf[BUF_SIZE];
  char command[MAXATOMLEN];
  int index, version, arity;


  ETERM *emsg, *msg_type, *fromp, *refp, *tuplep, *fnp, *arg1p, *arg2p, *resp;

  
  int arg1, arg2, res;
  ei_x_buff result;

  memset(buf, 0, BUF_SIZE);
  erl_init(NULL, 0);
  
/*  setlogmask( LOG_UPTO (LOG_NOTICE));
 */ 
  openlog("papged", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_LOCAL1);

  syslog(LOG_NOTICE, "openlog done");
  
  while ((res = read_cmd(buf)) > 0)
  {
     syslog(LOG_NOTICE, "read_cmd done");
     if ((emsg = erl_decode(buf)) != NULL)
     {
        syslog(LOG_NOTICE, "erl_decode done");
     }
     else
     {
        syslog(LOG_ERR, "erl_decode FAILED");
        break;
     }

     
     if ((msg_type = erl_element(1, emsg)) != NULL)
     {
        syslog(LOG_NOTICE, "erl_element succeeded!");
        syslog(LOG_NOTICE, "msg_type: %s", ERL_ATOM_PTR(msg_type));

        if (strncmp(ERL_ATOM_PTR(msg_type), "init", 4) == 0)
           {
              arg1p = erl_element(2, emsg);
              arg2p = erl_element(3, emsg);
              if (arg1p != NULL && arg2p != NULL)
              {
                 if (port_gpio_init(arg1p, arg2p))
                 {
                    resp = erl_format("ok");
                 }
                 else
                 {
                    resp = erl_format("{error, gpio_init_fail}");
                 }
              }
              else
              {
                 syslog(LOG_ERR,"init arguments incorrect");
              }
                 
           }
           else if (strncmp(ERL_ATOM_PTR(msg_type), "cast", 4) == 0)
           {
              if ((arg1p = erl_element(2, emsg))!= NULL)
              {
                 if (strncmp(ERL_ATOM_PTR(arg1p), "release", 7) == 0)
                 {
                    if (port_gpio_release(my_pin))
                    {
                       resp= erl_format("ok");
                    }
                    else
                    {
                       resp = erl_format("{error, gpio_release_fail}");
                    }
                 }
              }
              else
              {
              }
           }
           
     }
     else
     {
        syslog(LOG_NOTICE, "erl_element FAILED!!!!");
        break;
     }
     

     syslog(LOG_NOTICE, "formatted resp");
     
     if (write_cmd_eterm(resp))
        syslog(LOG_NOTICE, "write_cmd_eterm done");
     else
        syslog(LOG_NOTICE, "write_cmd_eterm FAILED");
  }

  syslog(LOG_NOTICE, "leaving gpio_port %d", res);
  
  closelog();

  erl_free_term(emsg); erl_free_term(msg_type);
  erl_free_term(fromp); erl_free_term(refp);
  erl_free_term(tuplep); erl_free_term(fnp);
  erl_free_term(arg1p); erl_free_term(arg2p);
  erl_free_term(resp);
  
  return 0;
  
     
/* /\* Reset the index, so that ei functions can decode terms from the  */
/*      * beginning of the buffer *\/ */
/*     index = 0; */

/*     /\* Ensure that we are receiving the binary term by reading and  */
/*      * stripping the version byte *\/ */
/*     if (ei_decode_version(buf, &index, &version)) return 1; */
    
/*     /\* Our marshalling spec is that we are expecting a tuple {Command, Arg1, Arg2, ...} *\/ */
/*     if (ei_decode_tuple_header(buf, &index, &arity)) return 2; */
   
/*     //if (arity != 3) return 3; */
    
/*     if (ei_decode_atom(buf, &index, command)) return 4; */
    
/*     /\* Prepare the output buffer that will hold {ok, Result} or {error, Reason} *\/ */
/*     if (ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return 5; */
  
/* 	/\* This is where we handle incoming commands *\/ */
/* 	if ( !strcmp("gpio_init", command) ) */
/* 	{ */
/* 		if (ei_decode_int(buf, &index, &arg1)) return 6; */
/* 		if (ei_decode_int(buf, &index, &arg2)) return 7; */

/* 		res = port_gpio_init(arg1, arg2);  */

/* 		if ( res ) */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8; */
/* 		} */
/* 		else */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99; */
/* 		} */

/* 	} */
/* 	else if( !strcmp("gpio_release", command) ) */
/* 	{ */

/* 		if (ei_decode_int(buf, &index, &arg1)) return 6; */

/* 		res = port_gpio_release(arg1);  */

/* 		if ( res ) */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8; */
/* 		} */
/* 		else */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99; */
/* 		} */
/* 	} */
/* 	else if( !strcmp("gpio_write", command) ) */
/* 	{ */

/* 		if (ei_decode_int(buf, &index, &arg1)) return 6; */
/* 		if (ei_decode_int(buf, &index, &arg2)) return 7; */

/* 		res = port_gpio_write(arg1, arg2);  */

/* 		if ( res ) */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8; */
/* 		} */
/* 		else */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99; */
/* 		} */
/* 	} */
/* 	else if( !strcmp("gpio_read", command) ) */
/* 	{ */

/* 		if (ei_decode_int(buf, &index, &arg1)) return 6; */

/* 		res = port_gpio_read(arg1);  */

/* 		if ( res ) */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8; */
/* 		} */
/* 		else */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99; */
/* 		} */
/* 	} */
/* 	else if( !strcmp("gpio_set_int", command) ) */
/* 	{ */

/* 		if (ei_decode_int(buf, &index, &arg1)) return 6; */
/* 		if (ei_decode_int(buf, &index, &arg2)) return 7; */

/* 		res = port_gpio_set_int(arg1, arg2);  */

/* 		if ( res ) */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8; */
/* 		} */
/* 		else */
/* 		{ */
/* 			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99; */
/* 		} */
/* 	} */
/* 	else { */
/* 		if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unsupported_command"))  */
/* 		return 99; */
/* 	} */

/*     write_cmd(&result); */

/*     ei_x_free(&result); */
/*   } */

}


