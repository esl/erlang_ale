/**
* @file   gpio_port.c
* @author Erlang Solutions Ltd
* @brief  GPIO erlang interface
* @description
*
* @section LICENSE
* Copyright (C) 2013 Erlang Solutions Ltd.
**/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include <erl_interface.h>
#include <ei.h>
#include <portutil.h>
#include <pihwm.h>
#include <pi_gpio.h>

#define BUF_SIZE 1024

/*! \addtogroup GPIO
*  @brief GPIO library functions
*  @{
*/

// to store file descriptors
//int gpio_fd[64] = { -1 };

static int my_pin;
static ETERM *my_condition;

// GPIO functions

/**
* @brief	Initialises the devname GPIO device
*
* @param	pin_t            The GPIO pin
* @param        direction_t      Direction of pin (input or output)
*
* @return 	1 for success, -1 for failure
*/
extern int gpio_init(unsigned int pin, unsigned int dir);

/**
* @brief	Release a GPIO pin
*
* @param	pin            The GPIO pin
*
* @return 	1 for success, -1 for failure
*/
extern int gpio_release(unsigned int pin);

/**
* @brief	Set pin with the value "0" or "1"
*
* @param	pin            The GPIO pin
* @param        valuet         Value to set (0 or 1)
*
* @return 	1 for success, -1 for failure
*/
extern int gpio_write(unsigned int pin, unsigned int val);

/**
* @brief	Read the value of the pin
*
* @param	pin            The GPIO pin
*
* @return 	The pin value if success, -1 for failure
*/
extern int gpio_read(unsigned int pin);

/**
* @brief	Set isr as the interrupt service routine (ISR) for the pin. Mode should be one of the strings "rising", "falling" or "both" to indicate which edge(s) the ISR is to be triggered on. The function isr is called whenever the edge specified occurs, receiving as argument the number of the pin which triggered the interrupt. 
*
* @param	pin	Pin number to attach interrupt to
* @param        isr	Interrupt service routine to call
* @param        mode	Interrupt mode
*
* @return 	Returns 1 on success. Never fails 
*/
extern int gpio_set_int(unsigned int pin, void(*isr)(int), char *mode);


/**
* @brief Port interface for gpio_init
*/
int
port_gpio_init (ETERM *pin_t, ETERM *direction_t)
{
   int pin, dir;
   /* convert erlang terms to usable values */
   pin = ERL_INT_VALUE(pin_t);
   syslog(LOG_NOTICE, "init with direction %s", ERL_ATOM_PTR(direction_t));
   
   if (strncmp(ERL_ATOM_PTR(direction_t), "input", 5) == 0)
   {
      dir = 1;
   } else if (strncmp(ERL_ATOM_PTR(direction_t), "output", 6) == 0)
   {
      dir = 0;
   } else
   {
      return -1;
   }

   syslog(LOG_NOTICE, "init with dir %d", dir);
   my_pin = pin;
      
   return gpio_init(pin, dir);
}

/**
* @brief Port interface for gpio_release
*/
int
port_gpio_release (int pin)
{
   return gpio_release(pin);
}

/**
* @brief Port interface for gpio_write
*/
int
port_gpio_write (int pin, ETERM *valuet)
{
   int value;
   value = ERL_INT_VALUE(valuet);
   return gpio_write(pin, value);
}

/**
* @brief Port interface for gpio_read
*/
int
port_gpio_read (int pin)
{
   return gpio_read(pin);
}

void
gpio_isr(int pin)
{
   ETERM *resp = erl_format("{gpio_interrupt, ~w}", my_condition);

   if (write_cmd_eterm(resp))
   {
      syslog(LOG_NOTICE, "gpio_interrupt for pin %d", pin);
   }
   else
   {
      syslog(LOG_ERR, "gpio_interrupt failed for pin %d", pin);
   }
      
}

/**
* @brief Port interface for gpio_set_int
*/
int
port_gpio_set_int (int pin, ETERM* condition_t)
{
   char mode[10];

   my_condition = erl_format("~w", condition_t);
   sprintf(mode, "%s", ERL_ATOM_PTR(condition_t));
  
   return gpio_set_int(pin, gpio_isr, mode);
}



/**
* @brief The main function.
* It waits for data in the buffer and calls the driver.
*/
int main() {
  unsigned char buf[BUF_SIZE];
  /* char command[MAXATOMLEN]; */
  /* int index, version, arity; */


  ETERM *emsg, *msg_type, *refp, *tuplep, *fnp, *arg1p, *arg2p, *resp;

  
  int res;
  /* ei_x_buff result; */

  memset(buf, 0, BUF_SIZE);
  erl_init(NULL, 0);
  
/*  setlogmask( LOG_UPTO (LOG_NOTICE));
 */ 
  openlog("gpio_port", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_LOCAL1);

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
                 resp = erl_format("{error, gpio_init_wrong_arguments}");
              }
              if (write_cmd_eterm(resp))
                 syslog(LOG_NOTICE, "write_cmd_eterm done for init ");
              else
                 syslog(LOG_NOTICE, "write_cmd_eterm FAILED for init");
           }
           else if (strncmp(ERL_ATOM_PTR(msg_type), "cast", 4) == 0)
           {
              if ((arg1p = erl_element(2, emsg))!= NULL)
              {
                 if (strncmp(ERL_ATOM_PTR(arg1p), "release", 7) == 0)
                 {
                    if (port_gpio_release(my_pin))
                    {
                       syslog(LOG_NOTICE, "gpio_relase went well for pin %d",
                              my_pin);
                    }
                    else
                    {
                       syslog(LOG_ERR, "gpio_release failed for pin %d", my_pin);
                    }
                 }
              }
              else
              {
                 syslog(LOG_ERR, "release arguments incorrect");
              }
           }
           else if (strncmp(ERL_ATOM_PTR(msg_type), "call", 4) == 0)
           {
              refp = erl_element(2, emsg);
              tuplep = erl_element(3, emsg);
              if (refp != NULL && tuplep != NULL)
              {
                 if ((fnp = erl_element(1, tuplep)) != NULL)
                 {
                    if (strncmp(ERL_ATOM_PTR(fnp), "write", 5) == 0)
                    {
                       if((arg1p = erl_element(2, tuplep)) != NULL)
                       {
                          if(port_gpio_write(my_pin, arg1p))
                          {
                             resp = erl_format("ok");
                          }
                          else
                          {
                             syslog(LOG_ERR, "port write failed");
                             resp = erl_format("{error, gpio_write_failed}");
                          }
                       }
                       else
                       {
                          syslog(LOG_ERR, "call with wrong tuple");
                          resp = erl_format("{error, call_expected_tuple}");
                       }
                 
                    }
                    else if (strncmp(ERL_ATOM_PTR(fnp), "read", 4) == 0)
                    {
                       if((res =port_gpio_read(my_pin)) !=-1)
                       {
                          resp = erl_format("~i",res);
                       }
                       else
                       {
                          syslog(LOG_ERR, "port read failed");
                          resp = erl_format("{error, gpio_read_failed}");
                       }
                    }
                    else if (strncmp(ERL_ATOM_PTR(fnp), "set_int", 7) == 0)
                    {
                       if((arg1p = erl_element(2, tuplep)) != NULL)
                       {
                          if(port_gpio_set_int(my_pin, arg1p))
                          {
                             resp = erl_format("ok");
                          }
                          else
                          {
                             syslog(LOG_ERR, "port set_int failed");
                             resp = erl_format("{error, gpio_set_int_failed}");
                          }
                       } else
                       {
                          syslog(LOG_ERR, "call with wrong tuple");
                          resp = erl_format("{error, call_expected_tuple}");
                       }
                    }
                 }
                 else
                 {
                    syslog(LOG_ERR,"call arguments incorrect");
                    resp = erl_format("{error, gpio_cal_wrong_arguments}");
                 }
              }

              /* Now we can send the response to the caller */
              if (write_cmd_eterm(erl_format("{port_reply,~w,~w}", refp, resp)))
              {
                 syslog(LOG_NOTICE, "successful reply to call");
              }
              else
              {
                 syslog(LOG_ERR, "error write_cmd_eterm");
              }
           }
     }
     else
     {
        syslog(LOG_NOTICE, "erl_element FAILED!!!!");
        break;
     }
  }

  syslog(LOG_NOTICE, "leaving gpio_port %d", res);
  
  closelog();

  erl_free_term(emsg); erl_free_term(msg_type);
  /* erl_free_term(fromp); erl_free_term(refp); */
  /* erl_free_term(tuplep); erl_free_term(fnp); */
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


