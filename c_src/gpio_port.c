#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ei.h>

#include "lib/port_comms.h"
#include "lib/pihwm.h"
#include "lib/pi_gpio.h"

#define BUF_SIZE 128

// to store file descriptors
//int gpio_fd[64] = { -1 };

// GPIO functions
int
port_gpio_init (int pin, int direction)
{
	return gpio_init(pin, direction);

	/*
	int fd = gpio_init(pin, direction);

	if ( fd > 0 )
	{
		gpio_fd[pin] = fd;
		return 1;
	}
	else
	{
		return -1;
	}
	*/
}

int
port_gpio_release (int pin)
{
	return gpio_release(pin);

	/*
	if ( gpio_release(pin) )
	{
		gpio_fd[pin] = -1;
		return 1;
	}
	else
	{
		return -1;
	}
	*/
}

int
port_gpio_write (int pin, int value)
{
	return gpio_write(pin, value);

	//return gpio_write(gpio_fd[pin], value);
}

int
port_gpio_read (int pin)
{
	return gpio_read(pin);

	// return gpio_read(gpio_fds[pin]);
}

int
port_gpio_set_int (int pin, int condition)
{
	// TODO: Implement a pthread listener
	return -1;
}



/* MAIN */
int main() {
  char buf[BUF_SIZE];
  char command[MAXATOMLEN];
  int index, version, arity;

  int arg1, arg2, res;
  ei_x_buff result;

  while (read_cmd(buf) > 0) {
    /* Reset the index, so that ei functions can decode terms from the 
     * beginning of the buffer */
    index = 0;

    /* Ensure that we are receiving the binary term by reading and 
     * stripping the version byte */
    if (ei_decode_version(buf, &index, &version)) return 1;
    
    /* Our marshalling spec is that we are expecting a tuple {Command, Arg1, Arg2, ...} */
    if (ei_decode_tuple_header(buf, &index, &arity)) return 2;
   
    //if (arity != 3) return 3;
    
    if (ei_decode_atom(buf, &index, command)) return 4;
    
    /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
    if (ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return 5;
  
	/* This is where we handle incoming commands */
	if ( !strcmp("gpio_init", command) )
	{
		if (ei_decode_int(buf, &index, &arg1)) return 6;
		if (ei_decode_int(buf, &index, &arg2)) return 7;

		res = port_gpio_init(arg1, arg2); 

		if ( res )
		{
			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8;
		}
		else
		{
			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99;
		}

	}
	else if( !strcmp("gpio_release", command) )
	{

		if (ei_decode_int(buf, &index, &arg1)) return 6;

		res = port_gpio_release(arg1); 

		if ( res )
		{
			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8;
		}
		else
		{
			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99;
		}
	}
	else if( !strcmp("gpio_write", command) )
	{

		if (ei_decode_int(buf, &index, &arg1)) return 6;
		if (ei_decode_int(buf, &index, &arg2)) return 7;

		res = port_gpio_write(arg1, arg2); 

		if ( res )
		{
			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8;
		}
		else
		{
			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99;
		}
	}
	else if( !strcmp("gpio_read", command) )
	{

		if (ei_decode_int(buf, &index, &arg1)) return 6;

		res = port_gpio_read(arg1); 

		if ( res )
		{
			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8;
		}
		else
		{
			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99;
		}
	}
	else if( !strcmp("gpio_set_int", command) )
	{

		if (ei_decode_int(buf, &index, &arg1)) return 6;
		if (ei_decode_int(buf, &index, &arg2)) return 7;

		res = port_gpio_set_int(arg1, arg2); 

		if ( res )
		{
			if (ei_x_encode_atom(&result, "ok") || ei_x_encode_int(&result, res)) return 8;
		}
		else
		{
			if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "fail.")) return 99;
		}
	}
	else {
		if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unsupported_command")) 
		return 99;
	}

    write_cmd(&result);

    ei_x_free(&result);
  }

  return 0;
}


