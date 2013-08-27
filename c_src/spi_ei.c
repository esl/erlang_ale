/**
* @file   spi_ei.c
* @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
* @brief  SPI erlang interface
* @description
*
* @section LICENSE
* Copyright (C) 2013 Erlang Solutions Ltd.
**/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "erl_interface.h"
#include "ei.h"

/*! \addtogroup SPI
*  @brief SPI library functions
*  @{
*/

typedef unsigned char byte;

/**
* @brief	Initialises the devname SPI device
*
* @param	devname    The sysfs entry
*
* @return 	file descriptor if success, -1 if fails
*/
extern int spi_init_name (char *devname);

/**
* @brief	Configures the SPI device
*
* @param	fd	File descriptor of the SPI device
* @param	mode	SPI mode
* @param	bits	Number of bits
* @param	speed	Bus speed
* @param	delay	Amount of delay
*
* @return	1 if success, -1 if fails
*/
extern int spi_config(int fd, uint8_t mode, uint8_t bits, uint32_t speed, uint16_t delay);

/**
* @brief	SPI transfer operation
*
* @param	fd	File descriptor of the SPI device
* @param	txbuf	Transmit buffer array
* @param	rxbuf	Receive buffer array
* @param	len	Length of transfer
*
* @return	1 if success, -1 if fails
*/
extern int spi_transfer (int fd, uint8_t txbuf[], uint8_t rxbuf[], uint8_t len);

extern int read_cmd(byte *buf);
extern int write_cmd(byte *buf, int len);
extern int strncmp(const char *s1, const char *s2, size_t n);

/** TX data buffer */
uint8_t *txbuf;

/**
* @brief Converts an erlang tuple in an C array and puts the data into the *data variable
* @param tuple    The erlang tuple
*/
void tuple_to_array(ETERM *tuple) {

  int tplsize = ERL_TUPLE_SIZE(tuple);

  txbuf=(uint8_t *) malloc (tplsize * sizeof (uint8_t));
  int i=0;
  for (i=0; i<tplsize; i++) {
    txbuf[i]=(uint8_t) ERL_INT_VALUE(erl_element(i+1, tuple));
  }
}

/**
* @brief The main function.
* It waits for data in the buffer and calls the driver
*/

int main() {

  ETERM *tuplep, *intp;
  ETERM *fnp;
  int res;
  byte buf[100];

  int fd;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    
    // calls the spi_init_name function and returns the fd or -1
    if (strncmp(ERL_ATOM_PTR(fnp), "spi_init", 8) == 0) {
      res = spi_init_name(erl_iolist_to_string(erl_element(2, tuplep)));
      fd = res;
      if (res < 0) {
	intp = erl_mk_int(-1);
      }
      else {
	intp = erl_mk_int(res);
      }
    }
    // calls the spi_config function and returns ok if success or -1 if fails
    else if (strncmp(ERL_ATOM_PTR(fnp), "spi_config", 10) == 0) {
      res = spi_config( fd, ERL_INT_VALUE(erl_element(2, tuplep)), ERL_INT_VALUE(erl_element(3, tuplep)), ERL_INT_VALUE(erl_element(4,tuplep)),
			ERL_INT_VALUE(erl_element(5, tuplep)) );
      intp = erl_mk_int(res);
    }
    // calls the spi_tranfer function and returns an erlang tuple with data or -1 if fails
    else if (strncmp(ERL_ATOM_PTR(fnp), "spi_transfer", 12) == 0) {
      tuple_to_array(erl_element(2, tuplep));
      int size = ERL_INT_VALUE(erl_element(3, tuplep));
      uint8_t rxbuf[size];

      res = spi_transfer(fd, txbuf, rxbuf, size);

      if (res < 0) {
	intp=erl_mk_int(-1);
      }
      // converts data array in an erlang tuple
      else {       
	ETERM *etermarray[size];
	int i=0;
	for (i=0; i<size; i++) {
	  etermarray[i]=erl_mk_int(rxbuf[i]);
	}
	intp=erl_mk_tuple(etermarray, size);
      }
    }
    
    // converts the result in the ETERM format
    //  intp = erl_mk_int(res);

    // converts the result in the Erlang external term format
    erl_encode(intp, buf);

    // sends the result to erlang 
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(intp);
  }
  return 0;
}
