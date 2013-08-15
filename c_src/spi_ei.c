/**
* @file   spi_ei.c
* @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
* @brief  spi erlang interface
**/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "erl_interface.h"
#include "ei.h"

typedef unsigned char byte;

// data buffer
uint8_t *txbuf;

//convert an erlang tuple in an array
void tuple_to_array(ETERM *tuple) {

  int tplsize = ERL_TUPLE_SIZE(tuple);

  txbuf=(uint8_t *) malloc (tplsize * sizeof (uint8_t));
  int i=0;
  for (i=0; i<tplsize; i++) {
    txbuf[i]=(uint8_t) ERL_INT_VALUE(erl_element(i+1, tuple));
  }
}

int main() {

  ETERM *tuplep, *intp;
  ETERM *fnp, *argp;
  int res;
  byte buf[100];
  long allocated, freed;

  int fd;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    // calls the spi_init_name function and returns the fd or -1
    if (strcmp(ERL_ATOM_PTR(fnp), "spi_init", 8) == 0) {
      res = spi_init_name(erl_iolist_to_string(erl_element(2, tuplep)));
      //      res = spi_init(ERL_INT_VALUE(erl_element(2, tuplep)));
      fd = res;
      if (res < 0) {
	intp = erl_mk_int(-1);
      }
      else {
	intp = erl_mk_int(res);
      }
    }
    // calls the spi_config function and return ok if success or -1 if fails
    else if (strcmp(ERL_ATOM_PTR(fnp), "spi_config", 10) == 0) {
      res = spi_config( fd, ERL_INT_VALUE(erl_element(2, tuplep)), ERL_INT_VALUE(erl_element(3, tuplep)), ERL_INT_VALUE(erl_element(4,tuplep)),
			ERL_INT_VALUE(erl_element(5, tuplep)) );
      intp = erl_mk_int(res);
    }
    // calls the spi_tranfer function and return an erlang tuple with data or -1 if fails
    else if (strncmp(ERL_ATOM_PTR(fnp), "spi_transfer", 12) == 0) {
      tuple_to_array(erl_element(2, tuplep));
      int size = ERL_INT_VALUE(erl_element(3, tuplep));
      uint8_t rxbuf[size];

      res = spi_transfer(fd, txbuf, rxbuf, size);

      if (res < 0) {
	intp=erl_mk_int(-1);
      }
      // convert data array in an erlang tuple
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
    erl_free_term(argp);
    erl_free_term(intp);
  }
}
