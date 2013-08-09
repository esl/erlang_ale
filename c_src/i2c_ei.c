/* i2c_ei.c */
#include <stdio.h>
#include <stdlib.h>
#include "erl_interface.h"
#include "ei.h"

typedef unsigned char byte;


unsigned char *data;

void tuple_to_array(ETERM *tuple) {

  int tplsize = ERL_TUPLE_SIZE(tuple);

  data=(char *) malloc (tplsize * sizeof (char));
  int i=0;
  for (i=0; i<tplsize; i++) {
    data[i]=(char) ERL_INT_VALUE(erl_element(i+1, tuple));
  }
}

int main() {

  ETERM *tuplep, *intp;
  ETERM *fnp, *argp;
  int res;
  byte buf[100];
  long allocated, freed;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    

    if (strncmp(ERL_ATOM_PTR(fnp), "i2c_init", 8) == 0) {
      res = i2c_init();
      if (res < 0) {
	intp = erl_mk_int(-1);
      }
      else {
	intp = erl_mk_int(res);
      }
    }
    else if (strcmp(ERL_ATOM_PTR(fnp), "i2c_write", 9) == 0) {
      tuple_to_array(erl_element(4, tuplep));
      res = i2c_write(ERL_INT_VALUE(erl_element(2,tuplep)), ERL_INT_VALUE(erl_element(3, tuplep)), data, ERL_INT_VALUE(erl_element(5, tuplep)));
      intp = erl_mk_int(res);
    }
    else if (strncmp(ERL_ATOM_PTR(fnp), "i2c_read", 8) == 0) {
      int size = ERL_INT_VALUE(erl_element(4, tuplep));
      data=(char *) malloc (size * sizeof (char));

      res = i2c_read(ERL_INT_VALUE(erl_element(2, tuplep)), ERL_INT_VALUE(erl_element(3, tuplep)), data, ERL_INT_VALUE(erl_element(4, tuplep)));

      if (res < 0) {
	intp=erl_mk_int(-1);
      }
      else {       
	ETERM *etermarray[size];
	int i=0;
	for (i=0; i<size; i++) {
	  etermarray[i]=erl_mk_int(data[i]);
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
