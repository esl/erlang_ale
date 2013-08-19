/**
* @file   pwm_nif.c
* @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
* @brief  Pulse width modulation NIF implementation
**/


#include "erl_nif.h"
#include "stdio.h"

extern int pwm_init();
extern int pwm_value(unsigned int value);
extern int pwm_release();

// Initialise the PWM peripheral
static ERL_NIF_TERM init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if ( pwm_init() < 0 )
    return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_atom(env, "pwm_initialization_error"));
  else
    return enif_make_atom(env, "ok");
}

// Set PWM value
static ERL_NIF_TERM value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int arg1;
  if (!enif_get_int(env, argv[0], &arg1)) {
    return enif_make_badarg(env);
  }
  
  pwm_value(arg1);
  return enif_make_atom(env, "ok");
}

// Release the PWM peripheral and unmaps the memory
static ERL_NIF_TERM release_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  pwm_release();
  return enif_make_atom(env, "ok");
}


static ErlNifFunc nif_funcs[] = {
     {"pwm_init", 0, init_nif},
     {"pwm_value", 1, value_nif},
     {"pwm_release", 0, release_nif}
};

ERL_NIF_INIT(pwm, nif_funcs, NULL, NULL, NULL, NULL)
