/**
* @file   pwm_nif.c
* @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
* @brief  Pulse width modulation NIF implementation
* @description
*
* @section LICENSE
* Copyright (C) 2013 Erlang Solutions Ltd.
**/


#include "erl_nif.h"
#include "stdio.h"

/*! \addtogroup PWM
*  @brief PWM library functions
*  @{
*/

/**
* @brief Initialises the PWM peripheral
*
* @return 1 if success, -1 if fails
*
* @todo Replace it with pwm_init_name(char *devname)
*/
extern int pwm_init();

/**
* @brief Sets PWM value
*
* @param value    PWM value
*
* @return 1 if success, -1 if fails
*
* @todo Replace it with pwm_value(int fd, unsigned int value)
*/
extern int pwm_value(unsigned int value);

/**
* @brief Releases the PWM device
*
* @return none
*
* @todo Replace it with pwm_release(int fd)
*/
extern int pwm_release();


/**
* @brief Calls the driver that initialize the PWM peripheral.
* @return Returns ok if success or the tuple {error, pwm_initialization_error} if fails.
*/
static ERL_NIF_TERM init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if ( pwm_init() < 0 )
    return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_atom(env, "pwm_initialization_error"));
  else
    return enif_make_atom(env, "ok");
}

/**
* @brief Calls the driver that sets the PWM value.
* @return Returns ok if success or a badarg exeption if fails.
*/
static ERL_NIF_TERM value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int arg1;
  if (!enif_get_int(env, argv[0], &arg1)) {
    return enif_make_badarg(env);
  }
  
  pwm_value(arg1);
  return enif_make_atom(env, "ok");
}

/**
* @brief Calls the driver that release the PWM peripheral.
* @return Returns ok if success or a badarg exeption if fails.
*/
static ERL_NIF_TERM release_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  pwm_release();
  return enif_make_atom(env, "ok");
}

/** NIF array structure */
static ErlNifFunc nif_funcs[] = {
     {"pwm_init", 0, init_nif},
     {"pwm_value", 1, value_nif},
     {"pwm_release", 0, release_nif}
};

ERL_NIF_INIT(pwm, nif_funcs, NULL, NULL, NULL, NULL)
