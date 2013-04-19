#ifndef PIHWM_H
#define PIHWM_H

#include <stdio.h>
#include <unistd.h>

// Board information
#define MODEL_A	100
#define MODEL_B	200
#define REV_1	10
#define REV_2	20
#define MEM_256	1
#define MEM_512	2

// Useful macros
#ifdef DEBUG
	#define debug(...) printf(__VA_ARGS__)
#else
	#define debug(...) ;
#endif

#define delay(d)				usleep(d*1000); //millisec
#define delayMicroseconds(d)	usleep(d);
#define delaySeconds(d)			sleep(d);

/* http://gcc.gnu.org/onlinedocs/gcc/Compound-Literals.html */
#define a(...)		(unsigned char[])__VA_ARGS__

#define size(a)		(sizeof(a) / sizeof((a)[0]))

typedef struct
{
	int model;
	int rev;
	int mem;
} board_t;

// Function prototypes for pihwm.c
board_t board_info ();
int board_model ();
int board_rev ();
int board_mem ();
int check_kernel_module (char* modulename);

#endif
