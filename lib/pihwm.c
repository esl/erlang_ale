#include <stdio.h>
#include <stdlib.h>
#include <linux/types.h>
#include "pihwm.h"


/*! \addtogroup General
*  @brief General library functions
*  @{
*/

/**
* @mainpage Documentation
*
* <h2>Introduction</h2>
* foo
* 
* <h2>Usage</h2>
* bar
*/

/**
* @brief Return board information (Model, PCB revision and Memory)
*
* @return board information
*/
board_t
board_info ()
{
	FILE *info;
	char rev_hex[5];
	unsigned int rev_int = 0;

	board_t board;
	board.model = -1;
	board.rev = -1;
	board.mem = -1;

	char *cmd = "cat /proc/cpuinfo | grep 'Revision' | awk '{print $3}'";

	if( !(info = popen(cmd, "r")) )
	{
		return board;
	}

	fgets(rev_hex, 5, info);
	sscanf(rev_hex, "%x", &rev_int);

	// Data from: http://raspberryalphaomega.org.uk/?p=428 
	switch(rev_int){
		case 2:
		case 3:
			board.model = MODEL_B;
			board.rev = REV_1;
			board.mem = MEM_256;
			break;

		case 4:
		case 5:
		case 6:
			board.model = MODEL_B;
			board.rev = REV_2;
			board.mem = MEM_256;
		break;

		case 7:
		case 8:
		case 9:
			board.model = MODEL_A;
			board.rev = REV_2;
			board.mem = MEM_256;
		break;

		case 13:
		case 14:
		case 15:
			board.model = MODEL_B;
			board.rev = REV_2;
			board.mem = MEM_512;
		break;

	default:
		// Default values (-1) already set.
		break;
	}

	return board;

}

/**
* @brief Return board model
*
* @return MODEL_A or MODEL_B
*/
int
board_model ()
{
	board_t b = board_info();
	return b.model;
}

/**
* @brief Return board revision
*
* @return REV_1 or REV_2
*/
int
board_rev ()
{
	board_t b = board_info();
	return b.rev;
}

/**
* @brief Return the amount of system memory
*
* @return MEM_256 or MEM_512
*/
int
board_mem ()
{
	board_t b = board_info();
	return b.mem;
}

/**
* @brief Check if the kernel module specified is loaded.
*
* @param 	name of the kernel module
* @return 	1 for success, -1 for failure
*/
int
check_kernel_module (char* modulename)
{
	FILE *lsmod;
	char cmd[100];
	char modcount[2];
	unsigned int modcount_int = 0;

	sprintf(cmd, "lsmod | grep %s | wc -l", modulename); 

	if( !(lsmod = popen(cmd, "r")) )
	{
		return -1;
	}

	fgets(modcount, 2, lsmod);
	modcount_int = atoi(modcount);

	if ( modcount_int > 0 )
	{
		return 1;
	}
	else
	{
		return -1;
	}

}

/*! @} */
