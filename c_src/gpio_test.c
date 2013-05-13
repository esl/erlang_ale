/* gpio_test.c -- Test of GPIO output.

   Copyright (C) 2012 Omer Kilic
   Copyright (C) 2012 Embecosm Limited

   Contributor Omer Kilic <omerkilic@gmail.com>
   Contributor Jeremy Bennett <jeremy.bennett@embecosm.com>

   This file is part of pihwm.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <stdio.h>

#include <pihwm.h>
#include <pi_gpio.h>

#define	BTN0	17
#define	BTN1	22	
#define	LED0	23
#define	LED1	24

int
main (void)
{
  unsigned int val, i;

  printf("main() start\n");

  gpio_init(LED0, OUTPUT);
  gpio_init(LED1, OUTPUT);
  gpio_init(BTN0, INPUT);
  gpio_init(BTN1, INPUT);

  printf("Pushbuttons:\n");
  for (i = 0; i < 10; i++)
    {
      val = gpio_read(BTN0);
      printf(" GPIO%d: %d  ", BTN0, val);
      val = gpio_read(BTN1);
      printf("GPIO%d: %d\n", BTN1, val);

      gpio_write(LED0, HIGH);
      gpio_write(LED1, HIGH);
      sleep(1);
      gpio_write(LED0, LOW);
      gpio_write(LED1, LOW);
      sleep(1);
    }

  gpio_release(LED0);
  gpio_release(LED1);
  gpio_release(BTN0);
  gpio_release(BTN1);

  printf("main() end\n");

  return 0;
}

