#ifndef PORT_COMMS_H
#define PORT_COMMS_H

int read_cmd (char *buf);
int write_cmd (ei_x_buff *buf);
int read_exact (char *buf, int len);
int write_exact (char *buf, int len);

#endif
