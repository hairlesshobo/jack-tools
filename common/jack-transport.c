/*  jack-transport.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>

#include "failure.h"
#include "jack-transport.h"
#include "print.h"

bool jack_transport_is_rolling(jack_client_t *client)
{
  jack_transport_state_t s = jack_transport_query(client , NULL);
  return s & JackTransportRolling;
}  
