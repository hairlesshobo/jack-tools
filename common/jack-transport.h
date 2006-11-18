#ifndef _COMMON_JACK_TRANSPORT_H
#define _COMMON_JACK_TRANSPORT_H

#include <stdbool.h>
#include <jack/jack.h>

bool jack_transport_is_rolling(jack_client_t *client);

#endif
