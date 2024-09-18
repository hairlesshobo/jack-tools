#include <getopt.h>
#include <stdlib.h>
#include <string.h>

#include "rju-record.h"
#include "status_utils.h"

void usage(void)
{
	printf("Usage: rju-record [options] sound-file\n");
	printf("\n");
	printf("  -a, --abort_on_error                : Abort recording if an error occurs (default: no)\n");
	printf("  -b, --bitrate [option]              : file bitrate (default: 16)\n");
	printf("                                        valid options: 16, 24, 32\n\n");
	printf("  -c, --channels [number]             : Number of channels (default: 2)\n");
	printf("  -d, --disk_transfer_size [number]   : Minimal disk transfer size in frames (default: 32)\n");
	printf("  -f, --format [option]               : File format (default: wav)\n");
	printf("                                        valid options: wav, bwf, rf64\n\n");
	printf("  -h, --help                          : Show this usage page\n");
	printf("  -l, --time_limit [number]           : Set a limit timer to record for N seconds (default: unlimited)\n");
	printf("  -m, --multi_file                    : Write to multiple single channel sound files (defualt: no)\n");
	printf("  -o, --output_type [option]          : Output type to generate when running\n");
	printf("                                        valid options: none, curses, json, text\n\n");
	printf("  -p, --port_pattern [string]         : Jack port pattern to connect to (default: system:capture_%%d)\n");
	printf("  -q, --port_offset [number]          : Jack port source offset (default: 0)\n");
	printf("  -u, --no_unique                     : Do not generate unique jack client name (ie. do not append PID)\n");
	printf("  -x, --buffer_size [number]          : Ring buffer size in frames (default: 16384)\n");
	exit(1);
}

int parse_opts(int argc, char *argv[], struct recorder *recorder_obj)
{
	recorder_obj->unique_name = true;
	recorder_obj->buffer_frames = 16384;
	recorder_obj->minimal_frames = 32;
	recorder_obj->channels = 2;
	recorder_obj->timer_seconds = -1.0;
	recorder_obj->timer_counter = 0;
	recorder_obj->multiple_sound_files = 0;
	recorder_obj->do_abort = 0;
	recorder_obj->abort_on_error = 0;
	recorder_obj->bit_rate = 16;
	recorder_obj->error_count = 0;
	recorder_obj->file_format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;
	recorder_obj->last_received_data_time = 0;
	recorder_obj->buffer_performance_index = 0;
	recorder_obj->buffer_performance_filled = false;
	recorder_obj->output_type = OUTPUT_CURSES;

	strncpy(recorder_obj->port_name_pattern, "system:capture_%d", PORT_NAME_PATTERN_WIDTH-1);

    struct option long_options[] = {
        {"abort_on_error", no_argument, NULL, 'a'},
        {"bitrate", required_argument, NULL, 'b'},
        {"channels", required_argument, NULL, 'c'},
        {"disk_transfer_size", required_argument, NULL, 'd'},
        {"format", required_argument, NULL, 'f'},
		{"help", no_argument, NULL, 'h'},
        {"time_limit", required_argument, NULL, 'l'},
        {"multi_file", no_argument, NULL, 'm'},
		{"output_type", required_argument, NULL, 'o'},
        {"port_pattern", required_argument, NULL, 'p'},
        {"port_offset", required_argument, NULL, 'q'},
        {"no_unique", no_argument, NULL, 'u'},
        {"buffer_size", required_argument, NULL, 'x'},
        {NULL, 0, NULL, 0}
    };

	char *format_name = "wav";
	int c;
	

	while ((c = getopt_long(argc, argv, "ab:c:d:f:hl:mo:p:q:ux:",
					long_options, NULL)) != -1) {
		switch (c) {
			// abort_on_error
			case 'a':
				recorder_obj->abort_on_error = true;
				break;

			// bitrate
			case 'b':
				recorder_obj->bit_rate = (int)strtol(optarg, NULL, 0);
				break;

			// channels
			case 'c':
				recorder_obj->channels = (int)strtol(optarg, NULL, 0);
				break;

			// disk_transfer_size
			case 'd':
				recorder_obj->minimal_frames = (int)strtol(optarg, NULL, 0);
				break;

			// format
			case 'f':
				format_name = malloc(12);
				strncpy(format_name, optarg, 11);
				break;

			// help
			case 'h':
				usage();
				break;

			// time_limit
			case 'l':
				recorder_obj->timer_seconds = (float)strtod(optarg, NULL);
				break;

			// multi_file
			case 'm':
				recorder_obj->multiple_sound_files = 1;
				break;

			// output_type
			case 'o':
				if (strcmp(optarg, "none") == 0)
					recorder_obj->output_type = OUTPUT_NONE;
				else if (strcmp(optarg, "curses") == 0)
					recorder_obj->output_type = OUTPUT_CURSES;
				else if (strcmp(optarg, "json") == 0)
					recorder_obj->output_type = OUTPUT_JSON;
				else if (strcmp(optarg, "text") == 0) {
					fprintf(stderr, "ERROR: 'text' output type not yet implemented\n");
					recorder_obj->output_type = OUTPUT_TEXT;
					exit(1);
				} else {
					fprintf(stderr, "ERROR: unknown output type provided: '%s'\n", optarg);
					exit(1);
				}
				break;

			// port_pattern
			case 'p':
				strncpy(recorder_obj->port_name_pattern, optarg, PORT_NAME_PATTERN_WIDTH-1);
				break;

			// port_offset
			case 'q':
				recorder_obj->port_offset = (int)strtol(optarg, NULL, 0);
				break;

			// no_unique
			case 'u':
				recorder_obj->unique_name = false;
				break;

			// buffer_size
			case 'x':
				recorder_obj->buffer_frames = (int)strtol(optarg, NULL, 0);
				break;

			// handle unknown options provided
			default:
				fprintf(stderr, "\n");
				usage();
				break;
		}
	}

	if (optind != argc - 1)
		usage();

	int file_format = 0;
	int file_bitrate = 0;

	switch (recorder_obj->bit_rate) {
		case (16):
			file_bitrate = SF_FORMAT_PCM_16;
			break;
		case (24):
			file_bitrate = SF_FORMAT_PCM_24;
			break;
		case (32):
			file_bitrate = SF_FORMAT_PCM_32;
			break;
		default:
			fprintf(stderr, "Unknown bitrate provided: %d\n", recorder_obj->bit_rate);

			usage();
			return EXIT_FAILURE;
	}

	if (strcmp(format_name, "wav") == 0) {
		strncpy(recorder_obj->format_name, "WAV", 4);
		file_format = SF_FORMAT_WAV;
	} else if (strcmp(format_name, "bwf") == 0 || strcmp(format_name, "rf64") == 0) {
		strncpy(recorder_obj->format_name, "BWF", 4);
		file_format = SF_FORMAT_RF64;
	} else {
		fprintf(stderr, "ERROR: unknown file format: %s\n", format_name);
		return EXIT_FAILURE;
	}

	recorder_obj->file_format = file_format | file_bitrate;

    if (recorder_obj->channels < 1) {
	    fprintf(stderr, "ERROR: channels count provided is less than 1: %d\n", recorder_obj->channels);
        return EXIT_FAILURE;
    } else if (recorder_obj->channels > MAX_NC) {
        fprintf(stderr, "ERROR: channels count provided is too high: %d (max is %d)\n", recorder_obj->channels, MAX_NC);
        return EXIT_FAILURE;
    }

    // clear the signal levels initially
    for (int i = 0; i < MAX_NC; i++) {
        recorder_obj->sig_lvl[i][0] = -99;
        recorder_obj->sig_lvl[i][1] = -99;
    }

	clear_peaks(recorder_obj);
	clear_max(recorder_obj);

	return 0;
}
