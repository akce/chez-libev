/*
 * C level platform info.
 *
 * Eventually this could be used to verify ftypes structs have their fields at the correct offsets;
 * perhaps even generate platform specific ev lib code.
 */
#include <ev.h>

#include <stdio.h>
#include <stddef.h>	// offsetof
#include <sys/stat.h>

int
main ()
	{
	// struct stat is included inline into ev_stat.
	printf("(define sizeof-struct-stat %d)\n", sizeof(struct stat));

	printf("(define sizeof-ev-io-t %d)\n", sizeof(struct ev_io));
	printf("(define sizeof-ev-timer-t %d)\n", sizeof(struct ev_timer));
	printf("(define sizeof-ev-periodic-t %d)\n", sizeof(struct ev_periodic));
	printf("(define sizeof-ev-signal-t %d)\n", sizeof(struct ev_signal));
	printf("(define sizeof-ev-child-t %d)\n", sizeof(struct ev_child));

	printf("(define sizeof-ev-stat-t %d)\n", sizeof(struct ev_stat));
	printf("(define offset-ev-stat-t:wd %d)\n", offsetof(struct ev_stat, wd));

	printf("(define sizeof-ev-idle-t %d)\n", sizeof(struct ev_idle));
	printf("(define sizeof-ev-prepare-t %d)\n", sizeof(struct ev_prepare));
	printf("(define sizeof-ev-check-t %d)\n", sizeof(struct ev_check));
	printf("(define sizeof-ev-fork-t %d)\n", sizeof(struct ev_fork));
	printf("(define sizeof-ev-cleanup-t %d)\n", sizeof(struct ev_cleanup));
	printf("(define sizeof-ev-embed-t %d)\n", sizeof(struct ev_embed));
	printf("(define sizeof-ev-async-t %d)\n", sizeof(struct ev_async));

	return 0;
	}
