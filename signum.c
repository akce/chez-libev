/*
 * Signal number accessors.
 * Signal numbers are not the same across platforms so this lets us get the correct value via C.
 * Helpful when using ev-signal.
 *
 * man signal(7) for info.
 */
#include <signal.h>

int SIGABRT_def		(void)	{return	SIGABRT;}
int SIGALRM_def		(void)	{return	SIGALRM;}
int SIGBUS_def		(void)	{return	SIGBUS;}
int SIGCHLD_def		(void)	{return	SIGCHLD;}
int SIGCLD_def		(void)	{return	SIGCLD;}
int SIGCONT_def		(void)	{return	SIGCONT;}
int SIGFPE_def		(void)	{return	SIGFPE;}
int SIGHUP_def		(void)	{return	SIGHUP;}
int SIGILL_def		(void)	{return	SIGILL;}
int SIGINT_def		(void)	{return	SIGINT;}
int SIGIO_def		(void)	{return	SIGIO;}
int SIGIOT_def		(void)	{return	SIGIOT;}
int SIGKILL_def		(void)	{return	SIGKILL;}
int SIGPIPE_def		(void)	{return	SIGPIPE;}
int SIGPOLL_def		(void)	{return	SIGPOLL;}
int SIGPROF_def		(void)	{return	SIGPROF;}
int SIGPWR_def		(void)	{return	SIGPWR;}
int SIGQUIT_def		(void)	{return	SIGQUIT;}
int SIGSEGV_def		(void)	{return	SIGSEGV;}
int SIGSTKFLT_def	(void)	{return	SIGSTKFLT;}
int SIGSTOP_def		(void)	{return	SIGSTOP;}
int SIGTSTP_def		(void)	{return	SIGTSTP;}
int SIGSYS_def		(void)	{return	SIGSYS;}
int SIGTERM_def		(void)	{return	SIGTERM;}
int SIGTRAP_def		(void)	{return	SIGTRAP;}
int SIGTTIN_def		(void)	{return	SIGTTIN;}
int SIGTTOU_def		(void)	{return	SIGTTOU;}
int SIGURG_def		(void)	{return	SIGURG;}
int SIGUSR1_def		(void)	{return	SIGUSR1;}
int SIGUSR2_def		(void)	{return	SIGUSR2;}
int SIGVTALRM_def	(void)	{return	SIGVTALRM;}
int SIGXCPU_def		(void)	{return	SIGXCPU;}
int SIGXFSZ_def		(void)	{return	SIGXFSZ;}
int SIGWINCH_def	(void)	{return	SIGWINCH;}
