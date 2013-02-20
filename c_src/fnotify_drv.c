//
// file notification driver 
//

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#if defined(__APPLE__)
// should work for bsd's
#define HAVE_KQUEUE
#include <sys/event.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#elif defined(__linux__)
#define HAVE_INOTIFY
#include <sys/inotify.h>
#else
#error "can not use fnotify_drv"
#endif

#include "erl_driver.h"

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif


#define FNOTIFY_ADD_WATCH  1
#define FNOTIFY_DEL_WATCH  2
#define FNOTIFY_ACTIVATE   3

#define FNOTIFY_REP_OK     0  // + value 
#define FNOTIFY_REP_ERROR  1

#define MAX_PATH_LEN   4096
#define MAX_EVENTS     2048

#define U8(ptr,i)  (((unsigned char*)(ptr))[(i)])
#define INT(ptr)   ((int)((long)(ptr)))

typedef struct _watch_data_t {
    struct _watch_data_t* next;
    int wd;
    int flags;   // monitor flags used
    char path[1];
} watch_data_t;
    
typedef struct {
    ErlDrvPort       port;
    ErlDrvEvent      event;  // kqueue or inotify
    int              active; // -1=always, 0=no, 1=once,...
    int              nwatch;
    watch_data_t*    first;
    watch_data_t*    last;
} drv_data_t;

ErlDrvEntry fnotify_drv_entry;

#define push_atom(atm) do {			\
	message[i++] = ERL_DRV_ATOM;		\
	message[i++] = (atm);			\
    } while(0)

#define push_nil() do {			\
	message[i++] = ERL_DRV_NIL;	\
    } while(0)

#define push_string(str) do {			\
	message[i++] = ERL_DRV_STRING;		\
	message[i++] = (ErlDrvTermData) (str);	\
	message[i++] = strlen(str);		\
    } while(0)

#define push_int(val) do {			\
	message[i++] = ERL_DRV_INT;		\
	message[i++] = (val);			\
    } while(0)

#define push_tuple(n) do {			\
	message[i++] = ERL_DRV_TUPLE;		\
	message[i++] = (n);			\
    } while(0)

#define push_list(n) do {			\
	message[i++] = ERL_DRV_LIST;		\
	message[i++] = (n);			\
    } while(0)

ErlDrvTermData atm_fevent;
ErlDrvTermData atm_create;
ErlDrvTermData atm_delete;
ErlDrvTermData atm_modify;
ErlDrvTermData atm_access;
ErlDrvTermData atm_moved_from;
ErlDrvTermData atm_moved_to;
ErlDrvTermData atm_write;
ErlDrvTermData atm_extend;
ErlDrvTermData atm_attrib;
ErlDrvTermData atm_link;
ErlDrvTermData atm_rename;
ErlDrvTermData atm_revoke;
ErlDrvTermData atm_close_write;
ErlDrvTermData atm_close_nowrite;
ErlDrvTermData atm_delete_self;
ErlDrvTermData atm_move_self;
ErlDrvTermData atm_open;
ErlDrvTermData atm_cookie;


static int        fnotify_drv_init(void);
static void       fnotify_drv_finish(void);
static void       fnotify_drv_stop(ErlDrvData);
static void       fnotify_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void       fnotify_drv_outputv(ErlDrvData, ErlIOVec*);
static void       fnotify_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       fnotify_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData fnotify_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT fnotify_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**,ErlDrvSizeT);
static void       fnotify_drv_timeout(ErlDrvData);

#ifdef DEBUG
extern void emit_error(int level, char* file, int line, ...);
#define DEBUGF(args...) emit_error(5,__FILE__,__LINE__,args)
#define INFOF(args...)  emit_error(3,__FILE__,__LINE__,args)
#else
#define DEBUGF(args...)
#define INFOF(args...)
#endif

static int debug_level = 3;
void emit_error(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if (level <= debug_level) {
	va_start(ap, line);
	fmt = va_arg(ap, char*);

	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
    }
}

static watch_data_t* watch_data_new(char* path, size_t len, int wd)
{
    watch_data_t* ptr = driver_alloc(len+sizeof(watch_data_t));
    if (ptr) {
	ptr->next = NULL;
	memcpy(ptr->path, path, len);
	ptr->path[len] = '\0';
	ptr->wd = wd;
    }
    return ptr;
}

static void watch_data_free(watch_data_t* ptr)
{
    driver_free(ptr);
}

static void watch_data_add(drv_data_t* dptr, watch_data_t* ptr)
{
    if (dptr->last)
	dptr->last->next = ptr;
    else
	dptr->first = ptr;
    dptr->last = ptr;
    dptr->nwatch++;
}

static void watch_data_del(drv_data_t* dptr, watch_data_t** pptr)
{
    if (pptr) {
	dptr->nwatch--;
	*pptr = (*pptr)->next;
	if (pptr == &dptr->first) {
	    if (dptr->first == NULL)
		dptr->last = NULL;
	}
    }
}

static watch_data_t** watch_data_find(drv_data_t* dptr, int wd)
{
    watch_data_t** pptr = &dptr->first;
    while(*pptr) {
	if ((*pptr)->wd == wd)
	    return pptr;
	pptr = &(*pptr)->next;
    }
    return NULL;
}

#if 0
static watch_data_t** watch_data_match(drv_data_t* dptr, char* path)
{
    watch_data_t** pptr = &dptr->first;
    while(*pptr) {
	if (strcmp((*pptr)->path, path) == 0)
	    return pptr;
	pptr = &(*pptr)->next;
    }
    return NULL;
}
#endif

//
// send event to erlang: 
//   {fevent,<id>,[flags,{cookie,Val}],Path,Name}
//
#if defined(HAVE_INOTIFY)
static void fnotify_send_event(drv_data_t* dptr, watch_data_t* wdata,
			       struct inotify_event *event)
{
    ErlDrvTermData message[1024];
    uint32_t mask = event->mask;
    int i = 0;
    int num_flags = 0;

    push_atom(atm_fevent);
    push_int(event->wd);
    if (mask & IN_ACCESS) { push_atom(atm_access); num_flags++; }
    // file was accessed (read)
    if (mask & IN_ATTRIB) { push_atom(atm_attrib); num_flags++; }
    // metadata changed, permission, timestamp etc
    if (mask & IN_CLOSE_WRITE) { push_atom(atm_close_write); num_flags++; }
    // file opened for writing was closed
    if (mask & IN_CLOSE_NOWRITE) { push_atom(atm_close_nowrite); num_flags++; }
    // file not opened for writing was closed
    if (mask & IN_CREATE) { push_atom(atm_create); num_flags++; }
    // file/directory created in watched directiory
    if (mask & IN_DELETE) { push_atom(atm_delete); num_flags++; }
    // file/directory deleted from watched directiory
    if (mask & IN_DELETE_SELF) { push_atom(atm_delete_self); num_flags++; }
    // watch file/directory was itself deleted 
    if (mask & IN_MODIFY) { push_atom(atm_modify); num_flags++; }
    // file was modified
    if (mask & IN_MOVE_SELF) { push_atom(atm_move_self); num_flags++; }
    // Watched file/directory was itself moved
    if (mask & IN_MOVED_FROM) { push_atom(atm_moved_from); num_flags++; }
    // file/directory was moved out of watched directory
    if (mask & IN_MOVED_TO) { push_atom(atm_moved_to); num_flags++; }
    // file/directory was moved into watched directory
    if (mask & IN_OPEN) { push_atom(atm_open); num_flags++; }
    // file was opened

    // extra options to inotify_add_watch
    // IN_DONT_FOLLOW 
    // IN_EXCL_UNLINK
    // IN_MASK_ADD
    // IN_ONESHOT
    // IN_ONLYDIR
    // returned by read:
    // IN_IGNORED  - watch was removed explicitly or automatically
    // IN_ISDIR    - subject of the event is a directory
    // IN_Q_OVERFLOW - event queue overflowed (wd = -1)
    // IN_UNMOUNT  - file system containing watched object was unmounted
    if (mask & (IN_MOVED_FROM|IN_MOVED_TO)) {
	push_atom(atm_cookie);
	push_int(event->cookie);
	push_tuple(2);
	num_flags++;
    }
    push_nil();
    push_list(num_flags+1);

    if (wdata)
	push_string(wdata->path);
    else
	push_nil();
    push_string(event->name);

    push_tuple(5);

    driver_output_term(dptr->port, message, i);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}

#elif defined(HAVE_KQUEUE)
static void fnotify_send_event(drv_data_t* dptr, struct kevent* kevp)
{
    watch_data_t* wdata;
    ErlDrvTermData message[1024];
    uint32_t mask;
    int i = 0;
    int num_flags = 0;

    if (kevp->filter != EVFILT_VNODE)
	return;
    wdata = (watch_data_t*) kevp->udata;
    mask = kevp->fflags;

    push_atom(atm_fevent);
    push_int(kevp->ident);

    if (mask & NOTE_DELETE) { push_atom(atm_delete); num_flags++; }
    // unlink() was called on the file referenced by the descriptor
    if (mask & NOTE_WRITE)  { push_atom(atm_write); num_flags++; }
    // a write occured on the file reference by the decsriptor
    if (mask & NOTE_EXTEND) { push_atom(atm_extend); num_flags++; }
    // a file reference by the decsriptor was extended
    if (mask & NOTE_ATTRIB) { push_atom(atm_attrib); num_flags++; }
    // The file referenced by the descriptor had its attributes changed
    if (mask & NOTE_LINK)   { push_atom(atm_link); num_flags++; }
    // The link count on the file changed
    if (mask & NOTE_RENAME) { push_atom(atm_rename); num_flags++; }
    // The file referenced by the descriptor was renamed
    if (mask & NOTE_REVOKE) { push_atom(atm_revoke); num_flags++; }
    // The file referenced by the descriptor was revoked via revoke(2) or
    // the underlaying filesystem was unmounted 
    push_nil();
    push_list(num_flags+1);

    if (wdata)
	push_string(wdata->path);
    else
	push_nil();
    push_nil();  // should be the "file" event is covering
    push_tuple(5);

    driver_output_term(dptr->port, message, i);

    if (dptr->active > 0) {
	dptr->active--;
	if (dptr->active == 0)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
    }
}
#endif

// initialize main desc
static int fnotify_init_watch(drv_data_t* dptr)
{
    int fd = -1;
#if defined(HAVE_INOTIFY)
    fd = inotify_init();
#elif defined(HAVE_KQUEUE)
    fd = kqueue();
#endif
    if (fd >= 0)
	dptr->event = (ErlDrvEvent)((long)fd);
    DEBUGF("kqueue fd=%d", fd);
    return fd;
}

static int fnotify_add_watch(drv_data_t* dptr, char* path, size_t len, int flags)
{
#if defined(HAVE_INOTIFY)
    watch_data_t* wdata;
    int wd;
    int iflags = IN_CREATE | IN_DELETE | IN_DELETE_SELF | 
	IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO;
    // iflags = IN_ALL_EVENTS?
    // IN_MOVE = IN_MOVED_FROM | IN_MOVED_TO | 
    // IN_CLOSE = IN_OPEN | IN_CLOSE_WRITE | IN_CLOSE_NOWRITE
    // FIXME: translate flags

    if (!(wdata = watch_data_new(path, len, -1)))
	return -1;
    if ((wd = inotify_add_watch(INT(dptr->event), wdata->path, iflags)) < 0) {
	int err = errno;
	watch_data_free(wdata);
	errno = err;
	return wd;
    }
    wdata->wd = wd;
    wdata->flags = flags;
    watch_data_add(dptr, wdata);
    return wd;
#elif defined(HAVE_KQUEUE)
    int wd;
    struct kevent ev_add;
    struct timespec timePoll  = { 0, 0 };
    watch_data_t* wdata;
    unsigned int vnode_events = NOTE_DELETE |  NOTE_WRITE | NOTE_EXTEND |
	NOTE_ATTRIB | NOTE_LINK | NOTE_RENAME | NOTE_REVOKE;
    // FIXME: translate flags

    if (!(wdata = watch_data_new(path, len, -1)))
	return -1;
    if ((wd = open(wdata->path, O_EVTONLY)) < 0) {
	int err = errno;
	INFOF("open failed, %s", strerror(errno));
	watch_data_free(wdata);
	errno = err;
	return wd;
    }
    DEBUGF("open fd=%d", wd);
    wdata->wd = wd;
    EV_SET(&ev_add, wd, EVFILT_VNODE, EV_ADD | EV_CLEAR, 
	   vnode_events, 0, wdata);
    if (kevent(INT(dptr->event), &ev_add, 1, NULL, 0, &timePoll) < 0) {
	int r = errno;
	INFOF("kevent EV_ADD|EV_CLEAR failed, %s", strerror(errno));
	watch_data_free(wdata);
	errno = r;
	return -1;
    }
    watch_data_add(dptr, wdata);
    return wd;
#endif
}

static int fnotify_del_watch(drv_data_t* dptr, int wd)
{
#if defined(HAVE_INOTIFY)
    watch_data_t** pptr = watch_data_find(dptr, wd);
    watch_data_t* wdata;
    int r;
    if (!pptr) {
	errno = ENOENT;
	return -1;
    }
    wdata = *pptr;

    if ((r = inotify_rm_watch(INT(dptr->event), wd)) < 0)
	return -1;
    watch_data_del(dptr, pptr);
    watch_data_free(wdata);
    return r;
#elif defined(HAVE_KQUEUE)
    watch_data_t** pptr = watch_data_find(dptr, wd);
    watch_data_t* wdata;
    struct kevent ev_del;
    struct timespec timePoll  = { 0, 0 };
    unsigned int vnode_events = NOTE_DELETE |  NOTE_WRITE | NOTE_EXTEND |
	NOTE_ATTRIB | NOTE_LINK | NOTE_RENAME | NOTE_REVOKE;
    int r;

    if (!pptr) {
	errno = ENOENT;
	return -1;
    }
    wdata = *pptr;

    // FIXME: translate flags
    EV_SET(&ev_del, wd, EVFILT_VNODE, EV_DELETE, vnode_events, 0, wdata);
    if ((r = kevent(INT(dptr->event), &ev_del, 1, NULL, 0, &timePoll)) < 0) {
	INFOF("kevent EV_DELETE failed, %s", strerror(errno));
	return -1;
    }
    DEBUGF("close fd=%d", wd);
    close(wd);
    watch_data_del(dptr, pptr);
    watch_data_free(wdata);
    return r;
#endif
}

static void fnotify_handle_event(drv_data_t* dptr)
{
#if defined(HAVE_INOTIFY)
    char buf[MAX_PATH_LEN] __attribute__((aligned(4)));
    ssize_t len, i=0;

    len = read(INT(dptr->event), buf, sizeof(buf));
    while(i < len) {
	struct inotify_event *event =
	    (struct inotify_event *) &buf[i];
	watch_data_t** pptr = watch_data_find(dptr, event->wd);
	watch_data_t* wdata = pptr ? *pptr : 0;
	fnotify_send_event(dptr, wdata, event);
	i += sizeof(struct inotify_event) + event->len;
    }
#elif defined(HAVE_KQUEUE)
    struct kevent ev_list[MAX_EVENTS] = { { 0 } };
    struct timespec timePoll  = { 0, 0 }; 
    int n;
    int i;

    n = kevent(INT(dptr->event), NULL, 0, ev_list, MAX_EVENTS, &timePoll);
    if (n < 0) {
	INFOF("kevent failed, %s", strerror(errno));
	exit(1);
	return;
    }
    for (i = 0; i < n; i++) {
	fnotify_send_event(dptr, &ev_list[i]);
    }
#endif    
}


static int fnotify_drv_init(void)
{
    atm_fevent = driver_mk_atom("fevent");
    atm_create = driver_mk_atom("create");
    atm_delete = driver_mk_atom("delete");
    atm_modify = driver_mk_atom("modify");
    atm_access = driver_mk_atom("access");
    atm_moved_from = driver_mk_atom("moved_from");
    atm_moved_to = driver_mk_atom("moved_to");
    atm_write = driver_mk_atom("write");
    atm_extend = driver_mk_atom("extended");
    atm_attrib = driver_mk_atom("attrib");
    atm_link = driver_mk_atom("link");
    atm_rename = driver_mk_atom("rename");
    atm_revoke = driver_mk_atom("revoke");
    atm_close_write = driver_mk_atom("close_write");
    atm_close_nowrite = driver_mk_atom("close_nowrite");
    atm_delete_self = driver_mk_atom("delete_self");
    atm_move_self = driver_mk_atom("move_self");
    atm_open  = driver_mk_atom("open");
    atm_cookie = driver_mk_atom("cookie");
    return 0;
}

static void       fnotify_drv_finish(void)
{
    INFOF("finish called");
}

static void       fnotify_drv_stop(ErlDrvData d)
{
    drv_data_t* dptr = (drv_data_t*) d;

    // fprintf(stderr, "fnotify_drv_stop called!!!\r\n");
    if (dptr) {
	watch_data_t* ptr;

	if (dptr->active)
	    driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	ptr = dptr->first;
	while(ptr) {
	    watch_data_t* next = ptr->next;
#if defined(HAVE_INOTIFY)
	    inotify_rm_watch(INT(dptr->event), ptr->wd);
#elif defined(HAVE_KQUEUE)
	    close(ptr->wd);
#endif
	    watch_data_free(ptr);
	    ptr = next;
	}
	DEBUGF("close fd=%d\r\n", INT(dptr->event));
	close(INT(dptr->event));
	driver_free(dptr);
    }
}

static void       fnotify_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    INFOF("output called");
}

static void       fnotify_drv_outputv(ErlDrvData d, ErlIOVec* iov)
{
    (void) d;
    (void) iov;
    INFOF("outputv called");
}

// netlink socket triggered process data
static void fnotify_drv_ready_input(ErlDrvData d, ErlDrvEvent event)
{
    drv_data_t* dptr = (drv_data_t*) d;
    (void) event;
    DEBUGF("ready_input event=%d fd=%d", INT(event), INT(dptr->event));
    fnotify_handle_event(dptr);
}

static void fnotify_drv_ready_output(ErlDrvData d, ErlDrvEvent event)
{
    (void) d;
    (void) event;
    INFOF("ready_output called");
}


static ErlDrvSSizeT fnotify_drv_ctl(ErlDrvData d,unsigned int cmd,char* buf,
				    ErlDrvSizeT len,char** rbuf,
				    ErlDrvSizeT rlen)
{
    drv_data_t* dptr = (drv_data_t*) d;
    char* rdata = *rbuf;
    int err = 0;

    INFOF("fnotify_drv_ctl: cmd=%d, fd=%d", cmd, INT(dptr->event));

    switch(cmd) {

    case FNOTIFY_ADD_WATCH: {
	int wd;
	if (len < 2)
	    goto L_einval;
	wd = fnotify_add_watch(dptr, buf+1, len-1, buf[0]);
	if (wd < 0) {
	    err = errno;
	    goto L_error;
	}
	rdata[0] = FNOTIFY_REP_OK;
	rdata[1] = (wd >> 24) & 0xff;
	rdata[2] = (wd >> 16) & 0xff;
	rdata[3] = (wd >> 8) & 0xff;
	rdata[4] = wd & 0xff;
	return 5;
    }

    case FNOTIFY_DEL_WATCH: {
	int wd, r;
	if (len != 4)
	    goto L_einval;
	wd = (U8(buf,0)<<24) | (U8(buf,1)<<16) | (U8(buf,2)<<8) | U8(buf,3);
	r = fnotify_del_watch(dptr, wd);
	if (r < 0) {
	    err = errno;
	    goto L_error;
	}
	break;
    }

    case FNOTIFY_ACTIVATE: {  // start/stop sending events
	int active;

	if (len != 2)
	    goto L_einval;
	active = (((uint8_t*)buf)[0] << 8) | ((uint8_t*)buf)[1];
	if (active == 0xffff)
	    active = -1;
	if (active) {
	    if (!dptr->active)
		driver_select(dptr->port, dptr->event, ERL_DRV_READ, 1);
	    dptr->active = active;
	}
	else {
	    if (dptr->active)
		driver_select(dptr->port, dptr->event, ERL_DRV_READ, 0);
	    dptr->active = 0;
	}
	break;
    }

    default:
	return -1;
    }

// L_ok:
    rdata[0] = FNOTIFY_REP_OK;
    return 1;

L_einval:
    err = EINVAL;
L_error:
    rdata[0] = FNOTIFY_REP_ERROR;
    {
	char* err_str = strerror(err);
	int   err_str_len = strlen(err_str);
	if (err_str_len > 255) err_str_len = 255;
	if (err_str_len >= rlen) err_str_len = rlen - 1;
	memcpy(&rdata[1], err_str, err_str_len);
	return err_str_len;
    }
}

static void       fnotify_drv_timeout(ErlDrvData d)
{
    (void) d;
    // fprintf(stderr, "fnotify_drv_timeout called!!!\r\n");
}


static ErlDrvData fnotify_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    int err;
    drv_data_t* dptr;

    // Setup
    if (!(dptr = driver_alloc(sizeof(drv_data_t))))
	return ERL_DRV_ERROR_ERRNO;

    memset(dptr, 0, sizeof(drv_data_t));
    dptr->port = port;

    if (fnotify_init_watch(dptr) < 0) {
	err = errno;
	driver_free(dptr);
	errno = err;
	return ERL_DRV_ERROR_ERRNO;
    }
    return (ErlDrvData) dptr;
}

DRIVER_INIT(nl_drv)
{
    ErlDrvEntry* ptr = &fnotify_drv_entry;

    ptr->driver_name = "fnotify_drv";
    ptr->init  = fnotify_drv_init;
    ptr->start = fnotify_drv_start;
    ptr->stop  = fnotify_drv_stop;
    ptr->output = fnotify_drv_output;
    ptr->ready_input  = fnotify_drv_ready_input;
    ptr->ready_output = fnotify_drv_ready_output;
    ptr->finish = fnotify_drv_finish;
    ptr->control = fnotify_drv_ctl;
    ptr->timeout = fnotify_drv_timeout;
    ptr->outputv = fnotify_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = 0;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = 0;  // add me

    return (ErlDrvEntry*) ptr;
}
