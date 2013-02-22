fnotify
=======

fnotify handles file change notifications.
On linux fnotify is implemented with inotify and
on mac os x and bsd's kevent is used.

The api is simple

    fnotify:watch(Path) -> {ok,Wd} | {error,Reason}
    fnotify:watch(Path,[Flag]) -> {ok,Wd} | {error,Reason}

To start watching a path

    fnotify:unwatch(Wd) -> ok | {error,Reason}

To stop watching it.

The situation with the events is a bit more tricky, but the events looks like

    {fevent,Wd,[Flag],Path,Name}

Simple flags passed to user are

-    create
-    delete
-    moved_from
-    moved_to
-    {cookie,Cookie}
-    attrib

Other flags include

-   modify
-   access
-   close_write
-   close_nowrite
-   delete_self
-   move_self
-   open
-   write
-   extend
-   link
-   rename
-   revoke

Mac OS X
========

Depending on how many libraries there are, a lot of 
filedescriptors may be consumed:

The way to add more file descriptors to processes on mac os x is to:

To check maximum possible
    sysctl kern.maxfilesperproc

To check process limits
    launchctl limit

To set process limits - works sometimes.
    launchctl limit maxfiles N N
