PRIVDIR := ../priv

all : $(PRIVDIR)/fnotify_drv.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/fnotify_drv.so : fnotify_drv.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/fnotify_drv.so fnotify_drv.o
