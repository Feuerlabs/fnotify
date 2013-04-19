PRIVDIR := ../priv

all : $(PRIVDIR)/fnotify_driver.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/inpevt_driver.so : fnotify_driver.o dosmap.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

clean:
	$(RM) -f $(PRIVDIR)/fnotify_driver.so fnotify_driver.o dosmap.o
