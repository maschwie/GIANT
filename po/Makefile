# Generated automatically from Makefile.in by configure.
LANGS = de
MKDIR = mkdir -p
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
localedir     = ../locale

all:

$(LANGS):
	-msgfmt -o $@.gmo $@.po
	$(MKDIR) $(localedir)/$@/LC_MESSAGES
	-$(INSTALL_DATA) $@.gmo $(localedir)/$@/LC_MESSAGES/giant.mo

install: $(LANGS)

clean:
	-$(RM) *.gmo 
	-$(RM) skeleton.po.new
