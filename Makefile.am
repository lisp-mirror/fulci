# lisp-skel: a template to build lisp system with quicklisp.
# Copyright (C) 2018 cage

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free  Software Foundation, either  version 3 of the  License, or
# (at your option) any later version.

# This program is distributed in the  hope that it will be useful, but
# WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
# MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU
# General Public License for more details.

# You should  have received a copy  of the GNU General  Public License
# along      with      this       program.       If      not,      see
# <http://www.gnu.org/licenses/>.


ACLOCAL_AMFLAGS = -I m4

bin_SCRIPTS = fulci

CONF_PATH_FILE = src/config.lisp

CONF_PATH_FILE_IN = src/config.lisp.in

BUILT_SOURCES  = src/config.lisp

CLEANFILES = $(bin_SCRIPTS) $(CONF_PATH_FILE)

SUBDIRS = po

EXTRA_DIST = config.rpath m4/ChangeLog  config.rpath m4/ChangeLog ChangeLog \
fulci.asd README.org src LICENSES.org COPYING src

nobase_dist_pkgdata_DATA = data/icons/search.png \
data/icons/movie.png                             \
data/icons/delete-movie.png                      \
data/icons/add-movie.png                         \
data/icons/lookup-internet-movie.png             \
data/icons/edit-movie.png                        \
data/icons/dvd.png                               \
data/icons/delete-dvd.png                        \
data/icons/add-dvd.png                           \
data/icons/edit-dvd.png                          \
data/icons/persons.png                           \
data/icons/genre.png                             \
data/icons/add.png                               \
data/icons/delete.png                            \
data/icons/edit.png                              \
data/icons/edit-small.png                        \
data/icons/add-small.png                         \
data/icons/delete-small.png                      \
data/icons/fulci-icon.png                        \
data/icons/goto-wiki.png                         \
data/icons/wiki-fetch-image.png                  \
data/icons/wiki-fetch-data.png                   \
data/icons/www-fetch-image.png                   \
data/icons/dvd-in-case.png                       \
data/icons/barcode.png                           \
data/icons/details.png                           \
data/icons/go-to-copy.png                        \
data/icons/world.png                             \
data/font.afm                                    \
data/countries.csv

$(PACKAGE): $(CONF_PATH_FILE) src/
	$(LISP_COMPILER) \
	--eval "(asdf:load-system '$(PACKAGE))" \
	--eval "(in-package main)" \
	--eval "(sb-ext:save-lisp-and-die \"$(PACKAGE)\" :toplevel 'main::main :executable t :purify t :save-runtime-options t)"


$(CONF_PATH_FILE): $(CONF_PATH_FILE_IN)
	grep "^;" $(CONF_PATH_FILE_IN)                          >  $(CONF_PATH_FILE);
	echo -e "(in-package :config)\n"                        >> $(CONF_PATH_FILE);
	echo "(alexandria:define-constant +sys-data-dir+"       >> $(CONF_PATH_FILE);
	echo -e "\""$(pkgdatadir)"/data/\" :test #'string=)\n"  >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +catalog-dir+"        >> $(CONF_PATH_FILE);
	echo -e "\""$(localedir)"\" :test #'string=)\n"         >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +text-domain+"        >> $(CONF_PATH_FILE);
	echo -e "\""$(PACKAGE)"\" :test #'string=)\n"           >> $(CONF_PATH_FILE);

	echo "(alexandria:define-constant +program-name+"       >> $(CONF_PATH_FILE);
	echo -e "\""$(PACKAGE)"\" :test #'string=)\n"           >> $(CONF_PATH_FILE);
	cat  $(CONF_PATH_FILE).in | sed "\/^;;.*$\/d"           >> $(CONF_PATH_FILE);
