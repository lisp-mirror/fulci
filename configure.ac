dnl fulci a program to organize your movies collection
dnl Copyright (C) 2019  cage

dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([fulci],[0.1],[https://notabug.org/cage/fulci/],[fulci])

AM_INIT_AUTOMAKE([-Wall foreign])

AM_GNU_GETTEXT([external])

AC_PATH_PROG([LISP_COMPILER],[sbcl],[no])

if test "$LISP_COMPILER" = "no" ; then
   AC_MSG_ERROR([Can not find SBCL, Common Lisp compiler.])
fi

AC_PATH_PROG([CURL],[curl],[no])

if test "$CURL" = "no" ; then
   AC_MSG_ERROR([Can not find curl.])
   exit 1;
fi

AC_PATH_PROG([GPG],[gpg],[no])

if test "$GPG" = "no" ; then
   AC_MSG_ERROR([Can not find gpg, crypto software.])
   exit 1;
fi

AC_PATH_PROG([BASH],[bash],[no])

if test "$BASH" = "no" ; then
   AC_MSG_ERROR([Can not find bash shell.])
   exit 1;
fi

AC_PATH_PROG([GAWK],[gawk],[no])

if test "$GAWK" = "no" ; then
   AC_MSG_ERROR([Can not find GNU AWK (gawk).])
   exit 1;
fi

AC_PROG_MKDIR_P

dnl check sbcl version
SBCL_MIN_VERSION="1.4.8";
SBCL_VERSION=`${LISP_COMPILER} --version | ${GAWK} -- '// {print $2}'`
SBCL_VERSION_OK=`echo "$SBCL_VERSION $SBCL_MIN_VERSION" | awk -f compare_version.awk`

if test "$SBCL_VERSION_OK" = "1" ; then
   AC_MSG_ERROR([Lisp compiler too old, $SBCL_MIN_VERSION is the oldest supported.])
   exit 1;
fi

AC_PATH_PROG([WISH],[wish],[no])

if test "$WISH" = "no" ; then
   AC_MSG_ERROR([Can not find wish, the TK interpreter.])
   exit 1;
fi

AC_PATH_PROG([SQLITE],[sqlite3],[no])

if test "$SQLITE" = "no" ; then
   AC_MSG_ERROR([Can not find sqlite3 binary.])
   exit 1;
fi

AC_PATH_PROG([GIMP],[gimp],[no])

if test "$GIMP" = "no" ; then
   AC_MSG_ERROR([Can not find gimp.])
   exit 1;
fi

AC_PATH_PROG([GV],[gv],[no])

if test "$GV" = "no" ; then
   AC_MSG_WARN([Can not find gv (PostScript visualizer).])
   exit 1;
fi


AC_CONFIG_FILES([Makefile quick_quicklisp.sh po/Makefile.in src/config.lisp.in])

AC_OUTPUT
