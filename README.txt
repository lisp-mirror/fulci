                                ━━━━━━━
                                 FULCI
                                ━━━━━━━


Table of Contents
─────────────────

1 Introduction
.. 1.1 Features
2 Dependency
.. 2.1 Programs
.. 2.2 Libraries
..... 2.2.1 C
..... 2.2.2 Common Lisp
3 Install
4 Usage
.. 4.1 Searching
..... 4.1.1 Search movie copies
..... 4.1.2 Searching titles
.. 4.2 Import data from imdb
.. 4.3 Import data from wikipedia
5 Notes
.. 5.1 Important note
6 BUGS
7 Translations
8 License
9 Privacy
10 Contributing
11 NO WARRANTY





1 Introduction
══════════════

  Fulci aims to be a simple software to organize your dvd/blu-ray/vcd
  collection.


1.1 Features
────────────

  • Adding titles or copies and perform search with an [expression
    language];
  • generating barcode to identify a copy of a movie;
  • optionally import data from wikipedia


[expression language] See section 4.1


2 Dependency
════════════

2.1 Programs
────────────

  • sbcl compiler;
  • sqlite3;
  • wish graphical shell (>= 8.6);
  • GIMP.


2.2 Libraries
─────────────

2.2.1 C
╌╌╌╌╌╌╌

  • libsqlite3 There is no needs for the header file, just the actual
    library is needed


2.2.2 Common Lisp
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  The script (see 3) provided will [download] and install all the needed
  libraries:

  • alexandria;
  • cl-ppcre;
  • sqlite;
  • dbi;
  • sxql;
  • yacc;
  • parse-number;
  • cl-syntax;
  • lquery;
  • local-time;
  • osicat;
  • cl-colors2;
  • nodgui;
  • drakma;
  • log4cl;
  • clunit2;
  • cl-i18n;
  • cl-jpeg;
  • cl-json;
  • osicat;
  • babel;
  • marshal;
  • cl-pslib;
  • cl-pslib-barcode;
  • cl-csv.


[download] See section 9


3 Install
═════════

  1. optional step needed only if you have not already the configure
     script, you will need `autotools' for that.

     ┌────
     │ $ autoreconf -fiv
     └────

  2. run `configure' and resolve the missing dependencies (if any)

     ┌────
     │ $ ./configure
     └────

  3. This script will download and install the library manager and the
     library on your home dir.

     ┌────
     │ $ ./quick_quicklisp.sh
     └────

     This step is optional if you have already installed quicklisp, in
     this case just load the [dependencies] using the client installed
     on your computer.

  4. build the executable:

     ┌────
     │ $ make
     └────

  5. install on your system:

     ┌────
     │ $ make install
     └────

  6. run the software!

     ┌────
     │ $ fulci
     └────


[dependencies] See section 2.2.2


4 Usage
═══════

4.1 Searching
─────────────

4.1.1 Search movie copies
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Type a search criteria to start a searching matching the string you
  typed in any field of a movie copy object.  You can start a query with
  an `<' followed by a number to lookup the latest `n' copies inserted
  into the database.

  Finally you can use a simple expression language if your query starts
  with a: `!' ([BNF] following):

  ┌────
  │ EXPRESSION    := EXPRESSION AND EXPRESSION |
  │                  EXPRESSION OR  EXPRESSION |
  │                  '(' EXPRESSION ')'        |
  │                  TERM
  │ TERM          := KEY = VALUE | KEY < VALUE | KEY > VALUE
  │ KEY           := 'director' | 'year' | 'title' | 'note' | 'tags' | 'genres' | 'country' |
  │                  'building' | 'room' | 'storage' | 'shelf'
  │ VALUE         := [a-z,A-Z,0-9,%]
  │ AND           := 'and'
  │ OR            := 'or'
  │ =             := '='
  │ <             := '<'
  │ >             := '>'
  │
  └────

  Example:

  All the horror movie stored in room `237' or `1408':

  ┌────
  │
  │ ! room = 237 or room = 1408
  │
  └────


[BNF] https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form


4.1.2 Searching titles
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  For searching a title you can type a string to search in any field or
  you can use a simple expression language similar as can be done for
  copies.

  ┌────
  │
  │ EXPRESSION    := EXPRESSION AND EXPRESSION |
  │                  EXPRESSION OR  EXPRESSION |
  │                  '(' EXPRESSION ')'        |
  │                  TERM
  │ TERM          := KEY = VALUE | KEY < VALUE | KEY > VALUE
  │ KEY           := 'director' | 'year' | 'title' | 'note' | 'tags' | 'genres' | 'country'
  │ VALUE         := [a-z,A-Z,0-9,%]
  │ AND           := 'and'
  │ OR            := 'or'
  │ =             := '='
  │ <             := '<'
  │ >             := '>'
  │
  └────

  Example:

  All the horror movie from [Lucio Fulci]

  ┌────
  │
  │ ! director = lucio%fulci and genres = horror
  │
  └────

  where `%' means: "a variable number of any character".


[Lucio Fulci] https://en.wikipedia.org/wiki/Lucio_Fulci


4.2 Import data from imdb
─────────────────────────

  People at [IMDB (Internet Movie Database)] are kind enough to provide
  a [dump of their database in Tab separated format value].  This
  software can import those data to build a very comprehensive local
  database.

  Just download the data, unzip, and follow the instruction under
  `Import > From TSV'.

  Please note that the license to use the aforementioned data is
  restrictive (from a FLOSS POV), check the [license] before use the
  data.


[IMDB (Internet Movie Database)] https://www.imdb.com/

[dump of their database in Tab separated format value]
https://datasets.imdbws.com/

[license] https://www.imdb.com/interfaces/


4.3 Import data from wikipedia
──────────────────────────────

  Fulci can fetch movie data and movie image from wikipedia when a title
  is added or updated.

  Please note that even if the movie textual data (director, runtime)
  are usually released under a perimissive license often the image data
  are not, it is the user responsability to comply with their local
  copyright law when this data are used.

  In any case please do *not* distribute this program together with a
  database built on top of non redistibutable data. If i discover that
  this happened i will remove the software from my repository.

  The others features of the software supposed to be simple to grok i
  think.


5 Notes
═══════

  • only [SBCL] compiler is supported;

  • please do *not* ask for features that are a violation of a website
    terms of service (scraping a website that does not allow that, for
    example);

  • please also do *not* ask for features that make the program
    interacts with website that wants an user account creation as a
    precondition to allow access to the data, i do not want this
    software to support users profiling/tracking;

  • I am not a lawyer.


[SBCL] http://sbcl.org/

5.1 Important note
──────────────────

  Older version of the software stored database in: `$HOME/.fulci', but
  starting from version `0.9.1' it is stored under:
  `$XDG_DATA_HOME/fulci/', if `$XDG_DATA_HOME' is not set on your system
  `$HOME/.local/share' is used.


6 BUGS
══════

  Please file bug reports on the [notabug repository].


[notabug repository] https://notabug.org/cage/fulci/


7 Translations
══════════════

  ━━━━━━━━━━━━━━━━━━━━━━━━━━
   language  progress  note
  ──────────────────────────
   italian       100%
  ━━━━━━━━━━━━━━━━━━━━━━━━━━

  You are [very welcome] to help with translations, the translation
  template can be found in `po/fulci.pot' in gettext format, there are a
  bunch of free software editor to work with this file or you can just
  [use emacs].


[very welcome] See section 10

[use emacs]
https://www.gnu.org/software/gettext/manual/html_node/PO-Mode.html


8 License
═════════

  This program is released under GNU General Public license version 3 or
  later (see COPYING file).

  The program use data and code from other sources, please see
  LICENSES.org for credits.

  Although any efforts has been put to make the list of credits
  exhaustive, errors are always possible.  Please send correction to
  cage-dev at twistfold dot it.


9 Privacy
═════════

  This software does collect nothing from its users in places different
  from their local computer.

  But it *does* start some https connections to the Wikipedia servers;
  this feature is totally optional and started only after an explicit
  user action. However if you plan to use this feature please check the
  [Wikipedia privacy policy] before.

  Moreover launching `quick_quicklisp.sh' will contact
  [https://www.quicklisp.org/], check the [quicklisp sources] for
  details.


[Wikipedia privacy policy]
https://meta.wikimedia.org/wiki/Privacy_policy

[quicklisp sources] https://beta.quicklisp.org/quicklisp.lisp


10 Contributing
═══════════════

  Any help is appreciated. If you intend to contribute please point your
  browser to the [issue tracker] or file a [pull request].


[issue tracker] https://notabug.org/cage/fulci/issues

[pull request] https://notabug.org/cage/fulci/pulls


11 NO WARRANTY
══════════════

  fulci: a program to organize your movies collection Copyright (C) 2019
  cage

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see [http://www.gnu.org/licenses/].


[http://www.gnu.org/licenses/] http://www.gnu.org/licenses/
