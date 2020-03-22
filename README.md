# SciDB and the hquery command

SciDB is a column-oriented database management system (DBMS) designed
for multidimensional data management and analytics common to
scientific, geospatial, industrial, and financial applications.  See
<http://www.paradigm4.com/> and <http://www.paradigm4.com/forum> for
more information about the Data Management and Analytics Software
System (DMAS) known as SciDB.

The hquery command is a Haskell command line and interactive
interpreter for SciDB AFL queries via shim.  See also

  * hquery -y,
  * hquery -h, or
  * hquery -m

for more information

There is a primitive API derived after the creation of hquery
defined by the following data structures and actions:

  * Environment
  * Err
  * mkGlobalManagerEnv
  * runQueries
  * unsafeRunQuery
  * getSciDbVersion

# The hquery manual page

```hquery(1)                    User Command                   hquery(1)

NAME
   hquery - Haskell query for SciDB via shim

SYNOPSIS
   hquery [-V n] [-W] [-t hstfile] [-c certstore] [-n]
             [-r http[s]] [-i host] [-p port] [-a true|false]
               [-e true|false] [-o fmt] [-b num] [-d|-s]
                 [-u usr] [-w pw]
                   [-f qyfile] [query...]
   hquery -g
   hquery -h
   hquery -l
   hquery -m
   hquery -v
   hquery -y

AVAILABILITY
   Marcus D. Gabriel.  All rights reserved 2014-19(c).

   marcus@gabriel.name

   License terms: GNU GPL version 3 (hquery -l)

DESCRIPTION
   The command hquery with no operands and no options will begin an
   interacitve session with a SciDB server using SciDB's shim
   protocol at http://localhost:8080.

   With operands, each string will be executed as a SciDB AFL (Array
   Functional Language) query returning a result in SciDB DCSV
   format and fetching only 23 lines per query by default.  If
   stdin is non-empty, it will be execute before the operands.
   SciDB AFL queries are case insensitive and terminated by
   semi-colons (;).

   Before submitting a query to the shim server, the command
   hquery will
     1. return syntax and parsing errors
     2. report bad nestings, e.g., 'uniq(store(A,B));'
     3. report unknown commands, ignoring them

   Additionally, the command hquery reports SciDB/shim server
   query exceptions without terminating the process.

   The command hquery accepts Haskell sytle comments and accepts
   the setting of the following following global variables:

     -- Number of lines to fetch
     n=Integer;
     -- Format of the output
     format={'csv'|'csv+'|'dcsv'|'dense'|'sparse'|'opaque'
            |'store'|'text'|'tsv'|'tsv+'};
     -- True to fetch data, false to not fetch data
     fetch={true|false};
     -- True to read lines, false to read bytes
     readinglines={true|false};
     -- Prefix to execute before a query by shim, e.g.,
     prefix='a_query(\'a_string\');'

   Prefix is an optional, semi-colon separated, AFL statements
   to precede a query in the same SciDB connection context.
   It is mainly used for SciDB namespace and role  setting.
   There is no terminating semi-colon so trailing semi-colons
   are removed otherwise the prefix is sent to  shim unverified
   and unaltered via the variable 'prefix'.

   The command 'quit;' or 'exit;' ends the interactive session
   with exit status 0.  The command "vars;" displays the interpreter
   variables and values, and the command "funs;" displays the
   currently defined interpreter functions and definitions: exit,
   funs, quit, upload, and vars.

   The upload command takes one argument, a local file path
   string, and returns a shim server-side file path.  It is used
   in queries such as load, input, load_module that take a shim
   server-side file path. Examples are

     load(m4x4_missing,
       upload('/home/scidb/scidb/m4x4_missing.scidb'));

     input(m4x4_missing,
       upload('/home/scidb/scidb/m4x4_missing.scidb'));

     load_module(upload('/home/scidb/scidb/module_1.txt'));

     join(input(m4x4,upload('/tmp/a\\'y\\'b.txt')),
       input(m4x4,upload('/tmp/z.txt')));

   All of the above is case insensitive and terminated with a
   semi-colon (;).

INTERACTIVE SESSION
   Multi-line queries are supported interactively as they
   are in non-interactive execution.  Ctrl-D at a new prompt
   ends the interactive session.  Ctrl-C returns to the
   interactive prompt, used to end input, especially
   multi-line input.

   The default prompt is 'True/dcsv/23?', for example,

     True/dcsv/23? list('arrays');

   which displays up to 23 lines of arrays, that is,

     Fetch/Format/NumberOfLines?

   You can change the global variables 'n', 'format', 'fetch',
   'readinglines', and 'prefix' during execution which changes
   the prompt:

     True/dcsv/23? n=200;              -- Fetch 200 lines
     True/dcsv/200? n=0;               -- Fetch all lines
     True/dcsv/0? format='csv';        -- Set format to csv
     True/csv/0? fetch=false;          -- Fetch no lines regardless
                                       -- the value of n
     False/csv/0? fetch=true;format=dcsv;n=23;
     True/dcsv/23? readinglines=false; -- Fetch all bytes regardless
                                       -- the value of n
     Bytes/dcsv/23? prefix='set_namespace(\'sensor_data\');';
     Bytes/dcsv/23p list('arrays');    -- List sensor_data arrays
     Bytes/dcsv/23p quit;

   In other words, the prompt indicates the default behaviour for
   the next query executed.

OPTIONS
   The following options are supported:

   -a True|false, true to read lines, false to read bytes
      (--reading-lines).  If false, the number of lines to
      fetch (-b) is ignored and the entire output buffer is
      downloaded.  This follows the recommendataion of the
      shim documentation.

   -b Number, number of lines to fetch (--number).

   -c Certificate store file, a certificate store to load and use
      for SSL/TLS connections (--certificate-store). The insecure
      option (-n) over-rides the certificate store (-c).

   -d Use basic digest access authorization, used by either the
      http and https protocols (--digest-authorization).  Digest
      authorization (-d) over-rides SciDB authorization (-s).

   -e True|false, true to fetch lines, false to not fetch lines
      (--fetch).

   -f Query file, a readable file of queries.  Multi-line queries
      are supported in a file (--file).  If stdin is non-empty,
      it is executed before the file of queries.  If operands are
      present, they are executed after the file of queries.

   -g Get and display the SciDB version via shim and then exit
      (--scidb-version).  This performs a simple check for shim 
      and SciDB availability.

   -h Display a short hquery summary (--help).

   -i IP address or hostname, default localhost (--host).

   -l Display the hquery license terms (--license).

   -m Display this internal manual page (--manual).

   -n Do not use certificate validation, insecure because
      of potential man-in-the-middle attacks (--insecure).
      The insecure option (-n) over-rides the certificate
      store (-c).

   -o Format, ouput format such as dcsv, csv, csv+, tsv, tsv+,
      etc (--format).

   -p Port number, default 8080 for protocol (-r) http and default
      8083 for protocol (-r) https (--port).

   -r Protocol, e.g., http or https, default http (--protocol).
      If the protocol is http, the certificate store (-c) and
      insecure (-n) options are disabled.

   -s Use SciDB access authorization, requires the https protocol
      (--scidb-authorization).  This option sets the https protocol
      except when digest authorization (-d) over-rides SciDB
      authorization (-s).

   -t History file, defualt ~/.hquery_history or /tmp/.hquery_history
      if ~ does not exist (--history).

   -u Username, defualt null (--username).  Used by the digest or
      SciDB authorization option (-d or -s).  Displays an error
      and exits if neither is used with digest or SciDB
      authorization.

   -v The version of hquery (--version).

   -w Password, default null (--password).  Used by the digest or
      SciDB authorization option (-d or -s).  Displays an error
      and exits if neither is used with digest or SciDB
      authorization.

   -y A synopsis of the internal manual page (--synopsis).

   -x Prefix, a prefix to execute before a query by shim
      (--prefix).  Note that the prefix on the command line
      is verified before being sent to shim.  Prefix is a
      semi-colon separated set of statements.  It is mainly
      used for SciDB namespace and role setting.  There is no
      terminating semi-colon so trailing semi-colons are removed.

   -V Same as -V1 (--verbose).
  -V0 Quiet.  No information is sent to stderr (--verbose=0).
  -V1 Shows some HTTP exceptions and trace information (--verbose=1).
  -V2 Shows additional URL information (--verbose=2).

   -W Wait on stdin (--wait-on-stdin).  In some cases, hquery can
      determine that stdin is ready in which case it is consumed.
      The -W option guarantees that hquery waits on stdin.

OPERANDS
   SciDB AFL queries.

USAGE NOTES
   The development of the utility hquery began with SciDB community
   edition 13 and continued with 14, 15, 16, 18, and 19.

   This version of hquery has been lightly tested with ghc version
   8.2.2 and 8.6.5 and SciDB 18.1, 19.3 and 19.11 community edition.
   Currently the command hquery has never been tested on a SciDB
   enterprise edition, and thus it is not known if SciDB authorization
   (-s) or a prefix (-x) actually works.

EXAMPLES
   To list all currently defined arrays with SciDB authorization
   required, use

     hquery -i coordinator -s -u ScidB -w SciDBPassword \
         "n=0; list('arrays');"

   To list up to 100 lines of functions with digest authorization
   required, use

     hquery -i coordinator -d -u Digest \
         -w DigestPassword "n=100; list('arrays');"

   To list up to 23 lines of operators, 23 being the default, use

     hquery -i coordinator "list('operators');"

   To list all functions by reading bytes instead of lines, use

     hquery -i coordinator -a false "list('functions');"

  To create an array A, use

     hquery -i coordinator "create array A <x:double> [i=0:99:0:10];"

   To execute the file of queries HQTests.qy with no authorization
   required via a TLS connection, use

     hquery -c ssl_cert.pem  -r https -i coordinator -f HQTest.qy

   To execute the file of queries HQTests.qy with digest authorization
   required via a TLS connection insecurely, use

     hquery -n -r https -i coordinator -d -u Digest \
         -w DigestPassword -f HQTests.qy

   To list all arrays in the sensor_data namespace, use

     hquery -i coordinator -b 0 -x "set_namespace('sensor_data');" \
         "list('arrays');"

   To find and project the arrays A, B, C and D to be removed, use
     
     hquery -otsv "project(apply(filter(list('arrays'),regex(name,'A|B|C|D')),
         remove,'remove('+name+');'),remove);" | hquery -W

   To display a synopsis of this internal manual page, use

     hquery -y

   To display a summary of usage, use

     hquery -h

   To display this internal manual page, use

     hquery -m

ENVIRONMENT VARIABLES
   HOME   Home directory for the history file, by defualt
          ~/.hquery_history.

   PAGER  Page the internal manual page or license terms
          using "${PAGER}", otherwise print it.

EXIT STATUS
   An exit status of 0 is returned if successful, otherwise non-zero
   is returned.

     EXIT CODE    MEANING
         1        Unknown error.
         2        No authorization specified (-s|-d) with
	          username/password.
         3        Invalid protocol, hostname, port, or history
	          file option.
         4        Cannot connect to hostname:port.
         5        Unreadable query file.
         6        Unreadable certicate store file.
         7        Invalid certificate store.
         8        Network/certificate manager initialization error.
         9        Empty, bad or no digest authentication.
        10        Unauthorized access.
        11        Could not connect to SciDB.
        12        Input syntax error for a SciDB query.
        13        Fetch (-e) or reading-lines (-a) not true or false.
        14        Bad command-line prefix (-x).
   15..255        Unknown error.

FILES
   ~/.hquery_history

SEE ALSO
   SciDB, SciDB iquery, SciDB shim at https://www.paradigm4.com/ 
   and https://www.paradigm4.com/forum for more information.

NOTES
   Please send bug reports to
   https://github.com/mdgabriel/scidb-hquery/issues.

BUGS
   No known bugs to date.
```
