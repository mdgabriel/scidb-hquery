module Manual(putManual) where

import System.Exit (exitWith)
import UtilsUnsafe (putOrPageStrLn)

-- | Display the internal manual.

putManual :: IO ()
putManual = putOrPageStrLn internalManual >>= exitWith

-- | Internal manual.  The definitive source of the internal manual is
-- the description section of the module Main.

internalManual :: String
internalManual =
    "hquery(1)                    User Command                   hquery(1)\n"++
    "\n"++
    "NAME\n"++
    "   hquery - Haskell query for SciDB via shim\n"++
    "\n"++
    "SYNOPSIS\n"++
    "   hquery [-V n] [-t hstfile] [-c certstore] [-n]\n"++
    "             [-r http[s]] [-i host] [-p port] [-a true|false]\n"++
    "               [-e true|false] [-o fmt] [-b num] [-d|-s] [-u usr] [-w pw]\n"++
    "                 [-f qyfile] [query...]\n"++
    "   hquery -g\n"++
    "   hquery -h\n"++
    "   hquery -l\n"++
    "   hquery -m\n"++
    "   hquery -v\n"++
    "   hquery -y\n"++
    "\n"++
    "AVAILABILITY\n"++
    "   Marcus D. Gabriel.  All rights reserved 2014-19(c).\n"++
    "\n"++
    "   marcus@gabriel.name\n"++
    "\n"++
    "   License terms: GNU GPL version 3 (hquery -l)\n"++
    "\n"++
    "DESCRIPTION\n"++
    "   The command hquery with no operands and no options will begin an\n"++
    "   interacitve session with a SciDB server using SciDB's shim\n"++
    "   protocol at http://localhost:8080.\n"++
    "\n"++
    "   With operands, each string will be executed as a SciDB AFL (Array\n"++
    "   Functional Language) query returning a result in SciDB DCSV\n"++
    "   format and fetching only 23 lines per query by default.  If\n"++
    "   stdin is non-empty, it will be execute before the operands.\n"++
    "   SciDB AFL queries are case insensitive and terminated by\n"++
    "   semi-colons (;).\n"++
    "\n"++
    "   Before submitting a query to the shim server, the command\n"++
    "   hquery will\n"++
    "     1. return syntax and parsing errors\n"++
    "     2. report bad nestings, e.g., 'uniq(store(A,B));'\n"++
    "     3. report unknown commands, ignoring them\n"++
    "\n"++
    "   Additionally, the command hquery reports SciDB/shim server\n"++
    "   query exceptions without terminating the process.\n"++
    "\n"++
    "   The command hquery accepts Haskell sytle comments and accepts\n"++
    "   the setting of the following following global variables:\n"++
    "\n"++
    "     -- Number of lines to fetch\n"++
    "     n=Integer;\n"++
    "     -- Format of the output\n"++
    "     format={'csv'|'csv+'|'dcsv'|'dense'|'sparse'|'opaque'\n"++
    "            |'store'|'text'|'tsv'|'tsv+'};\n"++
    "     -- True to fetch data, false to not fetch data\n"++
    "     fetch={true|false};\n"++
    "     -- True to read lines, false to read bytes\n"++
    "     readinglines={true|false};\n"++
    "     -- Prefix to execute before a query by shim, e.g.,\n"++
    "     prefix='a_query(\\'a_string\\');'\n"++
    "\n"++
    "   Prefix is an optional, semi-colon separated, AFL statements\n"++
    "   to precede a query in the same SciDB connection context.\n"++
    "   It is mainly used for SciDB namespace and role  setting.\n"++
    "   There is no terminating semi-colon so trailing semi-colons\n"++
    "   are removed otherwise the prefix is sent to  shim unverified\n"++
    "   and unaltered via the variable 'prefix'.\n"++
    "\n"++
    "   The command 'quit;' or 'exit;' ends the interactive session\n"++
    "   with exit status 0.  The command \"vars;\" displays the interpreter\n"++
    "   variables and values, and the command \"funs;\" displays the\n"++
    "   currently defined interpreter functions and definitions: exit,\n"++
    "   funs, quit, upload, and vars.\n"++
    "\n"++
    "   The upload command takes one argument, a local file path\n"++
    "   string, and returns a shim server-side file path.  It is used\n"++
    "   in queries such as load, input, load_module that take a shim\n"++
    "   server-side file path. Examples are\n"++
    "\n"++
    "     load(m4x4_missing,\n"++
    "       upload('/home/scidb/scidb/m4x4_missing.scidb'));\n"++
    "\n"++
    "     input(m4x4_missing,\n"++
    "       upload('/home/scidb/scidb/m4x4_missing.scidb'));\n"++
    "\n"++
    "     load_module(upload('/home/scidb/scidb/module_1.txt'));\n"++
    "\n"++
    "     join(input(m4x4,upload('/tmp/a\\\\'y\\\\'b.txt')),\n"++
    "       input(m4x4,upload('/tmp/z.txt')));\n"++
    "\n"++
    "   All of the above is case insensitive and terminated with a\n"++
    "   semi-colon (;).\n"++
    "\n"++
    "INTERACTIVE SESSION\n"++
    "   Multi-line queries are supported interactively as they\n"++
    "   are in non-interactive execution.  Ctrl-D at a new prompt\n"++
    "   ends the interactive session.  Ctrl-C returns to the\n"++
    "   interactive prompt, used to end input, especially\n"++
    "   multi-line input.\n"++
    "\n"++
    "   The default prompt is 'True/dcsv/23?', for example,\n"++
    "\n"++
    "     True/dcsv/23? list('arrays');\n"++
    "\n"++
    "   which displays up to 23 lines of arrays, that is,\n"++
    "\n"++
    "     Fetch/Format/NumberOfLines?\n"++
    "\n"++
    "   You can change the global variables 'n', 'format', 'fetch',\n"++
    "   'readinglines', and 'prefix' during execution which changes\n"++
    "   the prompt:\n"++
    "\n"++
    "     True/dcsv/23? n=200;              -- Fetch 200 lines\n"++
    "     True/dcsv/200? n=0;               -- Fetch all lines\n"++
    "     True/dcsv/0? format='csv';        -- Set format to csv\n"++
    "     True/csv/0? fetch=false;          -- Fetch no lines regardless\n"++
    "                                       -- the value of n\n"++
    "     False/csv/0? fetch=true;format=dcsv;n=23;\n"++
    "     True/dcsv/23? readinglines=false; -- Fetch all bytes regardless\n"++
    "                                       -- the value of n\n"++
    "     Bytes/dcsv/23? prefix='set_namespace(\\'sensor_data\\');';\n"++
    "     Bytes/dcsv/23p list('arrays');    -- List sensor_data arrays\n"++
    "     Bytes/dcsv/23p quit;\n"++
    "\n"++
    "   In other words, the prompt indicates the default behaviour for\n"++
    "   the next query executed.\n"++
    "\n"++
    "OPTIONS\n"++
    "   The following options are supported:\n"++
    "\n"++
    "   -a True|false, true to read lines, false to read bytes\n"++
    "      (--reading-lines).  If false, the number of lines to\n"++
    "      fetch (-b) is ignored and the entire output buffer is\n"++
    "      downloaded.  This follows the recommendataion of the\n"++
    "      shim documentation.\n"++
    "\n"++
    "   -b Number, number of lines to fetch (--number).\n"++
    "\n"++
    "   -c Certificate store file, a certificate store to load and use\n"++
    "      for SSL/TLS connections (--certificate-store). The insecure\n"++
    "      option (-n) over-rides the certificate store (-c).\n"++
    "\n"++
    "   -d Use basic digest access authorization, used by either the\n"++
    "      http and https protocols (--digest-authorization).  Digest\n"++
    "      authorization (-d) over-rides SciDB authorization (-s).\n"++
    "\n"++
    "   -e True|false, true to fetch lines, false to not fetch lines\n"++
    "      (--fetch).\n"++
    "\n"++
    "   -f Query file, a readable file of queries.  Multi-line queries\n"++
    "      are supported in a file (--file).  If stdin is non-empty,\n"++
    "      it is executed before the file of queries.  If operands are\n"++
    "      present, they are executed after the file of queries.\n"++
    "\n"++
    "   -g Get and display the SciDB version via shim and then exit\n"++
    "      (--scidb-version).  This performs a simple check for shim \n"++
    "      and SciDB availability.\n"++
    "\n"++
    "   -h Display a short hquery summary (--help).\n"++
    "\n"++
    "   -i IP address or hostname, default localhost (--host).\n"++
    "\n"++
    "   -l Display the hquery license terms (--license).\n"++
    "\n"++
    "   -m Display this internal manual page (--manual).\n"++
    "\n"++
    "   -n Do not use certificate validation, insecure because\n"++
    "      of potential man-in-the-middle attacks (--insecure).\n"++
    "      The insecure option (-n) over-rides the certificate\n"++
    "      store (-c).\n"++
    "\n"++
    "   -o Format, ouput format such as dcsv, csv, csv+, tsv, tsv+,\n"++
    "      etc (--format).\n"++
    "\n"++
    "   -p Port number, default 8080 for protocol (-r) http and default\n"++
    "      8083 for protocol (-r) https (--port).\n"++
    "\n"++
    "   -r Protocol, e.g., http or https, default http (--protocol).\n"++
    "      If the protocol is http, the certificate store (-c) and\n"++
    "      insecure (-n) options are disabled.\n"++
    "\n"++
    "   -s Use SciDB access authorization, requires the https protocol\n"++
    "      (--scidb-authorization).  This option sets the https protocol\n"++
    "      except when digest authorization (-d) over-rides SciDB\n"++
    "      authorization (-s).\n"++
    "\n"++
    "   -t History file, defualt ~/.hquery_history or /tmp/.hquery_history\n"++
    "      if ~ does not exist (--history).\n"++
    "\n"++
    "   -u Username, defualt null (--username).  Used by the digest or\n"++
    "      SciDB authorization option (-d or -s).  Displays an error\n"++
    "      and exits if neither is used with digest or SciDB\n"++
    "      authorization.\n"++
    "\n"++
    "   -v The version of hquery (--version).\n"++
    "\n"++
    "   -w Password, default null (--password).  Used by the digest or\n"++
    "      SciDB authorization option (-d or -s).  Displays an error\n"++
    "      and exits if neither is used with digest or SciDB\n"++
    "      authorization.\n"++
    "\n"++
    "   -y A synopsis of the internal manual page (--synopsis).\n"++
    "\n"++
    "   -x Prefix, a prefix to execute before a query by shim\n"++
    "      (--prefix).  Note that the prefix on the command line\n"++
    "      is verified before being sent to shim.  Prefix is a\n"++
    "      semi-colon separated set of statements.  It is mainly\n"++
    "      used for SciDB namespace and role setting.  There is no\n"++
    "      terminating semi-colon so trailing semi-colons are removed.\n"++
    "\n"++
    "   -V Same as -V1 (--verbose).\n"++
    "  -V0 Quiet.  No information is sent to stderr (--verbose=0).\n"++
    "  -V1 Shows some HTTP exceptions and trace information (--verbose=1).\n"++
    "  -V2 Shows additional URL information (--verbose=2).\n"++
    "\n"++
    "OPERANDS\n"++
    "   SciDB AFL queries.\n"++
    "\n"++
    "USAGE NOTES\n"++
    "   The development of the utility hquery began with SciDB community\n"++
    "   edition 13 and continued with 14, 15, 16, 18, and 19.\n"++
    "\n"++
    "   This version of hquery has been lightly tested with ghc\n"++
    "   version 8.2.2 and 8.6.5 and SciDB 18.1 and 19.3 community edition.\n"++
    "   Currently the command hquery has never been tested on a SciDB\n"++
    "   enterprise edition, and thus it is not known if SciDB\n"++
    "   authorization (-s) or prefix (-x) actually works.\n"++
    "\n"++
    "EXAMPLES\n"++
    "   To list all currently defined arrays with SciDB authorization required,\n"++
    "   use\n"++
    "\n"++
    "     hquery -i coordinator -s -u ScidB -w SciDBPassword \\\n"++
    "         \"n=0; list('arrays');\"\n"++
    "\n"++
    "   To list up to 100 lines of functions with digest authorization required,\n"++
    "   use\n"++
    "\n"++
    "     hquery -i coordinator -d -u Digest -w DigestPassword \\\n"++
    "         \"n=100; list('arrays');\"\n"++
    "\n"++
    "   To list up to 23 lines of operators, 23 being the default, use\n"++
    "\n"++
    "     hquery -i coordinator \"list('operators');\"\n"++
    "\n"++
    "   To list all functions by reading bytes instead of lines, use\n"++
    "\n"++
    "     hquery -i coordinator -a false \"list('functions');\"\n"++
    "\n"++
    "  To create an array A, use\n"++
    "\n"++
    "     hquery -i coordinator \"create array A <x:double> [i=0:99:0:10];\"\n"++
    "\n"++
    "   To execute the file of queries HQTests.qy with no authorization required\n"++
    "   via a TLS connection, use\n"++
    "\n"++
    "     hquery -c ssl_cert.pem  -r https -i coordinator -f HQTest.qy\n"++
    "\n"++
    "   To execute the file of queries HQTests.qy with digest authorization\n"++
    "   required via a TLS connection insecurely, use\n"++
    "\n"++
    "     hquery -n -r https -i coordinator -d -u Digest -w DigestPassword \\\n"++
    "         -f HQTests.qy\n"++
    "\n"++
    "   To list all arrays in the sensor_data namespace, use\n"++
    "\n"++
    "     hquery -i coordinator -b 0 -x \"set_namespace('sensor_data');\" \\\n"++
    "         \"list('arrays');\"\n"++
    "\n"++
    "   To display a synopsis of this internal manual page, use\n"++
    "\n"++
    "     hquery -y\n"++
    "\n"++
    "   To display a summary of usage, use\n"++
    "\n"++
    "     hquery -h\n"++
    "\n"++
    "   To display this internal manual page, use\n"++
    "\n"++
    "     hquery -m\n"++
    "\n"++
    "ENVIRONMENT VARIABLES\n"++
    "   HOME   Home directory for the history file, by defualt\n"++
    "          ~/.hquery_history.\n"++
    "\n"++
    "   PAGER  Page the internal manual page or license terms\n"++
    "          using \"${PAGER}\", otherwise print it.\n"++
    "\n"++
    "EXIT STATUS\n"++
    "   An exit status of 0 is returned if successful, otherwise non-zero\n"++
    "   is returned.\n"++
    "\n"++
    "     EXIT CODE    MEANING\n"++
    "         1        Unknown error.\n"++
    "         2        No authorization specified (-s|-d) with username/password.\n"++
    "         3        Invalid protocol, hostname, port, or history file option.\n"++
    "         4        Cannot connect to hostname:port.\n"++
    "         5        Unreadable query file.\n"++
    "         6        Unreadable certicate store file.\n"++
    "         7        Invalid certificate store.\n"++
    "         8        Network/certificate manager initialization error.\n"++
    "         9        Empty, bad or no digest authentication.\n"++
    "        10        Unauthorized access.\n"++
    "        11        Could not connect to SciDB.\n"++
    "        12        Input syntax error for a SciDB query.\n"++
    "        13        Fetch (-e) or reading-lines (-a) not true or false.\n"++
    "        14        Bad command-line prefix (-x).\n"++
    "   15..255        Unknown error.\n"++
    "\n"++
    "FILES\n"++
    "   ~/.hquery_history\n"++
    "\n"++
    "SEE ALSO\n"++
    "   SciDB, SciDB iquery, SciDB shim at https://www.paradigm4.com/ \n"++
    "   and https://www.paradigm4.com/forum for more information.\n"++
    "\n"++
    "NOTES\n"++
    "   Please send bug reports to marcus@gabriel.name.\n"++
    "\n"++
    "BUGS\n"++
    "   No known bugs to date.\n"
