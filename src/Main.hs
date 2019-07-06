{- | SciDB is a column-oriented database management system (DBMS)
designed for multidimensional data management and analytics common to
scientific, geospatial, industrial, and financial applications.  See
<http://www.paradigm4.com/> and <http://www.paradigm4.com/forum> for
more information about the Data Management and Analytics Software
System (DMAS) known as SciDB.

The documenation of 'main' is the manual page for 'hquery', a Haskell
command line and interactive interpreter for SciDB AFL queries via
shim.  See also

  * 'hquery' @-y@,
  * 'hquery' @-h@, or
  * 'hquery' @-m@

for more information

There is a primitive API derived after the creation of 'hquery'
defined by the following data structures and actions:

  * 'Environment'
  * 'Err'
  * 'mkGlobalManagerEnv'
  * 'runQueries'
  * 'unsafeRunQuery'
  * 'getSciDbVersion'
-}

module Main (main) where


-- Section: imports
import qualified Network.HTTP.Client    as HTTP (Request(..))

import Environment           (Environment(..),defaultEnv,maybePort,Verbosity(..))
import ErrM                  (Err(..))
import HQuery                (hquery,getSciDbVersion,mkGlobalManagerEnv
                             -- Below is added for Haddock documentation
                             ,runQueries,unsafeRunQuery)
import License               (putLicense)
import Manual                (putManual)
import Utils                 (dot,wrap,cleanDoubleQuotes,checkMajorVersion,stripUsing)
import UtilsUnsafe           (valid,VALID(..),fetchUsername,fetchPassword
                             ,isConnectable,managerSettings,base64Sha512Hash)

import Control.Exception     (try)
import Control.Monad         (unless,when)
import Data.Char             (toLower)
import Data.List             (intercalate)
import Data.Maybe            (fromMaybe,isJust,fromJust)
import Network.HTTP.Client   (HttpException(..))
import Safe                  (readMay)
import System.Console.GetOpt (ArgDescr(..),OptDescr(..),ArgOrder(..),usageInfo,getOpt)
import System.Directory      (getCurrentDirectory)
import System.Environment    (getEnvironment,getProgName,getArgs)
import System.Exit           (exitSuccess,exitWith,ExitCode(..))
import System.FilePath       (takeFileName,pathSeparator)
import System.IO             (stderr,stdout,hPutStrLn,Handle(..))

-- Section: main
{- |
The Haskell command line and interactive interpreter manual page
for 'hquery':

> hquery(1)                    User Command                   hquery(1)
> 
> NAME
>    hquery - Haskell query for SciDB via shim
> 
> SYNOPSIS
>    hquery [-V n] [-t hstfile] [-c certstore] [-n]
>              [-r http[s]] [-i host] [-p port] [-a true|false]
>                [-e true|false] [-o fmt] [-b num] [-d|-s] [-u usr] [-w pw]
>                  [-f qyfile] [query...]
>    hquery -g
>    hquery -h
>    hquery -l
>    hquery -m
>    hquery -v
>    hquery -y
> 
> AVAILABILITY
>    Marcus D. Gabriel.  All rights reserved 2014-19(c).
> 
>    marcus@gabriel.name
> 
>    License terms: GNU GPL version 3 (hquery -l)
> 
> DESCRIPTION
>    The command hquery with no operands and no options will begin an
>    interacitve session with a SciDB server using SciDB's shim
>    protocol at http://localhost:8080.
> 
>    With operands, each string will be executed as a SciDB AFL (Array
>    Functional Language) query returning a result in SciDB DCSV
>    format and fetching only 23 lines per query by default.  If
>    stdin is non-empty, it will be execute before the operands.
>    SciDB AFL queries are case insensitive and terminated by
>    semi-colons (;).
> 
>    Before submitting a query to the shim server, the command
>    hquery will
>      1. return syntax and parsing errors
>      2. report bad nestings, e.g., 'uniq(store(A,B));'
>      3. report unknown commands, ignoring them
> 
>    Additionally, the command hquery reports SciDB/shim server
>    query exceptions without terminating the process.
> 
>    The command hquery accepts Haskell sytle comments and accepts
>    the setting of the following following global variables:
> 
>      -- Number of lines to fetch
>      n=Integer;
>      -- Format of the output
>      format={'csv'|'csv+'|'dcsv'|'dense'|'sparse'|'opaque'
>             |'store'|'text'|'tsv'|'tsv+'};
>      -- True to fetch data, false to not fetch data
>      fetch={true|false};
>      -- True to read lines, false to read bytes
>      readinglines={true|false};
>      -- Prefix to execute before a query by shim, e.g.,
>      prefix='a_query(\'a_string\');'
> 
>    Prefix is an optional, semi-colon separated, AFL statements
>    to precede a query in the same SciDB connection context.
>    It is mainly used for SciDB namespace and role  setting.
>    There is no terminating semi-colon so trailing semi-colons
>    are removed otherwise the prefix is sent to  shim unverified
>    and unaltered via the variable 'prefix'.
> 
>    The command 'quit;' or 'exit;' ends the interactive session
>    with exit status 0.  The command "vars;" displays the interpreter
>    variables and values, and the command "funs;" displays the
>    currently defined interpreter functions and definitions: exit,
>    funs, quit, upload, and vars.
> 
>    The upload command takes one argument, a local file path
>    string, and returns a shim server-side file path.  It is used
>    in queries such as load, input, load_module that take a shim
>    server-side file path. Examples are
> 
>      load(m4x4_missing,
>        upload('/home/scidb/scidb/m4x4_missing.scidb'));
> 
>      input(m4x4_missing,
>        upload('/home/scidb/scidb/m4x4_missing.scidb'));
> 
>      load_module(upload('/home/scidb/scidb/module_1.txt'));
> 
>      join(input(m4x4,upload('/tmp/a\\'y\\'b.txt')),
>        input(m4x4,upload('/tmp/z.txt')));
> 
>    All of the above is case insensitive and terminated with a
>    semi-colon (;).
> 
> INTERACTIVE SESSION
>    Multi-line queries are supported interactively as they
>    are in non-interactive execution.  Ctrl-D at a new prompt
>    ends the interactive session.  Ctrl-C returns to the
>    interactive prompt, used to end input, especially
>    multi-line input.
> 
>    The default prompt is 'True/dcsv/23?', for example,
> 
>      True/dcsv/23? list('arrays');
> 
>    which displays up to 23 lines of arrays, that is,
> 
>      Fetch/Format/NumberOfLines?
> 
>    You can change the global variables 'n', 'format', 'fetch',
>    'readinglines', and 'prefix' during execution which changes
>    the prompt:
> 
>      True/dcsv/23? n=200;              -- Fetch 200 lines
>      True/dcsv/200? n=0;               -- Fetch all lines
>      True/dcsv/0? format='csv';        -- Set format to csv
>      True/csv/0? fetch=false;          -- Fetch no lines regardless
>                                        -- the value of n
>      False/csv/0? fetch=true;format=dcsv;n=23;
>      True/dcsv/23? readinglines=false; -- Fetch all bytes regardless
>                                        -- the value of n
>      Bytes/dcsv/23? prefix='set_namespace(\'sensor_data\');';
>      Bytes/dcsv/23p list('arrays');    -- List sensor_data arrays
>      Bytes/dcsv/23p quit;
> 
>    In other words, the prompt indicates the default behaviour for
>    the next query executed.
> 
> OPTIONS
>    The following options are supported:
> 
>    -a True|false, true to read lines, false to read bytes
>       (--reading-lines).  If false, the number of lines to
>       fetch (-b) is ignored and the entire output buffer is
>       downloaded.  This follows the recommendataion of the
>       shim documentation.
> 
>    -b Number, number of lines to fetch (--number).
> 
>    -c Certificate store file, a certificate store to load and use
>       for SSL/TLS connections (--certificate-store). The insecure
>       option (-n) over-rides the certificate store (-c).
> 
>    -d Use basic digest access authorization, used by either the
>       http and https protocols (--digest-authorization).  Digest
>       authorization (-d) over-rides SciDB authorization (-s).
> 
>    -e True|false, true to fetch lines, false to not fetch lines
>       (--fetch).
> 
>    -f Query file, a readable file of queries.  Multi-line queries
>       are supported in a file (--file).  If stdin is non-empty,
>       it is executed before the file of queries.  If operands are
>       present, they are executed after the file of queries.
> 
>    -g Get and display the SciDB version via shim and then exit
>       (--scidb-version).  This performs a simple check for shim 
>       and SciDB availability.
> 
>    -h Display a short hquery summary (--help).
> 
>    -i IP address or hostname, default localhost (--host).
> 
>    -l Display the hquery license terms (--license).
> 
>    -m Display this internal manual page (--manual).
> 
>    -n Do not use certificate validation, insecure because
>       of potential man-in-the-middle attacks (--insecure).
>       The insecure option (-n) over-rides the certificate
>       store (-c).
> 
>    -o Format, ouput format such as dcsv, csv, csv+, tsv, tsv+,
>       etc (--format).
> 
>    -p Port number, default 8080 for protocol (-r) http and default
>       8083 for protocol (-r) https (--port).
> 
>    -r Protocol, e.g., http or https, default http (--protocol).
>       If the protocol is http, the certificate store (-c) and
>       insecure (-n) options are disabled.
> 
>    -s Use SciDB access authorization, requires the https protocol
>       (--scidb-authorization).  This option sets the https protocol
>       except when digest authorization (-d) over-rides SciDB
>       authorization (-s).
> 
>    -t History file, defualt ~/.hquery_history or /tmp/.hquery_history
>       if ~ does not exist (--history).
> 
>    -u Username, defualt null (--username).  Used by the digest or
>       SciDB authorization option (-d or -s).  Displays an error
>       and exits if neither is used with digest or SciDB
>       authorization.
> 
>    -v The version of hquery (--version).
> 
>    -w Password, default null (--password).  Used by the digest or
>       SciDB authorization option (-d or -s).  Displays an error
>       and exits if neither is used with digest or SciDB
>       authorization.
> 
>    -y A synopsis of the internal manual page (--synopsis).
> 
>    -x Prefix, a prefix to execute before a query by shim
>       (--prefix).  Note that the prefix on the command line
>       is verified before being sent to shim.  Prefix is a
>       semi-colon separated set of statements.  It is mainly
>       used for SciDB namespace and role setting.  There is no
>       terminating semi-colon so trailing semi-colons are removed.
> 
>    -V Same as -V1 (--verbose).
>   -V0 Quiet.  No information is sent to stderr (--verbose=0).
>   -V1 Shows some HTTP exceptions and trace information (--verbose=1).
>   -V2 Shows additional URL information (--verbose=2).
> 
> OPERANDS
>    SciDB AFL queries.
> 
> USAGE NOTES
>    The development of the utility hquery began with SciDB community
>    edition 13 and continued with 14, 15, 16, 18, and 19.
> 
>    This version of hquery has been lightly tested with ghc
>    version 8.2.2 and 8.6.5 and SciDB 18.1 and 19.3 community edition.
>    Currently the command hquery has never been tested on a SciDB
>    enterprise >    edition, and thus it is not known if SciDB
>    authorization (-s) or prefix (-x) actually works.
> 
> EXAMPLES
>    To list all currently defined arrays with SciDB authorization required,
>    use
> 
>      hquery -i coordinator -s -u ScidB -w SciDBPassword \
>          "n=0; list('arrays');"
> 
>    To list up to 100 lines of functions with digest authorization required,
>    use
> 
>      hquery -i coordinator -d -u Digest -w DigestPassword \
>          "n=100; list('arrays');"
> 
>    To list up to 23 lines of operators, 23 being the default, use
> 
>      hquery -i coordinator "list('operators');"
> 
>    To list all functions by reading bytes instead of lines, use
> 
>      hquery -i coordinator -a false "list('functions');"
> 
>   To create an array A, use
> 
>      hquery -i coordinator "create array A <x:double> [i=0:99:0:10];"
> 
>    To execute the file of queries HQTests.qy with no authorization required
>    via a TLS connection, use
> 
>      hquery -c ssl_cert.pem  -r https -i coordinator -f HQTest.qy
> 
>    To execute the file of queries HQTests.qy with digest authorization
>    required via a TLS connection insecurely, use
> 
>      hquery -n -r https -i coordinator -d -u Digest -w DigestPassword \
>          -f HQTests.qy
> 
>    To list all arrays in the sensor_data namespace, use
> 
>      hquery -i coordinator -b 0 -x "set_namespace('sensor_data');" \
>          "list('arrays');"
> 
>    To display a synopsis of this internal manual page, use
> 
>      hquery -y
> 
>    To display a summary of usage, use
> 
>      hquery -h
> 
>    To display this internal manual page, use
> 
>      hquery -m
> 
> ENVIRONMENT VARIABLES
>    HOME   Home directory for the history file, by defualt
>           ~/.hquery_history.
> 
>    PAGER  Page the internal manual page or license terms
>           using "${PAGER}", otherwise print it.
> 
> EXIT STATUS
>    An exit status of 0 is returned if successful, otherwise non-zero
>    is returned.
> 
>      EXIT CODE    MEANING
>          1        Unknown error.
>          2        No authorization specified (-s|-d) with username/password.
>          3        Invalid protocol, hostname, port, or history file option.
>          4        Cannot connect to hostname:port.
>          5        Unreadable query file.
>          6        Unreadable certicate store file.
>          7        Invalid certificate store.
>          8        Network/certificate manager initialization error.
>          9        Empty, bad or no digest authentication.
>         10        Unauthorized access.
>         11        Could not connect to SciDB.
>         12        Input syntax error for a SciDB query.
>         13        Fetch (-e) or reading-lines (-a) not true or false.
>         14        Bad command-line prefix (-x).
>    15..255        Unknown error.
> 
> FILES
>    ~/.hquery_history
> 
> SEE ALSO
>    SciDB, SciDB iquery, SciDB shim at https://www.paradigm4.com/ 
>    and https://www.paradigm4.com/forum for more information.
> 
> NOTES
>    Please send bug reports to marcus@gabriel.name.
> 
> BUGS
>    No known bugs to date.
-}
main :: IO ()
main =
  do cmdLine  <- getArgs
     progName <- getProgName
     case getOpt (argOrder config) options cmdLine of
       (opts, args, []) -> runCmd $ foldl updateEnvField env opts
           where env = defaultEnv{command=takeFileName progName,operands=args}
       (_, _, errs) -> error $ concat errs ++ usageInfo (header progName) options


-- Section: summary, help, version
{- |
Given the programe name, returns the program name and the release
version.
-}
versionHeader :: String -> String
versionHeader progName = takeFileName progName ++ " release " ++ release config

{- |
Given the program name, returns a synopis of options.
-}
header :: String -> String
header progName = versionHeader (takeFileName progName) ++
                  "\nUsage: " ++ takeFileName progName ++ flagsOps config

{- |
Puts the program name and its version.
-}
putVersion :: IO ()
putVersion = do progName <- getProgName
                putStrLn (versionHeader progName)
                exitSuccess

{- |
Puts the program name and the synopsis of options to handle (stdout or
stderr) with the supplied the exit code.
-}
putSynopsis :: Handle -> String -> ExitCode -> IO ()
putSynopsis h s e = do progName <- getProgName
                       hPutStrLn h (takeFileName progName ++ ": " ++ s ++ header progName)
                       exitWith e

{- |
Puts the program name and a short summary.
-}
putSummary :: IO ()
putSummary = do progName <- getProgName
                putStrLn (usageInfo (header progName) options)
                exitSuccess


-- Section: configuration
{- |
Configuration information is initial, constant information, that is,
not needing updates.
-}
data Configuration =
    Config {
            argOrder       :: ArgOrder Flag,
            release        :: String,
            flagsOps       :: String
           }

{- |
To be configured once with the versions number and synopsis.  Other
items may of course be added to the configuration as needed.
-}
config :: Configuration
config =
    Config {
            argOrder       = RequireOrder,
            -- Update revision number: toggle case
            release        = "2.8.0.429",
            flagsOps       = " [[-g|-h|-l|-m|-v|-y] |"++
                             "\n                 [-V n] [-t hstfile] [-c certstore] [-n]"++
                             "\n                   [-r http[s]] [-i host] [-p port] [-a true|false]"++
                             "\n                     [-e true|false] [-o fmt] [-b num] [-d|-s] [-u usr] [-w pw]"++
                             "\n                       [-f qyfile] [query...]"++
                             "\n              ]"
           }
    where
    revision =
        let num = flip elem ['0'..'9']
        in  reverse.dropWhile (not.num).reverse.dropWhile (not.num)


-- Section: options
{- |
Flags set as a function of the command line options supplied.
-}
data Flag = Help | License | Manual | Synopsis | Version | Verbose Verbosity
          | Host String | Port String | Protocol String | File FilePath
          | History String | Username String | Password String
          | Fetch String | Format String | Number String | VersionSciDb
          | DigestAuth | SciDbAuth | CertStore String | Insecure
          | ReadingLines String | Prefix String

{- |
Options that can be given on the command line.
-}
options :: [OptDescr Flag]
options =
    [ Option "a" ["reading-lines"](ReqArg ReadingLines "TRUE|FALSE") "Reading lines: true lines or false bytes."
    , Option "b" ["number"]   (ReqArg Number "NUMBER")     "Number of lines to fetch when possible."
    , Option "c" ["certificate-store"] (ReqArg CertStore "CERTSTORE") "Load and use certificate store."
    , Option "d" ["digest-authorization"] (NoArg DigestAuth) "Use basic digest authorization."
    , Option "e" ["fetch"]    (ReqArg Fetch "TRUE|FALSE")  "Fetch lines: true or false."
    , Option "f" ["file"]     (ReqArg File "FILE")         "File of queries FILE."
    , Option "g" ["scidb-version"] (NoArg VersionSciDb)    "Get and display the SciDB version via shim."
    , Option "h" ["help"]     (NoArg Help)                 "Help summary."
    , Option "i" ["host"]     (ReqArg Host "HOSTNAME")     "Hostname or IP address HOSTNAME."
    , Option "l" ["license"]  (NoArg License)              "License terms."
    , Option "m" ["manual"]   (NoArg Manual)               "Manual page."
    , Option "n" ["insecure"] (NoArg Insecure)             "Do not use certificate validation (\"insecure\")."
    , Option "o" ["format"]   (ReqArg Format "FORMAT")     "Format dcsv, csv, csv+, tsv, tsv+, etc."
    , Option "p" ["port"]     (ReqArg Port "PORT")         "Port number PORT."
    , Option "r" ["protocol"] (ReqArg Protocol "PROTOCOL") "Protocol HTTP or HTTPS."
    , Option "s" ["scidb-authorization"] (NoArg SciDbAuth) "Use SciDB authorization."
    , Option "t" ["history"]  (ReqArg History "HISTORY")   "History file HISTORY."
    , Option "u" ["username"] (ReqArg Username "USERNAME") "Username USERNAME."
    , Option "v" ["version"]  (NoArg Version)              "Version of this command."
    , Option "w" ["password"] (ReqArg Password "PASSWORD") "Password PASWORD."
    , Option "y" ["synopsis"] (NoArg Synopsis)             "Help synopsis."
    , Option "x" ["prefix"]   (ReqArg Prefix "PREFIX")     "Prefix to execute before a query by shim."
    , Option "V" ["verbose"]  (OptArg inpver "012")        "Verbose information to stderr."
    ]
    where inpver :: Maybe String -> Flag
          inpver ms = Verbose $ case (fromMaybe (-1) . readMay . fromMaybe "1") ms of
                                  0 -> Verbose0
                                  1 -> Verbose1
                                  2 -> Verbose2
                                  _ -> VerboseDef

-- Section: updating a field in the environment
{- |
Every option, and only an option, must be able to be updated here.
-}
updateEnvField :: Environment -> Flag -> Environment
updateEnvField env opt =
    case opt of
      Host s     -> env{host      =      s}
      Port s     -> env{port      = Just s}
      Protocol s -> env{protocol  =      s}
      CertStore s-> env{certStore =      s}
      Insecure   -> env{insecure  =   True}
      DigestAuth -> env{digestAuth=   True}
      SciDbAuth  -> env{scidbAuth =   True}
      Fetch s    -> env{defFetch  = tOrF s}
      ReadingLines s->env{defReadingLines=tOrF s}
      Prefix s   -> env{defPrefix =xsemi s}
      Format s   -> env{defFormat =      s}
      Number s   -> env{defNumber =      s}
      Username s -> env{username  =      s}
      Password s -> env{password  =      s}
      History s  -> env{history   =      s}
      File s     -> env{file      =      s}
      Help       -> env{help      =   True}
      License    -> env{license   =   True}
      Manual     -> env{manual    =   True}
      Synopsis   -> env{synopsis  =   True}
      Version    -> env{version   =   True}
      VersionSciDb->env{versionSciDb= True}
      Verbose v  -> env{verbose   =      v}
    where tOrF s = case fmap toLower s of
                     "true"  -> Just True
                     "false" -> Just False
                     _       -> Nothing
          xsemi  = stripUsing (==';') 
--    _          -> error (command env ++ ": updateEnvField: unknown error")

-- Section: running the command given the environment
{- |
Update the environment with updateEnv, verify the environment with
verifyEnv, resolve option conflicts with resolveEnv, and run the
command given the environemnt with runWith which does the actual work,
usually imported from a module.
-}
runCmd :: Environment -> IO ()
runCmd env
    | synopsis env = putSynopsis stdout "\r" ExitSuccess
    | help    env  = putSummary
    | version env  = putVersion
    | manual  env  = putManual
    | license env  = putLicense
    | otherwise    =
      do env'  <- fmap resolveEnv (updateEnv0 env)
         verifyEnv0 env'
         env'' <- updateEnv1 env'
         verifyEnv1 env''
         when (versionSciDb env'') (putStrLn ("SciDB version "++sciDbVersion env'') >> exitSuccess)
         runWith env''

{- |
'updateEnv' updates the environment ('Environment') from the universe,
the external environment.
-}
updateEnv0, updateEnv1 :: Environment -> IO Environment
-- Usage order is important in runCmd, so please do not change it without study
updateEnv0 env= do let b_ = digestAuth env || scidbAuth env
                   when ((not.null.username) env || (not.null.password) env) $
                        unless b_ (putSynopsis stderr
                                   "\rNo authorization (-s|-d) specified with a username/password.\n"
                                   (ExitFailure 2)
                                  )
                   cd    <- getCurrentDirectory
                   es    <- getEnvironment
                   let pd = lookup "PWD" es
                   let hm = lookup "HOME" es
                   let e1 = maybe env{pwd =  cd} (\pd -> env{pwd  = pd}) pd
                   let e2 = maybe e1 (\hm ->  e1{home = hm}) hm
                   return e2

updateEnv1 env =
    do env' <- try (updateEnv1' env) :: IO (Either HttpException Environment)
       case env' of
         Left e      -> do case e of
                             HttpExceptionRequest _ ct -> putSynopsis stderr ("\r"++mk show ct++"\n") (ExitFailure 8)
                             InvalidUrlException s1 s2 -> putSynopsis stderr ("\r"++mk id ("Invalid URL: "++s1++": "++s2++"\n")) (ExitFailure 8)
                           undefined -- this point is never reached
         Right env'' -> fetchUsernamePassword env''
    where
      mk f s = intercalate "\n" $ wrap 80 $ cleanDoubleQuotes $ f s
      updateEnv1' env = do menv <- mkGlobalManagerEnv env
                           maybe (do hPutStrLn stderr ("Invalid certificate store: " ++ certStore env)
                                     putSynopsis stderr "\r" (ExitFailure 7)
                                     undefined
                                 ) return menv

{- | Given the 'Environment' from the command line, fetch from the user
the username and/or password interactively when required.
-}
fetchUsernamePassword :: Environment -> IO Environment
fetchUsernamePassword e = if digestAuth e || scidbAuth e
                          then do un <- fetch username (fetchUsername "")
                                  pw <- encode <$> fetch password (fetchPassword Nothing "")
                                  return e{username = un, password = pw}
                          else return e
    where fetch f g = if null (f e) then g else return (f e)
          encode = if scidbAuth e && checkMajorVersion (18>=) (sciDbVersion e)
                   then base64Sha512Hash else id

{- |
'verifyEnv' verifies the environment created from the command line
options and the supplied operands.
-}
verifyEnv0, verifyEnv1 :: Environment -> IO ()
verifyEnv0 env = do b0 <- valid HOSTNAME (host      env)
                    b1 <- valid PORT     (maybePort env)
                    b2 <- valid PROTOCOL (protocol  env)
                    b3 <- valid HISTORY  (history   env)
                    unless (b0 && b1 && b2 && b3) (putSynopsis stderr "\r" (ExitFailure 3))
                    b4 <- isConnectable (host env) (maybePort env)
                    unless b4 (putSynopsis stderr "\r" (ExitFailure 4))
                    b5 <- if null (file env) then return True else valid READABLE (file env)
                    unless b5 (putSynopsis stderr "\r" (ExitFailure 5))
                    b6 <-if null (certStore env) then return True else valid READABLE (certStore env)
                    unless b6 (putSynopsis stderr "\r" (ExitFailure 6))
                    let b_ = isJust (defFetch env) && isJust (defReadingLines env)
                    unless b_ (putSynopsis stderr "\r" (ExitFailure 13))
                    b' <- valid PREFIX (defPrefix env)
                    unless b' (putSynopsis stderr "\r" (ExitFailure 14))

verifyEnv1 env =
    do let b7 = (not . null) (digestHeaders env)
       when (digestAuth env) $
            unless b7 (putSynopsis stderr
                       "\rEmpty, bad or no digest authentication\n"
                       (ExitFailure 9)
                      )

{- |
'resolveEnv' resolves the environment created from the command line
options and the supplied operands, e.g., if an option over-rides
another, 'resolveEnv' takes this into account.
-}
resolveEnv :: Environment -> Environment
resolveEnv = resolveEnv5.resolveEnv4.resolveEnv3.resolveEnv2.resolveEnv1.resolveEnv0
    -- Order is important, so please do not change it without study
    where
      resolveEnv0 env = if digestAuth env then env{scidbAuth=False} else env
      resolveEnv1 env = if digestAuth env || scidbAuth env
                         then env else env{username = "", password = ""}
      resolveEnv2 env = if scidbAuth env then env{protocol="https"} else env
      resolveEnv3 env = if insecure env then env{certStore = ""} else env
      resolveEnv4 env = if protocol env == "http" then env{certStore = "",insecure=False} else env
      resolveEnv5 env = if history env == "" then env{history = history_file env} else env
          where history_file env = home env ++ [pathSeparator] ++ dot ++ command env ++ "_history"

{- |
'runWith' this environment.  'runWith' is the interface between the command
line options plus operands and the real work. 'runWith' is 'hquery'.
-}
runWith :: Environment -> IO ()
runWith = hquery
