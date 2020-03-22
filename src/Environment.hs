
module Environment(Environment(..), defaultEnv, maybePort,Verbosity(..)) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Header (RequestHeaders)

data Verbosity = Verbose0   -- ^ Verbosity level 0, no information to stderr
               | VerboseDef -- ^ Default verbosity, only relevant HTTP exceptions to stderr
               | Verbose1   -- ^ Verbosity level 1, additional HTTP exceptions and trace information to stderr
               | Verbose2   -- ^ Verbosity level 2, additional URL information to stderr
                 deriving (Show,Eq,Ord)

{- | Given an 'Environment', return a port or a default port.
-}
maybePort :: Environment -> String
maybePort e = fromMaybe port' (port e)
    where port' = (if protocol e == "https" then secPort else defPort) e

-- Section: Environment
{- |
The 'Environment' contains the command name, operands, defaults, and the
result of every option amongst other entries.  It is set by 'main' and
otherwise to be used as a read-only 'Environment'.
-}
data Environment =
    Env { command     :: String       -- ^ Program name
        , pwd         :: String       -- ^ Present Working Directory (PWD)
        , home        :: String       -- ^ Home directory or /tmp
        , history     :: String       -- ^ History file for command
        , protocol    :: String       -- ^ Protoocl http or https
        , certStore   :: String       -- ^ Load and use certificate store
        , insecure    :: Bool         -- ^ Use "insecure" SSL/TLS, i.e., no cert validation
        , scidbAuth   :: Bool         -- ^ Use SciDB authentication
        , digestAuth  :: Bool         -- ^ Use basic digest access authentication
        , digestHeaders::RequestHeaders-- ^ Digest request headers or nothing for the GET method
        , defFetch    :: Maybe Bool   -- ^ True fetch lines, false do not fetch lines
        , defFormat   :: String       -- ^ Format dcsv, csv, csv+, etc.
        , defNumber   :: String       -- ^ Number of lines to fetch, n=0 for all; default for bad number
        , defReadingLines::Maybe Bool -- ^ True to read lines from shim otheriwise read bytes
        , defPrefix   :: String       -- ^ Prefix to execute before a query by shim
        , host        :: String       -- ^ Hostname or IP address
        , port        :: Maybe String -- ^ Port number
        , defPort     :: String       -- ^ Default insecure or unencypted port number
        , secPort     :: String       -- ^ Default secure or encypted port number
        , username    :: String       -- ^ Username
        , password    :: String       -- ^ Password for username
        , operands    :: [String]     -- ^ List of queries
        , file        :: String       -- ^ File of queries
        , waitOnStdIn :: Bool         -- ^ Wait on stdin
        , help        :: Bool         -- ^ @-h@, display help summary
        , license     :: Bool         -- ^ @-l@, display license terms
        , manual      :: Bool         -- ^ @-m@, display internal man page
        , version     :: Bool         -- ^ @-v@, display version
        , synopsis    :: Bool         -- ^ @-y@, display manual synopsis
        , verbose     :: Verbosity    -- ^ @-V|-V[012]@, display verbose information to stderr
        , sciDbVersion:: String       -- ^ Store SciDB version, e.g., 18.1.0
        , versionSciDb:: Bool         -- ^ True to display SciDB version via shim
        } deriving Show

-- The default Environment
{- |
The defualt values, otherwise updated from the flags supplied by the
command line options.  The default program name is 'hquery'.
-}
defaultEnv :: Environment
defaultEnv =
    Env { command     = "hquery"                -- Default program name
        , pwd         = ""                      -- No PWD by default
        , home        = "/tmp"                  -- /tmp if $HOME does not exist
        , history     = ""                      -- History file for command
        , protocol    = "http"                  -- No authentication needed for http
        , certStore   = ""                      -- Load and use certificate store
        , insecure    = False                   -- Use "insecure" SSL/TLS, i.e., no cert validation
        , scidbAuth   = False                   -- Use SciDB authentication
        , digestAuth  = False                   -- Use basic digest access authentication
        , digestHeaders=[]                      -- Digest request headers or nothing for the GET method
        , defFetch    = Just True               -- True fetch lines, false do not fetch lines
        , defFormat   = "dcsv"                  -- Format dcsv, csv, csv+, etc.
        , defNumber   = "23"                    -- Default 23 lines, default for bad number
        , defReadingLines=Just True             -- True to read lines from shim otheriwise read bytes
        , defPrefix   = ""                      -- Prefix to execute before a query by shim
        , host        = "localhost"             -- Use local shim connection
        , port        = Nothing                 -- Nothing for default port number not needing authentication
        , defPort     = "8080"                  -- Default port number not needing authentication
        , secPort     = "8083"                  -- Default port number needing authentication
        , username    = ""                      -- Username needed for -d or -s option
        , password    = ""                      -- Password needed for -d or -s option
        , operands    = []                      -- No operands by default, i.e., interactive mode
        , file        = ""                      -- No file of queries by default
        , waitOnStdIn = False                   -- Wait on stdin
        , help        = False                   -- False or off by default
        , license     = False                   -- False or off by default
        , manual      = False                   -- False or off by default
        , synopsis    = False                   -- False or off by default
        , version     = False                   -- False or off by default
        , verbose     = VerboseDef              -- Default verbosity
        , sciDbVersion= ""                      -- Store SciDB version, e.g., 18.1.0
        , versionSciDb= False                   -- True to display SciDB version via shim
        }
