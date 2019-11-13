{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase #-}

{- |
There is a primitive API derived after the creation of 'hquery'
defined by the following data structures and actions:

    * 'Environment', the API only uses the following fields:

        * 'certStore'
        * 'defFetch'
        * 'defFormat'
        * 'defNumber'
        * 'defPrefix'
        * 'defReadingLines'
        * 'digestAuth'
        * 'host'
        * 'insecure'
        * 'password'
        * 'port'
        * 'protocol'
        * 'scidbAuth'
        * 'username'
        * 'verbose'

    * 'Err'
    * 'mkGlobalManagerEnv'
    * 'runQueries'
    * 'unsafeRunQuery'
    * 'getSciDbVersion'
-}

module HQuery (hquery
              ,getSciDbVersion
              ,mkGlobalManagerEnv
              ,runQueries
              ,unsafeRunQuery
              ) where

import Control.Monad                          (unless,when)
import Control.Monad.Catch                    (try,MonadCatch,MonadThrow)
import Control.Monad.IO.Class                 (MonadIO)
import Control.Monad.Reader                   (MonadReader,ReaderT,runReaderT,ask)
import Control.Monad.State                    (StateT,evalStateT,liftIO,get,put)
import Control.Monad.State.Class              (MonadState)
import Control.Monad.Trans                    (lift)
import Data.Char                              (toLower)
import Data.List                              (intercalate)
import Data.Maybe                             (isNothing,isJust,fromJust,fromMaybe)
import Data.X509.CertificateStore             (readCertificateStore)
import Network.Socket                         (withSocketsDo)
import Network.Connection                     (TLSSettings(..))
import Network.HTTP.Base                      (urlEncode,urlDecode)
import Network.HTTP.Client                    (defaultManagerSettings,requestHeaders,defaultRequest
                                              ,RequestBody,streamFile,requestBody,method,Request)
import Network.HTTP.Client.TLS                (getGlobalManager,setGlobalManager,applyDigestAuth)
import Network.HTTP.Simple                    (parseRequestThrow,httpLBS,setRequestManager)
import Network.HTTP.Conduit                   (HttpException(..),HttpExceptionContent(..),Response(..)
                                              ,managerResponseTimeout,responseTimeoutNone,newManager
                                              ,mkManagerSettings)
import Network.HTTP.Types.Status              (Status(..))
import Safe                                   (tailSafe,headMay)
import System.Console.Haskeline               (runInputT,defaultSettings,getInputLine,historyFile
                                              ,withInterrupt,handleInterrupt,outputStrLn)
import System.Console.Terminal.Size           (Window(..),size)
import System.Exit                            (exitWith,ExitCode(..))
import System.IO                              (stderr,stdin,hPutStrLn,hReady,getContents)

import qualified Data.Text.Lazy.Encoding as L (decodeUtf8)
import qualified Data.Text.Lazy          as L (unpack)
import qualified Data.ByteString.Char8   as C (pack,unpack)
import qualified Network.HTTP.Client     as H (Request(..))

import Environment                            (Environment(certStore,defFetch,defFormat,defNumber
                                                          ,defReadingLines,defPrefix,digestAuth
                                                          ,digestHeaders,file,history,host,insecure
                                                          ,operands,password,port,protocol,sciDbVersion
                                                          ,scidbAuth,username,verbose)
                                               ,defaultEnv,maybePort,Verbosity(..))
import ErrM                                   (Err(..))
import Interpreter                            (Results(..),interpret)
import Utils                                  (strip,wrap,nolines,cleanDoubleQuotes,replace,toSingleQuotedStr
                                              ,escapeSingleQuotes,toSingleQuotedStr,checkMajorVersion)
import UtilsUnsafe                            (managerSettings,valid,VALID(..))

-- | The 'Param' is a structure of parameters that can change during
-- execution. For example, see 'executeCommand'.  When 'readingLines'
-- is false, the value of ''number' is ignored and the entire output
-- buffer is downloaded.  This follows the recommendataion of the shim
-- documentation.

data Param =
    Param {session     :: String   -- ^ The current session id, a number
          ,number      :: String   -- ^ Number of lines to display
          ,fetch       :: Bool     -- ^ Fetch lines yes (true) or no (false)
          ,format      :: String   -- ^ Available base_format values are: csv, text, ...
                                   --     *** + becomes %2B ***
                                   --     csv     - Comma separated values
                                   --     csv+    - Comma separated values including coordinates
                                   --     dcsv    - "Display CSV", a more human-readable CSV variant
                                   --     dense   - A variant of 'text' suitable for dense data
                                   --     sparse  - A variant of 'text' suitable for sparse data
                                   --     opaque  - SciDB raw storage format
                                   --     store   - A variant of 'text' that includes overlap regions
                                   --     text    - SciDB native text format
                                   --     tsv     - Tab separated values (LinearTSV dialect)
                                   --     tsv+    - Tab separated values including coordinates (LinearTSV dialect)
          ,readingLines:: Bool     -- ^ True for path /read_lines, the default, and false for /read_bytes
          ,prefix      :: String   -- ^ An optional semi-colon separated, URL encoded, AFL statements to precede
                                   -- a query in the same SciDB  connection context. Mainly used for SciDB
                                   -- namespace and role setting.  There is no terminating semi-colon so
                                   -- trailing semi-colons are removed.
          ,fileBody    :: Maybe RequestBody -- ^ Just send a file as a request body otherwise Nothing
          }

defaultParam = Param {session = ""
                     ,number       = defNumber defaultEnv
                     ,fetch        = fromJust $ defFetch defaultEnv
                     ,format       = defFormat defaultEnv
                     ,readingLines = fromJust $ defReadingLines defaultEnv
                     ,prefix       = ""
                     ,fileBody     = Nothing
                     }

initParam :: StateT Param (ReaderT Environment IO) ()
initParam = do e <- ask
               p <- get
               put p{number       = defNumber e
                    ,fetch        = fromJust $ defFetch e
                    ,format       = defFormat e
                    ,readingLines = fromJust $ defReadingLines e
                    ,prefix       = defPrefix e
                    }

-- | Get the version of SciDB, e.g., 18.1.0, via shim given an 'Environment'.

getSciDbVersion :: Environment -> IO String
getSciDbVersion e = withSocketsDo (runReaderT (evalStateT fetchVersion defaultParam{number="0"}) e)
    where
      fetchVersion = do e <- ask
                        let url = urlPrefix e ++ "/version"
                        fetchURL (verbose e) url

-- | Given an 'Environment', make and set the global network manager,
-- check for digest authorization and the SciDB version, and update
-- the 'Environment'.  Either return 'Just' 'Environment' or 'Nothing'
-- in the IO monad in the case of an invalid certificate store when
-- the protocol is HTTPS and insecure validation is not used.  Be
-- careful: no resolving or verification of the 'Environment' is
-- performed.  When 'mkGlobalManagerEnv' returns 'Just' 'Environment',
-- this is the initialized 'Environment' to be used with the actions
-- 'runQueries' and 'unsafeRunQuery' and the function 'getSciDbVersion'.

mkGlobalManagerEnv :: Environment -> IO (Maybe Environment)
mkGlobalManagerEnv env =
    do mstore  <- readCertificateStore (certStore env)
       let defSettings = defaultManagerSettings
           tlsSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
           caStore     = fromJust mstore
           caSettings  = managerSettings caStore
       s <- if protocol env /= "https"
            then return $ Just defSettings
            else if insecure env
                 then return $ Just tlsSettings
                 else if isJust mstore
                      then return $ Just caSettings
                      else return Nothing
       if isNothing s
       then return Nothing
       else do let s' = fromJust s
               manager <- newManager s'{managerResponseTimeout=responseTimeoutNone}
               liftIO $ setGlobalManager manager
               request <- maybeSetDigestAuth' env -- Used to throw HTTP exceptions early
               let env' = env{digestHeaders = maybe [] requestHeaders request}
               if digestAuth env
               then do v <- getSciDbVersion env'
                       return $ Just env'{sciDbVersion=v}
               else do v <- getSciDbVersion env
                       return $ Just env{sciDbVersion=v}
    where
      maybeSetDigestAuth' =
          runReaderT (do env <- ask
                         maybeSetDigestAuth defaultRequest{H.host   = C.pack $ host env
                                                          ,H.port   = read $ maybePort env :: Int
                                                          ,H.secure = protocol env == "https"
                                                          }
                     )

maybeSetDigestAuth :: (MonadReader Environment m, MonadIO m, MonadThrow n) => Request -> m (n Request)
maybeSetDigestAuth r =
    do e <- ask
       m <- liftIO getGlobalManager
       let user = C.pack $ username e
           pass = C.pack $ password e
       if digestAuth e
       then liftIO $ applyDigestAuth user pass r{H.secure = protocol e == "https"} m
       else return $ return r

-- | Given an 'Environment', run a string that is one SciDB AFL
-- query without terminating semicolon (;).  The string is sent to the
-- shim server without modification or verification.  Fetch
-- 'defNumber' lines (23 by default); an exception occurs when
-- attempting to fetch no returned lines.  If fetch is false, do not
-- fetch any lines. For exmample, fetch should be set to false for
-- store, create and load.  See 'interpretOperatorId' in
-- Interpreter.hs for details.

unsafeRunQuery :: Environment -> String -> IO String
unsafeRunQuery e s =
    withSocketsDo (runReaderT (evalStateT _executeQuery defaultParam) e)
    where _executeQuery = do initParam
                             p <- get
                             executeAndGet (fetch p) s

-- | Given an 'Environment', run a string of semi-colon (;)
-- separated SciDB AFL queries and fetch 'defNumber' lines (23 by
-- default) from the result of each query if the query returns a
-- result. Like 'hquery', 'runQueries' determines if the SciDB
-- operator returns lines or not via 'interpretOperatorId' in
-- Interpreter.hs.

runQueries :: Environment -> String -> IO [Err String]
runQueries e s =
    withSocketsDo (runReaderT (evalStateT (executeQueries s) defaultParam) e)

executeQueries :: String -> StateT Param (ReaderT Environment IO) [Err String]
executeQueries s =
    do initParam
       case interpret s of
         Ok rs  -> filter (\case Ok "" -> False; _ -> True) <$> mapM executeResult rs
         Bad s' -> return [Bad s']

data ExitOrDont = ExitOnBad | DontExitOnBad deriving Eq

-- | Given an 'Environment', either run the given file of queries and
-- the operands on the command line as queries or run queries interactively.

hquery :: Environment -> IO ()
hquery e = withSocketsDo (runReaderT (evalStateT _hquery defaultParam) e)
    where _hquery =
              do initParam
                 e <- ask
                 r <- liftIO $ hReady stdin
                 if null (operands e) && null (file e) && not r
                 then do liftIO $ putStrLn ("SciDB version "++sciDbVersion e)
                         executeQuery DontExitOnBad "list('instances');"
                         iQuery
                 else do c <- liftIO $ if r then getContents else return ""
                         f <- liftIO $ (if null (file e) then return else readFile) (file e)
                         let a = intercalate "\n" ("":(nolines <$> operands e))
                         executeQuery ExitOnBad (c ++ f ++ a)
          iQuery =
              do e <- ask
                 p <- get
                 liftIO $ runInputT defaultSettings{historyFile = Just (history e)} (loop e p)
          loop e p = withInterrupt (_loop p)
              where
                _loop p = handleInterrupt (outputStrLn "^C" >> _loop p) $
                    do minput <- fmap strip <$> getInput (prompt p ++ " ")
                       case minput of
                         Nothing     -> return ()
                         Just ""     -> _loop p
                         Just q      -> do p' <- lift $ runReaderT (evalStateT (do executeQuery DontExitOnBad q
                                                                                   get
                                                                               ) p) e
                                           _loop p'
          getInput p = getInput' p False ""
              where getInput' p insideQuote q =
                        do mi <- getInputLine p
                           case mi of
                             Nothing -> return Nothing
                             Just "" -> return $ Just ""
                             Just l  -> processInput insideQuote q l
                    processInput insideQuote q []     = getInput' "Con> " insideQuote (q++"\n")
                    processInput insideQuote q (c:cs) =
                        case c of
                          '\\' -> case headMay cs of
                                    Nothing -> getInput' "Con> " insideQuote (q++[c,'\n'])
                                    Just hd -> processInput insideQuote (q++[c,hd]) (tailSafe cs)
                          '\'' -> processInput (toggle insideQuote) (q++[c]) cs where toggle=not
                          ';' | insideQuote     -> processInput insideQuote (q ++ [c]) cs
                              | null (strip cs) -> return $ Just (q ++ [c] ++ cs ++ "\n")
                              | otherwise       -> processInput insideQuote (q ++ [c]) cs
                          _    -> processInput insideQuote (q++[c]) cs
          prompt param = (if not (readingLines param) && fetch param then "Bytes" else show (fetch param))
                         ++"/"++format param++"/"++number param
                         ++(if null (prefix param) then "?" else "p")
          executeQuery f s =
              case interpret s of
                Ok rs  -> let n    = length rs
                              bads = findBads rs
                          in if null bads
                             then mapM_ executeResult' rs
                             else liftIO $
                                  mapM_ (\(i,r) -> putStderr ("Query "++show i++" of "++show n++": ") (show r))
                                  bads
                Bad s -> liftIO (putStderr "Input " s >> if f == DontExitOnBad then return () else exit 12)
          findBads rs = filter (\(_,r) -> case r of BadNesting _ -> True ; _ -> False) $ zip [1..] rs
          executeResult' r = do err <- executeResult r
                                liftIO $ case err of
                                           Bad s -> putStderr "Input " s
                                           Ok s  -> putStr s

executeResult :: Results -> StateT Param (ReaderT Environment IO) (Err String)
executeResult r =
    case r of
      BadNesting s -> return $ Bad ("bad nesting " ++ toSingleQuotedStr s)
      No s         -> do executeAndGet False s
                         return $ Ok ""
      Yes s        -> do p <- get
                         if fetch p
                         then Ok <$> executeAndGet True s
                         else executeResult (No s)
      Upload (q,ss)-> executeUpload q ss
      Unknown s    -> executeResult (Yes s)
      _            -> do executeCommand r
                         return $ Ok ""
    where
      executeUpload s []      = do let q = s++";" -- Trailing semi-colon (;) needed by executeQueries
                                   (intercalate "\n" <$>) . sequence <$> executeQueries q
      executeUpload s (p:ps) =
          do let fp     = fst p
                 needle = snd p
             b <- liftIO $ valid READABLE fp
             if b
             then do fetchSession
                     putVerbose "filepath to upload=" fp
                     u <- toSingleQuotedStr <$> uploadFile fp
                     putVerbose "uploaded filepath=" u
                     p <- get
                     err <- executeUpload (replace needle u s) ps
                     put p
                     releaseSession
                     return err
             else return $ Bad ("Bad filepath '"++escapeSingleQuotes fp++"'")
      uploadFile fp = do e  <- ask
                         p  <- get
                         rb <- liftIO $ streamFile fp
                         put p{fileBody=Just rb}
                         let url = urlPrefix e ++ "/upload?id=" ++ session p
                         fetchURL (verbose e) url
      executeCommand :: (MonadIO m, MonadState Param m) => Results -> m ()
      executeCommand r =
          case r of
            Command s -> case fmap toLower s of
                           "quit" -> exit 0
                           "exit" -> exit 0
                           "funs" -> liftIO $ putStrLn (    "exit   - exit or quit interpreter"
                                                       ++ "\nfuns   - list interpreter functions"
                                                       ++ "\nquit   - quit or exit interpreter"
                                                       ++ "\nupload - upload a file and run a query using it"
                                                       ++ "\nvars   - list interpreter variables and values"
                                                       )
                           "vars" -> do p <- get
                                        liftIO $ putStrLn (    "fetch        = " ++ show (fetch p)
                                                          ++ "\nreadinglines = " ++ show (readingLines p)
                                                          ++ "\nformat       = " ++ format p
                                                          ++ "\nnumber       = " ++ number p
                                                          ++ "\nprefix       = " ++ prefix p
                                                          )
                           _      -> liftIO $ putStderr "Ignored unknown command: " s
            Fetch mb  -> unless (isNothing mb) $ do p <- get
                                                    put $ p{fetch=fromJust mb}
            Format ms -> unless (isNothing ms) $ do p <- get
                                                    put $ p{format=fromJust ms}
            Lines mi  -> unless (isNothing mi) $ do p <- get
                                                    put $ p{number=show$fromJust mi}
            ReadingLines mb -> unless (isNothing mb) $ do p <- get
                                                          put p{readingLines=fromJust mb}
            Prefix s  -> do p <- get
                            put p{prefix=s}

executeAndGet :: Bool -> String -> StateT Param (ReaderT Environment IO) String
executeAndGet f s =
    do fetchSession
       e <- ask
       p <- get
       let url = urlPrefix e ++ "/execute_query?id=" ++ session p ++ addPrefix p ++ "&query=" ++ urlEncode s
                 ++ (if f then "&save=" ++ urlEncode (format p) else "") ++ addAuthCode e
       putVerbose "execute_query=" s
       fetchURL (verbose e) url
       d <- if f then readData else return ""
       releaseSession
       return d
    where
      addPrefix p = if null (prefix p) then "" else "&prefix=" ++ prefix p
      readData  =
            do e <- ask
               p <- get
               let url = urlPrefix e ++ (if readingLines p then "/read_lines" else "/read_bytes")
                         ++ "?id=" ++ session p
                         ++ "&n=" ++ (if readingLines p then number p else "0")
                   v   = quietIfDef (verbose e)
               fetchURL v url

fetchURL :: (MonadReader Environment m, MonadState Param m, MonadIO m, MonadCatch m)
            => Verbosity -> String -> m String
fetchURL v s =
               do env <- ask
                  r <- try (continueHttp s)
                  case r of 
                    Left  e -> do let prefix = "SciDB " ++ sciDbVersion env ++ " URL exception\n    "
                                  when (v > Verbose0) $ liftIO $ putHttpException prefix s e
                                  return ""
                    Right b -> do when (v > Verbose1) $ liftIO $ putStderr "URL=" s
                                  return $ L.unpack $ L.decodeUtf8 b
    where continueHttp s =
              do e <- ask
                 p <- get
                 request <- parseRequestThrow s
                 let request'=request{requestHeaders=digestHeaders e++requestHeaders request}
                 response <- if isNothing (fileBody p)
                             then httpLBS request'
                             else do -- The digest for the POST method apparently depends on the requestBody(?)
                                     mrequest <- maybeSetDigestAuth request{method="POST",requestBody=fromJust$fileBody p}
                                     httpLBS $ fromMaybe request' mrequest
                 return (responseBody response)
          putHttpException prefix s e =
              do putStderr prefix s
                 putStderr (tailSafe $ dropWhile (/='\n') prefix) (urlDecode s)
                 case e of
                   InvalidUrlException url reason -> putStderr (url ++ ": ") reason
                   HttpExceptionRequest req e ->
                       do w <- maybe consoleWidth width <$> size
                          case e of
                            StatusCodeException responseBody s ->
                                do let status = responseStatus responseBody
                                   putStderr "HTTP code     = " $ show $ statusCode status
                                   let m = statusMessage status
                                   putStderr "     message  = " $ C.unpack m
                                   putStderr response_          $ format w i0 $ C.unpack s
                                   when (unauthorized == m) (exit 10)
                                   when (noSciDBconnect == s) (exit 11)
                            TooManyRedirects _                  -> putStderr "TooManyRedirects" "" >> exit 8
                            OverlongHeaders                     -> putStderr "OverlongHeaders" "" >> exit 8
                            ResponseTimeout                     -> putStderr "ResponseTimeout" ""
                            ConnectionTimeout                   -> putStderr "ConnectionTimeout" ""
                            ConnectionFailure _                 -> putStderr "ConnectionFailure" ""
                            InvalidStatusLine _                 -> putStderr "InvalidStatusLine" "" >> exit 8
                            InvalidHeader _                     -> putStderr "InvalidHeader" "" >> exit 8
                            InvalidRequestHeader _              -> putStderr "InvalidRequestHeader" "" >> exit 8
                            InternalException _                 -> putStderr internalException (format w i1 $ cleanDoubleQuotes $ show e) >> exit 8
                            ProxyConnectException{}             -> putStderr "ProxyConnectException" "" >> exit 8
                            NoResponseDataReceived              -> putStderr "NoResponseDataReceived" ""
                            TlsNotSupported                     -> putStderr "TlsNotSupported" "" >> exit 8
                            WrongRequestBodyStreamSize _ _      -> putStderr "WrongRequestBodyStreamSize" "" >> exit 8
                            ResponseBodyTooShort _ _            -> putStderr "ResponseBodyTooShort" "" >> exit 8
                            InvalidChunkHeaders                 -> putStderr "InvalidChunkHeaders" "" >> exit 8
                            IncompleteHeaders                   -> putStderr "IncompleteHeaders" "" >> exit 8
                            InvalidDestinationHost _            -> putStderr "InvalidDestinationHost" "" >> exit 8
                            HttpZlibException _                 -> putStderr "HttpZlibException" ""
                            InvalidProxyEnvironmentVariable _ _ -> putStderr "InvalidProxyEnvironmentVariable" "" >> exit 8
                            ConnectionClosed                    -> putStderr "ConnectionClosed" ""
                            InvalidProxySettings _              -> putStderr "InvalidProxySettings" "" >> exit 8
                     where response_            = "     response = "
                           internalException    = "InternalException: " :: String {- Why is this needed? -}
                           i0                   = length response_
                           i1                   = length internalException
                           unauthorized         = C.pack "Unauthorized"
                           noSciDBconnect       = C.pack "Could not connect to SciDB"
                           -- Console terminal default width is 80 characters
                           consoleWidth         = 80
                           format w i s         = (intercalate "\n" . indent) ls
                               where ls         = (concatMap (wrap l) . lines) s
                                     l          = max consoleWidth w - i
                                     indent []  = []
                                     indent ls  = hd:((ss++) <$> tl)
                                         where hd = head ls
                                               tl = tail ls
                                               ss = replicate i ' '

fetchSession :: StateT Param (ReaderT Environment IO) ()
fetchSession =
         do e <- ask
            p <- get
            let url = urlPrefix e ++ "/new_session" ++ addAuthCode e
            s <- fetchURL (verbose e) url
            put p{session = strip s}
            p' <- get
            putVerbose "session=" (session p')
    where addAuthCode e = if scidbAuth e && checkMajorVersion (>=19) (sciDbVersion e)
                          then "?user=" ++ username e ++ "&password=" ++ password e
                          else ""

releaseSession :: StateT Param (ReaderT Environment IO) ()
releaseSession =
           do e <- ask
              p <- get
              let url = urlPrefix e ++ "/release_session?id=" ++ session p
                  v   = quietIfDef (verbose e)
              s <- fetchURL v url
              putVerbose "release_session=" (session p ++ s)

-- Not used: for documentation purposes
cancelSession :: StateT Param (ReaderT Environment IO) ()
cancelSession =
          do e <- ask
             p <- get
             let url = urlPrefix e ++ "/cancel?id=" ++ session p ++ addAuthCode e
                 v   = quietIfDef (verbose e)
             s <- fetchURL v url
             putVerbose "cancel_session=" (session p ++ s)

addAuthCode :: Environment -> String
addAuthCode e = if scidbAuth e && checkMajorVersion (18>=) (sciDbVersion e)
                then "&user=" ++ username e ++ "&password=" ++ password e
                else ""

putVerbose :: (MonadReader Environment m, MonadIO m) => String -> String -> m ()
putVerbose s t = do e <- ask
                    when (verbose e > VerboseDef) (putStderr s t)

quietIfDef :: Verbosity -> Verbosity
quietIfDef v = if v /= VerboseDef then v else Verbose0

urlPrefix :: Environment -> String
urlPrefix env  = protocol' ++ "://" ++ host' ++ ":" ++ port'
    where protocol' = protocol  env
          host'     = host      env
          port'     = maybePort env

putStderr :: MonadIO m => String -> String -> m ()
putStderr s t = unless (null t) $ liftIO $ hPutStrLn stderr (s++t)

exit :: MonadIO m => Int -> m a
exit i = liftIO $ exitWith $ if i == 0 then ExitSuccess else ExitFailure i
