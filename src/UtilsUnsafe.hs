{-# LANGUAGE LambdaCase #-}
module UtilsUnsafe
               (base64Sha512Hash
               ,fetchPassword
               ,fetchUsername
               ,isConnectable
               ,managerSettings
               ,putOrPageStrLn
               ,VALID(..)
               ,valid
               )
where

import qualified Data.ByteString       as B (empty)
import qualified Data.ByteString.Char8 as C (pack)

import Control.Monad            (unless)
import Data.Char                (toLower)
import Data.Default.Class       (def)
import Data.Maybe               (fromMaybe,fromJust,isNothing)
import Data.X509.CertificateStore (CertificateStore)
import Network.Connection       (TLSSettings(..))
import Network.HTTP.Client      (ManagerSettings)
import Network.HTTP.Client.TLS  (mkManagerSettings)
import Network.Socket           (addrAddress,addrFamily,addrProtocol,addrSocketType,close,connect,getAddrInfo,socket)
import Network.TLS              (Shared(..),ClientParams(..),Supported(..),defaultParamsClient)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Safe                     (readMay)
import System.Console.Haskeline (Settings,autoAddHistory,defaultSettings,getInputLine,getPassword,runInputT)
import System.Environment       (getEnvironment)
import System.Exit              (ExitCode(..))
import System.IO                (IOMode(..),openFile,hFlush,hClose,hPutStr,hPutStrLn,stderr)
import System.IO.Error          (ioeGetErrorString,tryIOError)
import System.Process           (createProcess,shell,CreateProcess(..),StdStream(..),waitForProcess)
import Text.Hostname            (validHostname)

import ErrM                     (Err(..))
import Interpreter              (Results(..),interpret)
import Utils                    (toSingleQuotedStr)

import Crypto.Hash              (hashWith, SHA512 (..))
import Data.ByteString          (ByteString)
import Data.ByteArray.Encoding  (convertToBase, Base (Base64))
import Data.Text                (pack)
import Data.Text.Encoding       (encodeUtf8)

-- | Given a string, return a Base64, SHA512 hash.
base64Sha512Hash s =
    let bs     = encodeUtf8 $ pack s
        digest = convertToBase Base64 (hashWith SHA512 bs)
    in show (digest :: ByteString)

-- | Put a string or page it if the environmental variable PAGER is set.
putOrPageStrLn :: String -> IO ExitCode
putOrPageStrLn str
    | null str  = return ExitSuccess
    | otherwise =
    do pager <- fmap (lookup "PAGER") getEnvironment
       if isNothing pager
           then putStrLn str >> return ExitSuccess
           else do (inh, _, _, pid) <- createProcess (shell $ fromJust pager){std_in = CreatePipe}
                   unless (isNothing inh) $ do hPutStr (fromJust inh) str
                                               hFlush (fromJust inh)
                                               hClose (fromJust inh)
                   waitForProcess pid

data VALID = HOSTNAME | PORT | PROTOCOL | HISTORY | READABLE | PREFIX

-- | Is the string a valid 'VALID'?
--
-- >   valid HOSTNAME "localhost"
-- >   valid PORT "8080"
-- >   valid PROTOCOL "http"
-- >   valid READABLE "/path_to_query_file/file"
-- >   -- Terminating semi-colon (;) optional
-- >   valid PREFIX "set_namespace('sensor_data');"
-- >   valid HISTORY "HOME/.hquery_history"
--
-- where HOME is replaced by the home directory of the user if it
-- exists, otherwise /tmp.

valid :: VALID -> String -> IO Bool
valid HOSTNAME s = if (validHostname . C.pack) (fmap toLower s)
                   then return True
                   else do hPutStrLn stderr ("Bad hostname '" ++ s ++ "'")
                           return False

valid PORT     s = if maybe False (\i -> 0 < i && i < 65536) (readMay s :: Maybe Int)
                   then return True
                   else do hPutStrLn stderr ("Bad port number '" ++ s ++ "'")
                           return False

valid PROTOCOL s = let t = fmap toLower s
                   in if t == "http" || t == "https"
                      then return True
                      else do hPutStrLn stderr ("Bad protocol '" ++ s ++ "'")
                              return False

valid HISTORY  s = do result <- tryIOError (openFile s AppendMode)
                      either (\ioe -> do {putIOE "Bad history file" s ioe; return False})
                             (\h -> do {hClose h; return True})
                             result

valid READABLE s = do result <- tryIOError (openFile s ReadMode)
                      either (\ioe -> do {putIOE "Unreadable file" s ioe; return False})
                             (\h -> do {hClose h; return True})
                             result

valid PREFIX s = let s'  = s ++ ";"
                     err = interpret s'
                 in do b <- case err of
                              Bad m -> do hPutStrLn stderr ("Input " ++ m)
                                          return False
                              Ok rs -> return $ all (\case
                                                     Yes _     -> True
                                                     No _      -> True
                                                     Unknown _ ->  True
                                                     _         ->  False
                                                    ) rs
                       if null s || b then return True
                       else do hPutStrLn stderr ("Bad prefix " ++ toSingleQuotedStr s')
                               return False

-- | Return true if one can connect to hostname @h@ and port @p@,
-- otherwise false with a message sent to stderr.
isConnectable :: String -> String -> IO Bool
isConnectable h p =
    do addrs' <- tryIOError (getAddrInfo Nothing (Just h) (Just p))
       either (\ioe -> do {putIOE "Bad hostname:port" (h++":"++p) ioe; return False})
              (\addrs -> do let addr = head addrs
                            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                            result <- tryIOError (connect sock (addrAddress addr))
                            either (\ioe -> do {putIOE "Cannot connect:" (h++":"++p) ioe; return False})
                                   (\_ -> do {close sock; return True})
                                   result
              )
              addrs'

putIOE :: String -> String -> IOError -> IO ()
putIOE p s ioe = hPutStrLn stderr (p ++ " '" ++ s ++ "' (" ++ ioeGetErrorString ioe ++ ")")

-- | Fetch the user name with prompt @Username@ or prompt string @s@.
fetchUsername :: String -> IO String
fetchUsername s = let s' = if null s then "Username: " else s ++ ": "
                  in fromMaybe "" <$> runInputT safeSettings (getInputLine s')

-- | Fetch the password with prompt @Password@ or prompt string @s@ and
-- masking character @c@ otherwise @*@.

fetchPassword :: Maybe Char -> String -> IO String
fetchPassword m s = let s' = if null s then "Password: " else s ++ ": "
                        c  = fromMaybe '*' m
                    in do s <- fromMaybe "" <$> runInputT safeSettings (getPassword (Just c) s')
                          putStrLn ""
                          return s

safeSettings :: Settings IO
safeSettings = defaultSettings{autoAddHistory = False}

-- | Given a certificate store, create manager settings for SSL/TLS
-- connections.

managerSettings :: CertificateStore -> ManagerSettings
managerSettings store = mkManagerSettings settings Nothing
  where settings = TLSSettings params
        params   = (defaultParamsClient "" B.empty) {
                       clientUseServerNameIndication = True
                     , clientShared = def {
                           sharedCAStore = store
                       }
                     , clientSupported = def {
                           supportedCiphers = ciphersuite_default
                       }
                   }
