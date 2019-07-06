{-# LANGUAGE QuasiQuotes #-}
{- |
The 'hquery' command via 'interpret' is based on the BNFC language defined
by [SciDbAFL.cf](SciDbAFL.html).
-}
module Interpreter(Results(..)
                  ,interpret
                  ) where

import Data.Char (toLower)
import Data.List (nub)
import System.IO (stderr,hPutStrLn)
import Text.RE.TDFA.String ((*=~),matches,re)

import AbsSciDbAFL (AFL(..),Query(..),Exp(..),Id(..),AString(..))
import ErrM (Err(..))
import ParSciDbAFL (myLexer,pAFL)
import PrintSciDbAFL (printTree)
import Utils (deEscapeSingleQuotes,stripUsing,toDoubleQuotedStr,)

data Results = Yes String -- ^ One can fetch a result if desired; it
                          -- is not an error to do so
             | No String -- ^ One cannot and should not fetch a
                         -- result; it is an error to do so
             | Unknown String -- ^ Unknown, possible user query
                              -- operator; could be Yes or No
             | BadNesting String -- ^ An argument query contains a No
                                 -- query; they should only contain
                                 -- Yes or Unknown queries
             | Command String -- ^ Command to be executed by hquery
             | Fetch (Maybe Bool) -- ^ Fetch lines: true (yes) or
                                  -- false (no); Nothing means no
                                  -- change
             | Format (Maybe String) -- ^ Format string, e.g., dcsv,
                                     -- csv+ (csv%2B); Nothing means
                                     -- no change
             | Lines (Maybe Integer) -- ^ Number of lines to fetch;
                                     -- Nothing means no change
             | ReadingLines  (Maybe Bool) -- ^ Reading lines: true to
                                          -- read lines and false to
                                          -- read bytes; Nothing means
                                          -- no change
             | Prefix String -- ^ An optional semi-colon separated,
                             -- URL encoded, AFL statements to precede
                             -- a query in the same SciDB connection
                             -- context. Mainly used for SciDB
                             -- namespace and role setting.  There is
                             -- no terminating semi-colon so trailing
                             -- semi-colons are removed.
             | Upload (String,[(FilePath,String)])
                       -- ^ The first String is the query with uploads
                       -- to perform and without a trailing semi-colon
                       -- (;).  The FilePath is the file to upload and
                       -- the second String is the String to replace
                       -- with the uploaded filepath, a single quoted
                       -- SciDB string.
               deriving (Eq, Ord, Show, Read)

-- | Return a list of results as an 'Err'.
interpret :: String -> Err [Results]
interpret s =
    let err = pAFL $ myLexer s
    in case err of
         Ok (Queries qs) -> let qs' = filter (/=QueryNil) qs
                            in Ok (fmap interpretQuery qs')
         Bad s'          -> Bad s'

interpretQuery :: Query -> Results
interpretQuery q =
    case q of
      QueryNil     -> No ""
      QueryArray{} -> No s
      QueryTemp{}  -> No s
      QueryExp exp -> interpretExp exp
    where
      s = printTree q
      interpretExp exp = case exp of
                           EFunc id es | any no es  -> BadNesting s
                                       | any up es  -> Upload (s,uploadMatches s)
                                       | otherwise  -> interpretId id
                           EVar (Id var)            -> Command var
                           Eeq a b                  -> interpretEeq a b
                           _                        -> Unknown s
      interpretId (Id id) = interpretFuncId id s
      no e = case e of
               EFunc id es -> bad id || any no es
               _           -> False
      bad (Id id) = interpretFuncId id "" == No ""
      up e = case e of
               EFunc id es -> anUp id es || any up es
               _           -> False
      anUp (Id id) [EString (AString _)] = fmap toLower id == "upload"
      anUp _       _                     = False
      interpretEeq a b = case a of
                           EVar (Id var) -> interpretVar var b
                           _             -> Unknown s
      interpretVar var b = case fmap toLower var of
                             "fetch"  -> case b of
                                           ETrue _  -> Fetch $ Just True
                                           EFalse _ -> Fetch $ Just False
                                           _        -> Fetch Nothing
                             "format" -> case b of
                                           EString (AString str) -> Format $ Just $ toDoubleQuotedStr str
                                           _                     -> Format Nothing
                             "n"      -> case b of
                                           EInt n -> Lines $ Just n
                                           _      -> Lines Nothing
                             "readinglines"  -> case b of
                                                  ETrue _  -> ReadingLines $ Just True
                                                  EFalse _ -> ReadingLines $ Just False
                                                  _        -> ReadingLines Nothing
                             "prefix" -> case b of
                                           EString (AString str) -> Prefix
                                                                    $ deEscapeSingleQuotes
                                                                    $ stripUsing (\c -> c=='\'' || c==';') str
                             _        -> Unknown s


uploadMatches :: String -> [(FilePath,String)]
uploadMatches s = fmap pair $ nub $ matches $ s *=~ uploadRe
    where
      uploadRe = [re|[Uu][Pp][Ll][Oo][Aa][Dd][[:space:]]*[(][[:space:]]*'([^']|\\')*'[[:space:]]*[)]|]
      pair s = ((tail . dropWhile (/='\'') . reverse . tail . dropWhile (/='\'') . reverse) s, s)

-- Determined by experience and particular to shim
interpretFuncId :: String -> (String -> Results)
interpretFuncId id =
    case fmap toLower id of
      "add_instances" -> No
      "add_user_to_role" -> No
      "aggregate" -> Yes
      "apply" -> Yes
      "attributes" -> Yes
      "avg_rank" -> Yes
      "bernoulli" -> Yes
      "between" -> Yes
      "build" -> Yes
      "cancel" -> No
      "cast" -> Yes
      "change_user" -> No
      "consume" -> No
      "create_namespace" -> No
      "create_role" -> No
      "create_user" -> No
      "cross_between" -> Yes
      "cross_join" -> Yes
      "cumulate" -> Yes
      "delete" -> Yes
      "dimensions" -> Yes
      "drop_namespace" -> No
      "drop_role" -> No
      "drop_role_for_user" -> No
      "drop_user" -> No
      "drop_user_from_role" -> No
      "filter" -> Yes
      "gemm" -> Yes
      "gesvd" -> Yes
      "glm" -> Yes
      "help" -> Yes
      "index_lookup" -> Yes
      "input" -> Yes
      "insert" -> Yes
      "join" -> Yes
      "kendall" -> Yes
      "limit" -> Yes
      "list" -> Yes
      "list_array_residency" -> Yes
      "list_instances" -> Yes
      "load" -> No
      "load_module" -> No
      "load_library" -> No
      "merge" -> Yes
      "move_array_to_namespace" -> No
      "mpi_init" -> No
      "pearson" -> Yes
      "project" -> Yes
      "quantile" -> Yes
      "rank" -> Yes
      "redimension" -> Yes
      "redistribute" -> Yes
      "regrid" -> Yes
      "remove" -> No
      "remove_instances" -> No
      "remove_versions" -> No
      "rename" -> No
      "repart" -> Yes
      "reshape" -> Yes
      "rng_uniform" -> Yes
      "save" -> No
      "scan" -> Yes
      "set_namespace" -> No
      "set_role_permissions" -> No
      "show" -> Yes
      "show_namespace" -> Yes
      "show_role_permissions" -> Yes
      "show_roles_for_user" -> Yes
      "show_user" -> Yes
      "show_user_in_role" -> Yes
      "slice" -> Yes
      "sort" -> Yes
      "spearman" -> Yes
      "spgemm" -> Yes
      "stats_instance" -> Yes
      "stats_instance_reset" -> No
      "stats_query" -> Yes
      "store" -> No
      "subarray" -> Yes
      "substitute" -> Yes
      "summarize" -> Yes
      "sync" -> No
      "transpose" -> Yes
      "tsvd" -> Yes
      "unfold" -> Yes
      "uniq" -> Yes
      "unload_library" -> No
      "unpack" -> Yes
      "unregister_instances" -> No
      "variable_window" -> Yes
      "versions" -> Yes
      "window" -> Yes
      "xgrid" -> Yes
      _ -> Unknown
