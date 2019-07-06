import Distribution.PackageDescription (BuildInfo)
import Distribution.Simple (UserHooks(..),defaultMainWithHooks,simpleUserHooks)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..),ComponentLocalBuildInfo)
import Distribution.Simple.PreProcess (PreProcessor(..),knownSuffixHandlers,mkSimplePreProcessor)
import Distribution.Simple.Program (Program(..),findProgramVersion,runDbProgram,simpleProgram)
import Distribution.Simple.Utils (info)
import System.FilePath (takeBaseName,takeDirectory)

main :: IO ()
main =
    defaultMainWithHooks simpleUserHooks{hookedPreProcessors = ("cf",ppBnfc):knownSuffixHandlers}

ppBnfc :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppBnfc _ lbi clbi =
    PreProcessor {platformIndependent = True
                 ,runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
                  do info verbosity ("Running "++programName bnfcProgram
                                    ++" --outputdir="++autogenDir
                                    ++" --haskell " ++ inFile
                                    )
                     runDbProgram verbosity bnfcProgram (withPrograms lbi)
                         ["--outputdir="++autogenDir, "--haskell", inFile]
                     info verbosity ("Creating "++programName bnfcProgram++" output file '"++outFile++"'"
                                    ++" from input file '"++inFile++"'")
                     outFile `makeFrom` inFile
                 }
        where autogenDir = autogenComponentModulesDir lbi clbi

bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc") {
    -- Invoking "bnfc --version" gives a string like "2.8.2"
    programFindVersion = findProgramVersion "--version" id
  }

makeFrom :: String -> String -> IO ()
makeFrom outFile inFile = do inFileStr <- readFile inFile
                             writeFile outFile (outFileStrFrom inFileStr)
    where outFileStrFrom inFileStr =
              "{- |\n"
              ++"This is the SciDB AFL BNFC file:\n"
              ++"\n"
              ++quote inFileStr++"\n"
              ++"-}\n"
              ++"\n"
              ++"module "++takeBaseName inFile++" where\n"
              ++"\n"
              ++"-- | BNFC configuration file as a string, for the purposes of documentation.\n"
              ++"bnfcFile :: String\n"
              ++"bnfcFile = "++show inFileStr++"\n"
          quote inFileStr = unlines $ fmap ("> "++) $ lines inFileStr

