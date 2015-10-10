{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Prelude
import System.Directory          (doesDirectoryExist)
import Control.Monad             (filterM, liftM)
import System.Exit               (ExitCode(..), exitFailure, exitSuccess)
import System.Process            (system)
import Control.Applicative       ((<$>), (<*>))
import Data.Monoid               (mempty, (<>))
import Data.Maybe                (fromMaybe)
import Data.Either.Combinators
import Github.Data.Definitions   (Error)
import Github.Data               (Repo(..))
import Github.Repos              (userRepos, organizationRepos, RepoPublicity(..))

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O

data Options = Options { directory :: String
                       , owner      :: String
                       } deriving Show

main :: IO ()
main = O.execParser (O.info (O.helper <*> options) mempty) >>= start

options :: O.Parser Options
options = Options
  <$> defStr "."             ( O.metavar "DIRECTORY"            <> O.help "Directory to clone everything into" )
  <*> O.strOption            ( O.short 'o' <> O.long "owner"     <> O.help "Name of the owner to clone the repos of.")

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str

def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional

asEmptyList :: Either a [b] -> [b]
asEmptyList e = fromRight [] e

start :: Options -> IO ()
start opts = do
  putStrLn $ "Looking up repositories for " ++ (owner opts) ++ " ..."
  usrR <- userRepos (owner opts) All
  orgR <- organizationRepos (owner opts)
  (cloneRepos opts) $ (asEmptyList usrR) ++ (asEmptyList orgR)

cloneRepos :: Options -> [Repo] -> IO ()
cloneRepos opts rawRepos = do
  putStrLn $ "Found " ++ (show $ length rawRepos) ++ " repositories for " ++ (owner opts) ++ "."

  -- Filter the ones that already exist
  repos <- filterM (\r -> (liftM not) $ doesDirectoryExist $ ((directory opts) ++ "/" ++ (repoName r))) rawRepos

  putStrLn $ (show $ length repos) ++ " that you don't already have."

  returnCodes <- mapM (\r -> system $ "cd " ++ (directory opts) ++ " && git clone " ++ (fromMaybe (repoHtmlUrl r) (repoSshUrl r))) repos
  let rs = all (\e -> e == ExitSuccess) returnCodes
  
  case rs
    of True   -> exitSuccess
       False  -> exitFailure
