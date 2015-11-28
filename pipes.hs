{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens (_Right, (^..))
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson.Parser (json')
import Data.Aeson.Lens (key, values, _String)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Pipes (for, runEffect, (>->))
-- import Pipes.Attoparsec (parse)
import Pipes.HTTP
import Pipes.ByteString as BS
import Pipes.Prelude as PP  (foldM)
import Control.Monad
import Control.Monad.Writer
import Text.Show.Pretty
import Data.ByteString.Char8 as BS

main = do
    req <- parseUrl "http://www.reddit.com/r/haskell.json"
    m <- newManager defaultManagerSettings
    withHTTP req m $ \resp -> do
      b <- PP.foldM (\a b -> return $ BS.append a b) (return BS.empty) return (responseBody resp)
      BS.putStrLn b

