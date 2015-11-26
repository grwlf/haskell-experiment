{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens (_Right, (^..))
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson.Parser (json')
import Data.Aeson.Lens (key, values, _String)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Pipes.Attoparsec (parse)
import Pipes.HTTP
import Text.Show.Pretty

main = do
    req <- parseUrl "http://www.reddit.com/r/haskell.json"
    withManager defaultManagerSettings $ \m ->
        withHTTP req m $ \resp -> do
            json <- evalStateT (parse json') (responseBody resp)

            putStrLn (ppShow json)

format :: T.Text -> T.Text
format txt =
    if   T.length txt <= columns
    then T.concat [bullet,                txt          ]
    else T.concat [bullet, T.take columns txt, ellipsis]
  where
    bullet   = "[*] "
    ellipsis = "..."
    columns = 60 - (T.length bullet + T.length ellipsis)
