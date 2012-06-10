-- | A server program that merely accepts scrobbles and prints them to standard output.

module Main where

import           Scrobble

import           Control.Monad
import           System.Environment

-- | Main scrobbling server.
main :: IO ()
main = do
  (port:_) <- fmap (map read) getArgs
  startScrobbleServer (config port) handlers

  where config port = Config (fromIntegral port) "localhost" (60*60)
        handlers = Handlers
          { handleHandshake = \s ->
              putStrLn $ "New session: " ++ show s

          , handleExpire = \s ->
              putStrLn $ "Session expired: " ++ show s

          , handleNowPlaying = \s np ->
              putStrLn $ "Now playing: " ++ show np

          , handleSubmissions = \s subs -> do
              forM_ subs $ \sub -> putStrLn $ "Listened: " ++ show sub
              return True
          }
