{-# LANGUAGE NamedFieldPuns #-} 
{-# LANGUAGE ViewPatterns   #-}

-- | A server for scrobbling, based upon the Audioscrobbler Realtime
-- Submission protocol v1.2
-- <http://www.audioscrobbler.net/development/protocol/>

module Scrobble.Server
  (startScrobbleServer)
  where

import           Scrobble.Types

import           Control.Applicative hiding (optional)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Time
import           Network
import           Network.URL
import           Numeric
import           Prelude             hiding (catch)
import           System.IO
import           System.Locale

--------------------------------------------------------------------------------
-- Server

-- | Start a scrobbling server.
startScrobbleServer :: Config -> Handlers -> IO ()
startScrobbleServer cfg handlers = do
  hSetBuffering stdout NoBuffering
  clients <- newMVar []
  listener <- listenOn (PortNumber (cfgPort cfg))
  expire <- forkIO $ expireClients handlers cfg clients
  flip finally (do sClose listener; killThread expire) $ forever $ do
    (h,_,_) <- accept listener
    forkIO $ do
      hSetBuffering h NoBuffering
      headers <- getHeaders h
      case requestMethod headers of
        Just ("GET",url_params -> params) -> handleInit cfg handlers h clients params
        Just ("POST",url) -> do
          rest <- hGetContents h
          case requestBody headers rest of
            Nothing -> return ()
            Just body -> dispatch handlers h clients url body
        _ -> return ()
      hClose h

-- | Expire client sessions after inactivity.
expireClients :: Handlers -> Config -> MVar [Session] -> IO ()
expireClients handlers cfg clients = forever $ do
  threadDelay (1000 * 1000 * 60)
  now <- getCurrentTime
  modifyMVar_ clients $ filterM $ \client -> do
    let expired = diffUTCTime now (sesTimestamp client) > cfgExpire cfg
    when expired $ handleExpire handlers client
    return (not expired)

-- | Handle initial handshake.
handleInit :: Config -> Handlers -> Handle -> MVar [Session] -> [(String,String)] -> IO ()
handleInit cfg handlers h clients params =
  case params of
    (makeSession -> Just sess) -> do
      handleHandshake handlers sess
      modifyMVar_ clients (return . (sess :))
      reply h [show OK
              ,sesToken sess
              ,selfurl "nowplaying"
              ,selfurl "submit"]
    _ -> reply h [show BADAUTH]

  where selfurl x = "http://" ++ cfgHost cfg ++ ":" ++ show (cfgPort cfg) ++ "/" ++ x

-- | Dispatch on commands.
dispatch :: Handlers -> Handle -> MVar [Session] -> URL -> String -> IO ()
dispatch handlers h clients url body =
  case parsePost body of
    Nothing -> error "Unable to parse POST body."
    Just params ->
      withSession h clients params $ \sess ->
        case url_path url of
          "nowplaying" -> handleNow handlers h sess params
          "submit" -> handleSubmit handlers h sess params
          _ -> error $ "Unknown URL: " ++ url_path url

-- | Look up the session and do something with it.
withSession ::  Handle -> MVar [Session] -> [(String,String)] -> (Session -> IO ()) -> IO ()
withSession h clients params go =
  case lookup "s" params of
    Nothing -> error "No session given."
    Just token -> do
      modifyMVar_ clients $ \sessions -> do
        case find ((==token) . sesToken) sessions of
          Nothing -> do reply h [show BADSESSION]
                        return sessions
          Just sess -> do go sess
                          now <- getCurrentTime
                          return (sess { sesTimestamp = now } :
                                  (filter ((/=token) . sesToken) sessions))

-- | Handle now playing command.
handleNow handlers h sess params = do
  case makeNowPlaying params of
    Nothing -> error $ "Invalid now playing notification: " ++ show params
    Just np -> do handleNowPlaying handlers sess np
                  reply h [show OK]

-- | Handle submit command.
handleSubmit handlers h sess params = do
  case makeSubmissions params of
    Nothing -> error $ "Unable to parse submissions: " ++ show params
    Just subs -> do
      ok <- handleSubmissions handlers sess subs
      when ok $
        reply h [show OK]

--------------------------------------------------------------------------------
-- Command data structures

-- | Make a session from a parameter set.
makeSession :: [(String,String)] -> Maybe Session
makeSession params =
  Session <$> bool (get "hs")
          <*> get "p"
          <*> get "c"
          <*> get "v"
          <*> get "u"
          <*> time (get "t")
          <*> get "a"
  where get k = lookup k params

-- | Make a now-playing notification.
makeNowPlaying :: [(String,String)] -> Maybe NowPlaying
makeNowPlaying params =
  NowPlaying <$> get "a"
             <*> get "t"
             <*> optional (get "b")
             <*> mint (get "l")
             <*> mint (get "n")
             <*> optional (get "m")
  where get k = lookup k params

-- | Make a batch of track submissions.
makeSubmissions :: [(String,String)] -> Maybe [Submission]
makeSubmissions params =
  forM [0..length (filter (isPrefixOf "a[" . fst) params) - 1] $ \i -> do
    let get k = lookup (k ++ "[" ++ show i ++ "]") params
    Submission <$> get "a"
               <*> get "t"
               <*> time (get "i")
               <*> source (get "o")
               <*> rating (get "r")
               <*> mint (get "l")
               <*> optional (get "b")
               <*> mint (get "n")
               <*> optional (get "m")

  where source m = m >>= \s -> lookup s sources where
          sources = [("P",UserChosen)
                    ,("R",NonPersonlizedBroadcast)
                    ,("E",Personalized)
                    ,("L",LastFm)
                    ,("U",Unknown)]
        rating m = m >>= \r -> fmap Just (lookup r ratings) <|> return Nothing where
          ratings = [("L",Love),("B",Ban),("S",Skip)]

--------------------------------------------------------------------------------
-- Some param parsing utilities

time m = m >>= parseTime defaultTimeLocale "%s"
bool = fmap (=="true")
mint m = m >>= \x -> case reads x of
  [(n,"")] -> return (Just n)
  _        -> return Nothing
optional m = do
  v <- m
  if null v
    then return Nothing
    else return (Just v)

--------------------------------------------------------------------------------
-- HTTP utilities

-- | Parse a POST request's parameters.
parsePost :: String -> Maybe [(String, String)]
parsePost body = fmap url_params (importURL ("http://x/x?" ++ body))

-- | Get the request method.
requestMethod :: [String] -> Maybe (String,URL)
requestMethod headers =
   case words (concat (take 1 headers)) of
     [method,importURL -> Just url,_] ->
       return (method,url)
     _ -> Nothing

-- | Get the request body.
requestBody :: [String] -> String -> Maybe String
requestBody headers body = do
  len <- lookup "content-length:" (map (break (==' ') . map toLower) headers)
  case readDec (unwords (words len)) of
    [(l,"")] -> return (take l body)
    _        -> Nothing

-- | Read up to the headers.
getHeaders :: Handle -> IO [String]
getHeaders h = go [] where
  go ls = do
    l <- hGetLine h
    if l == "\r"
       then return (reverse ls)
       else go (l : ls)

-- | Make a HTTP reply.
reply :: Handle -> [String] -> IO ()
reply h rs = hPutStrLn h resp where
  body = unlines rs
  resp = unlines ["HTTP/1.1 200 OK"
                 ,"Content-Length: " ++ show (length body)
                 ,""] ++
         body
