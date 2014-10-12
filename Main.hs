{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (words)
import Data.Text 
import Data.Conduit as C
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Web.Twitter.Conduit
import Web.Twitter.Types
import Web.Authenticate.OAuth
import Security

botName :: Text
botName = "iquit_pubnow"

void :: Monad m => m a -> m ()
void m = m >>= const (return ())


welcome :: String
welcome = "Welcome to Pub-Bot, your friendly pub-reminding twitter bot"

twInfo :: TWInfo
twInfo = def
     { twToken = def { twOAuth = tokens, twCredential = credential }
     , twProxy = Nothing
     }

runTwit :: TW (ResourceT (NoLoggingT IO)) a -> IO a
runTwit = runNoLoggingT . runTW twInfo

testmain :: IO [Status]
testmain = runNoLoggingT $ runTW twInfo (call mentionsTimeline)

main = putStrLn welcome >> runTwit (runBot pubLogic)

runBot :: (MonadResource m, MonadLogger m) => (StreamingAPI -> TW m ()) -> TW m ()
runBot logic = do
    strm <- stream userstream
    strm C.$$+- CL.mapM_ logic

pubLogic :: (MonadResource m, MonadLogger m) => StreamingAPI -> TW m ()
pubLogic (SStatus s) = case isMent of
                        True -> do 
                            call $ update res
                            liftIO $ putStrLn $ "I've tweeted at " ++ (unpack $ usr s)
                        False -> liftIO $ print $ userScreenName $ statusUser s
    where isMent = proc s
          res    = cons '@' $ usr s `append` " Hey, this is the bot responding!"
          usr    = userScreenName . statusUser
pubLogic s           = liftIO $ print s

proc :: Status -> Bool
proc s = maybe False (== botName) (statusInReplyToScreenName s)
