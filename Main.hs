{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (words)
import Data.Text 
import Data.Conduit as C
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^!), (^.), act)
import Web.Twitter.Conduit
import Web.Twitter.Types
import Web.Authenticate.OAuth
import Control.Monad.Logger
import Security

void :: Monad m =>m a -> m ()
void m = m >>= const (return ())


welcome :: String
welcome = "Welcome to Pub-Bot, your friendly pub-reminding twitter bot"

twInfo :: TWInfo
twInfo = def
     { twToken = def { twOAuth = tokens, twCredential = credential }
     , twProxy = Nothing
     }

testmain = print =<< (runNoLoggingT $ runTW twInfo (call homeTimeline))

main = putStrLn welcome >> runBot pubLogic

runBot :: (StreamingAPI -> IO ()) -> IO ()
runBot logic = runNoLoggingT . runTW twInfo $ do
    strm <- stream userstream
    strm C.$$+- CL.mapM_ (^! act (liftIO . logic))
    

pubLogic :: StreamingAPI -> IO ()
pubLogic (SStatus s) = case isMent of
                        True -> void $ runNoLoggingT $ runTW twInfo $ call $ update res
                        False -> print $ userScreenName $ statusUser s
    where isMent = proc s
          res    = usr s `append` " Hey, this is the bot responding!"
          usr    = userScreenName . statusUser
pubLogic s           = print s

proc s = True
