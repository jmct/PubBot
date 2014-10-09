{-# LANGUAGE OverloadedStrings #-}
module Security
       ( tokens
       , credential
       ) where

import Web.Twitter.Conduit
import Web.Authenticate.OAuth


tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "YOUR CONSUMER KEY"
    , oauthConsumerSecret = "YOUR CONSUMER SECRET"
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "YOUR ACCESS TOKEN")
    , ("oauth_token_secret", "YOUR ACCESS TOKEN SECRET")
    ]
