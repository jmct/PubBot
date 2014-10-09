import Web.Twitter.Conduit
import Web.Authenticate.OAuth
import Security

twInfo :: TWInfo
twInfo = def
     { twToken = def { twOAuth = tokens, twCredential = credential }
     , twProxy = Nothing
     }

main = undefined
