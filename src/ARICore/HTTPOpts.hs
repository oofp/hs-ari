module ARICore.HTTPOpts (getHTTPOpts) where

import Network.Wreq
import Control.Lens
import qualified Data.ByteString as BStr

getHTTPOpts::BStr.ByteString->BStr.ByteString->Options
getHTTPOpts userName pwd = defaults & auth ?~ basicAuth userName pwd
