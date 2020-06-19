module Lib
  ( server
  )
where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Monoid
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Req
import           Web.Scotty

data Update = Update {
  updateUpdateId  :: Int
  , updateMessage :: Message
} deriving (Generic, Show)

instance ToJSON Update where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Message = Message {
  messageMessageId :: Int,
  messageDate      :: Int,
  messageFrom      :: Maybe User,
  messageChat      :: Chat,
  messageText      :: Maybe String
} deriving (Generic, Show)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data User = User {
  userId                      :: Int,
  userIsBot                   :: Bool,
  userFirstName               :: String,
  userLastName                :: Maybe String,
  userUsername                :: Maybe String,
  userLanguageCode            :: Maybe String,
  userCanJoinGroups           :: Maybe Bool,
  userCanReadAllGroupMessages :: Maybe Bool,
  userSupportsInlineQueries   :: Maybe Bool
} deriving (Generic, Show)

instance ToJSON User where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Chat = Chat {
  chatId        :: Int,
  chatType      :: String,
  chatTitle     :: Maybe String,
  chatUsername  :: Maybe String,
  chatFirstName :: Maybe String
} deriving (Generic, Show)

instance ToJSON Chat where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

token = "bot1015970585:AAFZtOnHYwKEC_v4RlP_6f8bNdSaPFXk3L0"

telegramReq :: Text -> Value -> IO ()
telegramReq method payload = runReq defaultHttpConfig $ do
  r <- req POST
           (https "api.telegram.org" /: token /: method)
           (ReqBodyJson payload)
           jsonResponse
           mempty
  liftIO $ print (responseBody r :: Value)

setWebhook :: Text -> IO ()
setWebhook webhook = telegramReq "setWebhook" payload
  where payload = object ["url" .= webhook]

server :: IO ()
server = do
  setWebhook "https://dd5e9ffe51a2.ngrok.io"
  scotty 3000 $ post "/" $ do
    body :: Update <- jsonData
    liftIO $ print body
    html $ mconcat ["<h1>Scotty, ", " me up!</h1>"]

handleUpdate :: Update -> IO ()
handleUpdate = undefined

