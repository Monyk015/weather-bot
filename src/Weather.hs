module Weather
  ( getWeatherVector,
    Weather(..)
  )
where

import qualified Codec.Compression.GZip as GZip
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.ByteString        as ByteStringStrict
import qualified Data.ByteString.Lazy   as ByteString
import           Data.Csv
import           Data.Monoid
import           Data.Text
import           Data.Time
import           Data.Vector            (Vector)
import           GHC.Generics
import           Network.HTTP.Req

data Weather = Weather {
  date :: Maybe Day,
  tavg :: Maybe Double,
  tmin :: Maybe Double,
  tmax :: Maybe Double,
  prcp :: Maybe Double,
  snow :: Maybe Double,
  wdir :: Maybe Double,
  wspd :: Maybe Double,
  wpgt :: Maybe Double,
  pres :: Maybe Double,
  tsun :: Maybe Double
} deriving (Generic, Show)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "\"%Y-%-m-%-d\"" . show

instance FromRecord Weather

requestWeatherBulk :: IO ByteString.ByteString
requestWeatherBulk = runReq defaultHttpConfig $ do
  r <- req GET
           (https "bulk.meteostat.net" /: "daily" /: "34300.csv.gz")
           NoReqBody
           lbsResponse
           mempty
  pure $ responseBody r

getWeatherVector :: IO (Vector Weather)
getWeatherVector = do
  csv <- GZip.decompress <$> requestWeatherBulk
  (weathers :: Vector Weather) <- case Data.Csv.decode NoHeader csv of
    Right r -> return r
    Left e  -> error e
  pure weathers

