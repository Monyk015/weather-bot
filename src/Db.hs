module Db
  ( saveBulk
  )
where

import           Data.Time
import           Data.Vector
import           Database.MongoDB
import           Weather

maybeToValue (Just val) = val
maybeToValue Nothing    = Null

dayToTime day = UTC $ UTCTime day 0

run :: forall a . Action IO a -> IO a
run a = do
  pipe <- connect (host "127.0.0.1")
  e    <- access pipe master "weather" a
  close pipe
  pure e

saveBulk :: Vector Weather -> IO ()
saveBulk weatherVector =
  run $ (insertMany_ "weather" . toList . Data.Vector.map weatherToDoc)
    weatherVector
 where
  weatherToDoc w =
    [ "date" =: maybeToValue (dayToTime <$> date w)
    , "tavg" =: maybeToValue (Float <$> tavg w)
    , "tmin" =: maybeToValue (Float <$> tmin w)
    , "tmax" =: maybeToValue (Float <$> tmax w)
    , "prcp" =: maybeToValue (Float <$> prcp w)
    , "snow" =: maybeToValue (Float <$> snow w)
    , "wdir" =: maybeToValue (Float <$> wdir w)
    , "wspd" =: maybeToValue (Float <$> wspd w)
    , "wpgt" =: maybeToValue (Float <$> wpgt w)
    , "pres" =: maybeToValue (Float <$> pres w)
    , "tsun" =: maybeToValue (Float <$> tsun w)
    ]
