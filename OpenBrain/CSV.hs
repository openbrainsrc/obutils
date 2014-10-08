module OpenBrain.CSV where

import Control.Monad
import Control.Monad.Trans
import Data.List
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM


csvPrintLine :: MonadIO m => [String] -> m ()
csvPrintLine cells = liftIO $ putStrLn $ intercalate "," cells

quote :: String -> String
quote s = '"':s++"\""

appendCsvLine :: (MonadIO m, CSV.ToNamedRecord a) => FilePath ->  a -> m ()
appendCsvLine fout x = liftIO $ BL.appendFile fout $ CSV.encodeByNameWith opts (allHeaders x) [x] where
    opts = CSV.defaultEncodeOptions { CSV.encIncludeHeader  = False }

instance (CSV.ToNamedRecord a, CSV.ToNamedRecord b) => CSV.ToNamedRecord (a,b) where
  toNamedRecord (x,y) = HM.union (CSV.toNamedRecord x) (CSV.toNamedRecord y)

instance (CSV.ToNamedRecord a, CSV.ToNamedRecord b, CSV.ToNamedRecord c) => CSV.ToNamedRecord (a,b,c) where
  toNamedRecord (x,y,z) = HM.union (CSV.toNamedRecord x) $ HM.union (CSV.toNamedRecord y) (CSV.toNamedRecord z)

allHeaders :: CSV.ToNamedRecord a => a -> CSV.Header
allHeaders x = V.fromList $ HM.keys $ CSV.toNamedRecord x 

