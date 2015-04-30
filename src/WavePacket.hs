module WavePacket where

import Pipes
import qualified Pipes.Binary as PBinary
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Binary as DB
import Data.Time
import System.Locale
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.Int
import Data.Maybe

timeFormat :: String
timeFormat = "%F %X:%q"

timeEncode :: UTCTime -> String
timeEncode = formatTime defaultTimeLocale timeFormat

timeDecode :: BSL.ByteString -> UTCTime
timeDecode bsl = fromJust $ parseTime defaultTimeLocale timeFormat (DB.decode bsl :: String)

data WavePacket = WavePacket UTCTime BS.ByteString
    deriving (Show)
instance DB.Binary WavePacket where
    put (WavePacket time bs) = do
    	DB.put (timeEncode time)
	DB.put bs
    get = error ""

parseWavePacket :: A.Parser WavePacket
parseWavePacket = do
    time <- A.count 40 A.anyWord8
    l <- A.count 8 A.anyWord8
    bs <- A.count (fromIntegral (DB.decode $ BSL.pack l :: Int64)) A.anyWord8
    return $ WavePacket (timeDecode $ BSL.pack time) (BS.pack bs)

encodePipe :: Pipe WavePacket BS.ByteString IO ()
encodePipe = for cat PBinary.encode

waveDecoder :: Producer BS.ByteString IO () -> Producer WavePacket IO ()
waveDecoder b = do
    (wp, _) <- liftIO $ PP.runStateT PBinary.decode b
    case wp of
	(Left e) -> error (show e )
	(Right w) -> yield w

getWavePacket :: Maybe (Either PA.ParsingError WavePacket) -> Producer  WavePacket IO ()
getWavePacket p = do
    t <- liftIO getCurrentTime
    yield $ e t p
    where
        e t Nothing = WavePacket t BS.empty
	e _ (Just (Left pe)) = error $ "Can't get WavePakcet: " ++ PA.peMessage pe
	e _ (Just (Right w))  = w

decoded :: BS.ByteString
decoded = BS.pack [1..25]

encoded :: UTCTime -> BSL.ByteString
encoded t = DB.encode $ WavePacket t decoded

parsed :: UTCTime -> WavePacket
parsed t = case p $ encoded t of
    A.Done _ t1 -> t1
    _ -> error "Can't parse"
    where p = A.parse parseWavePacket
