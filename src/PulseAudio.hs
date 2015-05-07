module PulseAudio where

import qualified Control.Concurrent as C
import           Control.Monad
import qualified Data.ByteString    as BS
import           Data.Time
import           Pipes
import qualified Pipes.Prelude      as P
import           Sound.Pulse.Simple
import           WavePacket

newtype PAByteString = PAByteString BS.ByteString
newtype PAWavePacket = PAWavePacket WavePacket

paRead :: Simple -> Producer PAByteString IO ()
paRead = go
               where go s = do
                       bs <- liftIO $ simpleReadRaw s 4410
                       yield $ PAByteString bs
                       go s

paPlay :: Simple -> Consumer PAWavePacket IO ()
paPlay s = for cat (\(PAWavePacket (WavePacket t bs)) -> do
                       curr <- liftIO getCurrentTime
                       liftIO $ print $ "Waiting " ++ (show $ timeDiff t curr)
                       liftIO $ C.threadDelay $ timeDiff t curr
                       liftIO $ simpleWriteRaw s bs
                   )

timeDiff :: UTCTime -> UTCTime -> Int
timeDiff t1 t2 = floor (toRational (diffUTCTime t1 t2) * 1000000)
