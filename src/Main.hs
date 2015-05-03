import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString           as PB
--import qualified Pipes.Network.TCP as PTCP
--import qualified Pipes.Lift as PL
import System.Process
--import System.Cmd
import           System.IO
--import Network.Socket
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
--import GHC.Generics (Generic)
import           WavePacket
--import Server
import           Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Maybe
import qualified Control.Concurrent         as C
import           OpenALTest
import Control.Concurrent.STM
import           Control.Monad.State.Strict as S
--import Data.Attoparsec.ByteString.Lazy
import           Data.Binary
import System.Environment
import Sound.OpenAL
main :: IO ()
main = do
  d <- allDeviceSpecifiers
  print d
--  (t:_) <- getArgs
 -- pulseTest (t == "True")
  (timeString:file:_) <- getArgs
  let t = fromJust $ parseTime defaultTimeLocale "%F %X" timeString
  syncTest t file

syncTest :: UTCTime -> String -> IO ()
syncTest t filename = do
    (alSource,buffs) <- getALReady
    f <- openFile filename ReadMode
    now <- getCurrentTime
    print $ "Waiting " ++ (show $ (timeDiff t now) `div` 1000000) ++ " seconds"
    runEffect $ PB.fromHandle f >-> for cat (\bs -> yield $ OAWavePacket (WavePacket t bs)) >-> alConsumer alSource buffs
    _ <- getLine
    return ()

pulseTest :: Bool -> IO ()
pulseTest p = do
    t <- getCurrentTime
    if p then play t else out t
    void getLine
    void $ system "pactl play-sample 0 null"
    void getLine
    where
      pipe t = fromPA >-> for cat (\(OAByteString bs) -> yield $ OAWavePacket (WavePacket t bs))
      play t = do
        (s,buffs) <-getALReady
        void $ C.forkIO $ runEffect $ pipe t >-> alConsumer s buffs
      out t = void $ C.forkIO $ runEffect $ pipe t >-> P.print
