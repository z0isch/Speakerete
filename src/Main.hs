import           Pipes
import qualified Pipes.ByteString           as PB
import qualified Pipes.Prelude              as P
--import qualified Pipes.Network.TCP as PTCP
--import qualified Pipes.Lift as PL
import           System.Process
--import System.Cmd
import           System.IO
--import Network.Socket
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.ByteString.Unsafe
--import GHC.Generics (Generic)
import           WavePacket
--import Server
import qualified Control.Concurrent         as C
import           Control.Concurrent.STM
import           Control.Monad.State.Strict as S
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           OpenALTest
import           System.Locale
--import Data.Attoparsec.ByteString.Lazy
import           Data.Binary
import           Foreign.Ptr
import qualified PulseAudio                 as PA
import           Sound.OpenAL
import           Sound.Pulse.Simple
import           System.Environment
import           System.Posix.IO
import           System.Posix.Types

main :: IO ()
main = do
  (timeString:file:_) <- getArgs
  let t = fromJust $ parseTime defaultTimeLocale "%F %X" timeString
  pulseTest t file

soxConsumer :: Handle -> Consumer OAWavePacket IO ()
soxConsumer i = for cat (\(OAWavePacket (WavePacket _ bs)) -> do
    liftIO $ BS.hPut i bs
    liftIO $ hFlush i)

soxTest :: String -> IO ()
soxTest filename =  do
  f <- openFile filename ReadMode
  (i,_,_,_) <- runInteractiveCommand "play -t raw -c 2 -b 16 -L -e signed-integer -r 44100 -"
  runEffect $ PB.fromHandle f >-> for cat (\bs -> do
    liftIO $ BS.hPut i bs
    liftIO $ hFlush i)

toNamedPipe :: Fd -> Consumer OAWavePacket IO ()
toNamedPipe fd = for cat (\(OAWavePacket (WavePacket t bs)) -> do
  cstring <- liftIO $ unsafeUseAsCString bs return
  curr <- liftIO getCurrentTime
  liftIO $ C.threadDelay $ timeDiff t curr
  liftIO $ void $ fdWriteBuf fd (castPtr cstring) (fromIntegral $ BS.length bs))

paTest :: String -> IO ()
paTest filename = do
  f <- openFile filename ReadMode
  (i,_,_,_) <- runInteractiveCommand "pacat -d 1"
  runEffect $ PB.fromHandle f >-> for cat (\bs -> do
    liftIO $ BS.hPut i bs
    liftIO $ hFlush i)

syncTest :: UTCTime -> String -> IO ()
syncTest t filename = do
    (alSource,buffs) <- getALReady
    f <- openFile filename ReadMode
    now <- getCurrentTime
    print $ "Waiting " ++ (show $ (timeDiff t now) `div` 1000000) ++ " seconds"
    runEffect $ PB.fromHandle f >-> for cat (\bs -> yield $ OAWavePacket (WavePacket t bs)) >-> alConsumer alSource buffs
    _ <- getLine
    return ()

pulseTest :: UTCTime -> String-> IO ()
pulseTest t filename = do
    now <- getCurrentTime
    print $ "Waiting " ++ (show $ (timeDiff t now) `div` 1000000) ++ " seconds"
    f <- openFile filename ReadMode
    s <- simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (S16 LittleEndian) 44100 2) Nothing Nothing
    play t f s
    void getLine
    where
      pipe t f = PB.fromHandle f >-> for cat (\bs -> yield $ OAWavePacket (WavePacket t bs))
      play t f s = void $ C.forkIO $ runEffect $ pipe t f >-> for cat (\(OAWavePacket (WavePacket t bs)) -> do
                              now <- liftIO getCurrentTime
                              liftIO $ C.threadDelay (timeDiff t now)
                              liftIO $ simpleWriteRaw s bs)
      out t f = void $ C.forkIO $ runEffect $ pipe t f >-> P.print
