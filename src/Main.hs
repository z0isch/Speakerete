import Pipes
--import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB 
--import qualified Pipes.Network.TCP as PTCP
--import qualified Pipes.Lift as PL
--import System.Process
--import System.Cmd
import System.IO
--import Network.Socket
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
--import GHC.Generics (Generic)
import WavePacket
--import Server
import Data.Time.Clock
import Data.Time
--import Data.Time.Format
import System.Locale
import OpenALTest
import qualified Control.Concurrent as C
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
--import Sound.OpenAL
import Control.Monad.State.Strict as S
--import Data.Attoparsec.ByteString.Lazy
import Data.Binary
import qualified Network.NTP.Control as NTP
import qualified Network.NTP.Control.Packet as NTPP
import Data.Time.Clock.POSIX
import Data.Maybe

getNTPTime :: String -> IO UTCTime
getNTPTime hostname = do
    t <- NTP.queryHost hostname $ NTPP.opVariables [NTPP.Clock]
    v <- NTPP.readVariables t
    return $ posixSecondsToUTCTime $ fromJust $ NTPP.clock v

timeTest :: IO ()
timeTest = do
    t <- getNTPTime "localhost"
    let f = "%F %X:%q"
    let s = formatTime defaultTimeLocale f t
    print $ encode s
    print $ BSL.length $ encode s

main :: IO ()
main = do
    t <- getCurrentTime
    syncTest t "part1.wav" 

op :: Consumer PB.ByteString IO ()
op = flip evalStateT (0::Int) $ forever $ do
    i <- S.get
    bs <- lift await
    liftIO $ BS.writeFile ("out"++show i++".bin") bs
    S.put $ i+1

syncPipe :: UTCTime -> Pipe PB.ByteString WavePacket IO ()
syncPipe t = for cat (yield . WavePacket t)

syncTest :: UTCTime -> String -> IO ()
syncTest t filename = do
    getALReady
    alSource <- source
    f <- openFile filename ReadMode
    runEffect $ PB.fromHandle f >-> syncPipe t >-> alFormatPipe >-> alConsumer alSource
    now <- getCurrentTime
    print $ "Waiting " ++ (show $ (timeDiff t now) `div` 1000000) ++ " seconds"
    C.threadDelay $ timeDiff t now

pulseTest :: IO ()
pulseTest = do
--  system "pulseaudio --kill"
--  C.forkOS $ system "pulseaudio -D" >> return ()
    getALReady
    s <- source
    t <- getCurrentTime
    runEffect $ fromPA >-> for cat (\(OAByteString bs) -> yield $ OAWavePacket (WavePacket t bs)) >-> alConsumer s


countConsumer :: Consumer BS.ByteString IO ()
countConsumer = flip evalStateT 0 $ forever $ do
    bs <- lift await
    count <- S.get
    let newC = count + BS.length bs
    liftIO $ print newC
    S.put newC
