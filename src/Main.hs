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
--import Sound.OpenAL
import           Control.Monad.State.Strict as S
--import Data.Attoparsec.ByteString.Lazy
import           Data.Binary
import System.Environment

main :: IO ()
main = do
  (t:_) <- getArgs
  pulseTest (t == "True")
  --(timeString:file:_) <- getArgs
  --let t = fromJust $ parseTime defaultTimeLocale "%F %X" timeString
  --syncTest t file

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
    (alSource,buffs) <- getALReady
    f <- openFile filename ReadMode
    now <- getCurrentTime
    print $ "Waiting " ++ (show $ (timeDiff t now) `div` 1000000) ++ " seconds"
    runEffect $ PB.fromHandle f >-> syncPipe t >-> alFormatPipe >-> alConsumer alSource buffs
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

countConsumer :: Consumer BS.ByteString IO ()
countConsumer = flip evalStateT 0 $ forever $ do
    bs <- lift await
    count <- S.get
    let newC = count + BS.length bs
    liftIO $ print newC
    S.put newC
