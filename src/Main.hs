import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB 
import qualified Pipes.Network.TCP as PTCP
import qualified Pipes.Lift as PL
import System.Process
import System.Cmd
import System.IO
import Network.Socket
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import WavePacket
import Server
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import System.Locale
import OpenALTest
import qualified Control.Concurrent as C
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Sound.OpenAL
import Control.Monad.State.Strict as S
import Data.Attoparsec.ByteString.Lazy
import Data.Binary

timeTest = do
	t <- getCurrentTime
	let f = "%F %X:%q"
	let s = formatTime defaultTimeLocale f t
	print $ encode s
	print $ BSL.length $ encode s

main = do
	c <- newTVarIO []
	PTCP.connect "10.1.2.47" "11000" $ \(connectionSocket, remoteAddr) -> do
		getALReady
		alSource <- source
		runEffect $ PTCP.fromSocket connectionSocket 4096 >-> setupPackets c (16000*5) 5 >-> encodePacket >-> toManySockets

op :: Consumer PB.ByteString IO ()
op = do
	flip evalStateT (0::Int) $ forever $ do
		i <- S.get
		bs <- lift await
		liftIO $ BS.writeFile ("out"++show i++".bin") bs
		S.put $ i+1

syncPipe :: UTCTime -> Pipe PB.ByteString WavePacket IO ()
syncPipe t = for cat (\bs -> do
		yield $ WavePacket t bs)

syncTest :: UTCTime -> String -> IO ()
syncTest t filename = do
	getALReady
	alSource <- source
	f <- openFile filename ReadMode
	runEffect $ PB.fromHandle f >-> syncPipe t >-> delayPipe >-> alFormatPipe >-> alConsumer alSource

pulseTest :: IO ()
pulseTest = do
--	system "pulseaudio --kill"
--	C.forkOS $ system "pulseaudio -D" >> return ()
	getALReady
	s <- source
	runEffect $ fromPA >-> alConsumer s


countConsumer :: Consumer BS.ByteString IO ()
countConsumer = do
	flip evalStateT 0 $ forever $ do
		bs <- lift await
		count <- S.get
		let newC = count + (BS.length bs)
		liftIO $ print $ newC
		S.put newC
