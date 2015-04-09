module OpenALTest where

import System.IO 
import System.Process
import qualified Data.ByteString as BS
import Control.Monad
import Foreign.Marshal.Alloc
import Sound.OpenAL
import qualified Pipes as P
import qualified Pipes.Lift as L
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB
import qualified Control.Monad.State.Strict as ST
import Control.Monad
import qualified Control.Concurrent as C

newtype OAByteString = OAByteString BS.ByteString
newtype OggByteString = OggByteString BS.ByteString

toOgg :: P.Pipe OAByteString OggByteString IO ()
toOgg =  P.for P.cat (\(OAByteString bs) -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t raw -c 2 -r 44100 -e signed-integer -L -b 16 - -t ogg -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield $ OggByteString oBs)

fromOgg :: P.Pipe OggByteString OAByteString IO ()
fromOgg = P.for P.cat (\(OggByteString bs) -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t ogg - -t raw -c 2 -r 44100 -e signed-integer -L -b 16 -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield $ OAByteString oBs)
	
fromPA :: P.Producer OAByteString IO ()
fromPA = do
	P.liftIO $ C.forkOS $ system "pulseaudio" >> return ()
	P.liftIO $ C.threadDelay 30000
	(i,o,e,h) <- P.liftIO $ runInteractiveCommand "parec --device=drain.monitor --format=s16le --rate=44100 --channels=2"
	P.liftIO $ hClose i
	P.for (PB.fromHandle o) (\bs -> P.yield $ OAByteString bs)

alFormatPipe :: P.Pipe BS.ByteString OAByteString IO ()
alFormatPipe =  P.for P.cat (\bs -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t raw -c 1 -r 16000 -e floating-point -L -b 32 - -t raw -c 1 -r 16000 -e signed-integer -L -b 16 -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hFlush i
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield $ OAByteString oBs)

getALReady :: IO ()
getALReady = do
	d <- device
	context d
	return ()

alConsumer :: Source -> P.Consumer OAByteString IO ()
alConsumer s = flip ST.evalStateT [] $ forever $ do 
		OAByteString bs <- P.lift P.await
		ss <- P.liftIO $ get $ sourceState s
		b <- P.liftIO $ buff bs
		P.liftIO $ queueBuffers s [b]
		buffs <- ST.get
		ST.put $ buffs ++ [b]
		case ss of
			Playing -> do
				num <- P.liftIO $ get $ buffersProcessed s
				newBuffs <- ST.get
				let numOldBuffs = fromInteger $ toInteger $ num
				P.liftIO $ freeMemory s (take numOldBuffs newBuffs)
				ST.put $ drop numOldBuffs newBuffs
			_ -> P.liftIO $ play [s]

freeMemory :: Source -> [Buffer] -> IO ()
freeMemory s buffs = do
	when (length buffs > 0) $ print $ "Freeing " ++ (show $ length buffs) ++ " buffers"
	unqueueBuffers s buffs
	mapM_ freeBuff buffs 
	where 
		freeBuff :: Buffer -> IO ()
		freeBuff b =  do 
			(BufferData (MemoryRegion ptr _) _ _)  <- get $ bufferData b
			free ptr

device :: IO Device
device = do
	maybeDevice <- openDevice Nothing 
	case maybeDevice of
		Nothing -> error "openDevice failed"
		Just device -> return device

context :: Device -> IO Context
context dev = do
	ctx <- createContext dev []
	case ctx of
		Nothing -> error "no ctx"
		Just ctx -> do
			currentContext $= (Just ctx)
			processContext ctx
			return ctx

source :: IO Source
source = do
	liftM head $ genObjectNames 1
	
buff :: BS.ByteString -> IO Buffer
buff bs = do
	(ptr,len) <- BS.useAsCStringLen bs return
	b <- liftM head $ genObjectNames 1 :: (IO Buffer)
	let bd = BufferData (MemoryRegion ptr (fromIntegral $ len)) Stereo16 44100
	bufferData b $= bd
	return b
