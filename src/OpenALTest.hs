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

toOgg :: P.Pipe BS.ByteString BS.ByteString IO ()
toOgg =  P.for P.cat (\bs -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t raw -c 2 -r 44100 -e signed-integer -L -b 16 - -t ogg -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield oBs)

fromOgg :: P.Pipe BS.ByteString BS.ByteString IO ()
fromOgg = P.for P.cat (\bs -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t ogg - -t raw -c 1 -r 16000 -e signed-integer -L -b 16 -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield oBs)
	
fromPA :: P.Producer BS.ByteString IO ()
fromPA = do
	(i,o,e,h) <- P.liftIO $ runInteractiveCommand "parec --device=drain.monitor --format=s16le --rate=44100 --channels=2"
	P.liftIO $ hClose i
	PB.fromHandle o 

alFormatPipe :: P.Pipe BS.ByteString BS.ByteString IO ()
alFormatPipe =  P.for P.cat (\bs -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t raw -c 1 -r 16000 -e floating-point -L -b 32 - -t raw -c 1 -r 16000 -e signed-integer -L -b 16 -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hFlush i
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield oBs)

otherALFormat :: P.Pipe BS.ByteString BS.ByteString IO ()
otherALFormat = P.for P.cat (\bs -> do
	(i,o,e,pHandle) <- P.liftIO $ runInteractiveCommand "sox -t raw -c 2 -r 44100 -e signed-integer -L -b 16 - -t raw -c 1 -r 16000 -e signed-integer -L -b 16 -"
	P.liftIO $ BS.hPut i bs
	P.liftIO $ hFlush i
	P.liftIO $ hClose i
	oBs <- P.liftIO $ BS.hGetContents o
	P.yield oBs)

getALReady :: IO ()
getALReady = do
	d <- device
	context d
	return ()

alConsumer :: Source -> P.Consumer' BS.ByteString IO ()
alConsumer s = P.for P.cat (\bs -> do
	ss <- P.liftIO $ get $ sourceState s
	b <- P.liftIO $ buff bs
	P.liftIO $ queueBuffers s [b]
	case ss of
		Initial -> 	P.liftIO $ play [s]	
		_ -> P.liftIO $ freeMemory s)

--FIX ME!
freeMemory :: Source -> IO ()
freeMemory s = do
	num <- get $ buffersProcessed s :: IO ALint
	return ()
--	bs <- unqueueBuffers s []
--	mapM_ freeBuff bs
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
	let bd = BufferData (MemoryRegion ptr (fromIntegral $ len)) Mono16 16000
	bufferData b $= bd
	return b
