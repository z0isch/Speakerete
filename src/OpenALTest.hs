module OpenALTest where

import System.IO
import System.Process
import qualified Data.ByteString as BS
import Foreign.Marshal.Alloc
import Sound.OpenAL
import qualified Pipes as P
--import qualified Pipes.Lift as L
--import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB
import qualified Control.Monad.State.Strict as ST
import Control.Monad
import qualified Control.Concurrent as C
import WavePacket
import Data.Time
--import Data.Time.Clock
--import Data.Time.Format
import Control.Concurrent.STM

newtype OAByteString = OAByteString BS.ByteString
newtype OggByteString = OggByteString BS.ByteString
newtype OAWavePacket = OAWavePacket WavePacket
                       deriving (Show)
newtype OggWavePacket = OggWavePacket WavePacket

data TestPacket a = TestPacket WavePacket
data OggPacket
data OpenALPacket
data PulseAudioPacket

tP :: TestPacket OggPacket
tP = undefined

toCommand :: String -> BS.ByteString -> IO BS.ByteString
toCommand cmd bs = do
    (i,o,_,_) <- runInteractiveCommand cmd
    BS.hPut i bs
    hFlush i
    hClose i
    BS.hGetContents o

toOgg :: P.Pipe OAByteString OggByteString IO ()
toOgg =  P.for P.cat (\(OAByteString bs) -> do
    oBs <- P.liftIO $ toCommand "sox -t raw -c 2 -r 44100 -e signed-integer -L -b 16 - -t ogg -" bs
    P.yield $ OggByteString oBs)

fromOgg :: P.Pipe OggByteString OAByteString IO ()
fromOgg = P.for P.cat (\(OggByteString bs) -> do
    oBs <- P.liftIO $ toCommand "sox -t ogg - -t raw -c 2 -r 44100 -e signed-integer -L -b 16 -" bs
    P.yield $ OAByteString oBs)

fromPA :: P.Producer OAByteString IO ()
fromPA = do
    --_ <- P.liftIO $ C.forkOS $ void $ system "pulseaudio"
    (i,o,_,_) <- P.liftIO $ runInteractiveCommand "parec --device=null.monitor --format=s16le --rate=44100 --channels=2"
    P.liftIO $ hClose i
    P.for (PB.fromHandle o) (P.yield . OAByteString)

alFormatPipe :: P.Pipe WavePacket OAWavePacket IO ()
alFormatPipe =  P.for P.cat (\(WavePacket t bs) -> do
    oBs <- P.liftIO $ toCommand "sox -t raw -c 1 -r 16000 -e floating-point -L -b 32 - -t raw -c 2 -r 44100 -e signed-integer -L -b 16 -" bs
    P.yield $ OAWavePacket (WavePacket t oBs))

getALReady :: IO (Source, TVar [Buffer])
getALReady = do
  d <- device
  _ <- context d []
  alSource <- source
  buffs <- newTVarIO []
  C.forkIO $ freeMemoryLoop alSource buffs 1000000
  errors <- alcErrors d
  print errors
  return (alSource, buffs)

timeDiff :: UTCTime -> UTCTime -> Int
timeDiff t1 t2 = floor (toRational (diffUTCTime t1 t2) * 1000000)

delayedPlay :: WavePacket -> Source -> IO ()
delayedPlay (WavePacket t _) s = do
    ss <- get $ sourceState s
    curr <- getCurrentTime
    C.threadDelay $ timeDiff t curr
    case ss of
        Playing -> return ()
        _ -> play [s]

alConsumer :: Source -> TVar [Buffer] -> P.Consumer OAWavePacket IO ()
alConsumer s buffTVar = P.for P.cat (\(OAWavePacket wp@(WavePacket _ bs)) -> do
    b <- P.liftIO $ buff $ OAByteString bs
    P.liftIO $ queueBuffers s [b]
    P.liftIO $ atomically $ modifyTVar' buffTVar (\buffs -> buffs ++[b])
    _ <- P.liftIO $ C.forkIO $ delayedPlay wp s
    return ())

freeMemoryLoop :: Source -> TVar [Buffer] -> Int -> IO ()
freeMemoryLoop s buffTVar sec = do
  num <- liftM fromIntegral $ get $ buffersProcessed s
  usedBuffs <- atomically $ do
    buffs <- readTVar buffTVar
    writeTVar buffTVar (drop num buffs)
    return $ take num buffs
  freeMemory s usedBuffs
  C.threadDelay sec
  freeMemoryLoop s buffTVar sec

freeMemory :: Source -> [Buffer] -> IO ()
freeMemory s buffs = do
--    unless (null buffs) $ print $ "Freeing " ++ show (length buffs) ++ " buffers"
    _ <- unqueueBuffers s $ fromIntegral $ length buffs
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
	Just d -> return d

context :: Device -> [ContextAttribute]-> IO Context
context dev attrs = do
    ctx <- createContext dev attrs
    case ctx of
        Nothing -> error "no ctx"
	Just c -> do
	    currentContext $= Just c
	    processContext c
	    return c

source :: IO Source
source = liftM head $ genObjectNames 1

buff :: OAByteString -> IO Buffer
buff (OAByteString bs) = do
    (ptr,len) <- BS.useAsCStringLen bs return
    b <- liftM head $ genObjectNames 1 :: (IO Buffer)
    let bd = BufferData (MemoryRegion ptr (fromIntegral len)) Stereo16 44100
    bufferData b $= bd
    return b
