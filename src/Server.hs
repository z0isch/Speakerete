module Server where

import Pipes
--import qualified Pipes.Prelude as P
--import qualified Pipes.ByteString as PB
import qualified Pipes.Network.TCP as PTCP
--import qualified Pipes.Binary as PBinary
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA
--import Pipes.Core
--import Pipes.Lift
import Network.Socket
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import WavePacket
import Data.Time.Clock
--import Data.Time
import qualified Control.Concurrent as C 
import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.State.Strict
--import qualified Control.Monad.Catch as MC
import OpenALTest
--import qualified Data.Binary.Put as Put
import qualified Data.Binary as DB
--import qualified Sound.OpenAL as OA

type NumBytes = Int
data PacketState = PacketState [Socket] NumBytes UTCTime
data Packet = Packet [Socket] WavePacket
data EncodedPacket = EncodedPacket [Socket] BSL.ByteString

setupPackets :: TVar [Socket] -> Int -> Int -> Pipe BS.ByteString Packet IO ()
setupPackets clientsTVar byteLimit seconds = do
    initialTime <- liftIO getCurrentTime
    clients <- liftIO $ readTVarIO clientsTVar
    flip evalStateT (PacketState clients 0 initialTime) $ forever $ do
	bs <- lift await
	PacketState cs numBytes _ <- get
	diffCs <- liftIO $ readTVarIO clientsTVar
	let newNum = numBytes + BS.length bs
	let clientChange = length diffCs /= length cs
	if clientChange
	then do
            liftIO $ print "New Client!"
	    newTime <- liftIO getCurrentTime
	    put $ PacketState diffCs 0 newTime
	    lift $ yield $ Packet diffCs $ WavePacket (addUTCTime (fromIntegral seconds) newTime) bs
	else if newNum < byteLimit
	then do
	    liftIO $ print "Clients not playing yet"
	    put $ PacketState diffCs newNum initialTime
	    lift $ yield $ Packet diffCs $ WavePacket (addUTCTime (fromIntegral seconds) initialTime) bs
	else do
	    liftIO $ print "Clients playing."
	    newTime <- liftIO getCurrentTime
	    put $ PacketState diffCs byteLimit newTime
	    lift $ yield $ Packet diffCs $ WavePacket newTime bs

toManySockets :: Consumer' EncodedPacket IO ()
toManySockets = for cat (\(EncodedPacket socks bs) ->
    liftIO $ mapM_ (`PTCP.sendLazy` bs) socks)


encodePacket :: Pipe Packet EncodedPacket IO ()
encodePacket = for cat (\(Packet socks wp) ->
    yield $ EncodedPacket socks (DB.encode wp))

serverToClients :: Socket -> TVar [Socket] -> IO ()
serverToClients incomming tvar = runEffect $ PTCP.fromSocket incomming 4096 >-> setupPackets tvar (32000*5) 5 >-> encodePacket >-> toManySockets

addClient :: TVar [Socket] -> IO ()
addClient tvar = do
    (s,_) <- PTCP.bindSock (PTCP.Host "localhost") "11004"
    listen s 4096
    forever $ do
        (cS,_) <- accept s
	atomically $ modifyTVar tvar (\sockets -> sockets ++ [cS])

mainServer :: IO ()
mainServer = do
    clients <- newTVarIO []
    _ <- C.forkIO $ addClient clients
    PTCP.connect "10.1.2.47" "11000" $ \(connectionSocket, _) ->
	serverToClients connectionSocket clients

mainClient :: IO ()
mainClient = do
    (alSource,buffs) <-getALReady
    PTCP.connect "localhost" "11004" (\(s,_) -> process alSource buffs $ PTCP.fromSocket s 4096)
    where
    process alSource buffs p = do
    	(result,rest) <- PP.runStateT (PA.parse parseWavePacket) p
	runEffect $ getWavePacket result >-> alFormatPipe >-> alConsumer alSource buffs
        process alSource buffs rest
