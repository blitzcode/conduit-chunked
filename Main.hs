
{-# LANGUAGE   ScopedTypeVariables
             , RecordWildCards
             , RankNTypes
             , LambdaCase
             , BangPatterns
             , OverloadedStrings
             , GADTs
             , StandaloneDeriving
             , GeneralizedNewtypeDeriving
             , FlexibleContexts #-}

module Main where

import           Data.Word
--import           Data.Bits
import           Data.IORef
import           Data.Monoid
import           Data.Conduit as C
--import qualified Data.Foldable as F
--import           Text.Printf
--import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.State
--import           Control.Monad.Cont
--import           Control.Monad.ST
--import           Control.Monad.Primitive
--import           Control.Monad.Base
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Blaze
--import qualified Data.Vector as V 
--import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Internal      (ByteString (PS), mallocByteString)
--import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteString.Unsafe        (unsafeIndex)
import qualified Blaze.ByteString.Builder as BB
--import           Control.Monad.IO.Class
--import           System.IO.Unsafe
import           Criterion.Main
import           Criterion.Config
import           Foreign.ForeignPtr.Safe       (ForeignPtr)
import           Foreign.ForeignPtr.Unsafe     (unsafeForeignPtrToPtr)
import           Foreign.Ptr                   (Ptr)
import           Foreign.Storable              (pokeByteOff)

chunkSize :: Int
chunkSize = 65536

oneMBFileName :: String
oneMBFileName = "./test_1m.dat"

outputFileName :: String
outputFileName = "./test_out.dat"

criterionCfg :: Config
criterionCfg = defaultConfig { cfgPerformGC = ljust True
                             , cfgReport    = ljust "./report.html"
                             , cfgSamples   = ljust 10
                             }

-- Chunking by accumulating a list, then reverse and pack it into a bytestring once a
-- chunk is full (using a Data.ByteString.Builder has the same performance)

type OutputBSList = (Int, [Word8])

emptyOutputBSList :: OutputBSList
emptyOutputBSList = (0, [])

outputByteList :: Monad m
               => Word8
               -> OutputBSList
               -> forall i. ConduitM i B.ByteString m OutputBSList
outputByteList w8 obs@(numBytes, _)
    | numBytes >= chunkSize = flushOutputBSList obs >> addW8 emptyOutputBSList
    | otherwise             = addW8 obs
  where
    addW8 (numBytes', xs') =
        return (numBytes' + 1, w8 : xs')

flushOutputBSList :: Monad m => OutputBSList -> forall i. ConduitM i B.ByteString m ()
flushOutputBSList (numBytes, xs)
    | numBytes > 0 = yield (B.pack $ reverse xs)
    | otherwise    = return ()

processAndChunkOutputList :: Monad m => Conduit B.ByteString m B.ByteString
processAndChunkOutputList = flip evalStateT emptyOutputBSList loop 
  where
    loop = (lift await) >>= \case
               Nothing -> get >>= lift . flushOutputBSList
               Just bs -> do forM_ [0..B.length bs - 1] $ \i -> outputByteState (bs `B.index` i)
                             loop
    outputByteState w8 = get >>= (\obs -> lift $ outputByteList w8 obs) >>= put

-- Chunking with Blaze Builder

outputByteBB :: BB.Builder -> Word8 -> BB.Builder
outputByteBB builder w8 = BB.fromWord8 w8 <> builder

processAndChunkOutputBB :: Monad m => Conduit B.ByteString m BB.Builder
processAndChunkOutputBB = CL.map $ B.foldl' outputByteBB mempty

-- Chunking with a raw buffer

data S = S (ForeignPtr Word8) (Ptr Word8) {-# UNPACK #-} !Int

newS :: IO S
newS = do
    fptr <- mallocByteString {- defaultChunkSize -} chunkSize
    return (S fptr (unsafeForeignPtrToPtr fptr) 0)

processChunk :: ByteString -> S -> IO ([ByteString], S)
processChunk input =
    loop id 0
  where
    loop front idxIn s@(S fptr ptr idxOut)
        | idxIn >= B.length input = return (front [], s)
        | otherwise = do
            pokeByteOff ptr idxOut (unsafeIndex input idxIn)
            let idxOut' = idxOut + 1
                idxIn' = idxIn + 1
            if idxOut' >= {- defaultChunkSize -} chunkSize
                then do
                    let bs = PS fptr 0 idxOut'
                    s' <- newS
                    loop (front . (bs:)) idxIn' s'
                else loop front idxIn' (S fptr ptr idxOut')

processAndChunkOutputRaw :: MonadIO m => Conduit ByteString m ByteString
processAndChunkOutputRaw =
    liftIO newS >>= loop
  where
    loop s@(S fptr _ len) = do
        mbs <- await
        case mbs of
            Nothing -> yield $ PS fptr 0 len
            Just bs -> do
                (bss, s') <- liftIO $ processChunk bs s
                mapM_ yield bss
                loop s'

-- Chunk into raw memory, output as ByteString or Vector, same semantics as normal yield
--
-- Extremely slow, guess the overhead from polymorphism and the IORef is to blame

data ChunkBuilder = ChunkBuilder { cbForeignPtr :: ForeignPtr Word8
                                 , cbPtr        :: Ptr Word8
                                 , cbLen        :: IORef Int
                                 , cbChunkSize  :: Int
                                 }

newChunkBuilder :: IO ChunkBuilder
newChunkBuilder = do
    cbForeignPtr <- liftIO $ mallocByteString chunkSize
    cbLen        <- liftIO $ newIORef 0
    let cbPtr       = unsafeForeignPtrToPtr cbForeignPtr
        cbChunkSize = chunkSize
    return $ ChunkBuilder { .. }

yieldChunkBuilderBS :: MonadIO m => ForeignPtr Word8 -> Int -> ConduitM i B.ByteString m ()
yieldChunkBuilderBS fp len
    | len == 0  = return ()
    | otherwise = yield $ PS fp 0 len

yieldChunkBuilderVS :: MonadIO m => ForeignPtr Word8 -> Int -> ConduitM i (VS.Vector Word8) m ()
yieldChunkBuilderVS fp len
    | len == 0  = return ()
    | otherwise = yield $ VS.unsafeFromForeignPtr0 fp len

outputByteCB :: MonadIO m => ChunkBuilder -> (ForeignPtr Word8 -> Int -> m ()) -> Word8 -> m ()
outputByteCB ChunkBuilder { .. } yieldBuilder w8 = do
    idx <- (liftIO $ readIORef cbLen) >>= \len ->
                if   len == cbChunkSize
                then yieldBuilder cbForeignPtr len >> return 0
                else return len
    liftIO $ do pokeByteOff cbPtr idx w8
                writeIORef cbLen $! idx + 1

withBSChunkBuilder :: MonadIO m
                   => ((Word8 -> Producer m B.ByteString) -> ConduitM i B.ByteString m a)
                   -> ConduitM i B.ByteString m a
withBSChunkBuilder f = do
    cb <- liftIO $ newChunkBuilder
    res <- f (outputByteCB cb (yieldChunkBuilderBS))
    yieldChunkBuilderBS (cbForeignPtr cb) =<< (liftIO . readIORef $ cbLen cb)
    return res

withVSChunkBuilder :: MonadIO m
                   => ((Word8 -> Producer m (VS.Vector Word8)) -> ConduitM i (VS.Vector Word8) m a)
                   -> ConduitM i (VS.Vector Word8) m a
withVSChunkBuilder f = do
    cb <- liftIO $ newChunkBuilder
    res <- f (outputByteCB cb (yieldChunkBuilderVS))
    yieldChunkBuilderVS (cbForeignPtr cb) =<< (liftIO . readIORef $ cbLen cb)
    return res

processAndChunkOutputChunkBuilder :: MonadIO m => Conduit ByteString m ByteString
processAndChunkOutputChunkBuilder =
    withBSChunkBuilder $ \yieldByte ->
        awaitForever $ \bs ->
            forM_ [0..B.length bs - 1] $ \i -> yieldByte $ unsafeIndex bs i

-- Similar to the solution above, but this time using a state monad
--
-- Also very slow. Doing the actual loop over the bytes in the conduit monad is just too
-- slow. Needs to be in plain IO to get close to SoL.

data StateChunk = StateChunk { scForeignPtr :: !(ForeignPtr Word8)
                             , scPtr        :: !(Ptr Word8)
                             , scLen        :: !Int
                             , scChunkSize  :: !Int
                             }

{-# INLINE withStateChunkBuilder #-}
withStateChunkBuilder :: MonadIO m
                      => StateT StateChunk (ConduitM i B.ByteString m) a
                      -> ConduitM i B.ByteString m a
withStateChunkBuilder f = do
    fptr <- liftIO $ mallocByteString chunkSize
    (res, finalState) <- runStateT f $ StateChunk { scForeignPtr = fptr
                                                  , scPtr = unsafeForeignPtrToPtr fptr
                                                  , scLen = 0
                                                  , scChunkSize = chunkSize
                                                  }
    yieldStateChunk (scForeignPtr finalState) (scLen finalState)
    return res

{-# INLINE yieldStateChunk #-}
yieldStateChunk :: MonadIO m => ForeignPtr Word8 -> Int -> ConduitM i B.ByteString m ()
yieldStateChunk fp len
    | len == 0  = return ()
    | otherwise = yield $ PS fp 0 len

{-# INLINE yieldByteState #-}
yieldByteState :: MonadIO m => Word8 -> StateT StateChunk (ConduitM i B.ByteString m) ()
yieldByteState w8 = do
    StateChunk { .. } <- get
    idx <- if   scLen == scChunkSize
           then (lift $ yieldStateChunk scForeignPtr scLen) >> return 0
           else return scLen
    liftIO $ pokeByteOff scPtr idx w8
    modify (\x -> x { scLen = idx + 1 })

processAndChunkOutputStateChunkBuilder :: MonadIO m => Conduit ByteString m ByteString
processAndChunkOutputStateChunkBuilder =
    withStateChunkBuilder $ 
        let loop = (lift await) >>= \case
                Just bs -> do --lift $ yield bs
                              forM_ [0..B.length bs - 1] $ \i -> yieldByteState $ unsafeIndex bs i
                              {-
                              fptr <- liftIO $ mallocByteString $ B.length bs
                              let ptr = unsafeForeignPtrToPtr fptr
                              forM_ [0..B.length bs - 1] $ \i -> liftIO $
                                           pokeByteOff ptr i $ unsafeIndex bs i
                              lift . yield . PS fptr 0 . B.length $ bs

                              -- versus

                              fptr <- liftIO $ mallocByteString $ B.length bs

                              liftIO $ forM_ [0..B.length bs - 1] $ \i ->
                                           pokeByteOff ptr i $ unsafeIndex bs i
                              lift . yield . PS fptr 0 . B.length $ bs

                              -- is what makes any of these monadic solutions so slow
                              -}
                              loop
                Nothing -> return ()
         in loop

{-
forceFlush :: Monad m => ConduitM i o m ()
forceFlush = await >>= maybe (return ()) leftover
-}

processAndChunkOutputVectorBuilder :: (MonadBase IO m, MonadIO m)
                                   => Conduit ByteString m (VS.Vector Word8)
processAndChunkOutputVectorBuilder =
    CC.vectorBuilder chunkSize $ \yieldByte -> do
        awaitForever $ \bs ->
            liftIO . forM_ [0..B.length bs - 1] $ \i ->
                yieldByte $ unsafeIndex bs i

vectorToByteString :: MonadIO m => Conduit (VS.Vector Word8) m ByteString
vectorToByteString =
    awaitForever $ \v ->
       let (fptr, offs, len) = VS.unsafeToForeignPtr v
        in yield $ PS fptr offs len

main :: IO ()
main =
    defaultMainWith
        criterionCfg
        (return ())
        [
            bgroup "1M file size"
            [ bench "Speed-of-light" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with list" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputList
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with blaze-builder" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputBB
                                      =$ builderToByteString
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with raw memory buffer" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputRaw
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with chunk builder" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputChunkBuilder
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with state" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputStateChunkBuilder
                                      $$ CB.sinkFile outputFileName
            , bench "Chunking with vectorBuilder" .
                  nfIO . runResourceT  $ CB.sourceFile oneMBFileName
                                      =$ processAndChunkOutputVectorBuilder
                                      =$ vectorToByteString
                                      $$ CB.sinkFile outputFileName

            ]
        ]

-- =$ (awaitForever $ \bs -> (liftIO $ print "BS Chunk") >> yield bs)

