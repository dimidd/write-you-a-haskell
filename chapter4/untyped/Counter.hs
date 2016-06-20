module Counter where
import Data.IORef

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = makeOffsetCounter 0    

makeOffsetCounter :: Int -> IO Counter
makeOffsetCounter n = do
  r <- newIORef n
  return (\i -> do modifyIORef r (+i)
                   readIORef r)
