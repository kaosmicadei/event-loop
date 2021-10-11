{-# LANGUAGE RecordWildCards, BangPatterns #-}

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Random


data Any
    = AnyNull
    | AnyInt Int
    | AnyDouble Double
    | AnyChar Char 
    | AnyString String 
    | AnyList [Any]
  deriving Show

data Event
    = Close
    | Event
        { func     :: IO Any
        , callback :: Any -> IO Any 
        }


main :: IO ()
main = do

    taskQueue <- atomically $ newTChan
    backgroundChannel <- atomically $ newTChan

    let poolSize = 2

    concurrently_
        (mainThread poolSize taskQueue backgroundChannel)
        (background poolSize backgroundChannel taskQueue)


mainThread :: Int -> TChan (Maybe Any) -> TChan Event -> IO ()
mainThread n receiver sender =
    concurrently_
        (send `finally` replicateM_ n close)
        (get n)
  where
    close = atomically . writeTChan sender $ Close
    
    send = do
        
        putStrLn "Try add 2 and 2"
        event (pure $ AnyInt $ 2 + 2) pure
        
        putStrLn "Try multiply 3 and 4"
        event (pure $ AnyInt $ 3 * 4) pure
        
        putStrLn "Try subtract 5 and 3"
        event (pure $ AnyInt $ 5 - 3) pure
        
        putStrLn "Try divide 10 and 2 then subtract 2"
        event (pure $ AnyInt $ 10 `div` 2) $ \(AnyInt x) -> do
            pure $ AnyInt (x - 2)
            
    event f c = atomically . writeTChan sender $ Event f c
    
    get 0 = pure ()
    get n = do
        result <- atomically $ readTChan receiver
        case result of

            Nothing ->
                get (n-1)
            
            Just x -> do
                print x
                get n


background :: Int -> TChan Event -> TChan (Maybe Any) -> IO ()
background n receiver sender = do
    foldr concurrently_ (pure ()) $ replicate n loop
  where
    loop = do
        task <- atomically $ readTChan receiver
        case task of

            Close -> 
                atomically $ writeTChan sender Nothing

            Event{..} -> do
                !result <- func >>= callback
                r <- randomRIO (1,4) :: IO Int
                threadDelay $ r * 1000000     -- ^ To simulate a cost of computation
                atomically $ writeTChan sender (Just result)
                loop
