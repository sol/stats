module Main where

import           Control.Exception
import qualified Data.ByteString.Lazy as LB

import           Render
import           Run

main :: IO ()
main = LB.getContents >>= either die return . run >>= putStrLn . unlines . renderSections

die :: String -> IO a
die = throwIO . ErrorCall
