{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Codec.Archive.LibZip
import Text.XML (Name, Node(..), parseLBS_, def)
import Text.XML.Cursor (node, element, fromDocument, Cursor, (&.//), (&/), content)
import qualified Data.ByteString.Lazy as L (ByteString, pack)
import qualified Data.Text as T (Text, concat, null, snoc, empty)
import qualified Data.Text.IO as TIO (putStrLn)
import Control.Monad
import qualified Data.List as L (intersperse)

main :: IO ()
main = do
    (keynoteFile:_) <- getArgs
    xmlStr <- apxlFileContents keynoteFile
    let contentsPerSlide = filteringKeynote xmlStr
        printResults = zip [1..] contentsPerSlide
    mapM_ printContent printResults

printContent :: (Int, T.Text) -> IO ()
printContent (num, text) = do
    putStrLn ("[SLIDE " ++ (show num) ++ "]")
    TIO.putStrLn text

apxlFileContents :: FilePath -> IO L.ByteString
apxlFileContents keynoteFile = do
    xmlStr <- withArchive [] keynoteFile $ fileContents [] "index.apxl"
    return $ L.pack xmlStr

filteringKeynote :: L.ByteString -> [T.Text]
filteringKeynote = map texts . slideCursors
    where slideCursors = slides . documentCursor

texts :: Cursor -> T.Text
texts = T.concat . map getContent . paragraphs

getContent :: Cursor -> T.Text
getContent c | not $ T.null text = text `T.snoc` '\n'
             | otherwise         = text
    where text = (T.concat $ (return &.// content) c)

slides :: Cursor -> [Cursor]
slides = (return &.// element slideName)

paragraphs :: Cursor -> [Cursor]
paragraphs = (return &.// element paragraphName)

documentCursor :: L.ByteString -> Cursor
documentCursor = fromDocument . parseLBS_ def

paragraphName :: Name
paragraphName = "{http://developer.apple.com/namespaces/sf}p"

slideName :: Name
slideName = "{http://developer.apple.com/namespaces/keynote2}slide"