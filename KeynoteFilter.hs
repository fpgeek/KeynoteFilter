{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Codec.Archive.LibZip
import Text.XML (Name, Node(..), parseLBS_, def)
import Text.XML.Cursor (node, element, fromDocument, Cursor, (&.//), (&/), content)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Word
import Data.Text (Text, concat, unlines)
import Data.Text.IO (putStr)
import Prelude hiding (concat, putStr, unlines)

main :: IO ()
main = do
    (keynoteFile:_) <- getArgs
    xmlStr <- apxlFileContents keynoteFile
    let contents = filteringKeynote xmlStr
    putStr contents

apxlFileContents :: FilePath -> IO ByteString
apxlFileContents keynoteFile = do
    xmlStr <- withArchive [] keynoteFile $ fileContents [] "index.apxl"
    return $ pack xmlStr

filteringKeynote :: ByteString -> Text
filteringKeynote = unlines . filter isNotEmpty . texts
    where isNotEmpty "" = False
          isNotEmpty _  = True

texts :: ByteString -> [Text]
texts = map getContent . paragraphs

getContent :: Cursor -> Text
getContent = concat . (return &.// content)

paragraphs :: ByteString -> [Cursor]
paragraphs = (return &.// element paragraphName) . documentCursor

documentCursor :: ByteString -> Cursor
documentCursor = fromDocument . parseLBS_ def

paragraphName :: Name
paragraphName = "{http://developer.apple.com/namespaces/sf}p"