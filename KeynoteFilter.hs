{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Codec.Archive.LibZip
import Text.XML (Name, Node(..), parseLBS_, def)
import Text.XML.Cursor (node, element, fromDocument, Cursor, (&.//), (&/), content)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Word
import Data.Text (Text, concat)
import Data.Text.IO (putStrLn)
import Prelude hiding (concat, putStrLn)

main :: IO ()
main = do
    (keynoteFile:_) <- getArgs
    xmlStr <- apxlFileContents keynoteFile
    let contents = filteringKeynote xmlStr
    putStrLn contents

apxlFileContents :: FilePath -> IO ByteString
apxlFileContents keynoteFile = do
    xmlStr <- withArchive [] keynoteFile $ fileContents [] "index.apxl"
    return $ pack xmlStr

filteringKeynote :: ByteString -> Text
filteringKeynote = concat . texts

texts :: ByteString -> [Text]
texts = (return &.// element paragraphName &.// content) . documentCursor

documentCursor :: ByteString -> Cursor
documentCursor = fromDocument . parseLBS_ def

paragraphName :: Name
paragraphName = "{http://developer.apple.com/namespaces/sf}p"