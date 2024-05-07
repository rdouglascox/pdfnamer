{-# LANGUAGE OverloadedStrings #-}

module BibEntries (filestoentries) where 

-- this module deals with the task of reading a list of bib files 
-- and converting them into a list of bib entries 

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as ByteString

import qualified Text.BibTeX.Entry as BibEntry
import qualified Text.BibTeX.Parse as BibParse

import Text.Parsec 
import Data.Either

-- here's how we read a utf8 encoded bib file to text
filetotext :: FilePath -> IO Text.Text
filetotext f = do 
    c <- ByteString.readFile f 
    return (TextEncoding.decodeUtf8 c) 

-- we want to break the file into entries to parse 
-- we just break it into paragraphs 

splittext :: Text.Text -> [Text.Text] 
splittext = Text.splitOn "\n\n" 

parseentry :: Text.Text -> Either ParseError BibEntry.T 
parseentry t = runParser BibParse.entry () "" (Text.unpack t) 

parseentries :: [Text.Text] -> [BibEntry.T]   
parseentries = rights . map parseentry

filetoentry :: FilePath -> IO [BibEntry.T]
filetoentry f = do
   t <- filetotext f 
   let tt = splittext t 
   putStrLn ("Entries to parse: " ++ show (length tt))
   let e = parseentries tt 
   putStrLn ("Entries parsed: " ++ show (length e))
   return e

filestoentries :: [FilePath] -> IO [BibEntry.T]
filestoentries fs = concat <$> mapM filetoentry fs 



