{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Text.XML.HXT.Core
import Data.List.Split
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Text.XML.HXT.HTTP
import Control.Arrow.ArrowNavigatableTree
import Data.List
import Control.Monad (liftM)

import Network.Curl
import Control.Applicative
--import Database.SQLite.Simple (Connection,open,close,execute,query)
import Database.SQLite.Simple --(Connection,open,close,execute,query)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)

import Network.URI (URI, parseURI, parseRelativeReference, parseAbsoluteURI, parseURIReference, nonStrictRelativeTo,uriToString,uriPath, uriAuthority)
import Network.URI (uriRegName, uriScheme)

import Data.Maybe

-- Local imports:
import UrlCaching
import WikiSpidering
import MJHUri

import Phase01WikipediaSpidering









getCleanCachedUrl :: Connection -> String -> String
getCleanCachedUrl conn url = "CLEAN"




phase02DoCleaning :: IO()
phase02DoCleaning = do
    conn01 <- open "db01urlspidering.db"

    putStrLn "Cleaning URLs"
    validUrls <- getValidURLs conn01

    -- How many we got?
    putStrLn $ show $ length validUrls

    --
    let cleanUrlContents = map (getCleanCachedUrl conn01) validUrls
    putStrLn $ intercalate "\n" cleanUrlContents



    close conn01
    return()











main :: IO ()
main = do

    -- 1/ Spidering and downloading:
    x <- phase01DoSpidering


    -- 2/ Cleaning the HTML
    x <- phase02DoCleaning
    return ()




















cleanXmlFile :: String -> IO ()
cleanXmlFile src = do
          runX $
            readDocument [withValidate no,withParseHTML yes] src
            >>>
            cleanXML
            >>>
            writeDocument [withOutputHTML] dst
          return ()
            where
              dst = (head $ splitOn "." src) ++ "Out.html"










-- spiderOut :: String -> IO String
-- spiderOut srcFile = do
--     putStrLn "Getting URL"
--     (code,htmlbody) <- curlGetString "https://en.wikipedia.org/wiki/List_of_timelines" []
--     putStrLn "Got"
--     case code of
--         CurlOK -> do
--             putStr "JKJ"
--             putStr htmlbody
--             putStr "JKJ"
--         _ -> putStrLn $ "Error" ++ show code
--     putStrLn "Done"
--     return ""









isDroppedDivType :: String -> Bool
isDroppedDivType s = not $ null $ intersect (splitOn " " s)  [
                        "toc"
                        ,"vertical-navbox"
                        ,"mw-navigation"
                        ,"mw-editsection"
                        ,"mw-edit-section"
                        ,"siteSub"
                        ,"reference"
                        ,"thumb"
                        -- Boxes:
                        ,"mbox-small-left"
                        ,"ambox-Cleanup"
                        ,"ambox"
                        ,"infobox"
                        ,"hatnote"
                        ,"reflist"
                        ,"footer"
                        ,"catlinks"
                        ,"Bibliography"
                        ,"refbegin"
                        ,"sistersitebox"
                        ,"navbox"
                        ]


isDroppedTagType :: String -> Bool
isDroppedTagType s = (s `elem` [
                        "script"
                        ,"img"
                        ,"sup"
                        ])






cleanXML  :: ArrowXml a => a XmlTree XmlTree
cleanXML
    = processTopDown (
        ( none `when` isDroppedDivID)
        >>>
        ( none `when` isDroppedDivClass)
        >>>
        ( none `when` isDroppedTag)
        -- >>>
        -- ( none `when` isDroppedSection)
        -- >>>
        -- ( none `when`  (getChildren >>> isElem >>> hasAttr "id" >>> getAttrValue >>> isA (=="jkl") ) )
        --( none `when`  (getChildren >>> isElem >>> hasAttr "id" >>> getAttrValue >>> isA isDroppedDivType ) )
        --( none `when`  (isElem >>> hasAttr "id" >>> getAttrValue >>> isA isDroppedDivType ) )
        )
    where
     -- isExternalRef = isElem >>> hasName "a" >>> hasAttr "href" >>> getAttrValue "href" >>> isA isExtRef
     -- isExtRef = isPrefixOf "http:"
     -- addImg = replaceChildren ( getChildren <+> imgElement)
     -- imgElement = mkelem "img" [ sattr "src" "/icons/ref.png" , sattr "alt" "external ref" ] []
     isDroppedDivID = isElem >>>  hasAttr "id" >>> getAttrValue "id" >>> isA isDroppedDivType
     isDroppedDivClass = isElem >>>  hasAttr "class" >>> getAttrValue "class" >>> isA isDroppedDivType
     isDroppedTag = isElem >>> getName >>> isA isDroppedTagType
     --isDroppedSection = ( getChildren >>> isElem >>> getAttrValue "id" >>> isA (=="External_links")  )



