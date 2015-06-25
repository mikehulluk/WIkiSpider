{-# LANGUAGE OverloadedStrings #-}

module Phase01WikipediaSpidering
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






extractLinkUrls :: String -> IO [String]
extractLinkUrls contents = do
          runX $
            readString [withValidate no,withParseHTML yes] contents
            >>>
            deep ( isElem >>> hasName "a" >>> hasAttr "href" >>> getAttrValue "href" )




bSpiderUrl :: URI -> Bool
bSpiderUrl uri =
    let host = maybe "" uriRegName (uriAuthority uri)  in
    if host /= "en.wikipedia.org" then False
    else
        if   ("Timeline_of" `isInfixOf` path)
           ||("Timelines_of" `isInfixOf` path)
           ||("Chronology_of" `isInfixOf` path)
           --  ||("List_of" `isInfixOf` path)
           --  ||("Lists_of" `isInfixOf` path)
           ||("Events_" `isInfixOf` path)
           -- ||("History_of" `isInfixOf` path)
           -- ||("Outline_of" `isInfixOf` path)
           -- ||("Women_in" `isInfixOf` path)
           -- ||("Women_in" `isInfixOf` path)
           ||("_chronology" `isSuffixOf` path)
           ||("_timeline" `isSuffixOf` path)
           ||("_by_year" `isSuffixOf` path)
        then
            if   ("Special:" `isInfixOf` path)
              || ("Category:" `isInfixOf` path)
              || ("Talk:" `isInfixOf` path)
              || ("Template:" `isInfixOf` path)
              || ("Template_talk:" `isInfixOf` path)
            then False
            else True


        else False

    where
        path = uriPath uri
        auth = uriAuthority uri









spiderUrl :: Connection -> String -> IO [String]
spiderUrl conn rootUrl = do
  x <- getCachedUrl conn rootUrl

  case x of
    Just n -> do
        putStr "Found - length="
        putStrLn (show $ length n )


        a <- extractLinkUrls n
        let urls = catMaybes( map (normaliseUrl rootUrl) a )
        let uris = catMaybes( map parseAbsoluteURI urls )
        let spider_uris = sort $ filter (bSpiderUrl) uris
        let spider_uris_ready' = nub spider_uris
        let spider_uris_ready = take 100 spider_uris_ready'
        let spider_uris_ready = spider_uris_ready'


        
        let nUrls = show $ length spider_uris_ready
        putStrLn $ "Adding urls for spidering [" ++ nUrls ++"]"


        -- Add to the list of spidering links:
        let u = map show spider_uris_ready
        --let v = map (addUrlForSpidering conn) u


        -- Add to the list of valid urls links:
        let qqq = mapM (addUrlForSpidering conn) u
        qqqq <- qqq

        let qqq' = mapM (addUrlAsValidTimeline conn) u
        qqqq' <- qqq'

        let t = intercalate "\n" ( map show spider_uris_ready )
        writeFile "FollowedLink.txt" t
        --putStrLn $ t
        putStrLn "Done"
        return []

    Nothing -> do
        putStrLn "Not found"
        return []






runSpidering :: Connection -> IO()
runSpidering conn = do
    url <- getNextUrlToSpider conn
    case url of
        Nothing -> do
            putStrLn "Finished spidering"
            return ()
        (Just url) -> do
            putStrLn ("Spidering over:" ++ url)
            spiderUrl conn url

            -- And recurse:
            runSpidering conn

            return ()



phase01DoSpidering :: IO ()
phase01DoSpidering = do
  conn <- open "db01urlspidering.db"

  -- Spider out:
  -- t <- clearDBSpidering conn
  s <- spiderUrl conn "https://en.wikipedia.org/wiki/List_of_timelines"
  runSpidering conn

  close conn

