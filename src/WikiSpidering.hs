{-# LANGUAGE OverloadedStrings #-}

module WikiSpidering
where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)


data SpideringScheduledEntry = SpideringScheduledEntry Int String deriving (Show)
data SpideringCompletedEntry = SpideringCompletedEntry Int String deriving (Show)
instance FromRow SpideringScheduledEntry where
  fromRow = SpideringScheduledEntry <$> field <*> field
instance FromRow SpideringCompletedEntry where
  fromRow = SpideringCompletedEntry <$> field <*> field

data ValidUrlEntry = ValidUrlEntry Int String deriving (Show)
instance FromRow ValidUrlEntry where
  fromRow = ValidUrlEntry <$> field <*> field


addUrlForSpidering :: Connection -> String -> IO ()
addUrlForSpidering conn url = do
    -- putStrLn ("Adding URL for spidering: " ++ url)
    r <- query conn "SELECT * from spidering_completed where url=?" (Only (url::String)) :: IO [SpideringCompletedEntry]
    if (length r == 1) then do
        -- putStrLn ("<Already spidered> " ++ url)
        return ()
    else do
        s <- query conn "SELECT * from spidering_scheduled where url=?" (Only (url::String)) :: IO [SpideringScheduledEntry]
        if length s == 1 then do
            -- putStrLn ("<Already scheduled> " ++ url)
            return ()
        else do
            -- putStrLn "<Adding URL>"
            t <- execute conn "INSERT INTO spidering_scheduled (url) VALUES (?)" (Only(url :: String)) -- :: IO [SpideringScheduledEntry]
            return ()



addUrlAsValidTimeline :: Connection -> String -> IO ()
addUrlAsValidTimeline conn url = do
    r <- query conn "SELECT * from valid_timeline_urls where url=?" (Only (url::String)) :: IO [ValidUrlEntry]
    if (length r == 1) then do
        -- putStrLn ("<Already spidered> " ++ url)
        return ()
    else do
        t <- execute conn "INSERT INTO valid_timeline_urls (url) VALUES (?)" (Only(url :: String)) -- :: IO [SpideringScheduledEntry]
        return ()



getNextUrlToSpider :: Connection -> IO (Maybe String)
getNextUrlToSpider conn = do
    s <- query_ conn "SELECT * from spidering_scheduled LIMIT 1" :: IO [SpideringScheduledEntry]
    if length s == 0 then return Nothing
    else do
        let t = head s
        let (SpideringScheduledEntry id url) = t
        u <- query conn "DELETE from spidering_scheduled where id=?" (Only(id::Int) )  :: IO [SpideringScheduledEntry]
        putStrLn (" >> Found next url: " ++ url)
        t <- execute conn "INSERT INTO spidering_completed (url) VALUES (?)" (Only(url :: String)) -- :: IO [SpideringScheduledEntry]

        return (Just url)



clearDBSpidering :: Connection -> IO ()
clearDBSpidering  conn = do
  t  <- query_ conn "DELETE from spidering_scheduled"  :: IO [SpideringScheduledEntry]
  t' <- query_ conn "DELETE from spidering_completed"  :: IO [SpideringCompletedEntry]
  t' <- query_ conn "DELETE from valid_timeline_urls"  :: IO [SpideringCompletedEntry]
  return ()



getValidURLs :: Connection -> IO ([String])
getValidURLs conn = do
    s <- query_ conn "SELECT * from valid_timeline_urls"  :: IO [ValidUrlEntry]
    let t = map extractUrl s
    return t
  where
    extractUrl (ValidUrlEntry _ url) = url

