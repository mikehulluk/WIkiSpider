{-# LANGUAGE OverloadedStrings #-}

module UrlCaching 

where


import Network.Curl
import Control.Applicative 
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)






data CachedUrl = CachedUrl Int String String deriving (Show)
instance FromRow CachedUrl where
    fromRow = CachedUrl <$> field <*> field <*> field

putCachedUrlDB :: Sql.Connection -> String -> String-> IO ()
putCachedUrlDB conn url contents= do
    Sql.execute conn "INSERT INTO raw_downloads (url,contents) VALUES (?,?)" (url :: String, contents:: String)

getCachedUrlDB :: Sql.Connection -> String -> IO (Maybe String)
getCachedUrlDB conn url = do
    r <- Sql.query conn "SELECT * from raw_downloads where url=?" (Sql.Only (url::String))  :: IO [CachedUrl]
    let l = length r
    case l of
        0 -> do
            --putStrLn "Couldn't find"
            return Nothing
        _ -> do
            let (CachedUrl i url contents) = head r
            --putStrLn "Found:"
            return (Just contents)



getCachedUrl :: Sql.Connection -> String -> IO (Maybe String)
getCachedUrl conn url = do
    --putStrLn ("Looking up in cache:" ++ url)
    inDB <- getCachedUrlDB conn url
    case inDB of
        Just x -> return (Just x)
        Nothing -> do
            putStrLn "Fetching URL..."
            (code,htmlbody) <- curlGetString url []
            case code of
                CurlOK -> do
                    putStrLn "Found good url..."
                    putCachedUrlDB conn url htmlbody
                    return (Just htmlbody)
                _ -> do
                    putStrLn "Couldn't download URL"
                    return Nothing
            putStrLn "Fetched"
            return Nothing

