{-# LANGUAGE OverloadedStrings #-}

module MJHUri
where

import Network.URI (URI, parseURI, parseRelativeReference, parseAbsoluteURI, parseURIReference, nonStrictRelativeTo,uriToString,uriPath, uriAuthority)
import Network.URI (uriRegName, uriScheme)






myUriToString :: URI -> String
myUriToString uri  =
    (uriScheme uri ) ++ "//" ++ hostname ++ (uriPath uri)
    where
        hostname = maybe "" uriRegName (uriAuthority uri)



normaliseUrl':: String -> String ->  Maybe URI
normaliseUrl' rootUrl url =
    case (parseAbsoluteURI url, parseAbsoluteURI rootUrl, parseRelativeReference url ) of
        -- We have an absolute URL?
        ((Just uri), _, _) -> (Just uri)
        -- We have an relative URL?
        ( Nothing, (Just rootUri), (Just relativeUri)) -> (Just (relativeUri `nonStrictRelativeTo` rootUri ) )
        otherwise -> Nothing


normaliseUrl:: String -> String ->  Maybe String
normaliseUrl rootUrl url =
    case normaliseUrl' rootUrl url of
        Nothing -> Nothing
        Just uri -> (Just (myUriToString uri))


