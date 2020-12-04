module Main where

import Text.HTML.Scalpel
import Data.Maybe
import Data.Graph
import Data.List.Unique

type Link = String

main :: IO ()
main = do
    let links = allLinks "http://reddit.com"
    pureLinks <- links
    print $ fromMaybe [] pureLinks

allLinks :: Link -> IO (Maybe [Link])
allLinks uri = prepareLinks uri (removeDuplicates $ scrapeURL uri links)
    where
       links :: Scraper String [Link]
       links = chroots (anySelector) href

       href :: Scraper String Link
       href = do
           url <- attr "href" $ tagSelector "a"
           return $ url

removeDuplicates :: IO (Maybe [Link]) -> IO (Maybe [Link])
removeDuplicates input = do
    pureInput <- input
    let justInput = fromMaybe [] pureInput
    return (Just (uniq justInput))

prepareLinks :: Link -> IO (Maybe [Link]) -> IO (Maybe [Link])
prepareLinks current input = do
    pureInput <- input
    let justInput = fromMaybe [] pureInput
    return (Just (fixURLS current justInput))

fixURLS :: Link -> [Link] -> [Link]
fixURLS current input = map (\x -> if (head x) == '/' 
                                        then current++x
                                    else if (head x) == '#'
                                        then current++('/':x)
                                    else x) input

