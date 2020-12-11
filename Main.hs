
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.URI
import Network.Wreq as Wreq
import Data.Set as Set
import Data.Maybe
import Data.ByteString.Lazy.Char8 as ByteString
import Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Graph as Graph
import Data.List as List
import Text.HTML.TagSoup as TagSoup
import Control.Lens ((^.))
import Data.Array as Array
import Prelude
import Text.Format
import Data.Graph.Inductive.Query.SP as SP
import Data.Graph.Inductive.PatriciaTree as PT
import Control.Monad

data State = State 
    { pending  :: [URI]
    , crawling :: Set URI
    , visited  :: Set URI
    , graph    :: [(URI, [URI])]
    } deriving (Show)

data Config = Config 
    {
      root       :: URI
    , final      :: URI
    , threshold  :: Int
    } 

printPath :: PT.Gr URI Int -> [Node] -> IO()
printPath graph nodes = mapM_ (\x -> print $ fromJust $ Graph.lab graph x) nodes

beginCrawl :: Config -> IO()
beginCrawl config = do
    let state = State {
        pending = [(root config)],
        crawling = Set.empty,
        visited = Set.empty,
        graph = []
    }
    
    newState <- crawler config state

    let (nodes, nm) = NM.mkNodes NM.new $ Prelude.foldr (\(_,hrefs) y -> (++) y hrefs) [] (graph newState)
    let edges = fromJust $ NM.mkEdges nm $ Prelude.foldr (\(uri, hrefs) y -> (++) y (Prelude.map (\z -> (uri,z, 1) ) hrefs )) [] (graph newState) 
    let path = SP.sp (rootNode nodes config) (finalNode nodes config) (g nodes edges)

    printPath (g nodes edges) (fromJust path)

        where 
            g :: [LNode URI] -> [LEdge Int] -> PT.Gr URI Int
            g nodes edges = mkGraph nodes edges

            rootNode :: [LNode URI] -> Config -> Graph.Node
            rootNode nodes config = fst $ fromJust (List.find (\(_,label) -> label == (root config)) nodes)

            finalNode :: [LNode URI] -> Config -> Graph.Node
            finalNode nodes config = fst $ fromJust (List.find (\(_,label) -> label == (final config)) nodes)


crawler :: Config -> State -> IO State
crawler config state = do
    updatedState <- crawlNext state
    if List.null (pending updatedState) || List.length (visited updatedState)  > (threshold config) || List.elem (final config) (visited updatedState) 
        then return updatedState
    else do
        crawler config updatedState

crawlNext :: State -> IO State
crawlNext state = do
    if List.null (pending state)
        then return state
    else do
      let (uri, updatedState) = nextInPending state
      (_, uris) <- crawlDownLink uri
      return $ addToVisited uri uris updatedState

nextInPending :: State -> (URI, State)
nextInPending state = (uri, updatedState)
  where
    uri : rest = (pending state)
    updatedState = State {
          pending = rest,
          crawling = Set.insert uri (crawling state),
          visited = (visited state),
          graph = (graph state)
    }

addToVisited :: URI -> Set.Set URI -> State -> State
addToVisited uri crawled state = 
      State {
        visited = Set.insert uri (visited state),
        pending = (pending state) ++ List.filter (isLinkNew state) (Set.toList crawled),
        crawling = Set.delete uri (crawling state),
        graph = (graph state) ++ [(uri, Set.toList crawled)]
      }

isLinkNew :: State -> URI -> Bool
isLinkNew state uri =
  List.notElem uri (visited state) &&
  List.notElem uri (pending state) &&
  List.notElem uri (crawling state)

crawlDownLink :: URI -> IO(URI, Set.Set URI)
crawlDownLink uri = do
    html <- retrieveHTML uri
    crawledLinks <- scrapeHrefs uri html
    return (uri, crawledLinks)

retrieveHTML :: URI -> IO ByteString.ByteString
retrieveHTML uri = do
    r <- Wreq.get $ show uri
    return $ r ^. responseBody

scrapeHrefs :: URI -> ByteString.ByteString -> IO (Set.Set URI)
scrapeHrefs uri body = do
    return $ Set.fromList filteredLinks
    where
        isOutgoing link = uriPath uri /= uriPath link
        sameDomain link = fromMaybe False (fromSameDomain uri link)
        tags = TagSoup.parseTags body
        as = List.filter (TagSoup.isTagOpenName "a") tags
        hrefs = List.map (TagSoup.fromAttrib "href") as
        links = mapMaybe (toAbsoluteLink uri) hrefs
        filteredLinks = List.filter (\link -> isOutgoing link && sameDomain link) links

fromSameDomain :: URI -> URI -> Maybe Bool
fromSameDomain one two = do
  authOne <- uriAuthority one
  authTwo <- uriAuthority two
  return (uriRegName authOne == uriRegName authTwo)

toAbsoluteLink :: URI ->  ByteString.ByteString -> Maybe URI
toAbsoluteLink root linkByteString = do
    link <- parseURIReference (ByteString.unpack linkByteString)
    return (link {uriQuery = ""} `relativeTo` root)

main :: IO()
main = do
    beginCrawl configuration
    where
        configuration = Config {
            root = fromJust $ parseURIReference "https://en.wikipedia.org/wiki/Cat",
            final = fromJust $ parseURIReference "https://en.wikipedia.org/wiki/Adolf_Hitler",
            threshold = 100
        }