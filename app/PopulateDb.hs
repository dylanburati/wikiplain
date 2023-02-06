{-# LANGUAGE OverloadedStrings #-}
module Main where

import Numeric (readDec)
import PhpUpper (phpUpper)
import GHC.Int (Int64)
import System.Environment (getArgs)
import Data.Char (isLetter, isDigit)
import Data.Functor ((<&>))
import Data.List (find, findIndex, mapAccumL)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.XML.Types
    ( Content(ContentText),
      Event(EventEndElement, EventBeginElement, EventContent),
      Name )
import Text.XML.Stream.Parse (def)
import qualified Text.XML.Stream.Parse as P
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as CR
import Data.Acquire (mkAcquire, withAcquire)
import Data.Conduit ((.|), runConduit, runConduitRes, ConduitT)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as AT
import Database.SQLite3 (SQLData (..))
import qualified Database.SQLite3 as QL

data WikiPage = WikiPage
  { title :: Text
  , ns :: Int64
  , pageId :: Int64
  , redirect :: Maybe Text
  , text :: Text
  } deriving Show

pageMeta :: WikiPage -> String
pageMeta WikiPage {title = t, ns = n, pageId = i} = show (t,n,i)

isXMLSpace :: Char -> Bool
isXMLSpace ' '  = True
isXMLSpace '\t' = True
isXMLSpace '\r' = True
isXMLSpace '\n' = True
isXMLSpace _    = False

isPrefixOfCI :: Text -> Text -> Bool
isPrefixOfCI t1 t2 =
  T.isPrefixOf (T.toCaseFold t1) (T.toCaseFold t2)

isListPrefix :: (Eq a) => [a] -> [a] -> Bool
isListPrefix [] _ = True
isListPrefix _ [] = False
isListPrefix (p1:prest) (v1:vrest)
  | p1 == v1 = isListPrefix prest vrest
  | otherwise = False

capitalizeFirst :: Text -> Text
capitalizeFirst t =
  (T.map phpUpper (T.take 1 t)) <> (T.drop 1 t)

-- ========
-- BUILDER
-- ========

data WikiPagePartial = WikiPagePartial
  { title' :: [Text]
  , ns' :: Maybe Int64
  , pageId' :: Maybe Int64
  , redirect' :: Maybe Text
  , text' :: [Text]
  } deriving Show

type XMLPath = [Name]
type WikiPageBuilder = (XMLPath, WikiPagePartial)

emptyPagePartial :: WikiPagePartial
emptyPagePartial = WikiPagePartial
  { title' = []
  , ns' = Nothing
  , pageId' = Nothing
  , redirect' = Nothing
  , text' = []
  }

readDecHelper :: Text -> Int64
readDecHelper txt =
  case readDec (T.unpack txt) of
    [] -> -1
    ((x,_):_) -> x

onEvent :: Event -> WikiPageBuilder -> (WikiPageBuilder, [WikiPage])
onEvent evt bldr = case evt of
  (EventBeginElement name attrs) ->
    let path = name:(fst bldr)
        wp = onBegin path (snd bldr) attrs
    in ((path,wp), [])
  (EventContent (ContentText v)) ->
    let wp = onContent (fst bldr) (snd bldr) v
    in ((fst bldr,wp),[])
  (EventEndElement _) ->
    let (wp, res) = onEnd (fst bldr) (snd bldr)
        path = drop 1 (fst bldr)
    in ((path,wp), res)
  _ -> (bldr, [])

onBegin :: XMLPath -> WikiPagePartial -> [(Name, [Content])] -> WikiPagePartial
onBegin ["redirect","page"] wp attrs =
  case find titleAttr attrs of
    Just (_, contents) -> wp {redirect' = Just (toTitle contents)}
    _ -> wp
  where
    titleAttr ("title",_) = True
    titleAttr _ = False
    toTitle = capitalizeFirst . T.concat . (map xContent)
    xContent (ContentText v) = v
    xContent _ = ""
onBegin _ wp _ = wp

onContent :: XMLPath -> WikiPagePartial -> Text -> WikiPagePartial
onContent ["title","page"] wp v =
  wp {title' = (v:(title' wp))}
onContent ["ns","page"] wp v =
  let ns_ = readDecHelper $ T.dropWhile isXMLSpace v
  in wp {ns' = Just ns_}
onContent ["id","page"] wp v =
  let id_ = readDecHelper $ T.dropWhile isXMLSpace v
  in wp {pageId' = Just id_}
onContent ["text","revision","page"] wp v =
  wp {text' = (v:(text' wp))}
onContent _ wp _ = wp

onEnd :: XMLPath -> WikiPagePartial -> (WikiPagePartial, [WikiPage])
onEnd ["page"] WikiPagePartial { title' = (t:trest)
                                , ns' = Just n
                                , pageId' = Just i
                                , redirect' = r
                                , text' = txrev
                                } = do
  let tx = T.concat (reverse txrev)
  let ttl = capitalizeFirst $ T.dropAround isXMLSpace $ T.concat (reverse (t:trest))
  let pg = WikiPage { title = ttl
                    , ns = n
                    , pageId = i
                    , redirect = r
                    , text = tx
                    }
  (emptyPagePartial,[pg])

onEnd ["page"] _ = (emptyPagePartial,[])
onEnd _ wp = (wp,[])

bindingsql :: Int -> Int -> Text
bindingsql nrows ncols =
  T.intercalate "," $
    replicate nrows $
    "(" <> (T.intercalate "," (replicate ncols "?")) <> ")"

pagesql :: WikiPage -> [SQLData]
pagesql WikiPage {title = t, ns = n, pageId = i, redirect = r, text = tx} =
  [ SQLInteger i
  , SQLInteger n
  , SQLText t
  , maybe SQLNull SQLText r
  , maybe (SQLText tx) (const SQLNull) r
  ]

main :: IO ()
main = do
  xmlFile <- getArgs <&> head
  let instext = "INSERT INTO wiki_article (id,ns,title,redirect,text) VALUES "
  let chunkSz = 250

  withAcquire (mkAcquire (QL.open "../enwikitest.db") QL.close) $ \db ->
    withAcquire (mkAcquire (QL.prepare db (instext <> bindingsql chunkSz 5)) QL.finalize) $ \inserter ->
      runConduitRes $
        CC.sourceFile xmlFile
        .| P.parseBytes def
        .| CC.concatMapAccum onEvent ([],emptyPagePartial)
        .| CC.conduitVector chunkSz
        .| CC.mapM_ (\vec -> do
            let records = V.toList vec
            liftIO $ do
              stmt <- if (length records) == chunkSz
                then return inserter
                else QL.prepare db (instext <> bindingsql (length records) 5)
              final <- if (length records) == chunkSz
                then return (return . const ())
                else return QL.finalize

              _ <- QL.bind stmt $ concatMap pagesql records
              (QL.stepNoCB stmt >> return ()) `finally` (QL.reset stmt >> final stmt)
        )
