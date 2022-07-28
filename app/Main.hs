{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe)
import Data.XML.Types
import Text.XML.Stream.Parse (def)
import qualified Text.XML.Stream.Parse as P
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), runConduit, ConduitT)
import qualified Control.Monad.Trans.Resource as CR

data WikiPage = WikiPage Text (Maybe Text) Text deriving Show
type WikiPageListBuilder = ([WikiPage], Maybe Text, Maybe Text, [Text], [Name])

-- 0: pre <title>
-- 1: content <title>
-- 2: post </title> & pre <redirect>
-- 3: unused; content <redirect>
-- 4: post </redirect> & pre <revision>
-- 5: content <revision> & pre <text>
-- 6: content <text>
-- 7: content <revision> & post </text>
-- 8: post </revision>

setTitle :: WikiPageListBuilder -> Text -> WikiPageListBuilder
setTitle (acc,_,r,t,path) v = (acc,Just v,r,t,path)

setRedirect :: WikiPageListBuilder -> Text -> WikiPageListBuilder
setRedirect (acc,n,_,t,path) v = (acc,n,Just v,t,path)

addText :: WikiPageListBuilder -> Text -> WikiPageListBuilder
addText (acc,n,r,tail,path) v = (acc,n,r,(v:tail),path)

pushToXpath :: WikiPageListBuilder -> Name -> WikiPageListBuilder
pushToXpath (acc,n,r,t,path) started = (acc,n,r,t,started:path)

popFromXpath :: WikiPageListBuilder -> WikiPageListBuilder
popFromXpath (acc,n,r,t,ended:path) = (acc,n,r,t,path)
popFromXpath (acc,n,r,t,[]) = (acc,n,r,t,[]) -- todo: place under MonadThrow ctx to throw here

onEvent :: WikiPageListBuilder -> Event -> WikiPageListBuilder
onEvent bldr evt = case evt of
  (EventBeginElement name attrs) -> onBegin attrs (pushToXpath bldr name)
  (EventContent (ContentText v)) -> onContent v bldr
  (EventEndElement _) -> popFromXpath (onEnd bldr)
  _ -> bldr

onBegin :: [(Name, [Content])] -> WikiPageListBuilder -> WikiPageListBuilder
onBegin [] bldr@(_,_,_,_,["redirect","page"]) = bldr
onBegin (("title", [ContentText v]):_) bldr@(_,_,_,_,["redirect","page"]) =
  setTitle bldr v
onBegin (_:rest) bldr@(_,_,_,_,["redirect","page"]) = onBegin rest bldr
onBegin _ bldr = bldr

onContent :: Text -> WikiPageListBuilder -> WikiPageListBuilder
onContent v bldr@(_,_,_,_,["title","page"]) = setTitle bldr v
onContent v bldr@(_,_,_,_,["text","revision","page"]) = addText bldr v
onContent _ bldr = bldr

onEnd :: WikiPageListBuilder -> WikiPageListBuilder
onEnd (acc,Just n,r,t,["page"]) = do
  let parsed = WikiPage n r (T.concat $ reverse t)
  (parsed:acc,Nothing,Nothing,[],["page"])
onEnd (acc,_,_,_,["page"]) = (acc,Nothing,Nothing,[],["page"])
onEnd bldr = bldr


-- getWikiPage :: CR.MonadThrow m => ConduitT Event o m (Maybe [(Text, Text)])
-- getWikiPage = do
--   let attrParser = requireAttr "title"
--   pipeline <- P.many' $
--     P.choose
--       [ P.tagIgnoreAttrs "title" $ \() -> do
--           title <- P.content
--           return ("title", title)
--       , P.tag' "redirect" attrParser $ \(redir) -> return ("redirect", redir)
--       , P.tagIgnoreAttrs "revision" 
--         ((P.many' (P.tagIgnoreAttrs "text" $ \() -> do
--             text <- P.content
--             return ("text", text)))
--          .| CC.head)
--       ]
--   pipeline .| CC.
--   usable
--   .| P.tagIgnoreAttrs "title" $ return P.content
--   .| CC.map (WikiPage . fromJust)
--   .| CC.head

-- contentOrEmpty :: MonadThrow m => Name n -> Name n2 -> ConduitT Event o m Text
-- contentOrEmpty name, boundName = do
--   case parsed of
--     [] -> ""
--     (c:_) -> ""
--   where parsed = P.manyIgnore
--     (P.tagIgnoreAttrs name P.content)
--     (P.tagIgnoreAttrs boundName )

main :: IO ()
main = do
  result <- runConduit $
    CC.stdin
    .| P.parseBytes def
    .| CC.foldl onEvent ([],Nothing,Nothing,[],[]) 
  let (pages,_,_,_,_) = result
  putStrLn $ foldl (\acc curr -> acc ++ "\n" ++ (show curr)) "" (reverse pages)
