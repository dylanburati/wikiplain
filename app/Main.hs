{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Char (isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust, fromMaybe)
import Data.XML.Types
import Text.XML.Stream.Parse (def)
import qualified Text.XML.Stream.Parse as P
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), runConduit, ConduitT)
import qualified Control.Monad.Trans.Resource as CR

data WikiPage = WikiPage Text (Maybe Text) Text deriving Show
type WikiPageBuilder = (Maybe Text, Maybe Text, [Text], [Name])

-- 0: pre <title>
-- 1: content <title>
-- 2: post </title> & pre <redirect>
-- 3: unused; content <redirect>
-- 4: post </redirect> & pre <revision>
-- 5: content <revision> & pre <text>
-- 6: content <text>
-- 7: content <revision> & post </text>
-- 8: post </revision>

fixMwProblems :: Text -> Text
fixMwProblems t =
  T.unlines $ map rebalanceQuotes (T.lines t)
  where
    rebalanceQuotes line
      | T.isPrefixOf "{|" line = case balanceCheck $ T.unpack line of
          (False,_,q,l) -> (T.take q line) <> " style=\"" <> (T.drop l line)
          _ -> line
      | otherwise = line
    balanceCheck = foldl balCheck1 (True,0,0,0)
    balCheck1 (True,p,q,l) '"' = (False,p+1,q,l)   {- begin quote -}
    balCheck1 (False,p,q,l) '"' = (True,p+1,p+1,p) {- end quote -}
    balCheck1 (True,p,q,l) ch
      | (q > l) && (isLetter ch) = (True,p+1,q,p) {- letter after end quote -}
      | otherwise = (True,p+1,q,l)
    balCheck1 (False,p,q,l) _ = (False,p+1,q,l)

setTitle :: WikiPageBuilder -> Text -> WikiPageBuilder
setTitle (_,r,t,path) v = (Just v,r,t,path)

setRedirect :: WikiPageBuilder -> Text -> WikiPageBuilder
setRedirect (n,_,t,path) v = (n,Just v,t,path)

addText :: WikiPageBuilder -> Text -> WikiPageBuilder
addText (n,r,tail,path) v = (n,r,(v:tail),path)

pushToXpath :: WikiPageBuilder -> Name -> WikiPageBuilder
pushToXpath (n,r,t,path) started = (n,r,t,started:path)

popFromXpath :: WikiPageBuilder -> WikiPageBuilder
popFromXpath (n,r,t,ended:path) = (n,r,t,path)
popFromXpath (n,r,t,[]) = (n,r,t,[]) -- todo: place under MonadThrow ctx to throw here

onEvent :: Event -> WikiPageBuilder -> (WikiPageBuilder, [WikiPage])
onEvent evt bldr = case evt of
  (EventBeginElement name attrs) -> (onBegin attrs (pushToXpath bldr name), [])
  (EventContent (ContentText v)) -> (onContent v bldr, [])
  (EventEndElement _) ->
    let (nextbldr, res) = onEnd bldr
    in (popFromXpath nextbldr, res)
  _ -> (bldr, [])

onBegin :: [(Name, [Content])] -> WikiPageBuilder -> WikiPageBuilder
onBegin [] bldr@(_,_,_,["redirect","page"]) = bldr
onBegin (("title", [ContentText v]):_) bldr@(_,_,_,["redirect","page"]) =
  setRedirect bldr v
onBegin (_:rest) bldr@(_,_,_,["redirect","page"]) = onBegin rest bldr
onBegin _ bldr = bldr

onContent :: Text -> WikiPageBuilder -> WikiPageBuilder
onContent v bldr@(_,_,_,["title","page"]) = setTitle bldr v
onContent v bldr@(_,_,_,["text","revision","page"]) = addText bldr v
onContent _ bldr = bldr

onEnd :: WikiPageBuilder -> (WikiPageBuilder, [WikiPage])
onEnd (Just n,r,t,["page"]) = do
  let text = T.concat (reverse t)
  let pg = WikiPage n r (fixMwProblems text)
  ((Nothing,Nothing,[],["page"]), [pg])
onEnd (_,_,_,["page"]) = ((Nothing,Nothing,[],["page"]), [])
onEnd bldr = (bldr, [])

main :: IO ()
main = do
  runConduit $
    CC.stdin
    .| P.parseBytes def
    .| CC.concatMapAccum onEvent (Nothing,Nothing,[],[])
    .| CC.filter (\(WikiPage _ redir _) -> T.length (fromMaybe "" redir) == 0)
    .| CC.mapM_ (\(WikiPage title _ text) -> do
        _ <- TIO.putStr title
        _ <- putChar '\0'
        _ <- TIO.putStr text
        putChar '\0')
