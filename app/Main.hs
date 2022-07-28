{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Debug.Trace (trace)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import System.IO (stderr)
import System.Timeout (timeout)
import System.Environment (getArgs)
import Data.Char (isLetter)
import Data.List (find, mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Data.XML.Types
import Text.XML.Stream.Parse (def)
import qualified Text.XML.Stream.Parse as P
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit ((.|), runConduit, runConduitRes, ConduitT)
import qualified Control.Monad.Trans.Resource as CR
import qualified Text.Pandoc as PD
import Text.Pandoc.Class (setVerbosity)
import Text.Pandoc.Error (renderError, PandocError(PandocSomeError))
import qualified Text.Pandoc.Logging as PL

data WikiPage = WikiPage Text (Maybe Text) Text deriving Show
type WikiPageBuilder = (Maybe Text, Maybe Text, [Text], [Name])

breakAround :: (Char -> Bool) -> (Text -> Bool) -> Text -> [Text]
breakAround _ _ "" = []
breakAround testL testEnd t =
  let (pre,post) = T.break testL t
      match = case find testEnd (take 60 (T.inits post)) of
        Just m -> m
        Nothing -> T.take 1 post
  in case post of
    "" -> [pre]
    _ -> (pre:match:(breakAround testL testEnd (T.drop (T.length match) post)))

breakAroundTags :: Text -> [Text]
breakAroundTags =
  breakAround (== '<') tagDidEnd
  where
    tagDidEnd t
      | (T.length t) <= 1 = False
      | otherwise = (T.last t) `elem` ['<','>']

delimitInclusive :: (a -> Bool) -> (a -> Bool) -> [a] -> [(a, Bool)]
delimitInclusive testL testR lst =
  snd $ mapAccumL delimit False lst
  where
    delimit False elt
      | testL elt = (True,(elt,True))
      | otherwise = (False,(elt,False))
    delimit True elt
      | testR elt = (False,(elt,True))
      | otherwise = (True,(elt,True))

zipConsec :: [a] -> [(a, a)]
zipConsec = zip <*> tail

fixMwProblems :: Text -> Text
fixMwProblems t =
  T.concat $
    map blockquoteOwnLine $
    textPairIter $
    snd $ mapAccumL endBlockquoteInRef 0 $
    textPairIter $
    breakAroundTags t
  where
    -- textBoolPairIter xs = zipConsec $ cons ("",False) xs
    textPairIter xs = zipConsec ("":xs)
    blockquoteOwnLine (prev,"</blockquote>")
      | T.isSuffixOf "\n" prev = "</blockquote>"
      | otherwise              = "\n</blockquote>"
    blockquoteOwnLine (_,curr) = curr
    endBlockquoteInRef :: Int -> (Text, Text) -> (Int, Text)
    endBlockquoteInRef n (_,"<blockquote>") = (n+1,"<blockquote>")
    endBlockquoteInRef n (_,"</blockquote>")
      | n > 0 = (n-1,"</blockquote>")
      | otherwise = (0,"</blockquote>")
    endBlockquoteInRef _ (_,(T.stripPrefix "<ref" -> Just sffx)) = (0,"<ref" <> sffx)
    endBlockquoteInRef n (_,"</ref>") = (0,(T.replicate n "</blockquote>") <> "</ref>")
    endBlockquoteInRef n (_,curr) = (n,curr)

withoutTables :: Text -> Text
withoutTables t =
  T.unlines $ dropTableContent (T.lines t)
  where
    dropTableContent lines =
      fst $ unzip (filter (not . snd) (delimitInclusive isTableBegin isTableEnd lines))
    isTableBegin = T.isPrefixOf "{|"
    isTableEnd = T.isPrefixOf "|}"

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

convertOne :: MonadIO m => WikiPage -> m (Maybe (Text, Text))
convertOne (WikiPage title _ text) = do
  fullPlainC <-
    liftIO $ timeout 10000000 $ PD.runIO $
      setVerbosity PL.ERROR >>= \() ->
      PD.readMediaWiki PD.def text >>=
      PD.writePlain PD.def{ PD.writerWrapText = PD.WrapNone }
  plainC <- case fullPlainC of
    Just (Left err) ->
      liftIO $ timeout 10000000 $ PD.runIO $
        setVerbosity PL.ERROR >>= \() ->
        PD.readMediaWiki PD.def (withoutTables text) >>=
        (PD.writePlain PD.def{ PD.writerWrapText = PD.WrapNone })
    successOrTimeout -> return successOrTimeout
  case plainC of
    Nothing -> do
      _ <- liftIO $ TIO.hPutStrLn stderr $ "Timeout for " <> title
      return Nothing
    Just (Left err) -> do
      _ <- liftIO $ TIO.hPutStrLn stderr (renderError err)
      return Nothing
    Just (Right converted) -> return $ Just (title, converted)

main :: IO ()
main = do
  argv <- getArgs
  runConduitRes $
    CC.sourceFile (head argv)
    .| P.parseBytes def
    .| CC.concatMapAccum onEvent (Nothing,Nothing,[],[])
    .| CC.filter (\(WikiPage _ redir _) -> T.length (fromMaybe "" redir) == 0)
    .| CL.mapMaybeM convertOne
    .| CC.mapM_ (\(k,v) -> do
        _ <- liftIO . TIO.putStr $ k
        _ <- liftIO . putChar $ '\0'
        _ <- liftIO . TIO.putStr $ v
        liftIO . putChar $ '\0')

