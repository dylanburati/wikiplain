{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Numeric
import GHC.Int (Int64)
import System.Environment
import System.IO
import System.Random (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformFloat01M, StatefulGen)
import Data.Char (isLetter, isDigit)
import Data.List (find, findIndex, mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Control.Applicative (liftA2, Alternative ((<|>)))
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as CR
import Data.Acquire (mkAcquire, withAcquire)
import Data.Conduit ((.|), runConduit, runConduitRes, ConduitT)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Lift as CLF
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

isPrefixOfCI :: Text -> Text -> Bool
isPrefixOfCI t1 t2 =
  T.isPrefixOf (T.toCaseFold t1) (T.toCaseFold t2)

isListPrefix :: (Eq a) => [a] -> [a] -> Bool
isListPrefix [] _ = True
isListPrefix _ [] = False
isListPrefix (p1:prest) (v1:vrest)
  | p1 == v1 = isListPrefix prest vrest
  | otherwise = False

breakAround :: AT.Parser (Text,Text) -> AT.Parser Text -> Text -> [Text]
breakAround parseToL parseToR t =
  case AT.parseOnly (AT.manyTill parser AT.endOfInput) t of
    Right lst -> concatMap (\(a,b) -> [a, b]) lst
    Left _ -> [t]
  where
    parser = do
      (pre,starter) <- parseToL
      content <- parseToR
      finisher <- AT.take 1 <|> AT.take 0
      return (pre,(starter <> content <> finisher))

twoCharPScanner :: (Char -> Char -> Bool) ->
                   Char ->
                   Char ->
                   Maybe Char
twoCharPScanner test c1 c2
  | test c1 c2 = Nothing
  | otherwise = Just c2

twoCharTest :: Char -> Char -> Char -> Char -> Bool
twoCharTest v1 v2 c1 c2 = (v1 == c1) && (v2 == c2)

twoCharBreakBefore :: (Char -> Char -> Bool) -> AT.Parser (Text,Text)
twoCharBreakBefore test = do
  (match,stopchr1) <- AT.runScanner '\0' (twoCharPScanner test)
  {- last scan doesn't consume char 2 of stopping point -}
  stopchr2 <- AT.peekChar
  case stopchr2 of
    Nothing -> return (match,"") {- end of input -}
    Just _ -> return $ (,) (T.dropEnd 1 match) (T.singleton stopchr1)

twoCharScanner :: Char -> Char -> Char -> Char -> Maybe Char
twoCharScanner v1 v2 = twoCharPScanner (twoCharTest v1 v2)

twoCharDepthScanner :: Char -> Char ->
                       Char -> Char ->
                       (Int, Int, Char)  {- i-lastMatch, depth, str[i-1] -}
                       -> Char
                       -> Maybe (Int, Int, Char)
twoCharDepthScanner s1 s2 e1 e2 (dist,d,v1) v2
  | dist < 2                       = Just (dist+1,d,v2)
  | d <= 1 && e1 == v1 && e2 == v2 = Nothing
  | e1 == v1 && e2 == v2           = Just (1,d-1,v2)
  | s1 == v1 && s2 == v2           = Just (1,d+1,v2)
  | otherwise                      = Just (dist+1,d,v2)

removeXMLComments :: Text -> Text
removeXMLComments t =
  T.concat $ uncommented t
  where
    uncommented "" = []
    uncommented s =
      let (pre,cmtplus) = T.breakOn "<!--" s
          (_,post) = T.breakOn "-->" (T.drop 4 cmtplus)
      in (:) pre (uncommented (T.drop 3 post))

breakAroundTags :: Text -> [Text]
breakAroundTags =
  breakAround (breakerL '<') (breakerR '>')
  where
    breakerR c = AT.takeWhile (/= c)
    {- 1-char delimiter, so takeWhile never reads a part of the tag -}
    breakerL c = fmap (flip (,) "") (breakerR c)

breakAroundWikiLinks :: Text -> [Text]
breakAroundWikiLinks =
  breakAround
    (twoCharBreakBefore (twoCharTest '[' '['))
    (AT.scan (0,1,'\0') (twoCharDepthScanner '[' '[' ']' ']'))

-- only breaks around top-level templates
breakAroundWikiTemplates :: Text -> [Text]
breakAroundWikiTemplates =
  breakAround
    (twoCharBreakBefore (twoCharTest '{' '{'))
    (AT.scan (0,1,'\0') (twoCharDepthScanner '{' '{' '}' '}'))

breakAroundWikiTemplates' :: Text -> [Text]
breakAroundWikiTemplates' =
  breakAround
    (twoCharBreakBefore $ applyOr2 (twoCharTest '{' '{')
                                   (twoCharTest '}' '}'))
    (AT.take 0)
  where
    applyOr2 = liftA2 (liftA2 (||))
    {- liftA2 (||) is (f Bool) -> (f Bool) -> (f Bool) -}
    {- liftA2 of that is (g f Bool) -> (g f Bool) -> (g f Bool) -}

delimitInclusive :: (a -> Bool) -> (a -> a -> Bool) -> [a] -> [(a, Bool)]
delimitInclusive testL testR lst =
  snd $ mapAccumL delimit Nothing lst
  where
    delimit Nothing elt
      | testL elt = (Just elt,(elt,True))
      | otherwise = (Nothing,(elt,False))
    delimit (Just startelt) elt
      | testR startelt elt = (Nothing,(elt,True))
      | otherwise = (Just startelt,(elt,True))

delimitInclusiveAndGroup :: (a -> Bool) -> (a -> a -> Bool) -> [a] -> [([a], Bool)]
delimitInclusiveAndGroup testL testR lst =
  foldr makeGroups [] $ delimitInclusive testL testR lst
  where
    makeGroups (elt,curr) [] = [([elt], curr)]
    makeGroups (elt,curr) ((grp,prev):rest)
      | curr == prev = (((elt:grp),prev):rest)
      | otherwise = (([elt],curr):(grp,prev):rest)

zipConsec :: [a] -> [(a, a)]
zipConsec = zip <*> tail

withoutTables :: Text -> Text
withoutTables t =
  T.unlines $ dropTableContent (T.lines t)
  where
    dropTableContent lines =
      dropDelimited $ delimitInclusive isTableBegin isTableEnd lines
    dropDelimited dd = fst $ unzip (filter (not . snd) dd)
    isTableBegin = T.isPrefixOf "{|"
    isTableEnd _ = T.isPrefixOf "|}"

withoutInvokeTemplates :: Text -> Text
withoutInvokeTemplates t =
  T.concat $ dropInvokeTemplates $
    zipConsec $
    (breakAroundWikiTemplates' t) ++ [""]
  where
    dropInvokeTemplates consecs =
      replaceDelimited "1" $
        map (\((curr,nxt),isDelim) -> (curr,isDelim)) $
        delimitInclusive isInvokeBegin isInvokeEnd consecs
    replaceDelimited val lst =
      snd $ foldr (replaceDelimited1 val) (False,[]) lst
    replaceDelimited1 val (elt,True) (False,acc) = (True,val:acc)
    replaceDelimited1 val (elt,True) (True,acc) = (True,acc)
    replaceDelimited1 val (elt,False) (_,acc) = (False,elt:acc)
    isInvokeBegin ("{{",nxt) = isPrefixOfCI "#invoke:" nxt
    isInvokeBegin _ = False
    isInvokeEnd _ ("}}",_) = True
    isInvokeEnd _ _ = False

onlyLinksIn :: Text -> [Text]
onlyLinksIn t =
  filter (T.isPrefixOf "[[") $
    concatMap traverseThumbnails $
    breakAroundWikiLinks t
  where
    traverseThumbnails (T.stripPrefix "[[File:" -> Just txt) =
      breakAroundWikiLinks (T.dropEnd 2 txt)
    traverseThumbnails (T.stripPrefix "[[Image:" -> Just txt) =
      breakAroundWikiLinks (T.dropEnd 2 txt)
    traverseThumbnails txt = [txt]

onlyLinks :: Text -> [Text]
onlyLinks t =
  concatMap (onlyLinksIn . fst) $
    filter (not . snd) $
    delimitInclusive overridesLinkSyntax isMatchingPair $
    breakAroundTags t
  where
    overridesLinkSyntax (T.stripPrefix "<" -> Just tag) =
      ((getIdent tag) `elem` ["nowiki", "pre", "code", "math"]) &&
      (not (T.isSuffixOf "/>" tag))
    overridesLinkSyntax _ = False
    isMatchingPair tag1 (T.stripPrefix "</" -> Just tag2) =
      (getIdent tag1) == (getIdent tag2)
    isMatchingPair tag1 _ = False
    getIdent = fst . T.span identChar . snd . T.span (`elem` ['<','/'])
    identChar '-' = True
    identChar c = (isLetter c) || (isDigit c)

onlyTemplates :: Text -> [Text]
onlyTemplates t = filter (T.isPrefixOf "{{") (breakAroundWikiTemplates t)

convPage :: (Maybe String) -> WikiPage -> WikiPage
convPage (Just "--no-tables") wp = wp {text = withoutTables (text wp)}
convPage (Just "--no-invoke-tmpls") wp = wp {text = withoutInvokeTemplates (text wp)}
convPage (Just "--only-links") wp =
  wp {text = T.unlines $ onlyLinks (text wp)}
convPage (Just "--only-categories") wp =
  wp {text = T.unlines $ filter (T.isPrefixOf "[[Category:") $ onlyLinks (text wp)}
convPage (Just "--only-desc") wp =
  wp {text = onlyDesc (text wp)}
  where
    onlyDesc t = fromMaybe "" $
      find (isPrefixOfCI "{{short desc") $
      onlyTemplates t

convPage (Just "--only-infoboxes") wp =
  wp {text = onlyInfoboxes (text wp)}
  where
    onlyInfoboxes t = T.unlines $
      filter (isPrefixOfCI "{{infobox") $
      onlyTemplates t

convPage _ pg = pg

pagesQuery :: Text
pagesQuery = "SELECT id,ns,title,redirect,text FROM wiki_article "

-- not supposed to put Int directly, since (StateT s m) is a type constraint later
pagesQueryWithOffset :: (Integral s, Show s) => s -> Text
pagesQueryWithOffset n =
  pagesQuery <> "LIMIT 5000 OFFSET " <> (T.pack (show n))

pagesQueryMatchTitles :: Int -> Text
pagesQueryMatchTitles n =
  pagesQuery <> "WHERE title IN (" <> (T.intercalate "," (replicate n "?")) <> ")"

pagesFromStmt :: (MonadIO m) => QL.Statement -> m [WikiPage]
pagesFromStmt stmt = do
  liftIO $ (stepper stmt `finally` (QL.finalize stmt)) >>= (mapM rowToPage)
  where
    stepper sqlstmt = do
      r <- QL.step sqlstmt
      case r of
        QL.Row -> (QL.columns sqlstmt) >>= (\row ->
                    liftA2 (:) (return row) (stepper sqlstmt))
        QL.Done -> return []

    rowToPage [SQLInteger i,SQLInteger n,SQLText t,r',tx'] =
      rowToPage2 i n t r' tx'
    rowToPage _ =
      throw $ AssertionFailed "Wrong column type for one of: id,ns,title"
    rowToPage2 i n t SQLNull (SQLText tx) =
      return $ WikiPage {title = t, ns = n, pageId = i, redirect = Nothing, text = tx}
    rowToPage2 i n t (SQLText r) SQLNull =
      return $ WikiPage {title = t, ns = n, pageId = i, redirect = Just r, text = ""}
    rowToPage2 _ _ _ _ _ =
      throw $ AssertionFailed "Wrong column type for one of: redirect,text"

loadAllPages :: (MonadIO m, StatefulGen g m)
             => QL.Database
             -> g
             -> Float
             -> ConduitT () WikiPage m ()
loadAllPages db rand fraction =
  CLF.evalStateLC 0 (
    CC.repeatWhileM (loadPagesSt db) ((> 0) . length)
      .| CC.concatMap id
      .| CC.filter (isNothing . redirect)
    )
    .| CC.filterM (\_ -> do
       x <- uniformFloat01M rand
       return $ x < fraction)
  where
    loadPagesSt :: (Integral s, Show s, MonadState s m, MonadIO m)
                => QL.Database -> m [WikiPage]
    loadPagesSt db = do
      offset <- get
      rows <- liftIO $ do
        stmt <- QL.prepare db (pagesQueryWithOffset offset)
        pagesFromStmt stmt
      _ <- put (offset + 5000)
      return rows

loadPagesWithTitles :: (CR.MonadThrow m, MonadIO m)
                    => QL.Database
                    -> ConduitT () WikiPage m ()
loadPagesWithTitles db =
  CC.stdin
    .| CT.decode CT.utf8
    .| CT.lines
    .| CL.chunksOf 500
    .| CC.concatMapM (loadPagesV db)
    .| CC.filter (isNothing . redirect)
  where
    loadPagesV db lines = do
      let binds = map SQLText lines
      liftIO $ do
        stmt <- QL.prepare db (pagesQueryMatchTitles (length binds))
        _ <- QL.bind stmt binds
        pagesFromStmt stmt

data Args = Args
  { argConversion :: Maybe String
  , argFraction :: Maybe Float
  } deriving Show

parseArgs :: (Monad m) => Args -> (String, String) -> m Args
parseArgs a ("--fraction", v) = case readFloat v of
  ((x,_):_) -> return $ a {argFraction = Just x}
  _         -> throw $ AssertionFailed "Expected float value after --fraction"
parseArgs a (_, opt)
  {- opt stores const -}
  | opt == "--no-tables"        = return $ a {argConversion = Just "--no-tables"}
  | opt == "--no-invoke-tmpls"  = return $ a {argConversion = Just "--no-invoke-tmpls"}
  | opt == "--only-links"       = return $ a {argConversion = Just "--only-links"}
  | opt == "--only-categories"  = return $ a {argConversion = Just "--only-categories"}
  | opt == "--only-desc"        = return $ a {argConversion = Just "--only-desc"}
  | opt == "--only-infoboxes"   = return $ a {argConversion = Just "--only-infoboxes"}
  {- opt requires value -}
  | opt == "--fraction"         = return a
  {- unknown opt -}
  | isListPrefix "--" opt       = throw $ AssertionFailed ("Unknown option: " <> opt)
  | otherwise                   = throw $ AssertionFailed ("Command takes no arguments")

main :: IO ()
main = do
  let defaultArgs = Args { argConversion = Nothing
                         , argFraction = Nothing
                         }
  args <- getArgs >>= (\lst ->
    foldM parseArgs defaultArgs (zipConsec ("":lst)))

  rand <- newIOGenM (mkStdGen 1729)
  withAcquire (mkAcquire (QL.open "../enwiki.db") QL.close) $ \db -> do
    source <- case (argFraction args) of
      Just x -> return $ loadAllPages db rand x
      Nothing -> return $ loadPagesWithTitles db  -- read titles from stdin

    runConduit $
      source
      .| CC.map (\wp -> wp {text = removeXMLComments (text wp)})
      .| CC.map (convPage (argConversion args))
      .| CC.mapM_ (\(WikiPage{title = t, text = tx}) ->
          liftIO $ TIO.putStr t >> putChar '\0' >> TIO.putStr tx >> putChar '\0')
