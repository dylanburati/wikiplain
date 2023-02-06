{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Numeric (readFloat)
import GHC.Int (Int64)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformFloat01M, StatefulGen)
import Data.Char (isLetter, isDigit)
import Data.List (find, findIndex, intercalate, mapAccumL)
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative (liftA2, Alternative ((<|>)))
import Control.Exception
    (finally, throw, AssertionFailed(AssertionFailed))
import Control.Monad (foldM)
import Control.Monad.State (MonadIO(..))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as CR
import Data.Acquire (mkAcquire, withAcquire)
import Data.Conduit ((.|), runConduit, runConduitRes, ConduitT, bracketP)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Lift as CLF
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

emptyWikiPage :: WikiPage
emptyWikiPage = WikiPage {title = "", ns = 0, pageId = 0, redirect = Just "", text = ""}

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
    (AT.scan (1,0,'\0') (twoCharDepthScanner '[' '[' ']' ']'))

-- only breaks around top-level templates
breakAroundWikiTemplates :: Text -> [Text]
breakAroundWikiTemplates =
  breakAround
    (twoCharBreakBefore (twoCharTest '{' '{'))
    (AT.scan (1,0,'\0') (twoCharDepthScanner '{' '{' '}' '}'))

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

data Conversion
  = NoTables
  | NoInvokeTemplates
  | OnlyLinks
  | OnlyCategories
  | OnlyDescriptions
  | OnlyInfoboxes
  deriving (Show,Enum,Bounded)

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

convPage :: Conversion -> WikiPage -> WikiPage
convPage NoTables wp = wp {text = withoutTables (text wp)}
convPage NoInvokeTemplates wp = wp {text = withoutInvokeTemplates (text wp)}
convPage OnlyLinks wp = wp {text = T.unlines $ onlyLinks (text wp)}
convPage OnlyCategories wp =
  wp {text = T.unlines $ filter (T.isPrefixOf "[[Category:") $ onlyLinks (text wp)}
convPage OnlyDescriptions wp =
  wp {text = onlyDesc (text wp)}
  where
    onlyDesc t = fromMaybe "" $
      find (isPrefixOfCI "{{short desc") $
      onlyTemplates t

convPage OnlyInfoboxes wp =
  wp {text = onlyInfoboxes (text wp)}
  where
    onlyInfoboxes t = T.unlines $
      filter (isPrefixOfCI "{{infobox") $
      onlyTemplates t

pagesQuery :: Text
pagesQuery = "SELECT id,ns,title,redirect,text FROM wiki_article "

pagesQueryMatchTitles :: Int -> Text
pagesQueryMatchTitles n =
  pagesQuery <> "WHERE title IN (" <> (T.intercalate "," (replicate n "?")) <> ")"

pagesFromStmt :: (MonadIO m) => QL.Statement -> m [WikiPage]
pagesFromStmt stmt = liftIO $ do
  r <- QL.step stmt
  case r of
    QL.Row -> do
      row <- QL.columns stmt
      rest <- pagesFromStmt stmt
      return ((rowToPage row):rest)
    QL.Done -> return []

rowToPage :: [SQLData] -> WikiPage
rowToPage [SQLInteger i,SQLInteger n,SQLText t,r',tx'] =
  case [r',tx'] of
    [SQLNull,SQLText tx] ->
      WikiPage {title = t, ns = n, pageId = i, redirect = Nothing, text = tx}
    [SQLText r,SQLNull] ->
      WikiPage {title = t, ns = n, pageId = i, redirect = Just r, text = ""}
    _ ->
      throw $ AssertionFailed "Wrong column type for one of: redirect,text"
rowToPage _ =
  throw $ AssertionFailed "Wrong column type for one of: id,ns,title"


loadPage :: (MonadIO m) => QL.Statement -> m (Maybe WikiPage)
loadPage stmt = liftIO $ do
  r <- QL.step stmt
  case r of
    QL.Row -> do
      row <- QL.columns stmt
      return $ Just (rowToPage row)
    QL.Done -> return Nothing

loadAllPages :: (CR.MonadResource m, StatefulGen g m)
             => QL.Database
             -> g
             -> Float
             -> ConduitT () WikiPage m ()
loadAllPages db rand fraction =
  bracketP (QL.prepare db pagesQuery) QL.finalize $ \stmt ->
    CC.repeatWhileM (loadPage stmt) isJust
    .| CC.map (fromMaybe emptyWikiPage)
    .| CC.filter (isNothing . redirect)
    .| CC.filterM (\_ -> do
      x <- uniformFloat01M rand
      return $ x < fraction)


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
        pagesFromStmt stmt `finally` (QL.finalize stmt)

conversionName :: Conversion -> String
conversionName NoTables = "no-tables"
conversionName NoInvokeTemplates = "no-invoke-tmpls"
conversionName OnlyLinks = "only-links"
conversionName OnlyCategories = "only-categories"
conversionName OnlyDescriptions = "only-desc"
conversionName OnlyInfoboxes = "only-infoboxes"

conversionByName :: String -> Maybe Conversion
conversionByName s =
  conversionByName' s [NoTables ..]
  where
    conversionByName' s [] = Nothing
    conversionByName' s (hd:rest) 
      | s == (conversionName hd) = Just hd
      | otherwise                = conversionByName' s rest

data Args = Args
  { argConversion :: Maybe Conversion
  , argFraction :: Maybe Float
  } deriving Show

addConversionToArgs :: String -> Either String (Args -> Args)
addConversionToArgs s = case conversionByName s of
  Just c  -> Right (\args -> args { argConversion = Just c })
  Nothing -> Left $ "Invalid conversion: " <> s

addFractionToArgs :: String -> Either String (Args -> Args)
addFractionToArgs s = case readFloat s of
  ((x,""):_) | x>0, x<=1 -> Right (\args -> args { argFraction = Just x })
  _                      -> Left $ "Invalid float: " <> s

cliHeader :: String
cliHeader = "Usage: wikiplain [OPTION...] /path/to/db"

options :: [OptDescr (Either String (Args -> Args))]
options =
  [ Option ['c'] ["conversion"] (ReqArg addConversionToArgs "conv")
      ("The conversion to apply to wikitext (" <> (intercalate "|" (map conversionName [NoTables ..])) <> ")")
  , Option ['f'] ["fraction"] (ReqArg addFractionToArgs "number")
      "The fraction of articles to convert; if not provided, the program reads article titles from stdin"
  , Option ['h'] ["help"] (NoArg (Left "HELP"))
      "Print this help message"
  ]

parseArgs :: Args -> [String] -> IO (Args, String)
parseArgs defaults argv =
  let (results, params, errs1) = getOpt Permute options argv
      errs2 = errs1 ++ [msg | Left msg <- results]
      (param,errs) = case params of
        [p] -> (p, errs2)
        []  -> ("", errs2 ++ ["Missing required database file\n"])
        _   -> ("", errs2 ++ ["Extra arguments after database file\n"])
      optfns = [fn | Right fn <- results]
  in case errs of
    [] ->
      -- reduction step is Args (Args -> Args) -> Args
      return (foldl (\acc f -> f acc) defaults optfns, param)
    lst ->
      if "HELP" `elem` lst then do
        hPutStrLn stderr (usageInfo cliHeader options)
        exitWith ExitSuccess
      else
        ioError (userError (unlines lst <> usageInfo cliHeader options))

main :: IO ()
main = do
  let defaultArgs = Args { argConversion = Nothing
                         , argFraction = Nothing
                         }
  (args, dbfilename) <- getArgs >>= parseArgs defaultArgs
  
  rand <- newIOGenM (mkStdGen 1729)
  withAcquire (mkAcquire (QL.open (T.pack dbfilename)) QL.close) $ \db ->
    CR.runResourceT $ do
      source <- case (argFraction args) of
        Just x -> return $ loadAllPages db rand x
        Nothing -> return $ loadPagesWithTitles db  -- read titles from stdin

      runConduit $
        source
        .| CC.map (\wp -> wp {text = removeXMLComments (text wp)})
        .| CC.map (maybe id convPage (argConversion args))
        .| CC.mapM_ (\(WikiPage{title = t, text = tx}) ->
            liftIO $ TIO.putStr t >> putChar '\0' >> TIO.putStr tx >> putChar '\0')
