{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Numeric (readDec, readFloat, showInt)
import GHC.Int (Int64)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformFloat01M, StatefulGen)
import Data.Char (isLetter, isDigit, isSpace)
import Data.Either (fromRight)
import Data.List (find, findIndex, intercalate, mapAccumL, nub, uncons)
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
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
import Parse (tokenize, Token (..))

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
  T.concat (uncommentAcc t [])
  where
    uncommentAcc !s acc
      | s == ""   = reverse acc
      | otherwise = let (pre,cmt) = T.breakOn "<!--" s
                        post = snd $ T.breakOn "-->" cmt
                    in uncommentAcc (T.drop 3 post) (pre:acc)
-- removeXMLComments t =
--   T.concat $ uncommented t
--   where
--     uncommented "" = []
--     uncommented s =
--       let (pre,cmtplus) = T.breakOn "<!--" s
--           (_,post) = T.breakOn "-->" (T.drop 4 cmtplus)
--       in (:) pre (uncommented (T.drop 3 post))

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
  | OnlyLinksExcludingRefs
  | OnlyCategories
  | OnlyDescriptions
  | OnlyInfoboxes
  | OnlyDisambiguations
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

breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter pred [] = ([], [])
breakAfter pred (hd:tl)
  | pred hd = ([hd], tl)
  | otherwise = let (e1, e2) = breakAfter pred tl in (hd:e1, e2)

-- removes Element related tokens, and segments matching
--   [TokenBeginElement name False] ++ <balanced> ++ [TokenEndElement name]
--   for any name in the list
removeElements :: [Text] -> [Token] -> [Token]
removeElements prohibited tokens =
  removeElements' (isExcludedBegin prohibited) tokens (0, [])
  where
    isExcludedBegin exclLst tk
      | TokenBeginElement n False <- tk = n `elem` exclLst
      | otherwise = False
    removeElements' test [] (_, _) = []
    removeElements' test (hd:tl) (xDepth, stack)
      | TokenBeginElement tag False <- hd =
          let rd = if test hd then xDepth+1 else xDepth
          in removeElements' test tl (rd, hd:stack)
      | TokenBeginElement tag True <- hd =
          removeElements' test tl (xDepth, hd:stack)
      | TokenBeginTemplate <- hd =
          consWhen (xDepth == 0) hd $ removeElements' test tl (xDepth, hd:stack)
      | TokenEndTemplate <- hd =
          let (dropped, nextStack) = breakAfter (== TokenBeginTemplate) stack
              rd = xDepth - length (filter test dropped)
          in consWhen (xDepth == 0) hd $ removeElements' test tl (rd, nextStack)
      | TokenEndElement tag <- hd =
          let (dropped, nextStack) = breakAfter (== TokenBeginElement tag False) stack
              rd = xDepth - length (filter test dropped)
          in removeElements' test tl (rd, nextStack)
      | otherwise =
          consWhen (xDepth == 0) hd $ removeElements' test tl (xDepth, hd:stack)
    consWhen True = (:)
    consWhen False = const id

wikiRender :: Token -> Text
wikiRender (TokenContent c) = c
wikiRender TokenBeginLink = "[["
wikiRender TokenEndLink = "]]"
wikiRender TokenBeginTemplate = "{{"
wikiRender TokenEndTemplate = "}}"
wikiRender _ = ""

onlyLinksExcludingRefs :: Text -> [Text]
onlyLinksExcludingRefs tx =
  tokensToLinks $ removeElements ["nowiki", "pre", "code", "math", "ref"] (fromRight [] (tokenize tx))
  where
    tokensToLinks [] = []
    tokensToLinks (hd@TokenBeginLink : tl) =
      let (links, remaining) = unnestLinks tl [[hd]] []
      in filter (not . isThumbnail) (map renderLink links) ++ tokensToLinks remaining
    tokensToLinks (hd:tl) = tokensToLinks tl
    unnestLinks [] _ closedAccs = (closedAccs, [])
    unnestLinks remaining [] closedAccs = (closedAccs, remaining)
    unnestLinks (hd@TokenBeginLink : tl) accs closedAccs =
      unnestLinks tl ([hd] : map (hd :) accs) closedAccs
    unnestLinks (hd@TokenEndLink : tl) (innerAcc : outerAccs) closedAccs =
      unnestLinks tl (map (hd :) outerAccs) (reverse (hd:innerAcc) : closedAccs)
    unnestLinks (hd : tl) accs closedAccs =
      unnestLinks tl (map (hd :) accs) closedAccs
    renderLink lst = T.concat $ map wikiRender lst
    isThumbnail rendered = T.isPrefixOf "[[File:" rendered || T.isPrefixOf "[[Image:" rendered

onlyTemplatesV2 :: Maybe [Text] -> Text -> [Text]
onlyTemplatesV2 mbNames tx =
  tokensToTemplates mbNames $ removeElements ["nowiki", "pre", "code"] (fromRight [] (tokenize tx))
  where
    tokensToTemplates _ [] = []
    tokensToTemplates mbNames (hd@TokenBeginTemplate : tl) =
      let (tmpls, remaining) = unnestTemplates tl [[hd]] []
      in map renderLink (filter (shouldKeep mbNames) tmpls) ++ tokensToTemplates mbNames remaining
    tokensToTemplates mbNames (hd:tl) = tokensToTemplates mbNames tl
    unnestTemplates [] _ closedAccs = (closedAccs, [])
    unnestTemplates remaining [] closedAccs = (closedAccs, remaining)
    unnestTemplates (hd@TokenBeginTemplate : tl) accs closedAccs =
      unnestTemplates tl ([hd] : map (hd :) accs) closedAccs
    unnestTemplates (hd@TokenEndTemplate : tl) (innerAcc : outerAccs) closedAccs =
      unnestTemplates tl (map (hd :) outerAccs) (reverse (hd:innerAcc) : closedAccs)
    unnestTemplates (hd : tl) accs closedAccs =
      unnestTemplates tl (map (hd :) accs) closedAccs
    renderLink lst = T.concat $ map wikiRender lst
    shouldKeep Nothing _ = True
    shouldKeep (Just names) [] = False
    shouldKeep (Just names) [_] = False
    shouldKeep (Just names) (_:(TokenContent inner:_)) =
      let (nm, _) = T.span valid (T.stripStart inner)
      in T.toCaseFold (T.stripEnd nm) `elem` names
    shouldKeep (Just names) _ = False
    valid '#' = False
    valid '<' = False
    valid '>' = False
    valid '[' = False
    valid ']' = False
    valid '|' = False
    valid '{' = False
    valid '}' = False
    valid '_' = False
    valid _ = True

convPage :: Conversion -> Text -> Text
convPage NoTables = withoutTables . removeXMLComments
convPage NoInvokeTemplates = withoutInvokeTemplates . removeXMLComments
convPage OnlyLinks = T.unlines . onlyLinks . removeXMLComments
convPage OnlyLinksExcludingRefs = T.unlines . onlyLinksExcludingRefs
convPage OnlyCategories =
  T.unlines . filter (T.isPrefixOf "[[Category:") . onlyLinks . removeXMLComments
convPage OnlyDescriptions =
  fromMaybe "" .
    find (isPrefixOfCI "{{short desc") .
    onlyTemplates .
    removeXMLComments

convPage OnlyInfoboxes =
  T.unlines .
    filter (isPrefixOfCI "{{infobox") .
    onlyTemplates .
    removeXMLComments

convPage OnlyDisambiguations =
  T.unlines . onlyTemplatesV2 (Just disambiguationTmpls)

renderQuery :: (Text,[Text]) -> Text
renderQuery (sel,whr) =
  T.intercalate " " $ sel:((map ((<>) "WHERE ") (take 1 whr))
                           ++ (map ((<>) "AND ") (drop 1 whr)))

pagesQuerySelect :: Text
pagesQuerySelect = "SELECT id,ns,title,redirect,text FROM wiki_article"

pagesQuery' :: [Int64] -> (Text,[Text])
pagesQuery' []  = (pagesQuerySelect, ["redirect IS NULL"])
pagesQuery' nss =
  (pagesQuerySelect, ["redirect IS NULL",
                      "ns IN (" <> (T.intercalate "," (map (const "?") nss)) <> ")"])

pagesQuery :: [Int64] -> Text
pagesQuery = renderQuery . pagesQuery'

pagesQueryMatchIds' :: [Int64] -> Bool -> Int -> (Text,[Text])
pagesQueryMatchIds' nss usePageId n =
  let (sel,whr) = pagesQuery' nss
      col       = if usePageId then "id" else "title"
  in (sel,whr ++ [col <> " IN (" <> (T.intercalate "," (replicate n "?")) <> ")"])

pagesQueryMatchIds :: [Int64] -> Bool -> Int -> Text
pagesQueryMatchIds nss usePageId n =
  renderQuery $ pagesQueryMatchIds' nss usePageId n

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
             => Args
             -> QL.Database
             -> g
             -> Float
             -> ConduitT () WikiPage m ()
loadAllPages Args{argNamespaces = nss} db rand fraction =
  bracketP (QL.prepare db (pagesQuery nss)) QL.finalize $ \stmt -> do
    _ <- case nss of
           [] -> return ()
           l  -> liftIO $ QL.bind stmt (map SQLInteger l)
    CC.repeatWhileM (loadPage stmt) isJust
    .| CC.map (fromMaybe emptyWikiPage)
    .| CC.filterM (\_ -> do
      x <- uniformFloat01M rand
      return $ x < fraction)


loadPagesWithIds :: (CR.MonadThrow m, MonadIO m)
                 => Args
                 -> QL.Database
                 -> ConduitT () WikiPage m ()
loadPagesWithIds Args{argIntegerIds = usePageId, argNamespaces = nss} db =
  CC.stdin
    .| CT.decode CT.utf8
    .| CT.lines
    .| CL.chunksOf 500
    .| CC.concatMapM (loadPagesV db)
  where
    loadPagesV db lines = do
      let idBinds = if usePageId
                    then map SQLInteger [i | Right (i,"") <- map TR.decimal lines] 
                    else map SQLText lines
          nsBinds = map SQLInteger nss
          binds   = nsBinds ++ idBinds
      liftIO $ do
        stmt <- QL.prepare db (pagesQueryMatchIds nss usePageId (length binds))
        _ <- QL.bind stmt binds
        pagesFromStmt stmt `finally` (QL.finalize stmt)

data ConversionReturnType = OText | OBool deriving Show

convCast :: ConversionReturnType -> Text -> Text
convCast OText tx = tx
convCast OBool "" = "0"
convCast OBool _ = "1"

conversionName :: Conversion -> String
conversionName NoTables = "no-tables"
conversionName NoInvokeTemplates = "no-invoke-tmpls"
conversionName OnlyLinks = "only-links"
conversionName OnlyLinksExcludingRefs = "only-links-excluding-refs"
conversionName OnlyCategories = "only-categories"
conversionName OnlyDescriptions = "only-desc"
conversionName OnlyInfoboxes = "only-infoboxes"
conversionName OnlyDisambiguations = "only-dab"

conversionByName :: String -> Maybe (Conversion, ConversionReturnType)
conversionByName s =
  conversionByName' s [NoTables ..]
  where
    conversionByName' s [] = Nothing
    conversionByName' s (hd:rest) 
      | s == (conversionName hd) = Just (hd, OText)
      | s == (conversionName hd) <> "?" = Just (hd, OBool)
      | otherwise = conversionByName' s rest

data Args = Args
  { argConversions :: [(Conversion, ConversionReturnType)]
  , argFraction :: Maybe Float
  , argIntegerIds :: Bool
  , argNamespaces :: [Int64]
  } deriving Show

addConversionToArgs :: String -> Either String (Args -> Args)
addConversionToArgs s =
  case conversionByName s of
    Just c  -> Right (\args -> args { argConversions = c : argConversions args })
    Nothing -> Left $ "Invalid conversion: " <> s

addFractionToArgs :: String -> Either String (Args -> Args)
addFractionToArgs s = case readFloat s of
  ((x,""):_) | x>0, x<=1 -> Right (\args -> args { argFraction = Just x })
  _                      -> Left $ "Invalid float: " <> s

addNamespacesToArgs :: String -> Either String (Args -> Args)
addNamespacesToArgs s =
  case commaSepList s of
    Left msg -> Left msg
    Right [] -> Left "At least 1 namespace must be provided"
    Right lst -> Right (\args -> args { argNamespaces = lst })
  where
    commaSepList :: String -> Either String [Int64]
    commaSepList "" = Right []
    commaSepList v@(hd:_)
      | isSpace hd = Left "Namespace list can not include spaces"
      | otherwise  =
        case readDec v of
          (i, rest):_ -> case commaSepList (drop 1 rest) of
            Left msg  -> Left msg
            Right lst -> Right (i:lst)
          _                    -> Left ("Invalid integer(s): " <> v)

cliHeader :: String
cliHeader = "Usage: wikiplain [OPTION...] /path/to/db"

options :: [OptDescr (Either String (Args -> Args))]
options =
  [ Option ['c'] ["conversion"] (ReqArg addConversionToArgs "conv")
      ("The conversion to apply to wikitext (" <> (intercalate "|" (map conversionName [NoTables ..])) <> ")")
  , Option ['f'] ["fraction"] (ReqArg addFractionToArgs "number")
      "The fraction of articles to convert; if not provided, the program reads article titles from stdin"
  , Option []    ["integer-ids"] (NoArg (Right (\args -> args {argIntegerIds = True})))
      "Read and write page IDs instead of titles"
  , Option []    ["ns"] (ReqArg addNamespacesToArgs "ns1,ns2,...")
      "Filter out any articles which don't come from one of the listed namespace IDs (e.g. 0 for regular)"
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
        exitSuccess
      else
        ioError (userError (unlines lst <> usageInfo cliHeader options))

identifyPage :: Bool -> WikiPage -> Text
identifyPage False WikiPage{title = t} = t
identifyPage True WikiPage{pageId = i} = T.pack (showInt i "")

main :: IO ()
main = do
  let defaultArgs = Args { argConversions = []
                         , argFraction = Nothing
                         , argIntegerIds = False
                         , argNamespaces = []
                         }
  (args, dbfilename) <- getArgs >>= parseArgs defaultArgs
  let convFuncs = case (argConversions args) of
                    [] -> [id]
                    lst -> [convCast crt . convPage cnv | (cnv, crt) <- reverse lst]
  
  rand <- newIOGenM (mkStdGen 1729)
  withAcquire (mkAcquire (QL.open (T.pack dbfilename)) QL.close) $ \db ->
    CR.runResourceT $ do
      source <- case (argFraction args) of
        Just x -> return $ loadAllPages args db rand x
        Nothing -> return $ loadPagesWithIds args db  -- read pageIds/titles from stdin

      runConduit $
        source
        .| CC.map (\wp -> identifyPage (argIntegerIds args) wp :
                          map (\f -> f (text wp)) convFuncs)
        .| CC.mapM_ (liftIO . mapM_ (TIO.putStr <* const (putChar '\0')))

-- open encategories.db
-- ids <- (SELECT cl_from FROM categorylinks WHERE cl_to IN
--             ('Disambiguation_message_boxes', 'Set_index_article_templates'))
-- open enwiki.db
-- titles <- (SELECT title FROM wiki_article WHERE id IN {ids} AND title NOT LIKE '% cleanup')
-- titles ++ (SELECT title FROM wiki_article WHERE redirect IN {titles})

disambiguationTmpls :: [Text]
disambiguationTmpls = nub $ map T.toCaseFold [
  "Dab",
  "Disamb",
  "Numdisambig",
  "Bio-dab",
  "Hndisambig",
  "Numdab",
  "Shortcut disambig",
  "WP-disambig",
  "CJKVdab",
  "Meta disambig",
  "Disambig-plants",
  "Geodab",
  "Hndab",
  "Geo-dis",
  "Wikipedia disambiguation",
  "Sia",
  "Roaddis",
  "LatinNameDisambig",
  "SpeciesLatinNameDisambig",
  "DAB",
  "Letter-NumberCombdisambig",
  "Genus disambig",
  "WP disambig",
  "HnDis",
  "Set index",
  "Surnames",
  "Dbig",
  "Disambig",
  "Disambiguation page",
  "Hospitaldis",
  "Taxonomic authorities disambiguation",
  "Letter-NumberCombinationDisambiguation",
  "Airport disambig",
  "Callsigndis",
  "Disambig-Chinese-char-title",
  "MolFormDisambig",
  "Mathematics disambiguation",
  "Schooldis",
  "Personal name disambiguation",
  "Mathdab",
  "SIA",
  "Mountainindex",
  "Lakeindex",
  "Mil-unit-disambig",
  "Schooldab",
  "Chemdisambig",
  "Geodisambig",
  "Chemistry disambiguation",
  "Molecular formula disambiguation",
  "MolFormIndex",
  "LNCD",
  "Disam",
  "Letter-Number combination disambiguation",
  "Letter-NumberCombDisambig",
  "DisambigName",
  "DisambigNm",
  "DisambigN",
  "Species disambiguation",
  "Media index",
  "Project disambiguation",
  "Sportindex",
  "Roadindex",
  "Shipindex",
  "Set-index",
  "Case law disambiguation",
  "Chinese title disambig",
  "Setindex",
  "HNDIS",
  "Set-index article",
  "First name",
  "Forename",
  "Hndis",
  "Personal name",
  "Geodis",
  "Numberdis",
  "Disambig misspelling",
  "Begriffsklärung",
  "Music disambig",
  "Station dab",
  "Mil-unit-dis",
  "Letter-Number Combination Disambiguation",
  "Portal disambig",
  "DisambigG",
  "DisambigGeo",
  "Disambiggeo",
  "Chemistry set index",
  "Math dab",
  "School disambig",
  "Human name dab",
  "Template disambig",
  "Template dab",
  "Dis",
  "Template ambiguous",
  "Human name disambiguation",
  "Number disambiguation",
  "Place name disambiguation",
  "Ship index",
  "Surname",
  "Road index",
  "School disambiguation",
  "Disambiguation",
  "Hospital disambiguation",
  "Mountain index",
  "Given name",
  "Mathematical disambiguation",
  "Chinese title disambiguation",
  "Airport disambiguation",
  "Set index article",
  "Dmbox",
  "Sport index",
  "Call sign disambiguation",
  "Plant common name",
  "Lake index",
  "Molecular formula index",
  "Species Latin name disambiguation",
  "Letter-number combination disambiguation",
  "Species Latin name abbreviation disambiguation",
  "Genus disambiguation",
  "Taxonomy disambiguation",
  "Chemistry index",
  "Biology disambiguation",
  "Taxonomic authority disambiguation",
  "Military unit disambiguation",
  "Synagogue disambiguation",
  "Road disambiguation",
  "Enzyme index",
  "Media set index",
  "Caselaw disambiguation",
  "Fungus common name",
  "Phonetics disambiguation",
  "Animal common name",
  "Storm index",
  "River index",
  "Locomotive index",
  "Nickname",
  "Music disambiguation",
  "Station disambiguation",
  "Portal disambiguation",
  "Template disambiguation",
  "Opus number disambiguation",
  "WoO number disambiguation"
  ]
