-- Based on https://github.com/snoyberg/xml/blob/master/xml-conduit/src/Text/XML/Stream/Parse.hs
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( tokenize,
    Token (..),
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (..),
    (<$>),
  )
import qualified Control.Applicative as A
import Control.Monad (void)
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    endOfInput,
    manyTill,
    notInClass,
    option,
    peekChar,
    satisfy,
    skipMany,
    skipMany1,
    skipWhile,
    string,
    takeWhile,
    takeWhile1,
    (<?>),
  )
import qualified Data.Attoparsec.Text as AT
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Encoding.Utf32 (validate)
import Prelude hiding (takeWhile)
import Control.Exception (SomeException)

data Token
  = TokenBeginElement Text Bool
  | TokenEndElement Text
  | TokenBeginTemplate
  | TokenEndTemplate
  | TokenBeginLink
  | TokenEndLink
  | TokenComment
  | TokenContent Text
  deriving (Show, Eq)

parseIdent' :: Bool -> Parser Text
parseIdent' allowNamespace = takeWhile1 valid <?> "identifier"
  where
    valid '&' = False
    valid '<' = False
    valid '>' = False
    valid ':' = allowNamespace
    valid '?' = False
    valid '=' = False
    valid '"' = False
    valid '\'' = False
    valid '/' = False
    valid ';' = False
    valid '#' = False
    valid c = not $ isXMLSpace c

parseIdent :: Parser Text
parseIdent = parseIdent' False

parseName :: Parser Text
parseName = parseIdent' True

parseContent :: Parser Text
parseContent = takeWhile1 valid <?> "text content"
  where
    valid '<' = False
    valid '{' = False
    valid '}' = False
    valid '[' = False
    valid ']' = False
    valid _ = True

parseToken :: Parser Token
parseToken = do
  mbc <- peekChar
  case mbc of
    Just '<' -> char '<' >> parseLt
    Just '{' -> (string "{{" >> return TokenBeginTemplate) <|> TokenContent <$> (charT '{' <> parseContent)
    Just '}' -> (string "}}" >> return TokenEndTemplate) <|> TokenContent <$> (charT '}' <> parseContent)
    Just '[' -> (string "[[" >> return TokenBeginLink) <|> TokenContent <$> (charT '[' <> parseContent)
    Just ']' -> (string "]]" >> return TokenEndLink) <|> TokenContent <$> (charT ']' <> parseContent)
    _ -> TokenContent <$> parseContent
  where
    parseLt = do
      mbc <- peekChar
      case mbc of
        Just '?' -> char' '?' >> parseInstr
        Just '!' -> char' '!' >> option (TokenContent "<") (parseComment <|> parseCdata)
        Just '/' -> char' '/' >> parseEnd
        -- "<<" encountered; add the 1st char as content, but don't consume the 2nd in case it's a tag
        Just '<' -> return $ TokenContent "<"
        _ -> parseBegin
    parseInstr = do
      -- name <- parseIdent
      skipSpace
      manyTill anyChar (string "?>")
      return TokenComment
    parseComment = do
      char' '-'
      char' '-'
      manyTill anyChar (string "-->")
      return TokenComment
    parseCdata = do
      _ <- string "[CDATA["
      manyTill anyChar (string "]]>")
      return TokenComment
    parseEnd =
      do
        skipSpace
        n <- parseName
        skipSpace
        char' '>'
        return $ TokenEndElement n
    parseBegin =
      do
        -- not skipping space because MediaWiki doesn't
        -- https://github.com/wikimedia/mediawiki/blob/0d93947/includes/parser/Preprocessor_Hash.php#L308
        -- skipSpace
        n <- parseName <|> AT.take 1
        if isHTMLOrWikiTag n
          then do
            (_, (_, isClose, _)) <- AT.runScanner (False, False, Nothing) scannerGt
            return $ TokenBeginElement n isClose
          else return $ TokenContent $ T.cons '<' n
    scannerGt :: (Bool, Bool, Maybe Char) -> Char -> Maybe (Bool, Bool, Maybe Char)
    scannerGt (True, _, _) _ = Nothing
    scannerGt (False, hadSlash, Nothing) '>' = Just (True, hadSlash, Nothing)
    -- lenient. example <ref name="Dallas238"</ref>
    scannerGt (False, _, Nothing) '<' = Nothing
    scannerGt (False, _, Nothing) '"' = Just (False, False, Just '"')
    scannerGt (False, _, Nothing) c = Just (False, c == '/', Nothing)
    scannerGt (False, _, Just delim) c
      | c == delim = Just (False, False, Nothing)
      | otherwise = Just (False, False, Just delim)


parseAttribute :: Parser ()
parseAttribute = do
  skipSpace
  parseName
  skipSpace
  -- '<tag attribute ...>' is shorthand for '<tag attribute="" ...>'
  option
    ()
    ( char' '='
        >> skipSpace
        >> (squoted <|> dquoted <|> unquoted)
    )
  where
    squoted = char '\'' >> skipWhile (/= '\'') >> char' '\''
    dquoted = char '"' >> skipWhile (/= '"') >> char' '"'
    -- https://mathiasbynens.be/notes/unquoted-attribute-values
    unquoted = skipWhile valid
    valid '"' = False
    -- lenient. example: <ref name=NYCWorld'sLargest>
    -- valid '\'' = False
    valid '`' = False
    valid '=' = False
    valid '<' = False
    valid '>' = False
    valid c = not $ isXMLSpace c

skipSpace :: Parser ()
skipSpace = skipWhile isXMLSpace

isXMLSpace :: Char -> Bool
isXMLSpace ' ' = True
isXMLSpace '\t' = True
isXMLSpace '\r' = True
isXMLSpace '\n' = True
isXMLSpace _ = False

char' :: Char -> Parser ()
char' = void . char

charT :: Char -> Parser Text
charT = fmap T.singleton . char

isHTMLOrWikiTag :: Text -> Bool
isHTMLOrWikiTag "categorytree" = True
isHTMLOrWikiTag "ce" = True
isHTMLOrWikiTag "charinsert" = True
isHTMLOrWikiTag "chem" = True
isHTMLOrWikiTag "gallery" = True
isHTMLOrWikiTag "graph" = True
isHTMLOrWikiTag "hiero" = True
isHTMLOrWikiTag "imagemap" = True
isHTMLOrWikiTag "indicator" = True
isHTMLOrWikiTag "inputbox" = True
isHTMLOrWikiTag "langconvert" = True
isHTMLOrWikiTag "mapframe" = True
isHTMLOrWikiTag "maplink" = True
isHTMLOrWikiTag "math" = True
isHTMLOrWikiTag "nowiki" = True
isHTMLOrWikiTag "poem" = True
isHTMLOrWikiTag "pre" = True
isHTMLOrWikiTag "ref" = True
isHTMLOrWikiTag "references" = True
isHTMLOrWikiTag "score" = True
isHTMLOrWikiTag "section" = True
isHTMLOrWikiTag "source" = True
isHTMLOrWikiTag "syntaxhighlight" = True
isHTMLOrWikiTag "templatedata" = True
isHTMLOrWikiTag "templatestyles" = True
isHTMLOrWikiTag "timeline" = True
-- End https://en.wikipedia.org/wiki/Special:Version#mw-version-parser-extensiontags
isHTMLOrWikiTag "html" = True
isHTMLOrWikiTag "head" = True
isHTMLOrWikiTag "title" = True
isHTMLOrWikiTag "base" = True
isHTMLOrWikiTag "link" = True
isHTMLOrWikiTag "meta" = True
isHTMLOrWikiTag "style" = True
isHTMLOrWikiTag "body" = True
isHTMLOrWikiTag "article" = True
-- isHTMLOrWikiTag "section" = True
isHTMLOrWikiTag "nav" = True
isHTMLOrWikiTag "aside" = True
isHTMLOrWikiTag "h1" = True
isHTMLOrWikiTag "h2" = True
isHTMLOrWikiTag "h3" = True
isHTMLOrWikiTag "h4" = True
isHTMLOrWikiTag "h5" = True
isHTMLOrWikiTag "h6" = True
isHTMLOrWikiTag "hgroup" = True
isHTMLOrWikiTag "header" = True
isHTMLOrWikiTag "footer" = True
isHTMLOrWikiTag "address" = True
isHTMLOrWikiTag "p" = True
isHTMLOrWikiTag "hr" = True
-- isHTMLOrWikiTag "pre" = True
isHTMLOrWikiTag "blockquote" = True
isHTMLOrWikiTag "ol" = True
isHTMLOrWikiTag "ul" = True
isHTMLOrWikiTag "menu" = True
isHTMLOrWikiTag "li" = True
isHTMLOrWikiTag "dl" = True
isHTMLOrWikiTag "dt" = True
isHTMLOrWikiTag "dd" = True
isHTMLOrWikiTag "figure" = True
isHTMLOrWikiTag "figcaption" = True
isHTMLOrWikiTag "main" = True
isHTMLOrWikiTag "div" = True
isHTMLOrWikiTag "a" = True
isHTMLOrWikiTag "em" = True
isHTMLOrWikiTag "strong" = True
isHTMLOrWikiTag "small" = True
isHTMLOrWikiTag "s" = True
isHTMLOrWikiTag "cite" = True
isHTMLOrWikiTag "q" = True
isHTMLOrWikiTag "dfn" = True
isHTMLOrWikiTag "abbr" = True
isHTMLOrWikiTag "ruby" = True
isHTMLOrWikiTag "rt" = True
isHTMLOrWikiTag "rp" = True
isHTMLOrWikiTag "data" = True
isHTMLOrWikiTag "time" = True
isHTMLOrWikiTag "code" = True
isHTMLOrWikiTag "var" = True
isHTMLOrWikiTag "samp" = True
isHTMLOrWikiTag "kbd" = True
isHTMLOrWikiTag "sub" = True
isHTMLOrWikiTag "sup" = True
isHTMLOrWikiTag "i" = True
isHTMLOrWikiTag "b" = True
isHTMLOrWikiTag "u" = True
isHTMLOrWikiTag "mark" = True
isHTMLOrWikiTag "bdi" = True
isHTMLOrWikiTag "bdo" = True
isHTMLOrWikiTag "span" = True
isHTMLOrWikiTag "br" = True
isHTMLOrWikiTag "wbr" = True
isHTMLOrWikiTag "ins" = True
isHTMLOrWikiTag "del" = True
isHTMLOrWikiTag "picture" = True
-- isHTMLOrWikiTag "source" = True
isHTMLOrWikiTag "img" = True
isHTMLOrWikiTag "iframe" = True
isHTMLOrWikiTag "embed" = True
isHTMLOrWikiTag "object" = True
isHTMLOrWikiTag "video" = True
isHTMLOrWikiTag "audio" = True
isHTMLOrWikiTag "track" = True
isHTMLOrWikiTag "map" = True
isHTMLOrWikiTag "area" = True
isHTMLOrWikiTag "table" = True
isHTMLOrWikiTag "caption" = True
isHTMLOrWikiTag "colgroup" = True
isHTMLOrWikiTag "col" = True
isHTMLOrWikiTag "tbody" = True
isHTMLOrWikiTag "thead" = True
isHTMLOrWikiTag "tfoot" = True
isHTMLOrWikiTag "tr" = True
isHTMLOrWikiTag "td" = True
isHTMLOrWikiTag "th" = True
isHTMLOrWikiTag "form" = True
isHTMLOrWikiTag "label" = True
isHTMLOrWikiTag "input" = True
isHTMLOrWikiTag "button" = True
isHTMLOrWikiTag "select" = True
isHTMLOrWikiTag "datalist" = True
isHTMLOrWikiTag "optgroup" = True
isHTMLOrWikiTag "option" = True
isHTMLOrWikiTag "textarea" = True
isHTMLOrWikiTag "output" = True
isHTMLOrWikiTag "progress" = True
isHTMLOrWikiTag "meter" = True
isHTMLOrWikiTag "fieldset" = True
isHTMLOrWikiTag "legend" = True
isHTMLOrWikiTag "details" = True
isHTMLOrWikiTag "summary" = True
isHTMLOrWikiTag "dialog" = True
isHTMLOrWikiTag "script" = True
isHTMLOrWikiTag "noscript" = True
isHTMLOrWikiTag "template" = True
isHTMLOrWikiTag "slot" = True
isHTMLOrWikiTag "canvas" = True
isHTMLOrWikiTag "applet" = True
isHTMLOrWikiTag "acronym" = True
isHTMLOrWikiTag "bgsound" = True
isHTMLOrWikiTag "dir" = True
isHTMLOrWikiTag "noframes" = True
isHTMLOrWikiTag "isindex" = True
isHTMLOrWikiTag "keygen" = True
isHTMLOrWikiTag "listing" = True
isHTMLOrWikiTag "menuitem" = True
isHTMLOrWikiTag "nextid" = True
isHTMLOrWikiTag "noembed" = True
isHTMLOrWikiTag "param" = True
isHTMLOrWikiTag "plaintext" = True
isHTMLOrWikiTag "rb" = True
isHTMLOrWikiTag "rtc" = True
isHTMLOrWikiTag "strike" = True
isHTMLOrWikiTag "xmp" = True
isHTMLOrWikiTag "basefont" = True
isHTMLOrWikiTag "big" = True
isHTMLOrWikiTag "blink" = True
isHTMLOrWikiTag "center" = True
isHTMLOrWikiTag "font" = True
isHTMLOrWikiTag "multicol" = True
isHTMLOrWikiTag "nobr" = True
isHTMLOrWikiTag "spacer" = True
isHTMLOrWikiTag "tt" = True
isHTMLOrWikiTag "marquee" = True
isHTMLOrWikiTag "frameset" = True
isHTMLOrWikiTag "frame" = True
isHTMLOrWikiTag _ = False

compact :: Either String [Token] -> Either String [Token]
compact (Left e) = Left e
compact (Right lst) =
  Right $ compactAcc lst []
  where
    compactAcc :: [Token] -> [Token] -> [Token]
    compactAcc [] acc = reverse acc
    compactAcc (TokenContent hd1:tl1) (TokenContent hd2:tl2) =
      compactAcc tl1 (TokenContent (hd2 <> hd1):tl2)
    compactAcc (hd:tl) acc = compactAcc tl (hd:acc)

tokenize :: Text -> Either String [Token]
tokenize = compact . AT.parseOnly (manyTill parseToken endOfInput)
