{-
Copyright (C) 2010 Paul Rivier <paul*rivier#demotera*com> | tr '*#' '.@'

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Textile
   Copyright   : Copyright (C) 2010-2012 Paul Rivier and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Paul Rivier <paul*rivier#demotera*com>
   Stability   : alpha
   Portability : portable

Conversion from Textile to 'Pandoc' document, based on the spec
available at http://redcloth.org/textile.

Implemented and parsed:
 - Paragraphs
 - Code blocks
 - Lists
 - blockquote
 - Inlines : strong, emph, cite, code, deleted, superscript,
   subscript, links
 - footnotes
 - HTML-specific and CSS-specific attributes on headers

Left to be implemented:
 - dimension sign
 - all caps
 - continued blocks (ex bq..)

TODO : refactor common patterns across readers :
 - more ...

-}


module Text.Pandoc.Readers.Textile ( readTextile) where
import Text.Pandoc.Definition
import Text.Pandoc.Builder (Inlines', Blocks', trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML ( htmlTag, isInlineTag, isBlockTag )
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.Pandoc.Shared ( scrubStrTag )
import Text.HTML.TagSoup (parseTags, innerText, fromAttrib, Tag(..))
import Text.HTML.TagSoup.Match
import Data.List ( intercalate )
import Data.Char ( digitToInt, isUpper)
import Control.Monad ( guard, liftM )
import Control.Applicative ((<$>), (*>), (<*), (<$))
import Data.Monoid

-- | Parse a Textile text and return a Pandoc document.
readTextile :: ReaderOptions -- ^ Reader options
            -> String       -- ^ String to parse (assuming @'\n'@ line endings)
            -> Pandoc' [SrcSpan]
readTextile opts s =
  (readWith parseTextile) def{ stateOptions = opts } (s ++ "\n\n")


-- | Generate a Pandoc ADT from a textile document
parseTextile :: Parser [Char] ParserState (Pandoc' [SrcSpan])
parseTextile = do
  -- textile allows raw HTML and does smart punctuation by default
  oldOpts <- stateOptions `fmap` getState
  updateState $ \state -> state{ stateOptions =
                                   oldOpts{ readerSmart = True
                                          , readerParseRaw = True
                                          , readerOldDashes = True
                                          } }
  many blankline
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys/notes were...
  let firstPassParser = noteBlock <|> lineClump
  manyTill firstPassParser eof >>= setInput . concat
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  blocks <- parseBlocks
  return $ Pandoc nullMeta (B.toList blocks) -- FIXME

noteMarker :: Parser [Char] ParserState [Char]
noteMarker = skipMany spaceChar >> string "fn" >> manyTill digit (char '.')

noteBlock :: Parser [Char] ParserState [Char]
noteBlock = try $ do
  startPos <- getPosition
  ref <- noteMarker
  optional blankline
  contents <- liftM unlines $ many1Till anyLine (blanklines <|> noteBlock)
  endPos <- getPosition
  let newnote = (ref, contents ++ "\n")
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

-- | Parse document blocks
parseBlocks :: Parser [Char] ParserState (Blocks' [SrcSpan])
parseBlocks = mconcat <$> manyTill block eof

-- | Block parsers list tried in definition order
blockParsers :: [Parser [Char] ParserState (Blocks' [SrcSpan])]
blockParsers = [ codeBlock
               , header
               , blockQuote
               , hrule
               , commentBlock
               , anyList
               , rawHtmlBlock
               , rawLaTeXBlock'
               , maybeExplicitBlock "table" table
               , maybeExplicitBlock "p" para
               , endBlock
               ]

endBlock :: Parser [Char] ParserState (Blocks' [SrcSpan])
endBlock = string "\n\n" >> return mempty
-- | Any block in the order of definition of blockParsers
block :: Parser [Char] ParserState (Blocks' [SrcSpan])
block = choice blockParsers <?> "block"

commentBlock :: Parser [Char] ParserState (Blocks' [SrcSpan])
commentBlock = try $ do
  string "###."
  manyTill anyLine blanklines
  return mempty

codeBlock :: Parser [Char] ParserState (Blocks' [SrcSpan])
codeBlock = codeBlockBc <|> codeBlockPre

codeBlockBc :: Parser [Char] ParserState (Blocks' [SrcSpan])
codeBlockBc = try $ do
  string "bc. "
  contents <- manyTill anyLine blanklines
  return $ B.codeBlock (unlines contents)

-- | Code Blocks in Textile are between <pre> and </pre>
codeBlockPre :: Parser [Char] ParserState (Blocks' [SrcSpan])
codeBlockPre = try $ do
  (t@(TagOpen _ attrs),_) <- htmlTag (tagOpen (=="pre") (const True))
  result' <- (innerText . parseTags) `fmap` -- remove internal tags
               manyTill anyChar (htmlTag (tagClose (=="pre")))
  optional blanklines
  -- drop leading newline if any
  let result'' = case result' of
                      '\n':xs -> xs
                      _       -> result'
  -- drop trailing newline if any
  let result''' = case reverse result'' of
                       '\n':_ -> init result''
                       _      -> result''
  let classes = words $ fromAttrib "class" t
  let ident = fromAttrib "id" t
  let kvs = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ B.codeBlockWith (ident,classes,kvs) result'''

-- | Header of the form "hN. content" with N in 1..6
header :: Parser [Char] ParserState (Blocks' [SrcSpan])
header = try $ do
  char 'h'
  level <- digitToInt <$> oneOf "123456"
  attr <- attributes
  char '.'
  lookAhead whitespace
  name <- trimInlines . mconcat <$> manyTill inline blockBreak
  attr' <- registerHeader attr name
  return $ B.headerWith attr' level name

-- | Blockquote of the form "bq. content"
blockQuote :: Parser [Char] ParserState (Blocks' [SrcSpan])
blockQuote = try $ do
  string "bq" >> attributes >> char '.' >> whitespace
  B.blockQuote <$> para

-- Horizontal rule

hrule :: Parser [Char] st (Blocks' [SrcSpan])
hrule = try $ do
  skipSpaces
  start <- oneOf "-*"
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return B.horizontalRule

-- Lists handling

-- | Can be a bullet list or an ordered list. This implementation is
-- strict in the nesting, sublist must start at exactly "parent depth
-- plus one"
anyList :: Parser [Char] ParserState (Blocks' [SrcSpan])
anyList = try $ anyListAtDepth 1 <* blanklines

-- | This allow one type of list to be nested into an other type,
-- provided correct nesting
anyListAtDepth :: Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
anyListAtDepth depth = choice [ bulletListAtDepth depth,
                                orderedListAtDepth depth,
                                definitionList ]

-- | Bullet List of given depth, depth being the number of leading '*'
bulletListAtDepth :: Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
bulletListAtDepth depth = try $ B.bulletList  <$> many1 (bulletListItemAtDepth depth)

-- | Bullet List Item of given depth, depth being the number of
-- leading '*'
bulletListItemAtDepth :: Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
bulletListItemAtDepth = genericListItemAtDepth '*'

-- | Ordered List of given depth, depth being the number of
-- leading '#'
orderedListAtDepth :: Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
orderedListAtDepth depth = try $ do
  items <- many1 (orderedListItemAtDepth depth)
  return $ B.orderedList items

-- | Ordered List Item of given depth, depth being the number of
-- leading '#'
orderedListItemAtDepth :: Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
orderedListItemAtDepth = genericListItemAtDepth '#'

-- | Common implementation of list items
genericListItemAtDepth :: Char -> Int -> Parser [Char] ParserState (Blocks' [SrcSpan])
genericListItemAtDepth c depth = try $ do
  count depth (char c) >> attributes >> whitespace
  p <- mconcat <$> many listInline
  newline
  sublist <- option mempty (anyListAtDepth (depth + 1))
  return $ (B.plain p) <> sublist

-- | A definition list is a set of consecutive definition items
definitionList :: Parser [Char] ParserState (Blocks' [SrcSpan])
definitionList = try $ B.definitionList <$> many1 definitionListItem

-- | List start character.
listStart :: Parser [Char] st Char
listStart = oneOf "*#-"

listInline :: Parser [Char] ParserState (Inlines' [SrcSpan])
listInline = try (notFollowedBy newline >> inline)
         <|> try (endline <* notFollowedBy listStart)

-- | A definition list item in textile begins with '- ', followed by
-- the term defined, then spaces and ":=". The definition follows, on
-- the same single line, or spaned on multiple line, after a line
-- break.
definitionListItem :: Parser [Char] ParserState ((Inlines' [SrcSpan]), [(Blocks' [SrcSpan])])
definitionListItem = try $ do
  string "- "
  term <- mconcat <$> many1Till inline (try (whitespace >> string ":="))
  def' <- multilineDef <|> inlineDef
  return (term, def')
  where inlineDef :: Parser [Char] ParserState [(Blocks' [SrcSpan])]
        inlineDef = liftM (\d -> [B.plain d])
                    $ optional whitespace >> (trimInlines . mconcat <$> many listInline) <* newline
        multilineDef :: Parser [Char] ParserState [(Blocks' [SrcSpan])]
        multilineDef = try $ do
          optional whitespace >> newline
          s <- many1Till anyChar (try (string "=:" >> newline))
          -- this ++ "\n\n" does not look very good
          ds <- parseFromString parseBlocks (s ++ "\n\n")
          return [ds]

-- | This terminates a block such as a paragraph. Because of raw html
-- blocks support, we have to lookAhead for a rawHtmlBlock.
blockBreak :: Parser [Char] ParserState ()
blockBreak = try (newline >> blanklines >> return ()) <|>
             try (optional spaces >> lookAhead rawHtmlBlock >> return ())

-- raw content

-- | A raw Html Block, optionally followed by blanklines
rawHtmlBlock :: Parser [Char] ParserState (Blocks' [SrcSpan])
rawHtmlBlock = try $ do
  (_,b) <- htmlTag isBlockTag
  optional blanklines
  return $ B.rawBlock "html" b

-- | Raw block of LaTeX content
rawLaTeXBlock' :: Parser [Char] ParserState (Blocks' [SrcSpan])
rawLaTeXBlock' = do
  guardEnabled Ext_raw_tex
  B.rawBlock "latex" <$> (rawLaTeXBlock <* spaces)


-- | In textile, paragraphs are separated by blank lines.
para :: Parser [Char] ParserState (Blocks' [SrcSpan])
para = B.para . trimInlines . mconcat <$> manyTill inline blockBreak

-- Tables

-- | A table cell spans until a pipe |
tableCell :: Parser [Char] ParserState (Blocks' [SrcSpan])
tableCell = do
  c <- many1 (noneOf "|\n")
  content <- trimInlines . mconcat <$> parseFromString (many1 inline) c
  return $ B.plain content

-- | A table row is made of many table cells
tableRow :: Parser [Char] ParserState [(Blocks' [SrcSpan])]
tableRow = try $ ( char '|' *>
  (endBy1 tableCell (optional blankline *> char '|')) <* newline)

-- | Many table rows
tableRows :: Parser [Char] ParserState [[(Blocks' [SrcSpan])]]
tableRows = many1 tableRow

-- | Table headers are made of cells separated by a tag "|_."
tableHeaders :: Parser [Char] ParserState [(Blocks' [SrcSpan])]
tableHeaders = let separator = (try $ string "|_.") in
  try $ ( separator *> (sepBy1 tableCell separator) <* char '|' <* newline )

-- | A table with an optional header. Current implementation can
-- handle tables with and without header, but will parse cells
-- alignment attributes as content.
table :: Parser [Char] ParserState (Blocks' [SrcSpan])
table = try $ do
  headers <- option mempty tableHeaders
  rows <- tableRows
  blanklines
  let nbOfCols = max (length headers) (length $ head rows)
  return $ B.table mempty
    (zip (replicate nbOfCols AlignDefault) (replicate nbOfCols 0.0))
    headers
    rows


-- | Blocks like 'p' and 'table' do not need explicit block tag.
-- However, they can be used to set HTML/CSS attributes when needed.
maybeExplicitBlock :: String  -- ^ block tag name
                    -> Parser [Char] ParserState (Blocks' [SrcSpan]) -- ^ implicit block
                    -> Parser [Char] ParserState (Blocks' [SrcSpan])
maybeExplicitBlock name blk = try $ do
  optional $ try $ string name >> attributes >> char '.' >>
    optional whitespace >> optional endline
  blk



----------
-- Inlines
----------


-- | Any inline element
inline :: Parser [Char] ParserState (Inlines' [SrcSpan])
inline = do
    choice inlineParsers <?> "inline"

-- | Inline parsers tried in order
inlineParsers :: [Parser [Char] ParserState (Inlines' [SrcSpan])]
inlineParsers = [ inlineMarkup
                , str
                , whitespace
                , endline
                , code
                , escapedInline
                , rawHtmlInline
                , rawLaTeXInline'
                , note
                , try $ (char '[' *> inlineMarkup <* char ']')
                , link
                , image
                , mark
                , (B.str . (:[])) <$> characterReference
                , smartPunctuation inline
                , symbol
                ]

-- | Inline markups
inlineMarkup :: Parser [Char] ParserState (Inlines' [SrcSpan])
inlineMarkup = choice [ simpleInline (string "??") (B.cite [])
                      , simpleInline (string "**") B.strong
                      , simpleInline (string "__") B.emph
                      , simpleInline (char '*') B.strong
                      , simpleInline (char '_') B.emph
                      , simpleInline (char '+') B.emph  -- approximates underline
                      , simpleInline (char '-' <* notFollowedBy (char '-')) B.strikeout
                      , simpleInline (char '^') B.superscript
                      , simpleInline (char '~') B.subscript
                      , simpleInline (char '%') id
                      ]

-- | Trademark, registered, copyright
mark :: Parser [Char] st (Inlines' [SrcSpan])
mark = try $ char '(' >> (try tm <|> try reg <|> copy)

reg :: Parser [Char] st (Inlines' [SrcSpan])
reg = do
  oneOf "Rr"
  char ')'
  return $ B.str "\174"

tm :: Parser [Char] st (Inlines' [SrcSpan])
tm = do
  oneOf "Tt"
  oneOf "Mm"
  char ')'
  return $ B.str "\8482"

copy :: Parser [Char] st (Inlines' [SrcSpan])
copy = do
  oneOf "Cc"
  char ')'
  return $ B.str "\169"

note :: Parser [Char] ParserState (Inlines' [SrcSpan])
note = try $ do
  ref <- (char '[' *> many1 digit <* char ']')
  notes <- stateNotes <$> getState
  case lookup ref notes of
    Nothing   -> fail "note not found"
    Just raw  -> B.note <$> parseFromString parseBlocks raw

-- | Special chars
markupChars :: [Char]
markupChars = "\\*#_@~-+^|%=[]&"

-- | Break strings on following chars. Space tab and newline break for
--  inlines breaking. Open paren breaks for mark. Quote, dash and dot
--  break for smart punctuation. Punctuation breaks for regular
--  punctuation. Double quote breaks for named links. > and < break
--  for inline html.
stringBreakers :: [Char]
stringBreakers = " \t\n\r.,\"'?!;:<>«»„“”‚‘’()[]"

wordBoundaries :: [Char]
wordBoundaries = markupChars ++ stringBreakers

-- | Parse a hyphened sequence of words
hyphenedWords :: Parser [Char] ParserState String
hyphenedWords = do
  x <- wordChunk
  xs <-  many (try $ char '-' >> wordChunk)
  return $ intercalate "-" (x:xs)

wordChunk :: Parser [Char] ParserState String
wordChunk = try $ do
  hd <- noneOf wordBoundaries
  tl <- many ( (noneOf wordBoundaries) <|>
               try (notFollowedBy' note *> oneOf markupChars
                     <* lookAhead (noneOf wordBoundaries) ) )
  return $ hd:tl

-- | Any string
str :: Parser [Char] ParserState (Inlines' [SrcSpan])
str = do
  baseStr <- hyphenedWords
  -- RedCloth compliance : if parsed word is uppercase and immediatly
  -- followed by parens, parens content is unconditionally word acronym
  fullStr <- option baseStr $ try $ do
    guard $ all isUpper baseStr
    acro <- enclosed (char '(') (char ')') anyChar
    return $ concat [baseStr, " (", acro, ")"]
  updateLastStrPos
  return $ B.str fullStr

-- | Some number of space chars
whitespace :: Parser [Char] ParserState (Inlines' [SrcSpan])
whitespace = many1 spaceChar >> return B.space <?> "whitespace"

-- | In Textile, an isolated endline character is a line break
endline :: Parser [Char] ParserState (Inlines' [SrcSpan])
endline = try $ do
  newline >> notFollowedBy blankline
  return B.linebreak

rawHtmlInline :: Parser [Char] ParserState (Inlines' [SrcSpan])
rawHtmlInline = B.rawInline "html" . snd <$> htmlTag isInlineTag

-- | Raw LaTeX Inline
rawLaTeXInline' :: Parser [Char] ParserState (Inlines' [SrcSpan])
rawLaTeXInline' = try $ do
  guardEnabled Ext_raw_tex
  B.singleton <$> rawLaTeXInline

-- | Textile standard link syntax is "label":target. But we
-- can also have ["label":target].
link :: Parser [Char] ParserState (Inlines' [SrcSpan])
link = try $ do
  bracketed <- (True <$ char '[') <|> return False
  char '"' *> notFollowedBy (oneOf " \t\n\r")
  attr <- attributes
  name <- trimInlines . mconcat <$>
          withQuoteContext InSingleQuote (manyTill inline (try (string "\":")))
  let stop = if bracketed
                then char ']'
                else lookAhead $ space <|>
                       try (oneOf "!.,;:" *> (space <|> newline))
  url <- manyTill nonspaceChar stop
  let name' = if fmap scrubStrTag (B.toList name) == [Str "$" ()] then B.str url else name
  return $ if attr == nullAttr
              then B.link url "" name'
              else B.spanWith attr $ B.link url "" name'

-- | image embedding
image :: Parser [Char] ParserState (Inlines' [SrcSpan])
image = try $ do
  char '!' >> notFollowedBy space
  src <- manyTill anyChar (lookAhead $ oneOf "!(")
  alt <- option "" (try $ (char '(' >> manyTill anyChar (char ')')))
  char '!'
  return $ B.image src alt (B.str alt)

escapedInline :: Parser [Char] ParserState (Inlines' [SrcSpan])
escapedInline = escapedEqs <|> escapedTag

escapedEqs :: Parser [Char] ParserState (Inlines' [SrcSpan])
escapedEqs = B.str <$> (try $ string "==" *> manyTill anyChar (try $ string "=="))

-- | literal text escaped btw <notextile> tags
escapedTag :: Parser [Char] ParserState (Inlines' [SrcSpan])
escapedTag = B.str <$>
  (try $ string "<notextile>" *> manyTill anyChar (try $ string "</notextile>"))

-- | Any special symbol defined in wordBoundaries
symbol :: Parser [Char] ParserState (Inlines' [SrcSpan])
symbol = B.str . singleton <$> (oneOf wordBoundaries <|> oneOf markupChars)

-- | Inline code
code :: Parser [Char] ParserState (Inlines' [SrcSpan])
code = code1 <|> code2

code1 :: Parser [Char] ParserState (Inlines' [SrcSpan])
code1 = B.code <$> surrounded (char '@') anyChar

code2 :: Parser [Char] ParserState (Inlines' [SrcSpan])
code2 = do
  htmlTag (tagOpen (=="tt") null)
  B.code <$> manyTill anyChar (try $ htmlTag $ tagClose (=="tt"))

-- | Html / CSS attributes
attributes :: Parser [Char] ParserState Attr
attributes = (foldl (flip ($)) ("",[],[])) `fmap` many attribute

attribute :: Parser [Char] ParserState (Attr -> Attr)
attribute = classIdAttr <|> styleAttr <|> langAttr

classIdAttr :: Parser [Char] ParserState (Attr -> Attr)
classIdAttr = try $ do -- (class class #id)
  char '('
  ws <- words `fmap` manyTill anyChar (char ')')
  case reverse ws of
       []                      -> return $ \(_,_,keyvals) -> ("",[],keyvals)
       (('#':ident'):classes') -> return $ \(_,_,keyvals) ->
                                             (ident',classes',keyvals)
       classes'                -> return $ \(_,_,keyvals) ->
                                             ("",classes',keyvals)

styleAttr :: Parser [Char] ParserState (Attr -> Attr)
styleAttr = do
  style <- try $ enclosed (char '{') (char '}') anyChar
  return $ \(id',classes,keyvals) -> (id',classes,("style",style):keyvals)

langAttr :: Parser [Char] ParserState (Attr -> Attr)
langAttr = do
  lang <- try $ enclosed (char '[') (char ']') alphaNum
  return $ \(id',classes,keyvals) -> (id',classes,("lang",lang):keyvals)

-- | Parses material surrounded by a parser.
surrounded :: Parser [Char] st t   -- ^ surrounding parser
	    -> Parser [Char] st a    -- ^ content parser (to be used repeatedly)
	    -> Parser [Char] st [a]
surrounded border = enclosed (border *> notFollowedBy (oneOf " \t\n\r")) (try border)


simpleInline :: Parser [Char] ParserState t           -- ^ surrounding parser
                -> ((Inlines' [SrcSpan]) -> (Inlines' [SrcSpan]))       -- ^ Inline constructor
                -> Parser [Char] ParserState (Inlines' [SrcSpan])   -- ^ content parser (to be used repeatedly)
simpleInline border construct = groupedSimpleInline border construct <|> ungroupedSimpleInline border construct

ungroupedSimpleInline :: Parser [Char] ParserState t           -- ^ surrounding parser
                -> ((Inlines' [SrcSpan]) -> (Inlines' [SrcSpan]))       -- ^ Inline constructor
                -> Parser [Char] ParserState (Inlines' [SrcSpan])   -- ^ content parser (to be used repeatedly)
ungroupedSimpleInline border construct = try $ do
  st <- getState
  pos <- getPosition
  isWhitespace <- option False (whitespace >> return True)
  guard $ (stateQuoteContext st /= NoQuote)
       || (sourceColumn pos == 1)
       || isWhitespace
  border *> notFollowedBy (oneOf " \t\n\r")
  attr <- attributes
  body <- trimInlines . mconcat <$>
          withQuoteContext InSingleQuote
            (manyTill inline (try border <* notFollowedBy alphaNum))
  let result = construct $
        if attr == nullAttr
           then body
           else B.spanWith attr body
  return $ if isWhitespace
              then B.space <> result
              else result

groupedSimpleInline :: Parser [Char] ParserState t
                  -> ((Inlines' [SrcSpan]) -> (Inlines' [SrcSpan]))
                  -> Parser [Char] ParserState (Inlines' [SrcSpan])
groupedSimpleInline border construct = try $ do
    char '['
    withQuoteContext InSingleQuote (simpleInline border construct) >>~ char ']'




-- | Create a singleton list
singleton :: a -> [a]
singleton x = [x]

