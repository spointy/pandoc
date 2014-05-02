{-# LANGUAGE PatternGuards, CPP, ScopedTypeVariables #-}
{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.EPUB
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to EPUB.
-}
module Text.Pandoc.Writers.EPUB ( writeEPUB ) where
import Data.IORef
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.List ( isInfixOf, intercalate )
import System.Environment ( getEnv )
import Text.Printf (printf)
import System.FilePath ( (</>), takeExtension, takeFileName )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.SelfContained ( makeSelfContained )
import Codec.Archive.Zip
import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import Text.Pandoc.Shared hiding ( Element )
import qualified Text.Pandoc.Shared as Shared
import Text.Pandoc.Builder (fromList, setMeta)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Control.Monad.State
import Text.XML.Light hiding (ppTopElement)
import Text.Pandoc.UUID
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.Markdown ( writePlain )
import Data.Char ( toLower, isDigit, isAlphaNum )
import Network.URI ( unEscapeString )
import Text.Pandoc.MIME (getMimeType)
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import Control.Exception (catch, SomeException)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.HTML.TagSoup

-- A Chapter includes a list of blocks and maybe a section
-- number offset.  Note, some chapters are unnumbered. The section
-- number is different from the index number, which will be used
-- in filenames, chapter0003.xhtml.
data Chapter = Chapter (Maybe [Int]) [Block]

data EPUBMetadata = EPUBMetadata{
    epubIdentifier         :: [Identifier]
  , epubTitle              :: [Title]
  , epubDate               :: String
  , epubLanguage           :: String
  , epubCreator            :: [Creator]
  , epubContributor        :: [Creator]
  , epubSubject            :: [String]
  , epubDescription        :: Maybe String
  , epubType               :: Maybe String
  , epubFormat             :: Maybe String
  , epubPublisher          :: Maybe String
  , epubSource             :: Maybe String
  , epubRelation           :: Maybe String
  , epubCoverage           :: Maybe String
  , epubRights             :: Maybe String
  , epubCoverImage         :: Maybe String
  , epubStylesheet         :: Maybe Stylesheet
  } deriving Show

data Stylesheet = StylesheetPath FilePath
                | StylesheetContents String
                deriving Show

data Creator = Creator{
    creatorText            :: String
  , creatorRole            :: Maybe String
  , creatorFileAs          :: Maybe String
  } deriving Show

data Identifier = Identifier{
    identifierText         :: String
  , identifierScheme       :: Maybe String
  } deriving Show

data Title = Title{
    titleText              :: String
  , titleFileAs            :: Maybe String
  , titleType              :: Maybe String
  } deriving Show

dcName :: String -> QName
dcName n = QName n Nothing (Just "dc")

dcNode :: Node t => String -> t -> Element
dcNode = node . dcName

opfName :: String -> QName
opfName n = QName n Nothing (Just "opf")

plainify :: [Inline] -> String
plainify t =
  trimr $ writePlain def{ writerStandalone = False }
        $ Pandoc nullMeta [Plain $ walk removeNote t]

removeNote :: Inline -> Inline
removeNote (Note _) = Str "" ()
removeNote x        = x

toId :: FilePath -> String
toId = map (\x -> if isAlphaNum x || x == '-' || x == '_'
                     then x
                     else '_') . takeFileName

getEPUBMetadata :: WriterOptions -> Meta -> IO EPUBMetadata
getEPUBMetadata opts meta = do
  let md = metadataFromMeta opts meta
  let elts = onlyElems $ parseXML $ writerEpubMetadata opts
  let md' = foldr addMetadataFromXML md elts
  let addIdentifier m =
       if null (epubIdentifier m)
          then do
            randomId <- fmap show getRandomUUID
            return $ m{ epubIdentifier = [Identifier randomId Nothing] }
          else return m
  let addLanguage m =
       if null (epubLanguage m)
          then case lookup "lang" (writerVariables opts) of
                     Just x  -> return m{ epubLanguage = x }
                     Nothing -> do
                       localeLang <- catch (liftM
                            (map (\c -> if c == '_' then '-' else c) .
                             takeWhile (/='.')) $ getEnv "LANG")
                          (\e -> let _ = (e :: SomeException) in return "en-US")
                       return m{ epubLanguage = localeLang }
          else return m
  let fixDate m =
       if null (epubDate m)
          then do
            currentTime <- getCurrentTime
            return $ m{ epubDate = showDateTimeISO8601 currentTime }
          else return m
  let addAuthor m =
       if any (\c -> creatorRole c == Just "aut") $ epubCreator m
          then return m
          else do
            let authors' = map plainify $ docAuthors meta
            let toAuthor name = Creator{ creatorText = name
                                       , creatorRole = Just "aut"
                                       , creatorFileAs = Nothing }
            return $ m{ epubCreator = map toAuthor authors' ++ epubCreator m }
  addIdentifier md' >>= fixDate >>= addAuthor >>= addLanguage

addMetadataFromXML :: Element -> EPUBMetadata -> EPUBMetadata
addMetadataFromXML e@(Element (QName name _ (Just "dc")) attrs _ _) md
  | name == "identifier" = md{ epubIdentifier =
             Identifier{ identifierText = strContent e
                       , identifierScheme = lookupAttr (opfName "scheme") attrs
                       } : epubIdentifier md }
  | name == "title" = md{ epubTitle =
            Title{ titleText = strContent e
                 , titleFileAs = getAttr "file-as"
                 , titleType = getAttr "type"
                 } : epubTitle md }
  | name == "date" = md{ epubDate = fromMaybe "" $ normalizeDate'
                                                 $ strContent e }
  | name == "language" = md{ epubLanguage = strContent e }
  | name == "creator" = md{ epubCreator =
              Creator{ creatorText = strContent e
                     , creatorRole = getAttr "role"
                     , creatorFileAs = getAttr "file-as"
                     } : epubCreator md }
  | name == "contributor" = md{ epubContributor =
              Creator  { creatorText = strContent e
                       , creatorRole = getAttr "role"
                       , creatorFileAs = getAttr "file-as"
                       } : epubContributor md }
  | name == "subject" = md{ epubSubject = strContent e : epubSubject md }
  | name == "description" = md { epubDescription = Just $ strContent e }
  | name == "type" = md { epubType = Just $ strContent e }
  | name == "format" = md { epubFormat = Just $ strContent e }
  | name == "type" = md { epubType = Just $ strContent e }
  | name == "publisher" = md { epubPublisher = Just $ strContent e }
  | name == "source" = md { epubSource = Just $ strContent e }
  | name == "relation" = md { epubRelation = Just $ strContent e }
  | name == "coverage" = md { epubCoverage = Just $ strContent e }
  | name == "rights" = md { epubRights = Just $ strContent e }
  | otherwise = md
  where getAttr n = lookupAttr (opfName n) attrs
addMetadataFromXML _ md = md

metaValueToString :: MetaValue -> String
metaValueToString (MetaString s) = s
metaValueToString (MetaInlines ils) = plainify ils
metaValueToString (MetaBlocks bs) = plainify $ query (:[]) bs
metaValueToString (MetaBool b) = show b
metaValueToString _ = ""

getList :: String -> Meta -> (MetaValue -> a) -> [a]
getList s meta handleMetaValue =
  case lookupMeta s meta of
       Just (MetaList xs) -> map handleMetaValue xs
       Just mv            -> [handleMetaValue mv]
       Nothing            -> []

getIdentifier :: Meta -> [Identifier]
getIdentifier meta = getList "identifier" meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Identifier{ identifierText = maybe "" metaValueToString
                                        $ M.lookup "text" m
                     , identifierScheme = metaValueToString <$>
                                          M.lookup "scheme" m }
        handleMetaValue mv = Identifier (metaValueToString mv) Nothing

getTitle :: Meta -> [Title]
getTitle meta = getList "title" meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Title{ titleText = maybe "" metaValueToString $ M.lookup "text" m
                , titleFileAs = metaValueToString <$> M.lookup "file-as" m
                , titleType = metaValueToString <$> M.lookup "type" m }
        handleMetaValue mv = Title (metaValueToString mv) Nothing Nothing

getCreator :: String -> Meta -> [Creator]
getCreator s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Creator{ creatorText = maybe "" metaValueToString $ M.lookup "text" m
                  , creatorFileAs = metaValueToString <$> M.lookup "file-as" m
                  , creatorRole = metaValueToString <$> M.lookup "role" m }
        handleMetaValue mv = Creator (metaValueToString mv) Nothing Nothing

simpleList :: String -> Meta -> [String]
simpleList s meta =
  case lookupMeta s meta of
       Just (MetaList xs) -> map metaValueToString xs
       Just x -> [metaValueToString x]
       Nothing -> []

metadataFromMeta :: WriterOptions -> Meta -> EPUBMetadata
metadataFromMeta opts meta = EPUBMetadata{
      epubIdentifier         = identifiers
    , epubTitle              = titles
    , epubDate               = date
    , epubLanguage           = language
    , epubCreator            = creators
    , epubContributor        = contributors
    , epubSubject            = subjects
    , epubDescription        = description
    , epubType               = epubtype
    , epubFormat             = format
    , epubPublisher          = publisher
    , epubSource             = source
    , epubRelation           = relation
    , epubCoverage           = coverage
    , epubRights             = rights
    , epubCoverImage         = coverImage
    , epubStylesheet         = stylesheet
    }
  where identifiers = getIdentifier meta
        titles = getTitle meta
        date = fromMaybe "" $
              (metaValueToString <$> lookupMeta "date" meta) >>= normalizeDate'
        language = maybe "" metaValueToString $
           lookupMeta "language" meta `mplus` lookupMeta "lang" meta
        creators = getCreator "creator" meta
        contributors = getCreator "contributor" meta
        subjects = simpleList "subject" meta
        description = metaValueToString <$> lookupMeta "description" meta
        epubtype = metaValueToString <$> lookupMeta "type" meta
        format = metaValueToString <$> lookupMeta "format" meta
        publisher = metaValueToString <$> lookupMeta "publisher" meta
        source = metaValueToString <$> lookupMeta "source" meta
        relation = metaValueToString <$> lookupMeta "relation" meta
        coverage = metaValueToString <$> lookupMeta "coverage" meta
        rights = metaValueToString <$> lookupMeta "rights" meta
        coverImage = lookup "epub-cover-image" (writerVariables opts) `mplus`
             (metaValueToString <$> lookupMeta "cover-image" meta)
        stylesheet = (StylesheetContents <$> writerEpubStylesheet opts) `mplus`
                     ((StylesheetPath . metaValueToString) <$>
                       lookupMeta "stylesheet" meta)

-- | Produce an EPUB file from a Pandoc document.
writeEPUB :: WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO B.ByteString
writeEPUB opts doc@(Pandoc meta _) = do
  let version = fromMaybe EPUB2 (writerEpubVersion opts)
  let epub3 = version == EPUB3
  epochtime <- floor `fmap` getPOSIXTime
  let mkEntry path content = toEntry path epochtime content
  let vars = ("epub3", if epub3 then "true" else "false")
           : ("css", "stylesheet.css")
           : writerVariables opts
  let opts' = opts{ writerEmailObfuscation = NoObfuscation
                  , writerStandalone = True
                  , writerSectionDivs = True
                  , writerHtml5 = epub3
                  , writerTableOfContents = False -- we always have one in epub
                  , writerVariables = vars
                  , writerHTMLMathMethod =
                       if epub3
                          then MathML Nothing
                          else writerHTMLMathMethod opts
                  , writerWrapText = False }
  metadata <- getEPUBMetadata opts' meta

  -- cover page
  (cpgEntry, cpicEntry) <-
                case epubCoverImage metadata of
                     Nothing   -> return ([],[])
                     Just img  -> do
                       let coverImage = "cover-image" ++ takeExtension img
                       let cpContent = renderHtml $ writeHtml opts'
                               (Pandoc meta [RawBlock (Format "html") $ "<div id=\"cover-image\">\n<img src=\"" ++ coverImage ++ "\" alt=\"cover image\" />\n</div>"])
                       imgContent <- B.readFile img
                       return ( [mkEntry "cover.xhtml" cpContent]
                              , [mkEntry coverImage imgContent] )

  -- title page
  let tpContent = renderHtml $ writeHtml opts'{
                      writerVariables = ("titlepage","true"):vars }
                      (Pandoc meta [])
  let tpEntry = mkEntry "title_page.xhtml" tpContent

  -- handle pictures
  mediaRef <- newIORef []
  Pandoc _ blocks <- walkM (transformInline opts' mediaRef) doc >>=
                     walkM (transformBlock opts' mediaRef)
  pics <- readIORef mediaRef
  let readPicEntry entries (oldsrc, newsrc) = do
        res <- fetchItem (writerSourceURL opts') oldsrc
        case res of
             Left _        -> do
              warn $ "Could not find media `" ++ oldsrc ++ "', skipping..."
              return entries
             Right (img,_) -> return $
              (toEntry newsrc epochtime $ B.fromChunks . (:[]) $ img) : entries
  picEntries <- foldM readPicEntry [] pics

  -- handle fonts
  let mkFontEntry f = mkEntry (takeFileName f) `fmap` B.readFile f
  fontEntries <- mapM mkFontEntry $ writerEpubFonts opts'

  -- body pages

  -- add level 1 header to beginning if none there
  let blocks' = addIdentifiers
                $ case blocks of
                      (Header 1 _ _ : _) -> blocks
                      _                  -> Header 1 ("",["unnumbered"],[])
                                                 (docTitle meta) : blocks

  let chapterHeaderLevel = writerEpubChapterLevel opts
  -- internal reference IDs change when we chunk the file,
  -- so that '#my-header-1' might turn into 'chap004.xhtml#my-header'.
  -- the next two lines fix that:
  let reftable = correlateRefs chapterHeaderLevel blocks'
  let blocks'' = replaceRefs reftable blocks'

  let isChapterHeader (Header n _ _) = n <= chapterHeaderLevel
      isChapterHeader _ = False

  let toChapters :: [Block] -> State [Int] [Chapter]
      toChapters []     = return []
      toChapters (Header n attr@(_,classes,_) ils : bs) = do
        nums <- get
        mbnum <- if "unnumbered" `elem` classes
                    then return Nothing
                    else case splitAt (n - 1) nums of
                              (ks, (m:_)) -> do
                                let nums' = ks ++ [m+1]
                                put nums'
                                return $ Just (ks ++ [m])
                                -- note, this is the offset not the sec number
                              (ks, []) -> do
                                let nums' = ks ++ [1]
                                put nums'
                                return $ Just ks
        let (xs,ys) = break isChapterHeader bs
        (Chapter mbnum (Header n attr ils : xs) :) `fmap` toChapters ys
      toChapters (b:bs) = do
        let (xs,ys) = break isChapterHeader bs
        (Chapter Nothing (b:xs) :) `fmap` toChapters ys

  let chapters = evalState (toChapters blocks'') []

  let chapToEntry :: Int -> Chapter -> Entry
      chapToEntry num (Chapter mbnum bs) = mkEntry (showChapter num)
        $ renderHtml
        $ writeHtml opts'{ writerNumberOffset =
            fromMaybe [] mbnum }
        $ case bs of
              (Header _ _ xs : _) ->
                 -- remove notes or we get doubled footnotes
                 Pandoc (setMeta "title" (walk removeNote $ fromList xs)
                            nullMeta) bs
              _                   ->
                 Pandoc nullMeta bs

  let chapterEntries = zipWith chapToEntry [1..] chapters

  -- incredibly inefficient (TODO):
  let containsMathML ent = epub3 &&
                           "<math" `isInfixOf` (B8.unpack $ fromEntry ent)
  let containsSVG ent    = epub3 &&
                           "<svg" `isInfixOf` (B8.unpack $ fromEntry ent)
  let props ent = ["mathml" | containsMathML ent] ++ ["svg" | containsSVG ent]

  -- contents.opf
  let chapterNode ent = unode "item" !
                           ([("id", toId $ eRelativePath ent),
                             ("href", eRelativePath ent),
                             ("media-type", "application/xhtml+xml")]
                            ++ case props ent of
                                    []   -> []
                                    xs   -> [("properties", unwords xs)])
                        $ ()
  let chapterRefNode ent = unode "itemref" !
                             [("idref", toId $ eRelativePath ent)] $ ()
  let pictureNode ent = unode "item" !
                           [("id", toId $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", fromMaybe "application/octet-stream"
                               $ mediaTypeOf $ eRelativePath ent)] $ ()
  let fontNode ent = unode "item" !
                           [("id", toId $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", fromMaybe "" $ getMimeType $ eRelativePath ent)] $ ()
  let plainTitle = case docTitle meta of
                        [] -> case epubTitle metadata of
                                   []   -> "UNTITLED"
                                   (x:_) -> titleText x
                        x  -> plainify x
  let uuid = case epubIdentifier metadata of
                  (x:_) -> identifierText x  -- use first identifier as UUID
                  []    -> error "epubIdentifier is null"  -- shouldn't happen
  currentTime <- getCurrentTime
  let contentsData = UTF8.fromStringLazy $ ppTopElement $
        unode "package" ! [("version", case version of
                                             EPUB2 -> "2.0"
                                             EPUB3 -> "3.0")
                          ,("xmlns","http://www.idpf.org/2007/opf")
                          ,("unique-identifier","epub-id-1")] $
          [ metadataElement version metadata currentTime
          , unode "manifest" $
             [ unode "item" ! [("id","ncx"), ("href","toc.ncx")
                              ,("media-type","application/x-dtbncx+xml")] $ ()
             , unode "item" ! [("id","style"), ("href","stylesheet.css")
                              ,("media-type","text/css")] $ ()
             , unode "item" ! ([("id","nav")
                               ,("href","nav.xhtml")
                               ,("media-type","application/xhtml+xml")] ++
                               [("properties","nav") | epub3 ]) $ ()
             ] ++
             map chapterNode (cpgEntry ++ (tpEntry : chapterEntries)) ++
             (case cpicEntry of
                    []    -> []
                    (x:_) -> [add_attrs
                              [Attr (unqual "properties") "cover-image" | epub3]
                              (pictureNode x)]) ++
             map pictureNode picEntries ++
             map fontNode fontEntries
          , unode "spine" ! [("toc","ncx")] $
              case epubCoverImage metadata of
                    Nothing -> []
                    Just _ -> [ unode "itemref" !
                                [("idref", "cover_xhtml"),("linear","no")] $ () ]
              ++ ((unode "itemref" ! [("idref", "title_page_xhtml")
                                     ,("linear", if null (docTitle meta)
                                                    then "no"
                                                    else "yes")] $ ()) :
                  (unode "itemref" ! [("idref", "nav")
                                     ,("linear", if writerTableOfContents opts
                                                    then "yes"
                                                    else "no")] $ ()) :
                  map chapterRefNode chapterEntries)
          , unode "guide" $
             [ unode "reference" !
                   [("type","toc"),("title",plainTitle),
                    ("href","nav.xhtml")] $ ()
             ] ++
             [ unode "reference" !
                   [("type","cover"),("title","Cover"),("href","cover.xhtml")] $ () | epubCoverImage metadata /= Nothing
             ]
          ]
  let contentsEntry = mkEntry "content.opf" contentsData

  -- toc.ncx
  let secs = hierarchicalize blocks''

  let tocLevel = writerTOCDepth opts

  let navPointNode :: (Int -> String -> String -> [Element] -> Element)
                   -> Shared.Element -> State Int Element
      navPointNode formatter (Sec _ nums (ident,_,_) ils children) = do
        n <- get
        modify (+1)
        let showNums :: [Int] -> String
            showNums = intercalate "." . map show
        let tit' = plainify ils
        let tit = if writerNumberSections opts && not (null nums)
                     then showNums nums ++ " " ++ tit'
                     else tit'
        let src = case lookup ident reftable of
                       Just x  -> x
                       Nothing -> error (ident ++ " not found in reftable")
        let isSec (Sec lev _ _ _ _) = lev <= tocLevel
            isSec _                 = False
        let subsecs = filter isSec children
        subs <- mapM (navPointNode formatter) subsecs
        return $ formatter n tit src subs
      navPointNode _ (Blk _) = error "navPointNode encountered Blk"

  let navMapFormatter :: Int -> String -> String -> [Element] -> Element
      navMapFormatter n tit src subs = unode "navPoint" !
               [("id", "navPoint-" ++ show n)
               ,("playOrder", show n)] $
                  [ unode "navLabel" $ unode "text" tit
                  , unode "content" ! [("src", src)] $ ()
                  ] ++ subs

  let tpNode = unode "navPoint" !  [("id", "navPoint-0")] $
                  [ unode "navLabel" $ unode "text" (plainify $ docTitle meta)
                  , unode "content" ! [("src","title_page.xhtml")] $ () ]

  let tocData = UTF8.fromStringLazy $ ppTopElement $
        unode "ncx" ! [("version","2005-1")
                       ,("xmlns","http://www.daisy.org/z3986/2005/ncx/")] $
          [ unode "head" $
             [ unode "meta" ! [("name","dtb:uid")
                              ,("content", uuid)] $ ()
             , unode "meta" ! [("name","dtb:depth")
                              ,("content", "1")] $ ()
             , unode "meta" ! [("name","dtb:totalPageCount")
                              ,("content", "0")] $ ()
             , unode "meta" ! [("name","dtb:maxPageNumber")
                              ,("content", "0")] $ ()
             ] ++ case epubCoverImage metadata of
                        Nothing  -> []
                        Just _   -> [unode "meta" ! [("name","cover"),
                                            ("content","cover-image")] $ ()]
          , unode "docTitle" $ unode "text" $ plainTitle
          , unode "navMap" $
              tpNode : evalState (mapM (navPointNode navMapFormatter) secs) 1
          ]
  let tocEntry = mkEntry "toc.ncx" tocData

  let navXhtmlFormatter :: Int -> String -> String -> [Element] -> Element
      navXhtmlFormatter n tit src subs = unode "li" !
                                       [("id", "toc-li-" ++ show n)] $
                                            (unode "a" ! [("href",src)]
                                             $ (unode "span" tit))
                                            : case subs of
                                                 []    -> []
                                                 (_:_) -> [unode "ol" ! [("class","toc")] $ subs]

  let navtag = if epub3 then "nav" else "div"
  let navData = UTF8.fromStringLazy $ ppTopElement $
        unode "html" ! [("xmlns","http://www.w3.org/1999/xhtml")
                       ,("xmlns:epub","http://www.idpf.org/2007/ops")] $
          [ unode "head" $
            [ unode "title" plainTitle
            , unode "link" ! [("rel","stylesheet"),("type","text/css"),("href","stylesheet.css")] $ () ]
          , unode "body" $
              unode navtag ! [("epub:type","toc") | epub3] $
                [ unode "h1" ! [("id","toc-title")] $ plainTitle
                , unode "ol" ! [("class","toc")] $ evalState (mapM (navPointNode navXhtmlFormatter) secs) 1]
          ]
  let navEntry = mkEntry "nav.xhtml" navData

  -- mimetype
  let mimetypeEntry = mkEntry "mimetype" $ UTF8.fromStringLazy "application/epub+zip"

  -- container.xml
  let containerData = UTF8.fromStringLazy $ ppTopElement $
       unode "container" ! [("version","1.0")
              ,("xmlns","urn:oasis:names:tc:opendocument:xmlns:container")] $
         unode "rootfiles" $
           unode "rootfile" ! [("full-path","content.opf")
               ,("media-type","application/oebps-package+xml")] $ ()
  let containerEntry = mkEntry "META-INF/container.xml" containerData

  -- com.apple.ibooks.display-options.xml
  let apple = UTF8.fromStringLazy $ ppTopElement $
        unode "display_options" $
          unode "platform" ! [("name","*")] $
            unode "option" ! [("name","specified-fonts")] $ "true"
  let appleEntry = mkEntry "META-INF/com.apple.ibooks.display-options.xml" apple

  -- stylesheet
  stylesheet <- case epubStylesheet metadata of
                   Just (StylesheetPath fp)    -> UTF8.readFile fp
                   Just (StylesheetContents s) -> return s
                   Nothing -> UTF8.toString `fmap`
                              readDataFile (writerUserDataDir opts) "epub.css"
  let stylesheetEntry = mkEntry "stylesheet.css" $ UTF8.fromStringLazy stylesheet

  -- construct archive
  let archive = foldr addEntryToArchive emptyArchive
                 (mimetypeEntry : containerEntry : appleEntry : stylesheetEntry : tpEntry :
                  contentsEntry : tocEntry : navEntry :
                  (picEntries ++ cpicEntry ++ cpgEntry ++ chapterEntries ++ fontEntries))
  return $ fromArchive archive

metadataElement :: EPUBVersion -> EPUBMetadata -> UTCTime -> Element
metadataElement version md currentTime =
  unode "metadata" ! [("xmlns:dc","http://purl.org/dc/elements/1.1/")
                     ,("xmlns:opf","http://www.idpf.org/2007/opf")] $ mdNodes
  where mdNodes = identifierNodes ++ titleNodes ++ dateNodes ++ languageNodes
                  ++ creatorNodes ++ contributorNodes ++ subjectNodes
                  ++ descriptionNodes ++ typeNodes ++ formatNodes
                  ++ publisherNodes ++ sourceNodes ++ relationNodes
                  ++ coverageNodes ++ rightsNodes ++ coverImageNodes
                  ++ modifiedNodes
        withIds base f = concat . zipWith f (map (\x -> base ++ ('-' : show x))
                         ([1..] :: [Int]))
        identifierNodes = withIds "epub-id" toIdentifierNode $
                          epubIdentifier md
        titleNodes = withIds "epub-title" toTitleNode $ epubTitle md
        dateNodes = dcTag' "date" $ epubDate md
        languageNodes = [dcTag "language" $ epubLanguage md]
        creatorNodes = withIds "epub-creator" (toCreatorNode "creator") $
                       epubCreator md
        contributorNodes = withIds "epub-contributor"
                           (toCreatorNode "contributor") $ epubContributor md
        subjectNodes = map (dcTag "subject") $ epubSubject md
        descriptionNodes = maybe [] (dcTag' "description") $ epubDescription md
        typeNodes = maybe [] (dcTag' "type") $ epubType md
        formatNodes = maybe [] (dcTag' "format") $ epubFormat md
        publisherNodes = maybe [] (dcTag' "publisher") $ epubPublisher md
        sourceNodes = maybe [] (dcTag' "source") $ epubSource md
        relationNodes = maybe [] (dcTag' "relation") $ epubRelation md
        coverageNodes = maybe [] (dcTag' "coverage") $ epubCoverage md
        rightsNodes = maybe [] (dcTag' "rights") $ epubRights md
        coverImageNodes = maybe []
            (const $ [unode "meta" !  [("name","cover"),
                                       ("content","cover-image")] $ ()])
            $ epubCoverImage md
        modifiedNodes = [ unode "meta" ! [("property", "dcterms:modified")] $
               (showDateTimeISO8601 currentTime) | version == EPUB3 ]
        dcTag n s = unode ("dc:" ++ n) s
        dcTag' n s = [dcTag n s]
        toIdentifierNode id' (Identifier txt scheme)
          | version == EPUB2 = [dcNode "identifier" !
              ([("id",id')] ++ maybe [] (\x -> [("opf:scheme", x)]) scheme) $
              txt]
          | otherwise = [dcNode "identifier" ! [("id",id')] $ txt] ++
              maybe [] (\x -> [unode "meta" !
                  [("refines",'#':id'),("property","identifier-type"),
                   ("scheme","onix:codelist5")] $ x])
                (schemeToOnix `fmap` scheme)
        toCreatorNode s id' creator
          | version == EPUB2 = [dcNode s !
             ([("id",id')] ++
              maybe [] (\x -> [("opf:file-as",x)]) (creatorFileAs creator) ++
              maybe [] (\x -> [("opf:role",x)])
               (creatorRole creator >>= toRelator)) $ creatorText creator]
          | otherwise = [dcNode s ! [("id",id')] $ creatorText creator] ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","file-as")] $ x])
                   (creatorFileAs creator) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","role"),
                     ("scheme","marc:relators")] $ x])
                   (creatorRole creator >>= toRelator)
        toTitleNode id' title
          | version == EPUB2 = [dcNode "title" !
             ([("id",id')] ++
              maybe [] (\x -> [("opf:file-as",x)]) (titleFileAs title) ++
              maybe [] (\x -> [("opf:title-type",x)]) (titleType title)) $
              titleText title]
          | otherwise = [dcNode "title" ! [("id",id')] $ titleText title]
              ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","file-as")] $ x])
                   (titleFileAs title) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","title-type")] $ x])
                   (titleType title)
        schemeToOnix "ISBN-10" = "02"
        schemeToOnix "GTIN-13" = "03"
        schemeToOnix "UPC"     = "04"
        schemeToOnix "ISMN-10" = "05"
        schemeToOnix "DOI"     = "06"
        schemeToOnix "LCCN"    = "13"
        schemeToOnix "GTIN-14" = "14"
        schemeToOnix "ISBN-13" = "15"
        schemeToOnix "Legal deposit number" = "17"
        schemeToOnix "URN"     = "22"
        schemeToOnix "OCLC"    = "23"
        schemeToOnix "ISMN-13" = "25"
        schemeToOnix "ISBN-A"  = "26"
        schemeToOnix "JP"      = "27"
        schemeToOnix "OLCC"    = "28"
        schemeToOnix _         = "01"

showDateTimeISO8601 :: UTCTime -> String
showDateTimeISO8601 = formatTime defaultTimeLocale "%FT%TZ"

transformTag :: WriterOptions
             -> IORef [(FilePath, FilePath)] -- ^ (oldpath, newpath) media
             -> Tag String
             -> IO (Tag String)
transformTag opts mediaRef tag@(TagOpen name attr)
  | name == "video" || name == "source" || name == "img" = do
  let src = fromAttrib "src" tag
  let poster = fromAttrib "poster" tag
  let oldsrc = maybe src (</> src) $ writerSourceURL opts
  let oldposter = maybe poster (</> poster) $ writerSourceURL opts
  newsrc <- modifyMediaRef mediaRef oldsrc
  newposter <- modifyMediaRef mediaRef oldposter
  let attr' = filter (\(x,_) -> x /= "src" && x /= "poster") attr ++
              [("src", newsrc) | not (null newsrc)] ++
              [("poster", newposter) | not (null newposter)]
  return $ TagOpen name attr'
transformTag _ _ tag = return tag

modifyMediaRef :: IORef [(FilePath, FilePath)] -> FilePath -> IO FilePath
modifyMediaRef _ "" = return ""
modifyMediaRef mediaRef oldsrc = do
  media <- readIORef mediaRef
  case lookup oldsrc media of
         Just n  -> return n
         Nothing -> do
           let new = "media/file" ++ show (length media) ++
                    takeExtension oldsrc
           modifyIORef mediaRef ( (oldsrc, new): )
           return new

transformBlock  :: WriterOptions
                -> IORef [(FilePath, FilePath)] -- ^ (oldpath, newpath) media
                -> Block
                -> IO Block
transformBlock opts mediaRef (RawBlock fmt raw)
  | fmt == Format "html" = do
  let tags = parseTags raw
  tags' <- mapM (transformTag opts mediaRef)  tags
  return $ RawBlock fmt (renderTags tags')
transformBlock _ _ b = return b

transformInline  :: WriterOptions
                 -> IORef [(FilePath, FilePath)] -- ^ (oldpath, newpath) media
                 -> Inline
                 -> IO Inline
transformInline opts mediaRef (Image lab (src,tit)) = do
    let src' = unEscapeString src
    let oldsrc = maybe src' (</> src) $ writerSourceURL opts
    newsrc <- modifyMediaRef mediaRef oldsrc
    return $ Image lab (newsrc, tit)
transformInline opts _ (x@(Math _ _))
  | WebTeX _ <- writerHTMLMathMethod opts = do
    raw <- makeSelfContained Nothing $ writeHtmlInline opts x
    return $ RawInline (Format "html") raw
transformInline _ _ x = return x

writeHtmlInline :: WriterOptions -> Inline -> String
writeHtmlInline opts z = trimr $
  writeHtmlString opts{ writerStandalone = False }
    $ Pandoc nullMeta [Plain [z]]

(!) :: Node t => (t -> Element) -> [(String, String)] -> t -> Element
(!) f attrs n = add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) (f n)

-- | Version of 'ppTopElement' that specifies UTF-8 encoding.
ppTopElement :: Element -> String
ppTopElement = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++) . unEntity . ppElement
  -- unEntity removes numeric  entities introduced by ppElement
  -- (kindlegen seems to choke on these).
  where unEntity [] = ""
        unEntity ('&':'#':xs) =
                   let (ds,ys) = break (==';') xs
                       rest = drop 1 ys
                   in  case safeRead ('\'':'\\':ds ++ "'") of
                          Just x   -> x : unEntity rest
                          Nothing  -> '&':'#':unEntity xs
        unEntity (x:xs) = x : unEntity xs

mediaTypeOf :: FilePath -> Maybe String
mediaTypeOf x = case getMimeType x of
                     Just y@('i':'m':'a':'g':'e':_) -> Just y
                     Just y@('v':'i':'d':'e':'o':_) -> Just y
                     Just y@('a':'u':'d':'i':'o':_) -> Just y
                     _                              -> Nothing

data IdentState = IdentState{
       chapterNumber :: Int,
       identTable    :: [(String,String)]
       } deriving (Read, Show)

-- Returns filename for chapter number.
showChapter :: Int -> String
showChapter = printf "ch%03d.xhtml"

-- Add identifiers to any headers without them.
addIdentifiers :: [Block] -> [Block]
addIdentifiers bs = evalState (mapM go bs) []
 where go (Header n (ident,classes,kvs) ils) = do
         ids <- get
         let ident' = if null ident
                         then uniqueIdent ils ids
                         else ident
         put $ ident' : ids
         return $ Header n (ident',classes,kvs) ils
       go x = return x

-- Go through a block list and construct a table
-- correlating the automatically constructed references
-- that would be used in a normal pandoc document with
-- new URLs to be used in the EPUB.  For example, what
-- was "header-1" might turn into "ch006.xhtml#header".
correlateRefs :: Int -> [Block] -> [(String,String)]
correlateRefs chapterHeaderLevel bs =
  identTable $ execState (mapM_ go bs)
    IdentState{ chapterNumber = 0
              , identTable = [] }
 where go :: Block -> State IdentState ()
       go (Header n (ident,_,_) _) = do
          when (n <= chapterHeaderLevel) $
              modify $ \s -> s{ chapterNumber = chapterNumber s + 1 }
          st <- get
          let chapterid = showChapter (chapterNumber st) ++
                          if n <= chapterHeaderLevel
                             then ""
                             else '#' : ident
          modify $ \s -> s{ identTable = (ident, chapterid) : identTable st }
       go _ = return ()

-- Replace internal link references using the table produced
-- by correlateRefs.
replaceRefs :: [(String,String)] -> [Block] -> [Block]
replaceRefs refTable = walk replaceOneRef
  where replaceOneRef :: Inline -> Inline
        replaceOneRef x@(Link lab ('#':xs,tit)) =
          case lookup xs refTable of
                Just url -> Link lab (url,tit)
                Nothing  -> x
        replaceOneRef x = x

-- Variant of normalizeDate that allows partial dates: YYYY, YYYY-MM
normalizeDate' :: String -> Maybe String
normalizeDate' xs =
  let xs' = trim xs in
  case xs' of
       [y1,y2,y3,y4] | all isDigit [y1,y2,y3,y4] -> Just xs'     -- YYYY
       [y1,y2,y3,y4,'-',m1,m2] | all isDigit [y1,y2,y3,y4,m1,m2]  -- YYYY-MM
                                                 -> Just xs'
       _                                         -> normalizeDate xs'

toRelator :: String -> Maybe String
toRelator x
  | x `elem` relators = Just x
  | otherwise         = lookup (map toLower x) relatorMap

relators :: [String]
relators = map snd relatorMap

relatorMap :: [(String, String)]
relatorMap =
           [("abridger", "abr")
           ,("actor", "act")
           ,("adapter", "adp")
           ,("addressee", "rcp")
           ,("analyst", "anl")
           ,("animator", "anm")
           ,("annotator", "ann")
           ,("appellant", "apl")
           ,("appellee", "ape")
           ,("applicant", "app")
           ,("architect", "arc")
           ,("arranger", "arr")
           ,("art copyist", "acp")
           ,("art director", "adi")
           ,("artist", "art")
           ,("artistic director", "ard")
           ,("assignee", "asg")
           ,("associated name", "asn")
           ,("attributed name", "att")
           ,("auctioneer", "auc")
           ,("author", "aut")
           ,("author in quotations or text abstracts", "aqt")
           ,("author of afterword, colophon, etc.", "aft")
           ,("author of dialog", "aud")
           ,("author of introduction, etc.", "aui")
           ,("autographer", "ato")
           ,("bibliographic antecedent", "ant")
           ,("binder", "bnd")
           ,("binding designer", "bdd")
           ,("blurb writer", "blw")
           ,("book designer", "bkd")
           ,("book producer", "bkp")
           ,("bookjacket designer", "bjd")
           ,("bookplate designer", "bpd")
           ,("bookseller", "bsl")
           ,("braille embosser", "brl")
           ,("broadcaster", "brd")
           ,("calligrapher", "cll")
           ,("cartographer", "ctg")
           ,("caster", "cas")
           ,("censor", "cns")
           ,("choreographer", "chr")
           ,("cinematographer", "cng")
           ,("client", "cli")
           ,("collection registrar", "cor")
           ,("collector", "col")
           ,("collotyper", "clt")
           ,("colorist", "clr")
           ,("commentator", "cmm")
           ,("commentator for written text", "cwt")
           ,("compiler", "com")
           ,("complainant", "cpl")
           ,("complainant-appellant", "cpt")
           ,("complainant-appellee", "cpe")
           ,("composer", "cmp")
           ,("compositor", "cmt")
           ,("conceptor", "ccp")
           ,("conductor", "cnd")
           ,("conservator", "con")
           ,("consultant", "csl")
           ,("consultant to a project", "csp")
           ,("contestant", "cos")
           ,("contestant-appellant", "cot")
           ,("contestant-appellee", "coe")
           ,("contestee", "cts")
           ,("contestee-appellant", "ctt")
           ,("contestee-appellee", "cte")
           ,("contractor", "ctr")
           ,("contributor", "ctb")
           ,("copyright claimant", "cpc")
           ,("copyright holder", "cph")
           ,("corrector", "crr")
           ,("correspondent", "crp")
           ,("costume designer", "cst")
           ,("court governed", "cou")
           ,("court reporter", "crt")
           ,("cover designer", "cov")
           ,("creator", "cre")
           ,("curator", "cur")
           ,("dancer", "dnc")
           ,("data contributor", "dtc")
           ,("data manager", "dtm")
           ,("dedicatee", "dte")
           ,("dedicator", "dto")
           ,("defendant", "dfd")
           ,("defendant-appellant", "dft")
           ,("defendant-appellee", "dfe")
           ,("degree granting institution", "dgg")
           ,("delineator", "dln")
           ,("depicted", "dpc")
           ,("depositor", "dpt")
           ,("designer", "dsr")
           ,("director", "drt")
           ,("dissertant", "dis")
           ,("distribution place", "dbp")
           ,("distributor", "dst")
           ,("donor", "dnr")
           ,("draftsman", "drm")
           ,("dubious author", "dub")
           ,("editor", "edt")
           ,("editor of compilation", "edc")
           ,("editor of moving image work", "edm")
           ,("electrician", "elg")
           ,("electrotyper", "elt")
           ,("enacting jurisdiction", "enj")
           ,("engineer", "eng")
           ,("engraver", "egr")
           ,("etcher", "etr")
           ,("event place", "evp")
           ,("expert", "exp")
           ,("facsimilist", "fac")
           ,("field director", "fld")
           ,("film director", "fmd")
           ,("film distributor", "fds")
           ,("film editor", "flm")
           ,("film producer", "fmp")
           ,("filmmaker", "fmk")
           ,("first party", "fpy")
           ,("forger", "frg")
           ,("former owner", "fmo")
           ,("funder", "fnd")
           ,("geographic information specialist", "gis")
           ,("honoree", "hnr")
           ,("host", "hst")
           ,("host institution", "his")
           ,("illuminator", "ilu")
           ,("illustrator", "ill")
           ,("inscriber", "ins")
           ,("instrumentalist", "itr")
           ,("interviewee", "ive")
           ,("interviewer", "ivr")
           ,("inventor", "inv")
           ,("issuing body", "isb")
           ,("judge", "jud")
           ,("jurisdiction governed", "jug")
           ,("laboratory", "lbr")
           ,("laboratory director", "ldr")
           ,("landscape architect", "lsa")
           ,("lead", "led")
           ,("lender", "len")
           ,("libelant", "lil")
           ,("libelant-appellant", "lit")
           ,("libelant-appellee", "lie")
           ,("libelee", "lel")
           ,("libelee-appellant", "let")
           ,("libelee-appellee", "lee")
           ,("librettist", "lbt")
           ,("licensee", "lse")
           ,("licensor", "lso")
           ,("lighting designer", "lgd")
           ,("lithographer", "ltg")
           ,("lyricist", "lyr")
           ,("manufacture place", "mfp")
           ,("manufacturer", "mfr")
           ,("marbler", "mrb")
           ,("markup editor", "mrk")
           ,("metadata contact", "mdc")
           ,("metal-engraver", "mte")
           ,("moderator", "mod")
           ,("monitor", "mon")
           ,("music copyist", "mcp")
           ,("musical director", "msd")
           ,("musician", "mus")
           ,("narrator", "nrt")
           ,("onscreen presenter", "osp")
           ,("opponent", "opn")
           ,("organizer of meeting", "orm")
           ,("originator", "org")
           ,("other", "oth")
           ,("owner", "own")
           ,("panelist", "pan")
           ,("papermaker", "ppm")
           ,("patent applicant", "pta")
           ,("patent holder", "pth")
           ,("patron", "pat")
           ,("performer", "prf")
           ,("permitting agency", "pma")
           ,("photographer", "pht")
           ,("plaintiff", "ptf")
           ,("plaintiff-appellant", "ptt")
           ,("plaintiff-appellee", "pte")
           ,("platemaker", "plt")
           ,("praeses", "pra")
           ,("presenter", "pre")
           ,("printer", "prt")
           ,("printer of plates", "pop")
           ,("printmaker", "prm")
           ,("process contact", "prc")
           ,("producer", "pro")
           ,("production company", "prn")
           ,("production designer", "prs")
           ,("production manager", "pmn")
           ,("production personnel", "prd")
           ,("production place", "prp")
           ,("programmer", "prg")
           ,("project director", "pdr")
           ,("proofreader", "pfr")
           ,("provider", "prv")
           ,("publication place", "pup")
           ,("publisher", "pbl")
           ,("publishing director", "pbd")
           ,("puppeteer", "ppt")
           ,("radio director", "rdd")
           ,("radio producer", "rpc")
           ,("recording engineer", "rce")
           ,("recordist", "rcd")
           ,("redaktor", "red")
           ,("renderer", "ren")
           ,("reporter", "rpt")
           ,("repository", "rps")
           ,("research team head", "rth")
           ,("research team member", "rtm")
           ,("researcher", "res")
           ,("respondent", "rsp")
           ,("respondent-appellant", "rst")
           ,("respondent-appellee", "rse")
           ,("responsible party", "rpy")
           ,("restager", "rsg")
           ,("restorationist", "rsr")
           ,("reviewer", "rev")
           ,("rubricator", "rbr")
           ,("scenarist", "sce")
           ,("scientific advisor", "sad")
           ,("screenwriter", "aus")
           ,("scribe", "scr")
           ,("sculptor", "scl")
           ,("second party", "spy")
           ,("secretary", "sec")
           ,("seller", "sll")
           ,("set designer", "std")
           ,("setting", "stg")
           ,("signer", "sgn")
           ,("singer", "sng")
           ,("sound designer", "sds")
           ,("speaker", "spk")
           ,("sponsor", "spn")
           ,("stage director", "sgd")
           ,("stage manager", "stm")
           ,("standards body", "stn")
           ,("stereotyper", "str")
           ,("storyteller", "stl")
           ,("supporting host", "sht")
           ,("surveyor", "srv")
           ,("teacher", "tch")
           ,("technical director", "tcd")
           ,("television director", "tld")
           ,("television producer", "tlp")
           ,("thesis advisor", "ths")
           ,("transcriber", "trc")
           ,("translator", "trl")
           ,("type designer", "tyd")
           ,("typographer", "tyg")
           ,("university place", "uvp")
           ,("videographer", "vdg")
           ,("witness", "wit")
           ,("wood engraver", "wde")
           ,("woodcutter", "wdc")
           ,("writer of accompanying material", "wam")
           ,("writer of added commentary", "wac")
           ,("writer of added lyrics", "wal")
           ,("writer of added text", "wat")
           ]

