module Text.Pandoc.Readers.OPML ( readOPML ) where
import Data.Char (toUpper)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (unscrubStrTag)
import Text.XML.Light
import Text.Pandoc.Compat.TagSoupEntity (lookupEntity)
import Data.Generics
import Data.Monoid
import Control.Monad.State
import Control.Applicative ((<$>), (<$))

type OPML = State OPMLState

data OPMLState = OPMLState{
                        opmlSectionLevel :: Int
                      , opmlDocTitle     :: Inlines
                      , opmlDocAuthors   :: [Inlines]
                      , opmlDocDate      :: Inlines
                      } deriving Show

readOPML :: ReaderOptions -> String -> Pandoc' [SrcSpan]
readOPML _ inp  = setTitle (unscrubStrTag <$> opmlDocTitle st')
                   $ setAuthors (fmap unscrubStrTag <$> opmlDocAuthors st')
                   $ setDate (unscrubStrTag <$> opmlDocDate st')
                   $ doc $ mconcat bs
  where (bs, st') = runState (mapM parseBlock $ normalizeTree $ parseXML inp)
                             OPMLState{ opmlSectionLevel = 0
                                    , opmlDocTitle = mempty
                                    , opmlDocAuthors = []
                                    , opmlDocDate = mempty
                                    }

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = maybe (map toUpper e) (:[]) (lookupEntity e)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr elt =
  case lookupAttrBy (\x -> qName x == attr) (elAttribs elt) of
    Just z  -> z
    Nothing -> ""

asHtml :: String -> Inlines' [SrcSpan]
asHtml s = case readHtml def s of
                Pandoc _ [Plain ils] -> fromList ils
                _ -> mempty

asMarkdown :: String -> Blocks' [SrcSpan]
asMarkdown s = fromList bs
  where Pandoc _ bs = readMarkdown def s

getBlocks :: Element -> OPML (Blocks' [SrcSpan])
getBlocks e =  mconcat <$> (mapM parseBlock $ elContent e)

parseBlock :: Content -> OPML (Blocks' [SrcSpan])
parseBlock (Elem e) =
  case qName (elName e) of
        "ownerName"    -> mempty <$ modify (\st ->
                              st{opmlDocAuthors = [text $ strContent e]})
        "dateModified" -> mempty <$ modify (\st ->
                              st{opmlDocDate = text $ strContent e})
        "title"        -> mempty <$ modify (\st ->
                              st{opmlDocTitle = text $ strContent e})
        "outline" -> gets opmlSectionLevel >>= sect . (+1)
        "?xml"  -> return mempty
        _       -> getBlocks e
   where sect n = do let headerText = asHtml $ attrValue "text" e
                     let noteBlocks = asMarkdown $ attrValue "_note" e
                     modify $ \st -> st{ opmlSectionLevel = n }
                     bs <- getBlocks e
                     modify $ \st -> st{ opmlSectionLevel = n - 1 }
                     let headerText' = case map toUpper (attrValue "type" e) of
                                             "LINK"  -> link
                                               (attrValue "url" e) "" headerText
                                             _ -> headerText
                     return $ header n headerText' <> noteBlocks <> bs
parseBlock _ = return mempty
