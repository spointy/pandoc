{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.HTML (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

html :: (ToString a, ToPandoc a) => a -> String
html = writeHtmlString def{ writerWrapText = False } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test html

tests :: [Test]
tests = [ testGroup "inline code"
          [ "basic" =: (code "@&" :: Inlines) =?> "<code>@&amp;</code>"
          , "haskell" =: (codeWith ("",["haskell"],[]) ">>=" :: Inlines)
            =?> "<code class=\"haskell\">&gt;&gt;=</code>"
          , "nolanguage" =: (codeWith ("",["nolanguage"],[]) ">>=" :: Inlines)
            =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
          ]
        , testGroup "images"
          [ "alt with formatting" =:
            (image "/url" "title" ("my " <> emph "image") :: Inlines)
            =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
          ]
        ]
