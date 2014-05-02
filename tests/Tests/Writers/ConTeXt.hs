{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.ConTeXt (tests) where

import Test.Framework
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

context :: (ToString a, ToPandoc a) => a -> String
context = writeConTeXt def . toPandoc

context' :: (ToString a, ToPandoc a) => a -> String
context' = writeConTeXt def{ writerWrapText = False } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test context "my test" $ X =?> Y

which is in turn shorthand for

  test context "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test context

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with '}'" =: (code "}" :: Inlines) =?> ("\\mono{\\}}" :: String)
          , "without '}'" =: (code "]" :: Inlines) =?> ("\\type{]}" :: String)
          , property "code property" $ \s -> null s ||
                if '{' `elem` s || '}' `elem` s
                   then (context' (code s :: Inlines)) == "\\mono{" ++
                             (context' (str s :: Inlines)) ++ "}"
                   else (context' (code s :: Inlines)) == "\\type{" ++ s ++ "}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            (headerWith ("my-header",[],[]) 1 "My header" :: Blocks)
              =?> ("\\section[my-header]{My header}" :: String)
          ]
        , testGroup "bullet lists"
          [ "nested" =:
            ( bulletList [
                plain (text "top")
                  <> bulletList [
                    plain (text "next")
                     <> bulletList [plain (text "bot")]
                  ]
             ] :: Blocks ) =?> unlines
                [ "\\startitemize[packed]"
                , "\\item"
                , "  top"
                , "  \\startitemize[packed]"
                , "  \\item"
                , "    next"
                , "    \\startitemize[packed]"
                , "    \\item"
                , "      bot"
                , "    \\stopitemize"
                , "  \\stopitemize"
                , "\\stopitemize" ]
          ]
        ]

