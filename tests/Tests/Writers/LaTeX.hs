{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.LaTeX (tests) where

import Test.Framework
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

latex :: (ToString a, ToPandoc a) => a -> String
latex = writeLaTeX def . toPandoc

latexListing :: (ToString a, ToPandoc a) => a -> String
latexListing = writeLaTeX def{ writerListings = True } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test latex "my test" $ X =?> Y

which is in turn shorthand for

  test latex "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test latex

tests :: [Test]
tests = [ testGroup "code blocks"
          [ "in footnotes" =: (note (para "hi" <> codeBlock "hi") :: Inlines) =?>
            ("\\footnote{hi\n\n\\begin{Verbatim}\nhi\n\\end{Verbatim}\n}" :: String)
          , test latexListing "identifier" $ (codeBlockWith ("id",[],[]) "hi" :: Blocks) =?>
            ("\\begin{lstlisting}[label=id]\nhi\n\\end{lstlisting}" :: String)
          , test latexListing "no identifier" $ (codeBlock "hi" :: Blocks) =?>
            ("\\begin{lstlisting}\nhi\n\\end{lstlisting}" :: String)
          ]
        , testGroup "definition lists"
          [ "with internal link" =:
             (definitionList [(link "#go" "" (str "testing"), [plain (text "hi there")])] :: Blocks)
             =?>
             ("\\begin{description}\n\\itemsep1pt\\parskip0pt\\parsep0pt\n\\item[{\\hyperref[go]{testing}}]\nhi there\n\\end{description}" :: String)
          ]
        , testGroup "math"
          [ "escape |" =: (para (math "\\sigma|_{\\{x\\}}") :: Blocks) =?>
            ("$\\sigma|_{\\{x\\}}$" :: String)
          ]
        , testGroup "headers"
          [ "unnumbered header" =:
            (headerWith ("foo",["unnumbered"],[]) 1
               (text "Header 1" <> note (plain $ text "note")) :: Blocks) =?>
            ("\\section*{Header 1\\footnote{note}}\\label{foo}\n\\addcontentsline{toc}{section}{Header 1}\n" :: String)
          ]
        ]
