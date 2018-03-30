# telml-markup

The `telml-markup` package provides a simple, LaTeX-inspired markup
language with the possibility of writing extensible instructions.

**This is in early phases, and so should not be relied upon for any
kind of stability at this stage.** In particular, the set of tags
and how they are understood could change radically during development.

## Sample Document

~~~~
\code{telml-markup} uses a mostly HTML-like set of names, so we create
\em{emphasis} with \code{\\em}, \strong{bolding} with \em{\\strong},
and \code{fixed-width text} with \code{\\code}.

One major difference is the \code{\\link} tag, which takes the form
\code{\\link\{url\|text\}},
\link{https://github.com/aisamanra/telml-markup|like this}.
~~~~

This produces the following rendered HTML:

> <p><code>telml-markup</code> uses an HTML-like set of names, so we create
> <em>emphasis</em> with <code>\em</code>, <strong>bolding</strong> with <code>\strong</code>,
> and <code>fixed-width text</code> with <code>\code</code>.
> </p><p>One major difference is the <code>\link</code> tag, which takes the form
> <code>\link{url|text}</code>,
> <a href="https://github.com/aisamanra/telml-markup">like this</a>.
> </p>

## Basic Usage

The `render` function takes a [`telml`](https://github.com/aisamanra/telml)
document and renders it into the
[`blaze-html`](http://hackage.haskell.org/package/blaze-html-0.8.0.2)
[`Html`](http://hackage.haskell.org/package/blaze-html-0.8.0.2/docs/Text-Blaze-Html.html#t:Html)
type, which can then be rendered into HTML. For example, the following
is a minimal program which interprets input on `stdin` and prints the
rendered HTML (or an error message) to `stdout`.

~~~~{.haskell}
module Main

import Control.Monad ((>=>))
import Data.TeLML (parse)
import Data.TeLML.Markup (render)
import System.Exit (exitFailure)
import Text.Blaze.Renderer.String (renderMarkup)

main :: IO ()
main = do
  str <- getContents
  case (parse >=> render) str of
    Left err  -> putStrLn err >> exitFailure
    Right doc -> putStrLn (renderMarkup doc)
~~~~

We could invoke it at the command line like so:

~~~~
$ ./telml-markup-test <<EOF
> This should be \em{emphasized}.
>
> This, on the other hand, is \strong{bold}.
> EOF
<p>This should be<em>emphasized</em>.
</p><p>This, on the other hand, is <strong>bold</strong>.
</p>
~~~~

If we give it an unknown tag, or a tag with the wrong arity, it will
give us an error:

~~~~
$ ./telml-markup-test <<EOF
> This is a \fake{tag}.
> EOF
Error: no match for tag fake/1
$ ./telml-markup-test <<EOF
> This is a tag with \em{too|many|arguments}.
> EOF
Error: no match for tag em/3
~~~~

## Extended Usage

The `renderWith` function takes a list of additional tags and their
denotations. This allows you to add new tags to the markup for
particular purposes.

In order to define the meaning of a new tag, you can use the `mkTag`
function, which takes the tag name as well as something which defines
the meaning: usually a function of the arguments you want with certain
redundant wrappers around the argument in order to guide the
type-checker that in turn returns an `Html` fragment using the
`blaze-html` library. For example, an argument-less tag like `br` can
be defined as simply `mkTag "br" Text.Blaze.Html5.br`.

For tags that take arguments, we can take advantage of the
`TagArguments` type class in order to avoid fiddly argument-handling
and manual errors. The `TagArguments` class will allow you to provide
a function so long as all the arguments to the function are types that
it knows about---mostly wrapper types defined by `telml-markup`. For
example, the `H` type simply wraps already-rendered HTML, so if we
want to write a tag like `em` that takes a single argument, we can
write it like this:

```
import Data.TeLML.Markup
import Text.Blaze.Html5 (em)

-- \em{some argument}
emTag :: TagDescription
emTag = mkTag "em" (\ (H html) -> em html)
```

The `Hs` wrapper type wraps a variadic function, and can only be used
as the final trailing argument, as it will match any number of
arguments in a tag, rendering them all as HTML. We can define a list
tag like this:

```
import Data.TeLML.Markup
import Text.Blaze.Html5 (ul, li)

-- \list{one|two|three}
emTag :: TagDescription
emTag = mkTag "list" (\ (Hs hs) -> ul (mapM_ li hs))
```

Sometimes we want a tag that has a concrete string value: for example,
if we want a tag that allows us to write HTML `span` tags to add
classes to elements, we would want the classes to be just strings and
not already-rendered HTML strings. We can use the `Str` wrapper to
make sure that an argument is treated as a raw string:

```
import Data.TeLML.Markup
import Text.Blaze.Html5 (span, class_, toValue)

-- \span{arg|class}
spanTag :: TagDescription
spanTag = mkTag "span" $ \ (H arg) (Str cls) ->
    span ! class (toValue cls) $ arg
```

These tags have been straightforward, but arbitrary new tags with
different argument structures can be added, and the underlying
machinery will ensure that errors will be reported appropriately. The
tags can also produce arbitrarily complicated structures: they do,
after all, have the entirety of Haskell available to them! For
example, here we add a tag so that `\hello{...}` will render out to
the HTML string `<strong>Hello, ...!</strong>`:

~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>))
import qualified Data.TeLML as TeLML
import qualified Data.TeLML.Markup as TeLML
import qualified System.Exit as Sys
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Renderer.String as Html

myTags :: [TeLML.TagDescription]
myTags =
  [ TeLML.mkTag "hello" $ \(TeLML.H name) ->
    Html.strong ("Hello, " >> name >> "!")
  ]

main :: IO ()
main = do
  str <- getContents
  case (TeLML.parse >=> TeLML.renderWith myTags) str of
    Left err  -> putStrLn err >> Sys.exitFailure
    Right doc -> putStrLn (Html.renderMarkup doc)
~~~~

We can execute this to test it:

~~~~
$ ./telml-markup-extended-test <<EOF
> Now we can do this: \hello{friend}.
> EOF
<p>Now we can do this: <strong>Hello, friend!</strong>.
</p>
~~~~

Providing the wrong argument list will give us an arity error:

~~~~
$ ./telml-markup-extended-test <<EOF
> This does not use hello correctly: \hello{this|that}.
> EOF
Tag \hello expects argument structure \hello{frag}
~~~~

Additionally, for tags that specifically want strings intead of richer
structures, we will get type errors:

~~~~
$ ./telml-markup-extended-test <<EOF
> This tries to use emphasis in the link portion:
> \link{\em{url}|\em{text}}.
> EOF
Tag \link expects argument structure \link{str|frag}
~~~~
