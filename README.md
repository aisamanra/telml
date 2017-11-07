# telml

This is the repository for both the `telml` and the `telml-markup`
packages. The former implements a TeX-inspired data format, while the
latter uses that data format to implement a lightweight, extensible
markup format.

## What Is TeLML?

TeLML is a markup language I created for personal projects. You
probably don't want to use it! There's a good chance it doesn't cover
your use case, it's still subject to breaking changes, it's kind of
wonky, and it's probably still very buggy!

Still want to know more? Okay: TeLML is specifically a TeX-inspired
markup language that I created because I wanted a markup language
that 1. looked pleasant to me and 2. was easy to extend with new
constructs. As such, there are two layers to TeLML:

1. The data layer, which defines how to map a source file to a simple
   AST. This AST is not inherently associated with any particular
   semantics, but is rather a tree with a particular constrained shape.
   This layer is implemented in the `telml` library.
2. The markup layer, which defines a common set of interpretations
   for various AST chunks. This markup layer is HTML-inspired and
   therefore features basic constructs like emphasis, lists, tables,
   and images, but is specifically designed to be extended with new,
   application-specific constructs. This layer is implemented in the
   `telml-markup` library.

The data layer is straightforward: it borrows LaTeX syntax in a very
rigid, specific way. A _document_ is a sequence of fragments; in turn,
a _fragment_ is either a chunk of raw text, or a _tag_ associated with
an alphanumeric name and a sequence of documents. A _tag_ takes the
form `\tag_name{document1|document2|document3|...}`. In order to use
any of the reserved characters `\`, `{`, `}`, or `|` in plain text,
they should be escaped with a preceeding `\` character. Additionally,
at the top-level, TeLML files are parsed as a series of documents,
split on double-newlines.

This generic framework allows tags to be inserted easily into existing
text in a way that's lightweight and natural-looking to a user of
something like TeX. This system is, however, more consisten than TeX:
for example, all tags have payloads, so standalone TeX commands like
`\em` are not valid documents; all tag names must be followed by curly
braces, like `\em{}`.

The `telml-markup` library lets you interpret a pretty standard set of
tags as HTML, and additionally lets you add new custom tags to the
mix. That library is less well-specified, and could probably change
radically if I decide I want the semantics of tags to change.

## Frequently Asked Questions

### Why?

It's my party, and I'll bikeshed if I want to.

### Why Not Markdown?

Markdown isn't easily extensible, and also it's fraught with
edge-cases and difficult to implement in a new language. TeLML is
extensible, and it's easy to write a naïve recursive descent parser
for it in basically any language.

### Why Not XML?

I know XML is eXtensible, after all, but—do _you_ want to write XML by
hand? Because _I_ don't. Yeah, `\em{foo}` is more heavyweight than
Markdown's `_foo_`, but it's also a fair bit more lightweight than
XML's `<em>foo</em>`.

### Why Not ReST?

Because I didn't know about its extensibility when I started writing
TeLML, and now this exists and I use it and I don't really want to
switch.

### Why is your code so bad?

Get bent, pal.

## Historical Note

This repository contains a [cabal.project
file](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/),
which facilitates building multiple packages together; this repository
had previously been used for just the `telml` package, and the
corresponding
[`telml-markup`](https://github.com/aisamanra/telml-markup) repository
is left but will not be updated.
