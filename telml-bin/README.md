# telml

The `telml` command-line program is the easiest way to use `telml` for your purposes. This allows you to convert a document from `telml` to HTML quickly. It also allows you to create new tags by implementing them in the Lua programming language, to easily create a richer document.

For documentation on the TeLML language itself, see [the data format documentation](https://github.com/aisamanra/telml/blob/master/telml/README.md).

## Installing

Right now the only way to get `telml` is from source. To build and install it, use `cabal install`. It also requires you to have a copy of Lua version 54 on your system.

```
$ git clone https://github.com/aisamanra/telml.git
$ cd telml
$ cabal install telml-bin
```

## Simple usage

The usual use is as follows:

```
$ telml my-document.telml --tags=my-tags.lua
```

The `--tags` argument is optional, if you only want to use the built-in tags. You can also explicitly opt out of the built-in tags with `--no-default-tags`.

## Writing custom tags

Within the tag file, you should define your custom tags on the `telml` global table. For example, if I want a new tag called `\excited{...}` that adds an exclamation point after its argument, I can implement it like this:

```lua
-- implement a macro so that
-- \excited{Hello} => Hello!
function telml.excited(param)
  return param .. "!"
end
```

TeLML recognizes tags whose names aren't valid Lua identifiers: in that case, you can always use the element access syntax to define the function as well:

```lua
-- implement a macro so that
-- \weird-name{Hello} => Hello???
telml["weird-name"] = function(param)
  return param .. "???"
end
```

Lua's global state is persisted across macro invocations, so you can implement custom tags that implement non-idempotent behavior.

```lua
section = 0
function telml.section(name)
  section = section + 1
  return "<h1>" .. section .. ": " .. name .. "</h1>"
end
-- This produces an `h1` with an increasing number each time, so
-- \section{Foo} => <h1>1: Foo</h1>
-- \section{Bar} => <h1>2: Foo</h1>
-- \section{Baz} => <h1>3: Foo</h1>
```

You can also implement n-ary functions or variadic functions and invoke those using the TeLML multi-argument syntax:

```lua
-- implement a macro so that
-- \frac{2|4} => <sup>2</sup>/<sub>4</sub>
function telml.frac(num, denom)
  return "<sup>" .. num .. "</sup>/<sub>" .. denom .. "</sub>"
end

-- implement a macro so that
-- \enum{a|b|c} => 1. a; 2. b; 3. c;
function telml.enum(...)
  local args = {...}
  local result = ""
  for idx, arg in ipairs(args) do
    result = result .. idx .. ". " .. arg .. "; "
  end
  return result
end
```

It is an error to return something which isn't a string or something that can be trivially converted into a string. It's also an error to include a field on the `telml` object that's not a function, and it's an error to redefine `telml` to something which isn't a table.

However, it's worth noting that Lua is pretty lax about argument-passing. Lua allows you to pass too many arguments (in which case later arguments are ignored) or too few (in which case later arguments are provided with `nil`) and TeLML inherits this behavior. If you're worried about this, you can use variadic syntax and produce explicit errors.

```lua
-- this allows \excited{a|b} and produces "a!"
function telml.excited(x)
  return x .. "!"
end

-- this will fail with \excited{a|b}
function telml.safe_excited(...)
  local args = {...}
  if #args ~= 1 then
    error("safe_excited: expected one argument, got " .. #args)
  end
  return args[1] .. "!"
end
```

## Built-in tags

The current built-in tags are implemented in the `telml` program. Note that most built-in tags are more particular about argument count and will balk if provided more or fewer arguments than they expect.

This list is currently subject to change.

- `\em{foo}` for italics: `<em>foo</em>`
- `\strong{foo}` for bolding: `<strong>foo</strong>`
- `\h1{foo}`, `\h2{foo}`, and so forth for headers: `<h1>foo</h1>`
- `\p{foo}` for explicit paragraphs: `<p>foo</p>`
- `\blockquote{foo}` for block quotations: `<blockquote>foo</blockquote>`
- `\tt{foo}` for inline code: `<code>foo</code>`
- `\code{foo}` for code blocks: `<pre><code>foo</code></pre>`
- `\ul{a|b}` for unordered lists: `<ul><li>a</li><li>b</li></ul>`
- `\ol{a|b}` for ordered lists: `<ol><li>a</li><li>b</li></ol>`
- `\br{}` for explicit line breaks: `<br/>`
- `\comment{foo}` for including ignored comments
- `\link{target|text}` for links: `<a href="target">text</a>`
- `\img{target|alt-text}` for links: `<img src="target">alt-text</img>`
