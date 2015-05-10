# TeLML

A Tex-Like Markup Language—which is a bit of a misnomer, because
as presented, it's more of a structured data language optimized for
text documents than an actual
markup language. A _TeLML Document_ is a sequence of textual
fragments interspersed with "tags", which have the format

~~~~
\tagname{ document | document | ... }
~~~~

Whitespace is _not_ allowed between the backslash and the tag
name, but _is_ allowed between the tag name and the following
block. This means that the following is allowed:

~~~~
\tag
  { element_1
  | element_2
  | ...
  | element_n
  }
~~~~

In contrast to TeX, the block is _obligatory_, i.e.
the following is **not** a valid document:

~~~~
\p{this \br and that}
~~~~

Whereas this is:

~~~~
\p{this \br{} and that}
~~~~

(This restriction might at some point be lifted based on experience
with the format.)

The intended use for TeLML is as a building-block for specific
markup formats in which you might want to have arbitrary new
tags, but don't want to use an XML-based solution.

# Formal Grammar

~~~~
<document> ::= <fragment>*
<fragment> ::= <tag> | <text>

<text>     ::= /[^\]|\\[\{}|]*/

<tag>      ::= "\" <tagname> <spaces> "{" <arglist> "}"
<tagname>  ::= /[A-Za-z][A-Za-z0-9_-]*/
<arglist>  ::= document ("|" document)*
<spaces>   ::= /[ \t\r\n]*/
~~~~

# Possible Future Modifications

Variations on this theme that might be possible:

- It might (as stated above) be worthwhile to attempt to lift the
  restriction that every tag has an argument block, if nullary
  tags are commonly used.
- It might be nice to optionally allow `\begin{...}` and `\end{...}`
  tags to be parsed as a special case of certain delimiters.
- Maybe a special case for named arguments of the form
  `\tag{arg=stuff|arg=stuff}`, although this could be
  handled at present by using nested tags as in
  `\tag{\arg{stuff}|\arg{stuff}}`.
