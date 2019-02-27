## G86

G86 is a proposed encoding of binary data into ASCII.  It is similar
to encodings such as hexadecimal or Base64, but is more compact.
Every four binary bytes are encoded into five ASCII characters,
for about a 25% size increase.

The encoding uses nearly all printable ASCII characters (those from
codepoints 33 to 126).  It is thus not suitable for applications where
the allowed characters are more limited.  However, some troublesome
characters are still avoided:

*  The quote characters `'"` and not used.  This makes the encoding
convenient for inlining binary data into source code and formats such
as JSON.

*  The backslash `\` is not used.  The backslash is widely used for
escaping, even within quotes, in programming languages, mark-up,
and data serialization.

*  The special XML/SGML/HTML characters `<>&` are not used, so binary
data can be embedded into these documents.

*  The characters `,;` are not used, which allows the encoding to be
used in HTTP cookies.

In addition, G86 has these properties:

*  Binary data of any whole number of bytes can be encoded.  _n_
bytes is encoded as ⌈5&#xfeff;_n_/4⌉ characters.

*  The encoding preserves lexicographic order.

*  Any initial segment of the encoded text will decode to an initial
segment of the binary data.

The most similar encoding to G86 is
[Z85](https://rfc.zeromq.org/spec:32/Z85/), used in the ZeroMQ project.
It utilizes the fact that 85<sup>5</sup> > 256<sup>4</sup>, and it
has a more selective set of 85 characters than the earlier Ascii85.
However, it lacks many of the above properties: The spec only allows
input of length a multiple of four, the encoding does not preserve
lexicographic order, and `<>&` are used.  It _does_ exclude the
backquote `` ` ``, which G86 does not, since in my view this character
is less special than the other quotes and less often significant in
language syntax.

The lexicographic and initial segment properties are the reason an
86th character is needed.

### Spec (v0.1)

The input binary data is broken into four-byte chunks, each of which
is encoded independently into five characters.  To round up to a
multiple of four, up to three zero bytes are added; an equal number
of bytes will then be removed off the end of the final encoding.

The ASCII characters are drawn from the set of printable characters,
codepoints 33 to 126, excluding the six `&,;<>\`, for a total of 86:

```
!#$%()*+-./0123456789:=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~
```

The four bytes, as numbers from 0 to 255, are interpreted as base-_258_
“digits” in big-endian order.  The integer thus represented
is then written as five base-86 digits in big-endian order with
the above ASCII characters, in order, as “digits” for 0 to 85.
Since 258<sup>4</sup> < 86<sup>5</sup>, it will fit.

Since 258 > 256, this encoding can be inverted.

This completes the spec.

### Examples

Say we want to encode the byte string (written as ASCII)

```
Hello, world!
```

This is 13 characters, so we add three zero bytes at the end to get
four blocks of four bytes.  The byte values are then

```
72, 101, 108, 108; 111, 44, 32, 119; 111, 114, 108, 100; 33, 0, 0, 0.
```

Interpreting these as four integers written in base 258, they are
(in base 10)

```
1243243800; 1909197023; 1913876092; 566725896.
```

In base 86, the first has digits with values `22, 62, 52, 67, 22`,
which in ASCII is `=g]l=`.  Similarly, the last is `/G!!!`, but we
remove the last three characters (which are always the zero digit
`!`), corresponding to the three zero bytes we added, to get `/G`.
Put together, here is the whole G86 encoding:

```
=g]l=Jv^0IJ}|l3/G
```

Its length is 17 = ⌈5⋅13/4⌉ characters.

A use case is representing a 128-bit (or longer) key or identifier.
The 16 bytes expand to 20 characters.  Similarly, a 256-bit key would
be 40 characters.  For example, consider the common example of the
hexadecimal digits of π:

```
24 3f 6a 88 85 a3 08 d3 13 19 8a 2e 03 70 73 44
a4 09 38 22 29 9f 31 d0 08 2e fa 98 ec 4e 6c 89
```

Its G86 encoding:

```
0H_fZQ{)BO)~boV#*k#m[R{{J2)ahL$Xwhks56l[
```

This is only 11% longer than a UUID (36 characters), with twice
the information.


### Properties

The encoding clearly preserves lexicographic order among inputs of
the same length.  Since 258 is 3 times 86, extending an input with
zero bytes extends the encoding with “zero” digits (`!`), which
are lexicographically first in the G86 character set.  From this we
can see the encoding preservers lexicographic order generally.

The initial segment property also follows, although when decoding a
partial string we may have to discard information at the end about
the next byte (or use it, if the partial information is still useful).


### Future work

The part of the spec most likely to be reconsidered is the choice of
characters.  Some applications might work better with a different set,
although _ad hoc_ simple substitution can accommodate such situations.

One example is that `>` is not that special in HTML; it only is
relevant when in a tag, where there wouldn't be arbitrary data anyway.
So perhaps it is preferable to the backquote.


### Implementation

The reference implementation is written in Haskell, and provides a
basic Haskell API and a command-line executable.  The executable has
a very simple interface:

```
g86 < data.bin > data.txt
g86 -d < data.txt > data.bin
```

One limitation is that it is not streaming.  The whole file is loaded
into memory before decoding, so very large files or an “infinite”
stream may present problems.  This is not an essential limitation of
the spec, but is used in the reference implementation because it is
more convenient and the main use case is small amounts of binary data.

Either `cabal install` or `stack install` can be used to build.

