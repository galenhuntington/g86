## G86

G86 is a proposed encoding of binary data into ASCII.  It is similar
to encodings such as hexadecimal or Base64, but is more compact.
Every four binary bytes are encoded into five ASCII characters,
for about a 25% size increase.

The encoding uses nearly all non-space printable ASCII characters
(codepoints 33 to 126).  It is thus not suitable for applications where
the allowed characters are more limited.  However, some troublesome
characters are still avoided:

*  The quote characters `"'` are not used.  This makes the encoding
convenient for inlining binary data into source code and formats such
as JSON.

*  The backslash `\` is not used.  The backslash is widely utilized
for escaping in programming languages, mark-up, and data serialization.

*  The special XML/SGML/HTML characters `&<>` are not used, so binary
data can be embedded in these documents.

*  The characters `,;` are not used, which allows the encoding to be
used in HTTP cookies.

*  As noted, the space character (codepoint 32) is not used.  This
allows free interspersing of whitespace, and means line-breaking and
indenting will not compromise the data.  This is also a requirement
for cookies.

In addition, G86 has these properties:

*  Binary data of any whole number of bytes can be encoded.  _n_
bytes is encoded as ⌈5&#xfeff;_n_/4⌉ characters.

*  The encoding preserves lexicographic order.

*  Any initial segment of the encoded text will decode to an initial
segment of the binary data.

The most similar encoding to G86 is
[Z85](https://rfc.zeromq.org/spec:32/Z85/), used in the ZeroMQ project.
It utilizes the fact that 85⁵ ⩾ 256⁴, and it has a more selective
set of 85 characters than the earlier Ascii85.  However, it lacks
many of the above properties: the spec only allows input of length
a multiple of four, the encoding does not preserve lexicographic
order, and `<>&` are used.  It _does_ exclude the backquote `` ` ``,
which G86 does not, since in my view this character is less special
than the other quotes and less often significant in language syntax,
and also excludes `_|~`.

The lexicographic and initial segment properties are the reason an
86th character is added.

For encodings along similar lines that only use alphanumeric
characters, see [G43 and G56](https://github.com/galenhuntington/g56).

### Spec (v0.1)

The input is broken into four-byte chunks, each of which is encoded
independently into five characters.  To round up to a multiple of four,
up to three zero bytes are added; an equal number of bytes will then
be removed off the end of the output.

The ASCII characters are drawn from the set of non-space printable
characters, codepoints 33 to 126, excluding the six `&,;<>\`, for a
total of 86:

```
!#$%()*+-./0123456789:=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~
```

The four bytes, as numbers from 0 to 255, are interpreted as
base-258 digits in big-endian order.  The integer thus represented
is then written as five base-86 digits in big-endian order with
the above ASCII characters, in order, as “digits” for 0 to 85.
Since 258⁴ ⩽ 86⁵, it will fit.

Since 258 ⩾ 256, this encoding can be inverted.

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

The G86 encoding of these bytes:

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

As noted, the encoding brings a size increase of approximately
25%—approximate only because one cannot have a fractional character.
In comparison, hexadecimal is 100%, and Base64 about 33.3%.  Z85 is
also 25%, when it can be used.  There are many others, generally
for when different character sets are desired.  E.g., the more
human-friendly Crockford Base 32 has a 60% size increase.

The theoretical limit would be to use all 95 printable ASCII
codepoints, expanding about 21.8%, or even add the newline character
for 96 and 21.5%.  Approaching this requires complex encodings with
large block sizes.  E.g., one can get 22.2% by encoding nine bytes
into eleven characters, and more complexity and heavy calculation must
be added if the above properties are desired.  But it would be an odd
case to be willing to accept all of, but only, ASCII, and to tolerate
a difficult encoding, just to squeeze those last percentage points.

I thus regard G86 as a local optimum among “nearly-all-ASCII”
encodings.  It rates high on space usage, simplicity, calculation
required, block size, properties, and characters used.


### Future work

There aren't many directions in which the spec could change.  One could
for instance argue the choice of characters.  Some applications might
work better with a different set, although _ad hoc_ simple substitution
can accommodate such situations.

As an example, `>` is not _that_ special in HTML; it only is relevant
when in a tag, where there wouldn't be arbitrary data anyway.  So it
might be preferable to, say, the backquote.  Nevertheless, HTML and
XML recommendations still say `>` should be encoded.


### Implementation

The reference implementation is written in Haskell, and provides a
basic Haskell API and a command-line executable.  The executable has
a very simple interface:

```bash
g86 < data.bin > data.txt
g86 -d < data.txt > data.bin
```

One limitation is that it is not streaming; the whole file is loaded
into memory before decoding, so very large files or an “infinite”
stream may present problems.  This is not inherent to the encoding,
but is done in the reference implementation because it is more
convenient and the main use case is small amounts of binary data.

Either `cabal install` or `stack install` can be used to build.

