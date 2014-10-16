## draw2d - A Standard ML library for 2d drawing

_Copyright 2010-2014, Martin Elsman_

<img width="250" alt="House drawing" align="right" src="/images/house.png">

This repository contains a simple Standard ML 2D drawing library. The
library comes with multiple backends, including a LaTeX backend and an
SVG backend.

### Assumptions

<img width="250" alt="Pythagoras tree" align="right" src="/images/pythagoras.png">

The library assumes a Standard ML compiler that supports MLB
files. MLKit or MLton will work. To run the test examples, you also
need `pdflatex` installed.

### How to test it

To try a simple example, type

    $ cd test
    $ make simple

To try a series of examples, type

    $ make

The examples generate .tex-files, which are processed by `pdflatex`, and
.xml-files, which can be injected into HTML and loaded by a Web
browser.

See `Makefile` and `test/Makefile` for details of the building process. 

### Making your own drawings

<img width="250" alt="Bath room design" align="right" src="/images/bath.png">

See `draw.sig` for library signatures and `test/simple.sml` and
`test/draw_house.sml` for examples.

### LICENSE

The library is distributed under the MIT License; see the [LICENSE
file](/LICENSE) for details.
