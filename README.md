# draw2d - A Standard ML library for 2d drawing

_Copyright 2007-2014, Martin Elsman_

This repository contains a simple Standard ML 2D drawing library. The
library comes with multiple backends, including a LaTeX backend and an
SVG backend.

# Assumptions

The library assumes a Standard ML compiler that supports MLB
files. MLKit or MLton will work.

# How to test it

To try a simple example, type

    $ cd test
    $ make simple

To try a series of examples, type

    $ make

The examples generate .tex-files to be processed by pdflatex and
.xml-files to be loaded in a browser (e.g., Firefox).

See `Makefile` and `test/Makefile` for details of the building process. 

# Making your own drawings

See `draw.sig` for library signatures and `test/simple.sml` and
`test/draw_house.sml` for examples.

# LICENSE

The library is distributed under the MIT License; see the [LICENSE
file](/LICENSE) for details.
