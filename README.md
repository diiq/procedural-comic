# Procedural comic

Generate comic page layouts procedurally. Requires vecto. Load the library, then use `(comic [page-width-in-pixels] [page-height-in-pixels] [number-of-pages] [directory-name])` to generate a whole comics-worth of pages.

Pages include panels, and focal points.

MOST pages will be classic, rectilinear, three-row layouts.
![A comic](page_1.png?raw=true)

Some panels will be borderless.
![Borderless](page_2.png?raw=true)

A few pages will have skewed horiztonal gutters for more dynamic layouts.
![Skewed](page_3.png?raw=true)

A few will have inset circular panels for highlighting important details or expressions.
![Inset](page_4.png?raw=true)

Some pages will be single-panel splash pages.
