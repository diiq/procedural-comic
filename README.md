# Procedural comic

Generate comic page layouts procedurally. Requires vecto. Load the library, then use `(comic [page-width-in-pixels] [page-height-in-pixels] [number-of-pages] [directory-name])` to generate a whole comics-worth of pages.

Pages include panels, and focal points.

A few pages will have skewed horiztonal gutters for more dynamic layouts. A few will have inset circular panels for highlighting important details or expressions. Some panels will be borderless. Some pages will be single-panel splash pages. MOST pages will be classic, rectilinear, three-row layouts.

 ![A comic](page_1.png?raw=true)
 ![Borderless](page_2.png?raw=true)
 ![Skewed](page_3.png?raw=true)
 ![Inset](page_4.png?raw=true)
