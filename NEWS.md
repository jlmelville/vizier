# News

**July 2 2026**: version 0.6.0 is released. General housekeeping and fixed some palette bugs.
Plotly support stopped working at some point probably due to the palette bugs, but is now working
again. However, it is now an optional dependency, so `install.packages("plotly")` if you want to
use it.

**November 25 2023**: use [Polychrome](https://cran.r-project.org/package=Polychrome)
to generate default color schemes for categorical data.

**September 6 2020**: fix `alpha_scale` with `embed_plotly`. Also, new parameter: `limits` for
`embed_plotly`.

**August 26 2020**: version 0.4 includes the following improvements and fixes:

* Fixed bug where using recent versions of 
[paletteer](https://cran.r-project.org/package=paletteer) for choosing the color scheme was broken.
* The
[turbo colormap](https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html) (based
on a [github gist](https://gist.github.com/jlmelville/be981e2f36485d8ef9616aef60fd52ab)) has been
added as the `turbo` function.
* New argument `rev` to reverse the ordering of the colors in the palette. This is useful when
comparing `turbo` with other rainbow palettes because `turbo` goes from blue to red.
* For the 
[new color palettes in R 4.0](https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/index.html),
you can pass them by name, e.g. `color_scheme = "Okabe-Ito"`.
* `colorRampPalette` is only used if you need to interpolate the palette (i.e. if you ask for more
colors than exist in the palette). Colors will now be returned in the order they appear in the
palette.

**September 27 2018**: Color schemes with `embed_plotly` were badly messed up. This is now fixed.
You now also have control over whether to interpolate a discrete palette.
