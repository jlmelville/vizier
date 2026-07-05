# Turbo Color Palette

Create a vector of n contiguous colors using the Turbo rainbow colormap.

## Usage

``` r
turbo(n, start = 0, end = 1, rev = FALSE)
```

## Arguments

- n:

  the number of colors (`>= 1`) to be in the palette.

- start:

  the (corrected) hue in `[0, 1]` at which the rainbow begins.

- end:

  the (corrected) hue in `[0, 1]` at which the rainbow ends.

- rev:

  logical indicating whether the ordering of the colors should be
  reversed.

## Value

A character vector `cv` containing `n` hex color codes. This can be used
either to create a user-defined color palette for subsequent graphics by
passing `cv` to
[`grDevices::palette()`](https://rdrr.io/r/grDevices/palette.html), as a
`col =` specification in graphics functions or in `par`.

## References

Mikhailkov, A. (2019). Google AI Blog: Turbo, An Improved Rainbow
Colormap for Visualization.
<https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html>.
Accessed August 26, 2020.

## See also

This function originated as (Apache 2 licensed) code at
<https://gist.github.com/jlmelville/be981e2f36485d8ef9616aef60fd52ab>,
in turn based on Python code at
<https://gist.github.com/mikhailov-work/ee72ba4191942acecc03fe6da94fc73f>.

## Examples

``` r
# use like e.g. grDevices::rainbow
pie(rep(1, 12), col = rainbow(12), main = "RGB/HSV")

pie(rep(1, 12), col = turbo(12), main = "turbo")
```
