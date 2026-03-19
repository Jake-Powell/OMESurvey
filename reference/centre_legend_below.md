# Centre a ggplot legend beneath the plot using cowplot

Extract the legend from a ggplot object and place it centred beneath the
whole plot. (As opposed to centred under the axis area only.) If the
cowplot package is not installed, the plot is returned unchanged.

## Usage

``` r
centre_legend_below(p, rel_heights = c(1, 0.1))
```

## Arguments

- p:

  A ggplot object whose legend should be centred beneath the plot.

- rel_heights:

  A numeric vector of length two giving the relative heights of the plot
  area and the legend area when stacked vertically. Defaults to
  `c(1, 0.1)`, which gives the legend 10% of the vertical space the plot
  gets.

## Value

If cowplot is installed, a cowplot object with the legend centred
underneath the plot. Otherwise, the original ggplot object is returned
unchanged.

## Details

This function extracts the legend from the supplied ggplot object,
removes the legend from the plot itself, and then stacks the plot and
legend vertically using
[`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).
The relative heights of the two components can be controlled via
`rel_heights`.

## Examples

``` r
if (FALSE) { # \dontrun{
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg, fill = factor(cyl))) +
    ggplot2::geom_col()
  centre_legend_below(p)
} # }
```
