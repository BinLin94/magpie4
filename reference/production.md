# production

reads production out of a MAgPIE gdx file

## Usage

``` r
production(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = FALSE,
  attributes = "dm",
  water_aggr = TRUE,
  cumulative = FALSE,
  baseyear = 1995,
  disagg_lvst = "feedbased"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in gdxAggregate

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

- water_aggr:

  aggregate irrigated and non-irriagted production or not (boolean).

- cumulative:

  Logical; Determines if production is reported annually (FALSE,
  default) or cumulative (TRUE)

- baseyear:

  Baseyear used for cumulative production (default = 1995)

- disagg_lvst:

  Livestock grid-level disaggregation method: "feedbased" (default)
  splits ruminant production between pasture- and cropland-weighted grid
  cells by feed composition (pasture vs fodder share), and weights
  monogastric production by urban land; "glw" disaggregates using the
  gridded livestock distribution file
  (f71_livestock_distribution_0.5.mz, produced by
  mrland::calcLivestockDistribution; must be present next to gdx).

## Value

production as MAgPIE object (unit depends on attributes and cumulative)

## See also

[`reportProduction`](reportProduction.md), [`demand`](demand.md)

## Author

Benjamin Leon Bodirsky, Bin Lin

## Examples

``` r
if (FALSE) { # \dontrun{
x <- production(gdx)
} # }
```
