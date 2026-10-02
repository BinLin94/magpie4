# ManureExcretion

downscales Manure Excretion

## Usage

``` r
ManureExcretion(
  gdx,
  level = "reg",
  products = "kli",
  awms = c("grazing", "stubble_grazing", "fuel", "confinement"),
  agg = TRUE,
  disagg_lvst = "landbased",
  recycled = FALSE
)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level: glo, reg, cell, grid, iso

- products:

  livestock products

- awms:

  animal waste management systems: "grazing", "stubble_grazing", "fuel",
  "confinement"

- agg:

  aggregation over "awms" or over "products".

- disagg_lvst:

  grid-level disaggregation method (level "grid"/"iso" only):
  "landbased" (default) weights ruminant manure by pasture or crop
  production and monogastric manure by urban or cropland area; "glw"
  weights by the gridded livestock distribution
  (f71_livestock_distribution_0.5.mz next to gdx) times the pasture
  share (grazing, fuel) or cropland share (stubble_grazing).

- recycled:

  "glw" only: if TRUE, confinement manure is also weighted by the
  cropland share.

## Value

MAgPIE object

## Author

Benjamin Leon Bodirsky, Bin Lin

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- ManureExcretion(gdx)
  } # }
```
