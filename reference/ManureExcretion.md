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
  disagg_lvst = "landbased"
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

  large animal waste management categories:
  "grazing","stubble_grazing","fuel","confinement"),

- agg:

  aggregation over "awms" or over "products".

- disagg_lvst:

  Livestock grid-level disaggregation method: "landbased" (default)
  splits ruminant manure by awms category (grazing/fuel weighted by
  pasture production, stubble_grazing/ confinement weighted by cropland
  production) and monogastric manure by development state (urban vs
  cropland weighted); "glw" disaggregates using the gridded livestock
  distribution file (f71_livestock_distribution_0.5.mz, produced by
  mrland::calcLivestockDistribution; must be present next to gdx).

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
