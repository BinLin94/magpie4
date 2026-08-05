#' @title ManureExcretion
#' @description downscales Manure Excretion
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level: glo, reg, cell, grid, iso
#' @param products livestock products
#' @param awms large animal waste management categories: "grazing","stubble_grazing","fuel","confinement"),
#' @param agg aggregation over "awms" or over "products".
#' @param disagg_lvst Livestock grid-level disaggregation method: "foragebased" (default, matches
#' prior behavior) uses the pasture/cropland heuristic; "glw" forces the GLW-based weight
#' disaggregation (requires the gridded livestock weight file to be present next to gdx).
#'
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky, Bin Lin
#' @examples
#'
#'   \dontrun{
#'     x <- ManureExcretion(gdx)
#'   }
#'

ManureExcretion <- memoise(function(gdx,level="reg",products="kli",awms=c("grazing","stubble_grazing","fuel","confinement"), agg=TRUE, disagg_lvst = "foragebased") {

  products=findset(products,noset = "original")

  manure <- collapseNames(readGDX(gdx, "ov_manure", select = list(type = "level"))[,,"nr"])

  if(level%in%c("cell")){
    #downscale to cell using magpie info
    manure <- gdxAggregate(gdx = gdx,weight = 'production',x = manure,to = "cell",absolute = TRUE, products = readGDX(gdx,"kli"), product_aggr = FALSE)
  }
  if(level %in% c("grid","iso")) {

    # NEW: GLW-based disaggregation, mirrors the approach in magpie4::production(), selected via
    # disagg_lvst ("foragebased"/"glw"). Set disagg_lvst = "glw" to disaggregate cluster-level
    # manure directly using the gridded livestock weight file instead of the pasture/cropland
    # heuristic below.
    useGLWDisagg <- switch(disagg_lvst,
                           "foragebased" = FALSE,
                           "glw" = TRUE,
                           stop("disagg_lvst must be one of 'foragebased', 'glw'"))

    if (useGLWDisagg) {
      weightFile <- file.path(dirname(normalizePath(gdx)), "f71_livestock_weight_0.5.mz")
      if (!file.exists(weightFile)) {
        stop("disagg_lvst = 'glw' requested but weight file not found: ", weightFile)
      }
      cellular_manure <- gdxAggregate(gdx = gdx, weight = "production", x = manure, to = "cell",
                                      absolute = TRUE, products = readGDX(gdx, "kli"), product_aggr = FALSE)

      weight <- read.magpie(weightFile)[, , readGDX(gdx, "kli")]
      # weight is only available for historical/near-term years; hold constant for future model years
      weight <- time_interpolate(weight, interpolated_year = getYears(cellular_manure),
                                 integrate_interpolated_years = FALSE, extrapolation_type = "constant")

      # weight only varies by kli category, not by awms; disaggregate each awms slice separately
      # to avoid ambiguity aggregating a weight against manure's compound kli.awms dim3
      manure <- mbind(lapply(awms, function(a) {
        sliceManure <- collapseNames(cellular_manure[, , a])
        disagg <- gdxAggregate(gdx = gdx, x = sliceManure, weight = weight, absolute = TRUE, to = level)
        add_dimension(disagg, dim = 3.2, add = "awms", nm = a)
      }))

      ## testing
      if (abs((sum(manure) - sum(cellular_manure))) > 10e-10) {
        warning("disaggregation failure: mismatch of sums after disaggregation")
      }
    }

    if (!useGLWDisagg) {
    #kli_rum=readGDX(gdx,"kli_rum")
    #kli_mon=readGDX(gdx,"kli_mon")
    kli_rum=c("livst_rum","livst_milk")
    kli_mon=c("livst_pig","livst_chick","livst_egg")

    ruminants = manure[,,kli_rum]
    monogastrics = manure[,,kli_mon]

    ruminants_pasture <- ruminants[,,c("grazing","fuel")]
    ruminants_crop <- ruminants[,,c("stubble_grazing","confinement")]

    ruminants_pasture<-gdxAggregate(
      gdx=gdx,
      x = ruminants_pasture,
      weight = "production", products = "pasture",
      absolute = TRUE,to = level)

    kcr_without_bioenergy = setdiff(findset("kcr"),c("betr","begr"))
    ruminants_crop<-gdxAggregate(
      gdx=gdx,
      x = ruminants_crop,
      weight = "production", products = kcr_without_bioenergy, product_aggr=TRUE, attributes="nr",
      absolute = TRUE,to = level)

    ruminants <- mbind(ruminants_crop, ruminants_pasture)

    dev <- readGDX(gdx,"im_development_state")[,getYears(monogastrics),]
    monogastrics_cities <- monogastrics*(1-dev)
    monogastrics_cropland <- monogastrics*dev

    monogastrics_cities<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cities,
      weight = "land", types="urban",
      absolute = TRUE,to = level)

    monogastrics_cropland<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cropland,
      weight = "land", types="crop",
      absolute = TRUE,to = level)

    monogastrics <- monogastrics_cities + monogastrics_cropland

    manure <- mbind(monogastrics,ruminants)
    }
  }
  x=manure

  ##testing
  if (abs((sum(x)-sum(manure)))>10^-10) { warning("disaggregation failure: mismatch of sums after disaggregation")}

  x = x[,,list(kli = products,awms = awms)]
  if("awms"%in%agg){
    x=dimSums(x,dim="awms")
  }
  if("products"%in%agg){
    x=dimSums(x,dim="kli")
  }

  return(x)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
,hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))

