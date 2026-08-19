#' @title production
#' @description reads production out of a MAgPIE gdx file
#'
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#' @importFrom magclass read.magpie getCells<- setYears getYears<-
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any
#' other aggregation level defined in gdxAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"),
#' potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param cumulative Logical; Determines if production is reported annually (FALSE, default) or cumulative (TRUE)
#' @param baseyear Baseyear used for cumulative production (default = 1995)
#' @param disagg_lvst Livestock grid-level disaggregation method: "foragebased" (default, matches
#' prior behavior) uses the pasture/cropland heuristic; "glw" forces disaggregation using the
#' gridded livestock distribution file (f71_livestock_distribution_0.5.mz, produced by
#' mrland::calcLivestockDistribution; must be present next to gdx).
#' @return production as MAgPIE object (unit depends on attributes and cumulative)
#' @author Benjamin Leon Bodirsky, Bin Lin
#' @seealso \code{\link{reportProduction}}, \code{\link{demand}}
#' @examples
#' \dontrun{
#' x <- production(gdx)
#' }
#'
production <- memoise(function(gdx, file = NULL, level = "reg", products = "kall", product_aggr = FALSE, attributes = "dm",
                       water_aggr = TRUE, cumulative = FALSE, baseyear = 1995, disagg_lvst = "foragebased") {

  if (!all(products %in% readGDX(gdx, "kall"))) {
    products <- readGDX(gdx, products)
  }
  forestryProducts <- readGDX(gdx, "kforestry")

  if (level %in% c("glo", "reg", "regglo") | isCustomAggregation(level)) {
    if (water_aggr) {
      production <- readGDX(gdx, "ov_prod_reg", select = list(type = "level"))
      production[, , forestryProducts] <- production[, , forestryProducts]
    } else {
      if (!all(products %in% findset("kcr"))) {
        stop("Irrigation only exists for production of kcr products")
      }
      area <- readGDX(gdx, "ov_area", select = list(type = "level"))[, , products]
      yield <- readGDX(gdx, "ov_yld", select = list(type = "level"))[, , products]
      production <- area * yield
      production <- superAggregateX(production, aggr_type = "sum", level = "reg")
    }
  } else if (level %in% c("cell")) {
    if (all(products %in% c(findset("kcr"), findset("kli"), "pasture"))) {
      if (water_aggr) {
        production <- readGDX(gdx, "ov_prod", select = list(type = "level"))[, , products]
      } else {
        stopifnot("Irrigation only exists for production of kcr products" = all(products %in% findset("kcr")))
        area <- readGDX(gdx, "ov_area", select = list(type = "level"))[, , products]
        yield <- readGDX(gdx, "ov_yld", select = list(type = "level"))[, , products]
        production <- area * yield
      }
    } else if (all(products %in% findset("kres")) & all(findset("kres") %in% products)) {
      stop("Benni: I think this is not calculated correctly. Production should be not biomass, but removed biomass.")
      production <- ResidueBiomass(gdx = gdx, level = level, products = "kcr", product_aggr = "kres", attributes = "dm",
                                   water_aggr = water_aggr)
    } else {
      stop("Cellular production does not yet exist for all of these products")
    }

  } else if (level %in% c("grid", "iso")) {

    if (all(products %in% c(findset("kcr"), "pasture"))) {

      ### Loading yield data (model output on cluster level and input data on cellular level)

      # defining if nocc or cc option was switched on
      yields_test <- dimSums(readGDX(gdx, "f14_yields"), dim = c(1, 3))
      if (all(setYears(yields_test[, "y1995", ], NULL) == yields_test[, "y1995", , invert = TRUE])) {
        nocc <- TRUE
      } else {
        nocc <- FALSE
      }

      # load cellular yields

      yields <- read.magpie(file.path(dirname(normalizePath(gdx)), "lpj_yields_0.5.mz"))[, , products]
      if (length(getCells(yields)) == "59199") {
        mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
        map_grid_iso <- readRDS(mapfile)
        yields <- setCells(yields, map_grid_iso$grid)
      }

      if (is.null(getYears(yields))) yields <- setYears(yields, "y1995")

      # adding missing years
      if (length(expand_time <- setdiff(readGDX(gdx, "t"), getYears(yields))) != 0) {
        last   <- paste0("y", max(getYears(yields, as.integer = TRUE)))
        yields <- add_columns(yields, addnm = expand_time, dim = 2.1)
        yields[, expand_time, ] <- setYears(yields[, last, ], NULL)
      }

      # set nocc option
      if (nocc) {
        yields[, getYears(yields)[2:length(getYears(yields))], ] <- setYears(yields[, "y1995", ], NULL)
      } else {
        # OR check if cc results trying to be disaggregated with nocc cellular data
        if (all(setYears(yields[, "y1995", ], NULL) == yields[, "y1995", , invert = TRUE])) {
          stop("Model output with climate change cannot be disaggregated with cellular data without climate change.
                Provide cellular data with climate change (can be found under the corresponding cellular data tgz on:
                /p/projects/landuse/data/input/archive/")
        }
      }

      # set yields to model output times
      yields <- yields[, readGDX(gdx, "t"), ]


      ### Use croparea to disaggregate
      # in case pasture is missing, add pasture
      if ("pasture" %in% products) {

        productsExcludingPasture <- setdiff(products, "pasture")
        pasturearea <- setNames(land(gdx = gdx, level = "grid", types = "past"), "pasture")
        pasturearea <- add_columns(add_dimension(pasturearea, dim = 3.2, add = "w", nm = "rainfed"),
                                   addnm = "irrigated", dim = 3.2)
        pasturearea[, , "irrigated"] <- 0

        if (length(productsExcludingPasture) > 0) {
          area   <- croparea(gdx = gdx, level = "grid", products = productsExcludingPasture, water_aggr = FALSE,
                             product_aggr = FALSE)
          area <- mbind(area, pasturearea)
        } else {
          area   <- pasturearea
        }

      } else {
        area   <- croparea(gdx = gdx, level = "grid", products = products, water_aggr = FALSE,
                           product_aggr = FALSE)
      }

      production <- area * yields
      if (water_aggr) {
        production <- dimSums(production, dim = "w")
      }

      x <- production(gdx = gdx, level = "cell", products = products, product_aggr = FALSE, attributes = "dm",
                      water_aggr = water_aggr)
      production <- gdxAggregate(gdx = gdx, x = x, weight = production, absolute = TRUE, to = level)


    } else if (all(products %in% findset("kres")) & all(findset("kres") %in% products)) {
      # disaggregation for crop residues
      production <- gdxAggregate(
        gdx = gdx,
        x = production(gdx = gdx, level = "cell", products = products, product_aggr = FALSE, attributes = "dm",
                       water_aggr = water_aggr),
        weight = "ResidueBiomass", product_aggr = "kres", attributes = "dm",
        absolute = TRUE, to = "grid"
      )
    } else if (all(products %in% findset("kli"))) {

      # NEW: GLW-based disaggregation, selected via disagg_lvst ("foragebased"/"glw"). Set
      # disagg_lvst = "glw" to disaggregate cluster-level production directly using the gridded
      # livestock distribution file instead of the pasture/cropland heuristic below.
      useGLWDisagg <- switch(disagg_lvst,
                             "foragebased" = FALSE,
                             "glw" = TRUE,
                             stop("disagg_lvst must be one of 'foragebased', 'glw'"))

      if (useGLWDisagg) {
        lvstDistFile <- file.path(dirname(normalizePath(gdx)), "f71_livestock_distribution_0.5.mz")
        if (!file.exists(lvstDistFile)) {
          stop("disagg_lvst = 'glw' requested but distribution file not found: ", lvstDistFile)
        }
        cellular_production <- production(gdx = gdx, level = "cell", products = products, product_aggr = FALSE,
                                          attributes = "dm", water_aggr = water_aggr)
        lvstDist <- read.magpie(lvstDistFile)[, , products]
        checkExtensiveLivestockDist(lvstDist, lvstDistFile)
        # lvstDist is only available for historical/near-term years; hold constant for future model years
        lvstDist <- time_interpolate(lvstDist, interpolated_year = getYears(cellular_production),
                                     integrate_interpolated_years = FALSE, extrapolation_type = "constant")
        production <- gdxAggregate(gdx = gdx, x = cellular_production, weight = lvstDist,
                                   absolute = TRUE, to = level)

        ## testing
        if (abs((sum(production) - sum(cellular_production))) > 10e-10) {
          warning("disaggregation failure: mismatch of sums after disaggregation")
        }
      }

      if (!useGLWDisagg) {
      warning("Disaggregation of livestock to grid level starts from regional level instead of cluster level.")
      x <- production(gdx = gdx, level = "reg", products = "kli", product_aggr = FALSE, attributes = "dm",
                      water_aggr = water_aggr)

      kli_rum    <- c("livst_rum", "livst_milk")
      kli_mon    <- c("livst_pig", "livst_chick", "livst_egg")
      ruminants    <- x[, , kli_rum]
      monogastrics <- x[, , kli_mon]

      warning("Disaggregation of livestock is done based on an method which is currently inconsistent with the method used in madrat")

      feed <- feed(gdx, level = "reg")
      feedshr <- collapseNames(feed[, , "pasture"] / dimSums(feed[, , c("pasture", "foddr")], dim = 3.2))[, , kli_rum]
      feedshr[!is.finite(feedshr)] <- 0

      ruminants_pasture <- ruminants * feedshr
      ruminants_crop <- ruminants * (1 - feedshr)

      ruminants_pasture <- gdxAggregate(
        gdx = gdx,
        x = ruminants_pasture,
        weight = "production", products = "pasture",
        absolute = TRUE, to = "grid"
      )

      ruminants_crop <- gdxAggregate(
        gdx = gdx,
        x = ruminants_crop,
        weight = "production", products = "foddr",
        absolute = TRUE, to = "grid"
      )

      ruminants <- ruminants_crop + ruminants_pasture

      monogastrics <- gdxAggregate(
        gdx = gdx,
        x = monogastrics,
        weight = "land", types = "urban",
        absolute = TRUE, to = "grid"
      )

      production <- mbind(monogastrics, ruminants)

      ## testing
      if (abs((sum(production) - sum(x))) > 10e-10) {
        warning("disaggregation failure: mismatch of sums after disaggregation")
      }
      }

    } else {
      stop("Gridded production so far only exists for production of kcr, kli and kres products")
    }

  } else {
    stop(paste0("Level ", level, " does not exist yet."))
  }

  out <- production[, , products]
  if (any(attributes != "dm")) {
    att <- readGDX(gdx, "fm_attributes")[, , products][, , attributes]
    out <- out * att
  }
  if (product_aggr) {
    out <- dimSums(out, dim = 3.1)
  }

  if (cumulative) {
    years <- getYears(out, as.integer = TRUE)
    im_years <- new.magpie("GLO", years, NULL)
    im_years[, , ] <- c(1, diff(years))
    out[, "y1995", ] <- 0
    out <- out * im_years[, getYears(out), ]
    out <- as.magpie(apply(out, c(1, 3), cumsum))
    out <- out - setYears(out[, baseyear, ], NULL)
  }

  out <- gdxAggregate(gdx = gdx, x = out, weight = NULL, to = level, absolute = TRUE,
                      products = products, product_aggr = product_aggr, water_aggr = water_aggr)
  out(out, file)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
, hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))

#' Guard against a livestock distribution weight normalised within country
#'
#' cluster-to-grid disaggregation (gdxAggregate(from = "cell", to = "grid")) renormalises
#' the weight within each cluster. A weight normalised within COUNTRY only agrees with that
#' when every cluster lies inside one country, which is false for most MAgPIE clusters, and
#' silently misallocates production between countries that share a cluster (see the
#' calcLivestockDistribution rename / output = "head" fix). This checks for that exact
#' signature - most countries' cells summing to ~1 - and fails loudly instead of continuing.
#' @noRd
checkExtensiveLivestockDist <- function(dist, distFile) {
  if (!"iso" %in% getSets(dist)) return(invisible(NULL))
  iso <- getItems(dist, dim = "iso", full = TRUE)
  if (is.null(iso)) return(invisible(NULL))
  isoSum <- tapply(as.numeric(dist[, 1, 1]), factor(iso), sum, na.rm = TRUE)
  isoSum <- isoSum[is.finite(isoSum) & isoSum > 0]
  if (length(isoSum) < 10) return(invisible(NULL))
  nUnit <- sum(abs(isoSum - 1) < 1e-6)
  if (nUnit / length(isoSum) > 0.9) {
    stop(basename(distFile), " is normalised within country (", nUnit, " of ",
         length(isoSum), " countries sum to 1), but cluster-to-grid disaggregation ",
         "needs an extensive weight. Regenerate the input with ",
         "calcOutput(\"LivestockDistribution\", output = \"head\", ...) in fullCELLULARMAGPIE.")
  }
  invisible(NULL)
}
