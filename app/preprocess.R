library(dplyr)
library(duckdbfs)
library(ggplot2)
library(mapgl)
library(glue)
library(memoise)

CACHE <-  "/tmp/Rtmp-urban" # "cache/"
css <- shiny::HTML(paste0("<link rel='stylesheet' type='text/css' href='https://demos.creative-tim.com/material-dashboard/assets/css/material-dashboard.min.css?v=3.2.0'>"))


duckdbfs::load_h3()
duckdbfs::load_spatial()
# When duckdb is I/O limited, helps to have many more processes than CPU threads
con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "SET threads = 60;")

aws_public_endpoint <- "s3-west.nrp-nautilus.io"
aws_s3_endpoint <- "s3-west.nrp-nautilus.io"

# use to populate selctors for state and county

providers <- list("all" = "all", "eBird" = "CLO", "iNaturalist" = "iNaturalist", "All others", "other")
observation_types <- list("all" = "all",
                          "Human observation" = "HUMAN_OBSERVATION",
                          "Machine observation" = "MACHINE_OBSERVATION",
                          "Specimen" = "SPECIMEN")

rank <- c("all",  "kingdom", "phylum", "class", "order", "family", "genus", "species")
svi_themes <- list(
  "Overall Social Vulnerability" = "RPL_THEMES",
  "Socio-economic status" = "RPL_THEME1",
  "Household Characteristics" = "RPL_THEME2",
  "Racial & Ethnic Minority Status" = "RPL_THEME3",
  "Housing Type & Transportation" = "RPL_THEME4")


svi_label <- function(svi_theme) {
  labels <- names(svi_themes)
  labels[svi_themes == svi_theme]
}

get_states <- memoise(function() {
  glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022/SVI2022_US_county.parquet") |>
    duckdbfs::open_dataset(recursive = FALSE) |>
    dplyr::distinct(STATE) |>
    dplyr::pull(STATE)
})

get_counties <- memoise(function(state = "California") {
  glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022/SVI2022_US_county.parquet") |>
    duckdbfs::open_dataset(recursive = FALSE) |> 
    dplyr::distinct(STATE, COUNTY) |>
    dplyr::filter(STATE %in% {state}) |>
    dplyr::arrange(COUNTY) |>
    utils::head(1000) |>
    dplyr::pull(COUNTY)
  }, cache = memoise::cache_filesystem(CACHE)
)

# get the unique taxa values in the h0 area
get_taxa <- memoise(function(rank = "class", state, county) {
  aoi <- area_hexes(state, county)
  h0 <- get_h0(aoi)
  tiles <- paste0(glue("https://{aws_public_endpoint}/public-gbif/2025-06/taxonomy/h0="), h0, "/data_0.parquet")
  open_dataset(tiles, recursive = FALSE) |>
    distinct(.data[[rank]]) |>
    #arrange(.data[[rank]]) |>
    #head(1000) |>
    pull(.data[[rank]])
})

# This has FIPS, h10, STATE, COUNTY, geom, year, currently public-social-vulnerability hex tracts dont
# tracts <- open_dataset(glue("https://{aws_public_endpoint}/public-census/year=2022/tracts-hex-z10.parquet"))




area_hexes <- function(state= "California", county = "Alemeda County") {
  tracts <- open_dataset(glue("https://{aws_public_endpoint}/public-census/year=2022/tracts-hex-z10.parquet"))
  county_hexes <- tracts |>
    filter(STATE %in% {state}, COUNTY %in% {county}) |>
    mutate(h10 = toupper(unnest(h10)))
}

get_h0 <- memoise(function(aoi){
  h0 <- aoi |> mutate(h0 = h3_cell_to_parent(h10, 0L)) |> distinct(h0) |> pull(h0) |> toupper()
  if(length(h0) < 1) { # avoid throwning error
      return("8029FFFFFFFFFFF")
  }
  h0
})

get_gbif <- function(aoi) {
  # determine h0 parent cells of this area:
  h0 <- get_h0(aoi)
  tiles <- paste0(glue("https://{aws_public_endpoint}/public-gbif/2025-06/hex/h0="), h0, "/data_0.parquet")
  open_dataset(tiles) |> select(kingdom, phylum, class, order, family, genus, species,
                                basisofrecord, institutioncode, coordinateuncertaintyinmeters, h10)
}

gbif_richness_fraction <- function(gbif, aoi, rank = "class", taxon = "Aves") {

  if (rank != "all") {
    gbif <- gbif |> filter(.data[[rank]] == {taxon})
  }

  species_area = aoi |>
    inner_join(gbif, 'h10') |>
    count(species, FIPS, geom, name = "counts")

  total = species_area |> distinct(species) |> count() |> pull(n)

  richness = species_area |>
    group_by(FIPS, geom) |>
    summarise(richness_n = n(),
              richness = n() / total,
              counts = sum(counts),
              unique_species = total,
              .groups = "drop")
  
  richness
}

compute_data <- function(state = "California",
                         county = "Alameda County",
                         rank = "class",
                         taxon = "Aves",
                         svi_metric = "RPL_THEMES",
                         observation_type = "all",
                         institution = "all") {

  
  aoi <- area_hexes(state, county)
  gbif <- get_gbif(aoi)
                        
  # Filter by observation_type groupings
  if (observation_type != "all") {
    if (observation_type == "SPECIMEN") {
      gbif <- gbif |> dplyr::filter(grepl("SPECIMEN", basisofrecord))
    } else {
      gbif <- gbif |> dplyr::filter(basisofrecord == {observation_type})
    }
  }

  # Filter by institution groupings
  if (institution != "all") {
    if (institution == "other") {
      gbif <- gbif |> dplyr::filter(!(institutioncode %in% c("CLO", "iNaturalist")))

    } else {
      gbif <- gbif |> dplyr::filter(institutioncode == institution)
    }
  }

  richness <- gbif |> gbif_richness_fraction(aoi, rank, taxon)

  # Access SVI
  svi <- open_dataset(glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022/SVI2022_US_tract.parquet")) |>
         dplyr::select("RPL_THEMES",
                "RPL_THEME1",
                "RPL_THEME2",
                "RPL_THEME3",
                "RPL_THEME4",
                "FIPS", "COUNTY") |> 
         dplyr::filter(RPL_THEMES > 0)
#         mutate(RPL_THEMES = ifelse(RPL_THEMES < 0, NA, RPL_THEMES))

  # Access CalEnviroScreen
  # ces <- open_dataset(glue("https://{aws_public_endpoint}/public-calenviroscreen/ces_2021.parquet"), format="parquet")
  # ces_subset <- ces |> select("Poverty", "FIPS")

  combined <- richness  |>
    left_join(svi, "FIPS")  |>
  #  inner_join(ces_subset, "FIPS") |>
    mutate(vulnerability = cut(.data[[svi_metric]], breaks = c(0, .25, .50, .75, 1), 
                            labels = c("Q1-Lowest", "Q2-Low", "Q3-Medium", "Q4-High")))   # |>
  #  mutate(poverty_bin = cut(Poverty, breaks = c(0, 25, 50, 75, 100), 
  #                          labels = c("0-25", "25-50", "50-75", "75-100")))

  combined

}


combined_sf <- memoise(function(state = "California", 
                                county = "Alameda County",
                                rank = "class",
                                taxon = "Aves", 
                                svi_metric = "RPL_THEMES",
                                observation_type = "all",
                                institution = "all")
  {

    if(is.null(county)) {
      county <- "Alameda County"
    }

    out <- compute_data(state, county, rank,
                        taxon, svi_metric,
                        observation_type, institution) |>
           mutate(log_counts = log(counts)) |>
           mutate(popup = paste0(
                    "<strong>Observed species richness: </strong>", richness_n,
                    "<br><strong>Overall Vulnerability: </strong>", round(RPL_THEMES,4),
                    "<br><strong>Economic: </strong>", round(RPL_THEME1,4),
                    "<br><strong>Household: </strong>", round(RPL_THEME2,4),
                    "<br><strong>Racial/Ethnic: </strong>", round(RPL_THEME3,4),
                    "<br><strong>Housing/Transit: </strong>", round(RPL_THEME4,4)
                  )
           ) |>
           to_sf(crs = "EPSG:4326")

    

    ## Handle case of nrows = 0
    if(nrow(out) < 1) {
      warning(glue("No data found matching this query.
                    state: {state}, county: {county}, rank: {rank},
                    taxon: {taxon}, svi_metric: {svi_metric}"))
    }

    return(out)

  },
  cache = cache_filesystem(CACHE)
)

# Colormap
greens = function(column = "richness") {
  interpolate(column = column,
                         values = c(0, 1),
                         stops = c("#c7fcc7",
                                   "#004600"),
                         na_color = "lightgrey")
}

viridis_pal <- 
  function(column = "richness",
           n = 20,
           min_v = 0,
           max_v = 1) {
  
  pal <- viridisLite::viridis(n)
  fill_color = mapgl::step_expr(
      column = column,
      base = pal[1],
      stops = pal[2:n],
      values = seq(min_v, max_v, length.out = n-1),
      na_color = "lightgrey"
    )
  
  fill_color
}

generate_palette <- function(gdf, variable, palette_fn = viridis_pal) {

  map_color_column <- variable
  vmin <- 0
  vmax <- 1
  if (variable == "counts") {
    map_color_column <- "log_counts"
  } 

  vmax <- max( max(gdf[[map_color_column]], na.rm=TRUE), 1)
  vmin <- min(gdf[[map_color_column]], na.rm=TRUE)

  viridis_pal(column = map_color_column, max_v = vmax,  min_v = vmin)
}
