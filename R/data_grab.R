
#' Fetch GRAIP-Lite Data
#'
#' @description This is a higher level wrapper around the \link{get_GL} and \link{get_GL_layers}
#' functions. This function can fetch multiple File Geodatabases (GDB) and returns all the layers within the GDB.
#' @param gdb A \code{character} vector of the GDB(s), e.g. \code{'Deschutes'}.
#' @param ... Arguments to pass to \link{get_GL}.
#' @author Josh Erickson
#' @return A list.
#' @seealso `get_GL()` `get_GL_layers()`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # fetch R1 GRAIP-Lite run
#'
#' GL <- fetchGL(gdb = c('R1'), quiet = TRUE)
#'
#' }

fetchGL <- function(gdb, ...) {

  gdb <- .get_GL_gdb_names(gdb)

  sapply(gdb, function(x) {
    suppressWarnings(get_GL(x, layers = get_GL_layers(x)$name, ...))
  })
}

#' Get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) Data
#'
#' @description This function calls Rocky Mountain Research Station (zip files) to get Geomorphic Road Analysis and Inventory Package Lite (GRAIP-Lite) data. These
#' datasets contain both spatial and non-spatial data in the form of a File Geodatabase (GDB).
#' @param gdb A \code{character} of the GDB, e.g. \code{'R1'}.
#' @param version A \code{character} indicating the data version, e.g. \code{'v3'}.
#' @param layers A \code{character} of the layer(s) within the GDB, e.g. \code{'HUCs_GL_Runs_R01'} (default).
#' @param quiet A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.
#' @param simplify A \code{logical}; whether to return a simplified list (\code{data.frame} or \code{sf}) if length(layers) == 1.
#' @param ... Arguments to pass to `sf::read_sf()`.
#' @author Josh Erickson
#' @seealso `get_GL_layers()`
#' @return An \code{sf} or \code{data.frame} object.
#'
#' @note Please use \code{\link{get_GL_layers}} to get the layer id information needed for the layer argument. This will
#' help with joining \code{sf} and \code{data.frame} objects.
#'
#' @details  Road Density, Proximity and Erosion Data (both unit sets1) and Stability Index Information. Data is available nationally and by Forest Service Region.
#' The GDB's currently available:
#' \itemize{
#' \item  \strong{Region 1}
#' \item  \strong{Region 2}
#' \item \strong{Region 3}
#' \item \strong{Region 4}
#' \item \strong{Region 5}
#' \item \strong{Region 6}
#' \item  \strong{Region 8}
#' \item  \strong{Region 9}
#' \item \strong{Region 10}
#' \item \strong{National Slope Stability}
#' }
#' @export
#' @examples
#' \dontrun{
#'
#' # get R1 GRAIP-Lite
#' GL_R1 <- get_GL('R1')
#'
#' # get multiple layers in a list
#'
#' GL_R1_multiple <- get_GL(gdb = 'R1',
#' layers = c('WCATT_HUCs_GL_Data_USC_Units_R01', 'WCATT_HUCs_GL_Data_SI_Units_R01'))
#'
#' # Or run with a SQL query
#' r1_gl <- get_GL(gdb = 'R1', layers = 'WCATT_HUCs_GL_Data_USC_Units_R01',
#' query = "select * from \"WCATT_HUCs_GL_Data_USC_Units_R01\" where STATES='CN,MT' OR STATES='MT' AND SUBSTR(HUC_12, 1, 4) = '1701'")
#'
#' }
#'
#'
get_GL <- function(gdb, layers = 'WCATT_HUCs_GL_Data_USC_Units_R01', version = 'v3', quiet = FALSE, simplify = TRUE, ...) {

  gdb <- .get_GL_gdb_names(gdb)

  GL <- list()

  for(i in layers){

    GL_get <- try(list(sf::read_sf(paste0('/vsizip//vsicurl/https://www.fs.usda.gov/research/sites/default/files/2023-02/rmrs',gdb,'_',version,'_gdb.zip'),
                                   layer = i,
                                   quiet = quiet,
                                   as_tibble = FALSE, ...)),
                                   silent = TRUE)

    names(GL_get) <- i

    GL <- append(GL, GL_get)

  }

  if(length(layers) == 1) {if(isTRUE(simplify)){GL <- GL[[1]]} else {GL}}

  GL

}

#' Get GL Layers
#'
#' @param gdb A \code{character} of the GDB, e.g. \code{'R1'}.
#' @param version A \code{character} indicating the data version, e.g. \code{'v3'}.
#' @author Josh Erickson
#'
#' @return A list of metadata about the GDB
#' @export
#'
#' @note Refer to \code{\link{get_GL}} for information on File Geodatabase (GDB) availability.
#'
#' @examples
#' \dontrun{
#' GL_layers <- get_GL_layers('R1')
#' }
#'
get_GL_layers <- function(gdb, version = 'v3') {

  gdb <- .get_GL_gdb_names(gdb)

  layers <- try(sf::st_layers(paste0('/vsizip//vsicurl/https://www.fs.usda.gov/research/sites/default/files/2023-02/rmrs',gdb,'_',version,'_gdb.zip')), silent = TRUE)

  as.data.frame(sapply(layers, I))
}


#' matching helper
#' @param gdb A character.
#' @return A gdb character.
.get_GL_gdb_names <- function(gdb) {

  gdb_names <- tolower(c('Region1', 'Region2', 'Region3', 'Region4', 'Region5',
                         'Region6', 'Region8', 'Region9', 'Region10',
                         'Region 1', 'Region 2', 'Region 3', 'Region 4',
                         'Region 5', 'Region 6', 'Region 8', 'Region 9',
                         'Region 10', 'Mt. Hood', 'Slope Stability', 'SS',
                         'R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R8', 'R9', 'R10'
  ))

  gdb <- match.arg(tolower(gdb), choices = gdb_names, several.ok = TRUE)

  ifelse(gdb %in% c('region1', 'region 1', 'r1'), '-region01',
         ifelse(gdb %in% c('region2', 'region 2', 'r1'), '-region06',
                ifelse(gdb %in% c('region3', 'region 3', 'r1'), '-region03',
                       ifelse(gdb %in% c('region4', 'region 4', 'r1'), '-region04',
                              ifelse(gdb %in% c('region5', 'region 5', 'r1'), '-region05',
                                     ifelse(gdb %in% c('region6', 'region 6', 'r1'), '-region06',
                                            ifelse(gdb %in% c('region8', 'region 8', 'r1'), '-region08',
                                                   ifelse(gdb %in% c('region9', 'region 9', 'r1'), '-region09',
                                                          ifelse(gdb %in% c('region10', 'region 10', 'r1'), '-region10',
                                                                 ifelse(gdb %in% c('ss', 'slope stability'), '-national_slopestability', NA))))))))))

}


#' Get NorWeST stream temperature scenarios
#'
#' @description This function calls Rocky Mountain Research Station (zip files) to get NorWest stream temperature data \insertCite{isaak2017norwest}{fishguts}.
#' @param processing_units A \code{character} of the Processing Units, e.g. \code{'SpoKoot'}.
#' @param type A \code{character} indicating the type of simple feature, e.g. \code{'points'}, \code{'lines'}.
#' @param quiet A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.
#' @param ... Arguments to pass to `sf::read_sf()`.
#' @author Josh Erickson
#' @return An \code{sf} object.
#' @references {
#' \insertAllCited
#' }
#'
#' @details  NorWeST stream temperature scenario maps were developed at a 1-kilometer resolution using spatial statistical stream network models.
#'  Stream temperature data used to fit the temperature model that created the scenario maps were screened using a consistent set of criteria to ensure the use of accurate temperature measurements.
#' Application of these techniques in previous research has yielded accurate and unbiased stream temperature models and predictions (R2 ~ 0.90; RMSE < 1.0 ˚C; for more details, click here).
#'  Multiple historical scenarios from 1993 to 2015 are available, as are future scenarios for mid-century (2030–2059), end-of-century (2070–2099), and date agnostic temperature increases
#'  (e.g., +1C relative to historical baseline periods). The stream temperature scenarios can be downloaded as geospatial data, which enables the display and querying of
#'  stream temperatures for river basins and other areas of interest across the western U.S.
#'
#' \itemize{
#' \item  \strong{Eastern Montana} Includes data up to 2020
#' \item  \strong{Clearwater River Basin} Includes data up to 2015
#' \item \strong{SpoKoot} Includes data up to 2015
#' \item \strong{Missouri Headwaters Unit} Includes data up to 2015
#' \item \strong{Upper Yellowstone-Bighorn} Includes data up to 2015
#' \item \strong{Upper Missouri-Marias}Includes data up to 2015
#' }
#' @export
#' @examples
#' \dontrun{
#'
#' # get SpoKoot NorWest polylines
#'
#' NorWest_lines <- get_NorWestStreams(processing_units = 'SpoKoot', type = 'lines'))
#'
#'
#' }
#'
#'

get_NorWestStreams <- function(processing_units, type = 'lines', quiet = TRUE, ...){

  .norwest_names <- .get_norwest_streamsegment_names(processing_units = processing_units, type = type)

    nw_get <- try(sf::read_sf(.norwest_names,
                                   quiet = quiet,
                                   as_tibble = FALSE, ...),
                  silent = TRUE)

  nw_get
}

#' matching helper
#' @param processing_units A \code{character} of the Processing Units, e.g. \code{'SpoKoot'}.
#' @param type A \code{character} indicating the type of simple feature, e.g. \code{'points'}, \code{'lines'}.
#' @return A processing_units character.
.get_norwest_streamsegment_names <- function(processing_units, type) {

  nw_names <- tolower(c('Eastern Montana', 'Clearwater River Basin', 'CRWB', 'SpoKoot', 'Koot',
                         'Missouri Headwaters', 'Missouri HW', 'Missouri Headwaters Unit', 'Upper Yellowstone-Bighorn',
                         'Upper Yellowstone', 'Bighorn', 'Upper Missouri-Marias', 'Upper Missouri',
                         'Marias'
  ))

  nw_names <- match.arg(tolower(processing_units), choices = nw_names, several.ok = TRUE)

  switch(type,
         'points' = {ifelse(nw_names %in% c('eastern montana'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/eastern-montana/NorWeST_PredictedStreamTemperatureLines_EMT.zip',
                          ifelse(nw_names %in% c('clearwater river basin', 'cwrb'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/170603_Clearwater/NorWeST_PredictedStreamTempLines_Clearwater.zip',
                                 ifelse(nw_names %in% c('spokoot', 'koot'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/170101_02_03_SpoKoot/NorWeST_PredictedStreamTempLines_Spokoot.zip',
                                        ifelse(nw_names %in% c('missouri headwaters', 'missouri hw', 'missouri headwaters unit'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100200_MissouriHeadwaters/NorWeST_PredictedStreamTempLines_MissouriHW_Aug.zip',
                                               ifelse(nw_names %in% c('upper yellowstone-bighorn', 'upper yellowstone', 'bighorn'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100700_100800_100901_100902_UpperYellowstoneBighorn/NorWeST_PredictedStreamTempLines_UpperYellowstoneBighorn_Aug.zip',
                                                      ifelse(nw_names %in% c('upper missouri-marias', 'upper missouri', 'marias'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100100_301_302_401_402_500_MariasMissouri/NorWeST_PredictedStreamTempLines_UpperMissouriMarias_Aug.zip', NA))))))},
         'lines' = {ifelse(nw_names %in% c('eastern montana'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/eastern-montana/NorWeST_PredictedStreamTemperaturePoints_EMT.zip',
                          ifelse(nw_names %in% c('clearwater river basin', 'cwrb'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/170603_Clearwater/NorWeST_PredictedStreamTempPoints_Clearwater.zip',
                                 ifelse(nw_names %in% c('spokoot', 'koot'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/170101_02_03_SpoKoot/NorWeST_PredictedStreamTempPoints_Spokoot.zip',
                                        ifelse(nw_names %in% c('missouri headwaters', 'missouri hw', 'missouri headwaters unit'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100200_MissouriHeadwaters/NorWest_PredictedStreamTempPoints_MissouriHW_Aug.zip',
                                               ifelse(nw_names %in% c('upper yellowstone-bighorn', 'upper yellowstone', 'bighorn'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100700_100800_100901_100902_UpperYellowstoneBighorn/NorWeST_PredictedStreamTempPoints_UpperYellowstoneBighorn_Aug.zip',
                                                      ifelse(nw_names %in% c('upper missouri-marias', 'upper missouri', 'marias'), '/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/ModeledStreamTemperatureMaps/100100_301_302_401_402_500_MariasMissouri/NorWeST_PredictedStreamTempPoints_UpperMissouriMarias_Aug.zip', NA))))))}
  )

}



#' Get Climate Shield scenarios
#'
#' @description This function calls Rocky Mountain Research Station (zip files) to get Climate Shield
#' bull trout and cutthroat trout population occurrence scenarios for the western US \insertCite{isaak2017climate}{fishguts}.
#' @param processing_units A \code{character} of the Processing Units, e.g. \code{'SpoKoot'}.
#' @param type A \code{character} indicating the type of fish, e.g. \code{'bt'}, \code{'ct'}.
#' @param climate_scenario A \code{character} indicating the climate scenario, e.g. \code{'baseline'}, \code{'moderate'}, \code{'extreme'}.
#' @param revised A \code{logical} for \insertCite{isaak2022metapopulations}{fishguts} revised version.
#' @param quiet A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.
#' @param ... Arguments to pass to `sf::read_sf()`.
#' @author Josh Erickson
#' @return An \code{sf} object.
#' @references {
#' \insertAllCited
#' }
#'
#' @details  The Climate Shield models developed by \insertCite{isaak2015cold}{fishguts} provide stream-specific rangewide
#'  probabilistic predictions about the occurrence of juvenile Bull Trout and Cutthroat Trout under different
#'  scenarios of climate change and Brook Trout invasions. That information is available here as easy-to-use
#'  digital maps (.pdf files) and ArcGIS shapefiles for all streams within the historical ranges of these two
#'  native trout species across the western U.S. The geographic areas match the NorWeST production units because those stream temperature scenarios are integral
#'  to Climate Shield.
#'
#'  The climate scenarios used in this project represent baseline (1980s), moderate (2040s), and extreme (2080s)
#'  climate change conditions and were chosen because they bracket what might be considered a “pristine” historical
#'  condition and “worst-case” end-of-century conditions.
#' @note Readme can be found at this [link](https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/publications_posters/2022/Appendix_S6.pdf).
#' \itemize{
#' \item  \strong{Clearwater River Basin} Includes data up to 2015
#' \item \strong{SpoKoot} Includes data up to 2015
#' \item \strong{Missouri Headwaters Unit} Includes data up to 2015
#' \item \strong{Upper Yellowstone-Bighorn} Includes data up to 2015
#' \item \strong{Upper Missouri-Marias}Includes data up to 2015
#' }
#' @export
#' @examples
#' \dontrun{
#'
#' # get R1 GRAIP-Lite
#' spokoot_cs <- get_ClimateShield(processing_unit = 'SpoKoot', type = 'bt', climate_scenario = 'baseline')
#' }
#'
#'

get_ClimateShield <- function(processing_units,
                              type = 'bt',
                              climate_scenario = 'baseline',
                              revised = FALSE,
                              quiet = TRUE, ...){

  if(revised){

   .climate_shield_names <- paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/2022/BullTroutPatchScenarios_',capit(climate_scenario),'Climate_Isaak_et_al_2022_AppendixS6.zip')

  } else {

   .climate_shield_names <- .get_climate_shield_names(processing_units = processing_units, type = type, climate_scenario = climate_scenario)

  }

  nw_get <- try(sf::read_sf(.climate_shield_names,
                            quiet = quiet,
                            as_tibble = FALSE, ...),
                silent = TRUE)

  nw_get

}

#' matching helper
#' @param processing_units A \code{character} of the Processing Units, e.g. \code{'SpoKoot'}.
#' @param type A \code{character} indicating the type of fish, e.g. \code{'bt'}, \code{'ct'}.
#' @param climate_scenario A \code{character} indicating the climate scenario, e.g. \code{'baseline'}, \code{'moderate'}, \code{'extreme'}.
#' @return A processing_units character.
.get_climate_shield_names <- function(processing_units, type, climate_scenario) {

  nw_names <- tolower(c('Eastern Montana', 'Clearwater River Basin', 'CRWB', 'SpoKoot', 'Koot',
                        'Missouri Headwaters', 'Missouri HW', 'Missouri Headwaters Unit', 'Upper Yellowstone-Bighorn',
                        'Upper Yellowstone', 'Bighorn', 'Upper Missouri-Marias', 'Upper Missouri',
                        'Marias'
  ))

  nw_names <- match.arg(tolower(processing_units), choices = nw_names, several.ok = TRUE)

  cs_name <- switch(climate_scenario,
                    'baseline' = '1980',
                    'moderate' = '2040',
                    'extreme' = '2080')

  switch(type,
         'bt' = {ifelse(nw_names %in% c('clearwater river basin', 'cwrb'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/Clearwater/Clearwater_B0BK0',cs_name,'.zip/ClimateShield/Clearwater/B0BK0',cs_name,'_Clearwater.shp'),
                                   ifelse(nw_names %in% c('spokoot', 'koot'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/SpoKoot/SpoKoot_B0BK0',cs_name,'.zip/ClimateShield/SpoKoot/B0BK0', cs_name,'_SpoKoot.shp'),
                                                        ifelse(nw_names %in% c('upper missouri-marias', 'upper missouri', 'marias'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/UpperMissouriMarias/UpperMissouriMarias_B0BK0',cs_name,'.zip/ClimateShield/UpperMissouriMarias/B0BK0', cs_name,'_UpperMissouriMarias.shp'), NA)))},
         'ct' = {ifelse(nw_names %in% c('clearwater river basin', 'cwrb'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/Clearwater/Clearwater_C0BK0',cs_name,'.zip/ClimateShield/Clearwater/C0BK0',cs_name,'_Clearwater.shp'),
                                  ifelse(nw_names %in% c('spokoot', 'koot'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/SpoKoot/SpoKoot_C0BK0',cs_name,'.zip/ClimateShield/SpoKoot/C0BK0',cs_name,'_SpoKoot.shp'),
                                         ifelse(nw_names %in% c('missouri headwaters', 'missouri hw', 'missouri headwaters unit'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/MissouriHW/MissouriHW_C0BK0',cs_name,'.zip/ClimateShield/MissouriHW/C0BK0',cs_name,'_MissouriHW.shp'),
                                                ifelse(nw_names %in% c('upper yellowstone-bighorn', 'upper yellowstone', 'bighorn'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/UpperYellowstoneBighorn/UpperYellowstoneBighorn_C0BK0',cs_name,'.zip/ClimateShield/UpperYellowstoneBighorn/C0BK0',cs_name,'_UpperYellowstoneBighorn.shp'),
                                                       ifelse(nw_names %in% c('upper missouri-marias', 'upper missouri', 'marias'), paste0('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/UpperMissouriMarias/UpperMissouriMarias_C0BK0',cs_name,'.zip/ClimateShield/UpperMissouriMarias/C0BK0',cs_name,'_UpperMissouriMarias.shp'), NA)))))}
  )

}


#' Get Bull Trout Natal Habitat Patches
#'
#' @description This function calls Rocky Mountain Research Station (zip files) to get bull trout natal habitat patches
#' from \insertCite{isaak2022metapopulations}{fishguts} paper with associated model parameters.
#' @return A \code{sf} object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # get R1 GRAIP-Lite
#' isaak2022 <- get_BullTroutNatalPatches()
#' }
#'
get_BullTroutNatalPatches <- function() {

  try(sf::read_sf('/vsizip//vsicurl/https://www.fs.usda.gov/rm/boise/AWAE/projects/ClimateShield/downloads/LookUpTables/2022/BullTroutPatches_ObservedDataset_Isaak_et_al_2022_AppendixS1.zip',
                  as_tibble = FALSE),
      silent = TRUE)

}
