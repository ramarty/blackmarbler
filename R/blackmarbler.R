# TODO
# 1. Cloud mask: https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.1_July_2020.pdf
#   file:///Users/robmarty/Downloads/Thesis_Zihao_Zheng.pdf

if(F){
  library(purrr)
  library(furrr)
  library(stringr)
  library(rhdf5)
  library(raster)
  library(dplyr)
  library(sf)
  library(lubridate)
}

month_start_day_to_month <- function(x){
  
  month <- NA
  
  if(x == "001") month <- "01"
  
  if(x == "032") month <- "02"
  
  if(x == "060") month <- "03"
  if(x == "061") month <- "03"
  
  if(x == "091") month <- "04"
  if(x == "092") month <- "04"
  
  if(x == "121") month <- "05"
  if(x == "122") month <- "05"
  
  if(x == "152") month <- "06"
  if(x == "153") month <- "06"
  
  if(x == "182") month <- "07"
  if(x == "183") month <- "07"
  
  if(x == "213") month <- "08"
  if(x == "214") month <- "08"
  
  if(x == "244") month <- "09"
  if(x == "245") month <- "09"
  
  if(x == "274") month <- "10"
  if(x == "275") month <- "10"
  
  if(x == "305") month <- "11"
  if(x == "306") month <- "11"
  
  if(x == "335") month <- "12"
  if(x == "336") month <- "12"
  
  return(month)
}

month_start_day_to_month <- Vectorize(month_start_day_to_month)

pad2 <- function(x){
  if(nchar(x) == 1) out <- paste0("0", x)
  if(nchar(x) == 2) out <- paste0(x)
  return(out)
}
pad2 <- Vectorize(pad2)

pad3 <- function(x){
  if(nchar(x) == 1) out <- paste0("00", x)
  if(nchar(x) == 2) out <- paste0("0", x)
  if(nchar(x) == 3) out <- paste0(x)
  return(out)
}
pad3 <- Vectorize(pad3)


file_to_raster <- function(f, 
                           variable){
  # Converts h5 file to raster.
  # ARGS
  # --f: Filepath to h5 file
  
  ## Boundaries
  # Only works on later years
  #spInfo <- h5readAttributes(f,"/") 
  
  #xMin<-spInfo$WestBoundingCoord
  #yMin<-spInfo$SouthBoundingCoord
  #yMax<-spInfo$NorthBoundingCoord
  #xMax<-spInfo$EastBoundingCoord
  
  ## Data
  h5_data <- H5Fopen(f) 
  
  if(f %>% str_detect("VNP46A1|VNP46A2")){
    
    tile_i <- f %>% str_extract("h\\d{2}v\\d{2}")
    
    grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/blackmarbler/main/data/blackmarbletiles.geojson")
    grid_i_sf <- grid_sf[grid_sf$TileID %in% tile_i,]
    
    grid_i_sf_box <- grid_i_sf %>%
      st_bbox()
    
    xMin <- min(grid_i_sf_box$xmin) %>% round()
    yMin <- min(grid_i_sf_box$ymin) %>% round()
    xMax <- max(grid_i_sf_box$xmax) %>% round()
    yMax <- max(grid_i_sf_box$ymax) %>% round()
    
    out <- h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`[[variable]]
    
    #names(h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`)
    #h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`$QF_Cloud_Mask %>% as.vector() %>% table()
    #if(product_id %in% c("VNP46A1", "VNP46A2")){
    #out <- h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`$`DNB_At_Sensor_Radiance_500m`
    
    #}
    
    #if(product_id == "VNP46A2"){
    #  out <- h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`$`Gap_Filled_DNB_BRDF-Corrected_NTL`
    #}
    
    # print(names(h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`))
    
  } else{
    lat <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lat
    lon <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lon
    out <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`[[variable]]
    #land_water_mask <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$Land_Water_Mask
    
    xMin <- min(lon) %>% round()
    yMin <- min(lat) %>% round()
    xMax <- max(lon) %>% round()
    yMax <- max(lat) %>% round()
    
    # print(names(h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`))
  }
  
  ## Metadata
  nRows      <- nrow(out)
  nCols      <- ncol(out)
  res        <- nRows
  nodata_val <- NA
  myCrs      <- 4326
  
  ## Make Raster
  
  #transpose data to fix flipped row and column order 
  #depending upon how your data are formatted you might not have to perform this
  out <- t(out)
  #land_water_mask <- t(land_water_mask)
  
  #assign data ignore values to NA
  out[out == nodata_val] <- NA
  
  #turn the out object into a raster
  outr <- raster(out,crs=myCrs)
  #land_water_mask_r <- raster(land_water_mask,crs=myCrs)
  
  #create extents class
  rasExt <- raster::extent(c(xMin,xMax,yMin,yMax))
  
  #assign the extents to the raster
  extent(outr) <- rasExt
  #extent(land_water_mask_r) <- rasExt
  
  #water to 0
  outr[][outr[] %in% 65535] <- NA 
  
  h5closeAll()
  
  return(outr)
}

read_bm_csv <- function(year, 
                        day,
                        product_id){
  #print(paste0("Reading: ", product_id, "/", year, "/", day))
  df_out <- tryCatch(
    {
      df <- read.csv(paste0("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/",product_id,"/",year,"/",day,".csv"))
      
      df$year <- year
      df$day <- day
      
      df
    },
    error = function(e){
      warning(paste0("Error with year: ", year, "; day: ", day))
      data.frame(NULL)
    }
  )
  
  Sys.sleep(0.1)
  
  return(df_out)
}

create_dataset_name_df <- function(product_id,
                                   all = TRUE,
                                   years = NULL, 
                                   months = NULL,
                                   days = NULL){
  
  #### Prep dates
  if(product_id %in% c("VNP46A1", "VNP46A2")) months <- NULL
  if(product_id %in% c("VNP46A3"))            days <- NULL
  if(product_id %in% c("VNP46A4")){
    days <- NULL
    months <- NULL
  } 
  
  #### Determine end year
  year_end <- Sys.Date() %>% 
    substring(1,4) %>% 
    as.numeric()
  
  #### Make parameter dataframe
  if(product_id %in% c("VNP46A1", "VNP46A2")){
    param_df <- cross_df(list(year = 2012:year_end,
                              day  = pad3(1:366)))
  }
  
  if(product_id == "VNP46A3"){
    param_df <- cross_df(list(year            = 2012:year_end,
                              day = c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336",
                                      "060", "091", "121", "152", "182", "213", "244", "274", "305", "335")))
  }
  
  if(product_id == "VNP46A4"){
    param_df <- cross_df(list(year = 2012:year_end,
                              day  = "001"))
  }
  
  #### Add month if daily or monthly data
  if(product_id %in% c("VNP46A1", "VNP46A2", "VNP46A3")){
    
    param_df <- param_df %>%
      dplyr::mutate(month = day %>% 
                      month_start_day_to_month() %>%
                      as.numeric())
    
  }
  
  #### Subset time period
  ## Year
  if(!is.null(years)){
    param_df <- param_df[param_df$year %in% years,]
  }
  
  ## Month
  if(product_id %in% c("VNP46A1", "VNP46A2", "VNP46A3")){
    
    if(!is.null(months)){
      param_df <- param_df[as.numeric(param_df$month) %in% as.numeric(months),]
    }
    
    if(!is.null(days)){
      param_df <- param_df[as.numeric(param_df$day) %in% as.numeric(days),]
    }
    
  }
  
  #### Create data
  files_df <- map2_dfr(param_df$year,
                       param_df$day,
                       read_bm_csv,
                       product_id)
  
  return(files_df)
}

download_raster <- function(file_name, 
                            temp_dir,
                            variable,
                            bearer){
  
  year       <- file_name %>% substring(10,13)
  day        <- file_name %>% substring(14,16)
  product_id <- file_name %>% substring(1,7)
  
  wget_command <- paste0("wget -e robots=off -m -np .html,.tmp -nH --cut-dirs=3 ",
                         "'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/",product_id,"/", year, "/", day, "/", file_name,"'",
                         " --header 'Authorization: Bearer ",
                         bearer,
                         "' -P ",
                         temp_dir, 
                         "/") #                          " --no-if-modified-since"
  
  system(wget_command)
  
  r <- file_to_raster(file.path(temp_dir, product_id, year, day, file_name), variable)
  
  return(r)
}

#' Make Black Marble Raster
#' 
#' Make a raster of nighttime lights from [NASA Black Marble data](https://blackmarble.gsfc.nasa.gov/)

#' @param roi_sf Region of interest; sf polygon. Must be in the [WGS 84 (epsg:4326)](https://epsg.io/4326) coordinate reference system.
#' @param product_id One of the following: 
#' * `"VNP46A1"`: Daily (raw)
#' * `"VNP46A2"`: Daily (corrected)
#' * `"VNP46A3"`: Monthly
#' * `"VNP46A4"`: Annual
#' @param date Date of raster data. Entering one date will produce a raster. Entering multiple dates will produce a raster stack. 
#' * For `product_id`s `"VNP46A1"` and `"VNP46A2"`, a date (eg, `"2021-10-03"`). 
#' * For `product_id` `"VNP46A3"`, a date or year-month (e.g., `"2021-10-01"`, where the day will be ignored, or `"2021-10"`).
#' * For `product_id` `"VNP46A4"`, year or date  (e.g., `"2021-10-01"`, where the month and day will be ignored, or `2021`). 
#' @param bearer NASA bearer token. For instructions on how to create a token, see [here](https://github.com/ramarty/blackmarbler#bearer-token-).
#' @param variable Variable to used to create raster (default: `NULL`). If `NULL`, uses the following default variables: 
#' * For `product_id` `:VNP46A1"`, uses `DNB_At_Sensor_Radiance_500m`. 
#' * For `product_id` `"VNP46A2"`, uses `Gap_Filled_DNB_BRDF-Corrected_NTL`. 
#' * For `product_id`s `"VNP46A3"` and `"VNP46A4"`, uses `NearNadir_Composite_Snow_Free`. 
#' For information on other variable choices, see [here](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.2_April_2021.pdf); for `VNP46A1`, see Table 3; for `VNP46A2` see Table 6; for `VNP46A3` and `VNP46A4`, see Table 9.
#'
#' @return Raster
#'
#' @examples
#' \dontrun{
#' # Define bearer token
#' bearer <- "BEARER-TOKEN-HERE"
#' 
#' # sf polygon of Ghana
#' library(geodata)
#' roi_sf <- gadm(country = "GHA", level=0, path = tempdir()) %>% st_as_sf()
#' 
#' # Daily data: raster for October 3, 2021
#' ken_20210205_r <- bm_raster(roi_sf = roi_sf,
#'                             product_id = "VNP46A2",
#'                             date = "2021-10-03",
#'                             bearer = bearer)
#' 
#' # Monthly data: raster for March 2021
#' ken_202103_r <- bm_raster(roi_sf = roi_sf,
#'                           product_id = "VNP46A3",
#'                           date = "2021-03-01",
#'                           bearer = bearer)
#' 
#' # Annual data: raster for 2021
#' ken_2021_r <- bm_raster(roi_sf = roi_sf,
#'                         product_id = "VNP46A4",
#'                         date = 2021,
#'                         bearer = bearer)
#'}
#'
#' @export
#' 
#' @import purrr
#' @import furrr
#' @import stringr
#' @import rhdf5
#' @import raster
#' @import dplyr
#' @import sf
#' @import lubridate

bm_raster <- function(roi_sf,
                      product_id,
                      date,
                      bearer,
                      variable = NULL){
  
  # Checks ---------------------------------------------------------------------
  if(nrow(roi_sf) > 1){
    stop("roi must be 1 row")
  }
  
  if(!("sf" %in% class(roi_sf))){
    stop("roi must be an sf object")
  }
  
  # NTL Variable ---------------------------------------------------------------
  if(is.null(variable)){
    if(product_id == "VNP46A1") variable <- "DNB_At_Sensor_Radiance_500m"
    if(product_id == "VNP46A2") variable <- "Gap_Filled_DNB_BRDF-Corrected_NTL"
    if(product_id %in% c("VNP46A3", "VNP46A4")) variable <- "NearNadir_Composite_Snow_Free"
  }
  
  # Download data --------------------------------------------------------------
  r_list <- lapply(date, function(date_i){
    
    out <- tryCatch(
      {
        
        #### Make name for raster based on date
        if(product_id %in% c("VNP46A1", "VNP46A2")){
          date_name_i <- paste0("t", date_i %>% str_replace_all("-", "_"))
        }
        
        if(product_id %in% c("VNP46A3")){
          date_name_i <- paste0("t", date_i %>% str_replace_all("-", "_") %>% substring(1,7))
        }
        
        if(product_id %in% c("VNP46A4")){
          date_name_i <- paste0("t", date_i %>% str_replace_all("-", "_") %>% substring(1,4))
        }
        
        r <- bm_raster_i(roi_sf = roi_sf,
                         product_id = product_id,
                         date = date_i,
                         bearer = bearer,
                         variable = variable)
        
        names(r) <- date_name_i
        
        return(r)
        
      },
      error=function(e) {
        return(NULL)
      }
    )
    
  })
  
  # Clean output ---------------------------------------------------------------
  # Remove NULLs
  r_list <- r_list[!sapply(r_list,is.null)]
  
  if(length(r_list) == 1){
    r <- r_list[[1]]
  } else if (length(r_list) > 1){
    r <- stack(r_list)
  } else{
    r <- NULL
  }
  
  return(r)
}

bm_raster_i <- function(roi_sf,
                        product_id,
                        date,
                        bearer,
                        variable){
  
  # Black marble grid ----------------------------------------------------------
  grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/blackmarbletiles.geojson")
  
  # Prep dates -----------------------------------------------------------------
  ## For monthly, allow both yyyy-mm and yyyy-mm-dd (where -dd is ignored)
  if(product_id == "VNP46A3"){
    
    if(nchar(date) %in% 7){
      date <- paste0(date, "-01")
    }
    
  }
  
  ## For year, allow both yyyy and yyyy-mm-dd (where -mm-dd is ignored)
  if(product_id == "VNP46A4"){
    
    if(nchar(date) %in% 4){
      date <- paste0(date, "-01-01")
    }
    
  }
  
  # Grab tile dataframe --------------------------------------------------------
  #product_id <- "VNP46A4"
  #date <- "2021-10-15"
  
  year  <- date %>% year()
  month <- date %>% month()
  day   <- date %>% yday()
  
  bm_files_df <- create_dataset_name_df(product_id = product_id,
                                        all = T, 
                                        years = year,
                                        months = month,
                                        days = day)
  
  # Intersecting tiles ---------------------------------------------------------
  # Remove grid along edges, which causes st_intersects to fail
  grid_sf <- grid_sf[!(grid_sf$TileID %>% str_detect("h00")),]
  grid_sf <- grid_sf[!(grid_sf$TileID %>% str_detect("v00")),]
  
  inter <- st_intersects(grid_sf, roi_sf, sparse = F) %>% as.vector()
  grid_use_sf <- grid_sf[inter,]
  
  # Make Raster ----------------------------------------------------------------
  tile_ids_rx <- grid_use_sf$TileID %>% paste(collapse = "|")
  bm_files_df <- bm_files_df[bm_files_df$name %>% str_detect(tile_ids_rx),]
  
  temp_dir <- tempdir()
  
  unlink(file.path(temp_dir, product_id), recursive = T)
  
  r_list <- lapply(bm_files_df$name, function(name_i){
    download_raster(name_i, temp_dir, variable, bearer)
  })
  
  if(length(r_list) == 1){
    r <- r_list[[1]]
  } else{
    
    #r <- r_big_mosaic(r_list)
    
    #r_listr <<- r_list
    
    ## Mosaic rasters together
    names(r_list)    <- NULL
    r_list$fun       <- max
    
    r <- do.call(raster::mosaic, r_list) 
    
  }
  
  ## Crop
  r <- r %>% crop(roi_sf)
  
  unlink(file.path(temp_dir, product_id), recursive = T)
  
  return(r)
}


