# Figures for readme

library(ggplot2)
source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")

# Setup ------------------------------------------------------------------------
bearer <- "HERE"

roi_sf <- getData('GADM', country='LBN', level=0) %>% st_as_sf()
product_id <- "VNP46A3"
year <- 2018
month <- 5
day <- 1

# Testing ----------------------------------------------------------------------
roi_sf <- getData('GADM', country='VUT', level=0) %>% st_as_sf()

r2020 <- bm_raster(roi_sf = roi_sf,
                   product_id = "VNP46A4",
                   date = 2020,
                   bearer = bearer)

writeRaster(r2020, "~/Desktop/vut/vut_2020a.tif")





r2021 <- bm_raster(roi_sf = roi_sf,
                   product_id = "VNP46A4",
                   date = 2021,
                   bearer = bearer)

writeRaster(r2021, "~/Desktop/vut/vut_2021.tif")





r <- bm_raster(roi_sf = roi_sf,
               product_id = "VNP46A4",
               date = 2012:2021,
               bearer = bearer)

writeRaster(r, "~/Desktop/vut/vut_2012_2021.tif")


a <- brick("~/Desktop/vut/vut_2012_2021.tif")

exact_extract(a, roi_sf, "mean")


r_monthly <- bm_raster(roi_sf = roi_sf,
                       product_id = "VNP46A1",
                       date = "2021-01-01",
                       bearer = bearer)

r_monthly <- bm_raster(roi_sf = roi_sf,
                       product_id = "VNP46A3",
                       date = seq.Date(from = ymd("2021-01-01"), to = ymd("2021-02-01"), by = "month"),
                       bearer = bearer)

month = c("2021-10-01", "9999", "2021-11-01")

if(product_id == "VNP46A3"){
  # TRYCATCH
  r_list <- lapply(month, function(month_i){
    
    r <- bm_raster(roi_sf = roi_sf,
                   product_id = "VNP46A3",
                   month = month_i,
                   bearer = bearer)
    
    names(r) <- paste0("t", month_i %>% str_replace_all("-", "_") %>% substring(1,7))
    
    return(r)
  })
}

ss <- stack(r_list)

writeRaster(ss, "~/Desktop/temp.tif")

a <- stack("~/Desktop/temp.tif")

names(ss) <- paste0("month_", month %>% str_replace_all("-", "_"))

# Map --------------------------------------------------------------------------
r <- bm_raster(roi_sf = roi_sf,
               product_id = "VNP46A3",
               month = "2021-10-01",
               bearer = bearer)

# Make map ---------------------------------------------------------------------
#### Make raster
r <- bm_raster(roi_sf = roi_sf,
               product_id = "VNP46A3",
               month = "2021-10-01",
               bearer = bearer)

#### Prep data
r <- r %>% mask(roi_sf) 

r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
names(r_df) <- c("value", "x", "y")

## Transform NTL
r_df$value[r_df$value <= 1] <- 0

r_df$value_adj <- log(r_df$value+1)

##### Map 
p <- ggplot() +
  geom_raster(data = r_df, 
              aes(x = x, y = y, 
                  fill = value_adj)) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5) +
  labs(title = "Nighttime Lights: October 2021") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggsave(p,
       filename = file.path("~/Documents/Github/download_blackmarble/man/figures/ntl_gha.png"),
       height = 5, width = 6)

# Extract timeseries -----------------------------------------------------------

library(exactextractr)
library(ggplot2)

#### Polygons on Ghana
# Load both country and admin 1 level. Country-level is needed as bm_raster() requires
# a polygon that is just one row.
gha_0_sf <- getData('GADM', country='LBN', level=0) %>% st_as_sf()
gha_1_sf <- getData('GADM', country='LBN', level=1) %>% st_as_sf()

r <- bm_raster(roi_sf = gha_0_sf,
               product_id = "VNP46A4",
               date = 2012:2013,
               bearer = bearer)

ntl_df <- exact_extract(r, gha_1_sf, 'mean', progress = FALSE)
ntl_df$NAME_1 <- gha_1_sf$NAME_1

ntl_df %>%
  pivot_longer(cols = -NAME_1) %>%
  mutate(year = name %>% str_replace_all("mean.t", "") %>% as.numeric()) %>%
  ggplot() +
  geom_col(aes(x = year,
               y = value),
           fill = "darkorange") +
  facet_wrap(~NAME_1) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Ghana Admin Level 1: Annual Average Nighttime Lights") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) 

ggsave(p,
       filename = file.path("~/Documents/Github/download_blackmarble/man/figures/ntl_trends_gha.png"),
       height = 5, width = 6)

ntl_df <- exact_extract(r, gha_1_sf, 'mean', progress = FALSE) %>%
  as.data.frame()


r

gha_ntl_df <- map_df(2012:2021, function(year){
  
  r <- bm_raster(roi_sf = gha_0_sf,
                 product_id = "VNP46A4",
                 year = year,
                 bearer = bearer)
  
  gha_1_sf$ntl <- exact_extract(r, gha_1_sf, 'mean', progress = FALSE)
  gha_1_sf$year <- year
  gha_1_sf$geometry <- NULL # We only want data; remove geometry to make object smaller
  
  return(gha_1_sf)
})

p <- gha_ntl_df %>%
  ggplot() +
  geom_col(aes(x = year,
               y = ntl),
           fill = "darkorange") +
  facet_wrap(~NAME_1) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Ghana Admin Level 1: Annual Average Nighttime Lights") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) 

ggsave(p,
       filename = file.path("~/Documents/Github/download_blackmarble/man/figures/ntl_trends_gha.png"),
       height = 5, width = 6)




