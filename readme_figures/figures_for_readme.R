# Figures for readme

if(F){
  library(blackmarbler)
  library(readr)
  library(geodata)
  library(ggplot2)
  
  # Setup ------------------------------------------------------------------------
  bearer <- read_csv("~/Desktop/bearer_bm.csv") %>%
    pull(token)
  
  roi_sf <- gadm(country = "GHA", level=0, path = tempdir()) %>% st_as_sf()
  product_id <- "VNP46A3"
  year <- 2018
  month <- 5
  day <- 1
  
  # Make map ---------------------------------------------------------------------
  #### Make raster
  r <- bm_raster(roi_sf = roi_sf,
                 product_id = "VNP46A3",
                 date = "2021-10-01",
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
         filename = file.path("~/Documents/Github/blackmarbler/man/figures/ntl_gha.png"),
         height = 5, width = 6)
  
  # Extract timeseries -----------------------------------------------------------
  
  library(exactextractr)
  library(ggplot2)
  
  #### Polygons on Ghana
  # Load both country and admin 1 level. Country-level is needed as bm_raster() requires
  # a polygon that is just one row.
  gha_0_sf <- gadm(country = "GHA", level=0, path = tempdir()) %>% st_as_sf()
  gha_1_sf <- gadm(country = "GHA", level=1, path = tempdir()) %>% st_as_sf()
  
  r <- bm_raster(roi_sf = gha_0_sf,
                 product_id = "VNP46A4",
                 date = 2012:2021,
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
         filename = file.path("~/Documents/Github/blackmarbler/man/figures/ntl_trends_gha.png"),
         height = 5, width = 6)
  
}