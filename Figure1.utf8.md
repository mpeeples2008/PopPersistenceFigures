---
title: 'Figure 1: Urban Life Spans'
output:
  html_document:
    df_print: paged
---


```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.5.3
```

```
## -- Attaching packages ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.0.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.4.0
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```
## Warning: package 'tibble' was built under R version 3.5.3
```

```
## Warning: package 'tidyr' was built under R version 3.5.3
```

```
## Warning: package 'readr' was built under R version 3.5.3
```

```
## Warning: package 'purrr' was built under R version 3.5.3
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
```

```
## Warning: package 'stringr' was built under R version 3.5.3
```

```
## Warning: package 'forcats' was built under R version 3.5.3
```

```
## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(sf)
```

```
## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

```r
library(rnaturalearth)
```

```
## Warning: package 'rnaturalearth' was built under R version 3.5.3
```

```r
library(ggrepel)
```

```
## Warning: package 'ggrepel' was built under R version 3.5.3
```

```r
library(rworldmap)
```

```
## Warning: package 'rworldmap' was built under R version 3.5.3
```

```
## Loading required package: sp
```

```
## ### Welcome to rworldmap ###
```

```
## For a short introduction type : 	 vignette('rworldmap')
```

Import cite locations and clean the data.

```r
sites <- read_csv('Site_LatLong.csv') %>%
  mutate(site = if_else(Site == 'Tenochitlan/Mexico City', 'Tenochtitlan/Mexico City', Site)) %>%
  mutate(site = if_else(site == 'Elba', 'Ebla', site)) %>%
  mutate(site = if_else(site == 'Ur', 'Uruk', site)) %>%
  select(-Site) %>%
  add_row(site = 'Cahokia', Lat = 38.653889, Long = -90.064444) %>%
  add_row(site = 'San Juan', Lat = 18.406389, Long = -66.063889) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  filter(site != 'Pueblo Bonito')%>%
  mutate(site = if_else(str_detect(site, 'Timbukt'), 'Timbuktu', site)) %>%
  # remove extra sites
  filter(!(site %in% c('Scribe S', 'Sand Canyon Pueblo', 'Cuexcomate', 'Hawwiku', 'Walatowa', 'Byblos', 'Megiddo', 'Anyang', 'Xochicalco', 'Pueblo Bonito', 'Honey Bee Village', 'Calixtlahuaca', 'Tayasal', 'Santa Rita','Caracol')))
```

```
## Parsed with column specification:
## cols(
##   Site = col_character(),
##   Lat = col_double(),
##   Long = col_double()
## )
```

Prepare the world map.

```r
world_map <- st_as_sf(getMap(resolution = "low"))

crs_goode <- "+proj=igh"

lats <- c(
  90:-90, # right side down
  -90:0, 0:-90, # third cut bottom
  -90:0, 0:-90, # second cut bottom
  -90:0, 0:-90, # first cut bottom
  -90:90, # left side up
  90:0, 0:90, # cut top
  90 # close
)
longs <- c(
  rep(180, 181), # right side down
  rep(c(80.01, 79.99), each = 91), # third cut bottom
  rep(c(-19.99, -20.01), each = 91), # second cut bottom
  rep(c(-99.99, -100.01), each = 91), # first cut bottom
  rep(-180, 181), # left side up
  rep(c(-40.01, -39.99), each = 91), # cut top
  180 # close
)
goode_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )

goode_outline <- st_transform(goode_outline, crs = crs_goode)

# get the bounding box in transformed coordinates and expand by 10%
xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1

# turn into enclosing rectangle
goode_encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = crs_goode)

# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
goode_without <- st_difference(goode_encl_rect, goode_outline)
```

Plot the map and save it for patching later.

```r
a <- ggplot(world_map) + 
  geom_sf(fill = "lightgray", color = "gray", size = 0.5/.pt) +
  geom_sf(data = goode_without, fill = "white", color = "NA") +
  geom_sf(data = goode_outline, fill = NA, color = "gray", size = 0.5/.pt) +
  geom_sf(data = sites, color='black', size = 3/.pt) +
    geom_text_repel(data = sites, aes(geometry = geometry, label = site), stat = 'sf_coordinates', size = 3.25, force = 1.5) +
  coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
  cowplot::theme_minimal_grid() +
  labs(x = '', y = '') +
  theme(
    panel.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray", size = 0.25)
  ) 
```

Import city dates data and preprocess.

```r
new_dat <- read_csv('CityDatesDraft1 - Sheet1.csv') %>%
  mutate(start = pmin(`Start Occupation`, `Start Peak`, na.rm = TRUE),
         end = pmax(`End Occupation`, `End Peak`, na.rm = TRUE),
         site = `Site/City`) %>%
  # remove extra cities
  filter(!(site %in% c('Scribe S', 'Sand Canyon Pueblo', 'Cuexcomate', 'Hawwiku', 'Walatowa', 'Byblos', 'Megiddo', 'Anyang', 'Xochicalco', 'Pueblo Bonito', 'Honey Bee Village', 'Calixtlahuaca', 'Tayasal', 'Santa Rita', 'Caracol'))) %>%
  # add data for cities not in original file
  add_row(site = 'Mohenjo-daro', start = -2500, end = -1900) %>%
  add_row(site = 'Cahokia', start = 600, end = 1350) %>% # another start date given as 1050
  add_row(site = 'Moscow', start = 1147, end = 2020) %>%
  add_row(site= 'New York', start = 1624, end = 2020) %>%
  add_row(site = 'London', start = 47, end = 2020) %>%
  add_row(site = 'San Juan', start = 1521, end = 2020) %>%
  mutate(site = if_else(str_detect(site, 'Timbukt'), 'Timbuktu', site)) %>%
  mutate(end = if_else(end == 2000, 2020, end)) %>%
  mutate(end = if_else(site == 'Tiwanaku', 1100, end),
         start = if_else(site == 'Tiwanaku', 110, start),
         end = if_else(site == 'Teotihuacan', 2020, end))
```

```
## Warning: Missing column names filled in: 'X11' [11]
```

```
## Parsed with column specification:
## cols(
##   `Site/City` = col_character(),
##   Region = col_character(),
##   `Total Duration` = col_double(),
##   `Peak Duration` = col_double(),
##   `Start Occupation` = col_double(),
##   `End Occupation` = col_double(),
##   `Start Peak` = col_double(),
##   `End Peak` = col_double(),
##   Source = col_character(),
##   Notes = col_character(),
##   X11 = col_character()
## )
```

Plot the city spans and combine with the map using patchwork.

```r
b <- ggplot(new_dat, aes(y = reorder(site, -start))) +

  theme_minimal() +
  labs(x = 'Year', y = '') +
    scale_x_continuous(limits = c(-4600, 2020), breaks = seq(-6000,2000, 2000), labels = c('6000', '4000', '2000', 'BC/AD', '2000')) + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())


b + annotation_custom(ggplotGrob(a), xmin = -5500, xmax = 0, ymin = 'Kumasi', ymax = 'Copan') +   geom_linerange(aes(xmin = start, xmax = end), alpha = .4) +
  geom_point(data = new_dat, aes(x = start, y = site)) +
    geom_point(data = new_dat, aes(x = end, y = site)) +  geom_text(data = new_dat, aes(x = start, label = site), nudge_x = -100, hjust = 1, size = 3.5)
```

<img src="Figure1_files/figure-html/unnamed-chunk-5-1.png" width="1344" />

Save thee result.

```r
ggsave('figure1.png', height = 6, width = 14, dpi = 400)
ggsave('figure1.pdf', height = 6, width = 14)
```

