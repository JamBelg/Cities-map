rm(list=ls())
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)


city_coords <- getbb("Bern Switzerland")


# Pull the data from OpenStreetMap
limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

highway <- opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary")) %>%
  osmdata_sf(quiet = FALSE)

streets <- opq(bbox = limits)%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- opq(bbox = limits)%>%
  add_osm_feature(key = "waterway", value = c("canal", "river", "riverbank")) %>%
  osmdata_sf()
lake <- opq(bbox = limits) %>%
  add_osm_feature(key="water", value=c("lake", "river", "canal")) %>%
  osmdata_sf()
water <-opq(bbox = limits) %>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

rail <- opq(bbox = limits) %>%
  add_osm_feature(key = "railway", value = c("monorail", "light_rail", "rail", "subway", "tram")) %>%
  osmdata_sf()
# Plot
ggplot() +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "#8f6033",
          size = 3,
          alpha = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .6) +
  geom_sf(data = rail$osm_lines,
          inherit.aes = FALSE,
          color ="#A8391D",
          size = .2,
          alpha = .6) +
  # geom_sf(data = river$osm_polygons,
  #         inherit.aes = FALSE,
  #         color = "#3C6594",
  #         fill = "#3C6594",
  #         size = .2,
  #         alpha = .5) +
  # geom_sf(data = lake$osm_multipolygons,
  #         inherit.aes = FALSE,
  #         color = "#3C6594",
  #         fill = "#3C6594",
  #         size = .2,
  #         alpha = .5) +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "#3C6594",
          color = "#3C6594",
          size = .2,
          alpha = .5) +
  
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = TRUE) +
  theme_void() +
  labs(title = "Bern",
       subtitle = paste0(round(mean(city_coords[1,]),2),"°N / ",round(mean(city_coords[2,])),"°E"),
       caption = "@ Jamel Belgacem") +
  theme(
    plot.background = element_rect(fill = "#282828"),
    plot.title= element_text(size=24, hjust=.5,
                             color="White",
                             face="bold"),
    plot.caption= element_text(size=8,
                               color="White",
                               face="plain",
                               hjust=0.01),
    plot.subtitle = element_text(size = 8, hjust=.5,
                                 color="White",
                                 margin=margin(2, 0, 5, 0))
  )

ggsave("Cities map/Bern_map/plot.png", width = 8, height = 8)
