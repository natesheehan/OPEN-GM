
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf") %>% rename(iso_code = iso_a3) %>% select(iso_code,geometry)
class(world)

lat_df = main_df %>% filter(wy == "22/39")

map = right_join(world,lat_df)

ggplot(data = map) +
  geom_sf(aes(fill = GISAID.total.submissions)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")  ggraph(desire_lines, layout = "stress") +
  geom_edge_link() +
  geom_node_point(
    aes(filter = centrality_degree() > 2,
        colour = centrality_power()),
    size = 4
  ) + geom_sf()

sf::sf_use_s2(FALSE)
world_cent = world %>% select(iso_code, geometry) %>% st_centroid(geometry)

count = split_author_matrix()
count$count_1 = countrycode(count$datum1, origin = 'country.name', destination = 'iso3c')
count$count_2 = countrycode(count$datum2, origin = 'country.name', destination = 'iso3c')
count = count %>% select(count_1,count_2,weight)

desire_lines = od::od_to_sf(count,world_cent)

map = st_as_sf(map)
d = st_as_sf(desire_lines)

#world = st_transform(world, crs = "+proj=moll")
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(world) + tm_borders() +
  tm_shape(d) +
  tm_lines(
    palette = "plasma",
    lwd = "weight",
    scale = 12,
    title.lwd = "Number of publications",
    alpha = 0.9,
    col = "weight",
    title = "Global collaboration network GISAID",
    legend.lwd.show = FALSE
  ) +
  tm_scale_bar() +
  tm_layout(legend.bg.alpha = 0.5,
            legend.bg.color = "white") +
  tm_layout(legend.outside = FALSE) +  tmap_style("classic")
