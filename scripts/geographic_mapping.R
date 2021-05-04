library("tidyverse")
library("readxl")
library("here")
library("sf")
library("rgeos")
library("ggmap")

UK <- st_read(here::here("gov_data", "GB_Postcodes", "PostalArea.shp"))

data <- read_rds(here::here("clean_data", "cleaned_data.rds")) %>%
  mutate(postcode = as.character(plyr::revalue(postcode, c("America" = NA, "Australia" = NA, "Canada" = NA,
                                                           "Portugal" = NA, "Sweden" = NA, "ZZ99 3" = NA))),
         region_postcode = substr(postcode, 1, nchar(postcode)),
         region_postcode = str_replace_all(region_postcode, "[:space:]", "")) %>%
  filter(postcode != is.na(postcode))  %>%
  group_by(region_postcode, cohort) %>%
  summarise(count = n()) %>%
  ungroup(region_postcode) %>%
  mutate(freq = count/sum(count)) %>%
  group_by(region_postcode)

levels(data$cohort) <-  c("Non-SARS-CoV-2 (n = 211)", "SARS-CoV-2 (n = 313)")

postcodes <- unique(data$region_postcode)

all <- st_read(here::here("gov_data", "GB_Postcodes", "PostalSector.shp"))

test <- all
test$RMSect <- str_replace(test$RMSect, "[:space:]", "")
test <- test %>%
  filter(RMSect %in% postcodes)

minx <- st_bbox(test %>%
                  filter(Sprawl == "London"))[1]
maxx <- st_bbox(test %>%
                  filter(Sprawl == "London"))[3]
miny <- st_bbox(test %>%
                  filter(Sprawl == "London"))[2]
maxy <- st_bbox(test %>%
                  filter(Sprawl == "London"))[4]
test <- left_join(test %>%
                    rename("postcode" = RMSect),
                  data %>%
                    rename("postcode" = region_postcode),
                  by = "postcode")

hospital <- test %>%
  filter(RefPC == "NW12LW") %>%
  as_tibble() %>%
  select(x, y) %>%
  distinct()

outside_london <- test %>%
  as_tibble() %>%
  filter(Sprawl != "London") %>%
  group_by(cohort) %>%
  summarise(sum = sum(count))

map_participants <- ggplot()+
  geom_sf(data = UK) +
  geom_sf(data = test, aes(fill = freq), inherit.aes = F) +
  geom_point(data = hospital, aes(x = x, y = y), size = 1, colour = "red", inherit.aes = F) +
  coord_sf(xlim = c(minx, maxx),
           ylim = c(miny, maxy)) +
  facet_wrap(~ cohort) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral",
                       guide = guide_colourbar(barwidth = 10, direction = "horizontal")) +
  labs(title = "Permanent address of cases and controls in London",
       fill = "Proportion of individuals")

ggsave("map_of_participants.png", plot = map_participants, path = here("output"), dpi = 320)

##Higher level
postal_district <- st_read(here::here("gov_data", "GB_Postcodes", "PostalDistrict.shp"))

higher_level <- data %>%
  mutate(postal_district = substr(region_postcode, 1, nchar(region_postcode)-1)) %>%
  select(-region_postcode) %>%
  group_by(postal_district, cohort) %>%
  summarise(count = sum(count),
            freq = sum(freq))

postal_district <- postal_district
postal_district$PostDist <- str_replace(postal_district$PostDist, "[:space:]", "")
postal_district <- postal_district %>%
  filter(PostDist %in% higher_level$postal_district)

higher_level <- left_join(postal_district %>%
                            rename("postcode" = PostDist),
                          higher_level %>%
                            rename("postcode" = postal_district),
                          by = "postcode")

hl_map <- ggplot()+
  geom_sf(data = UK) +
  geom_sf(data = higher_level, aes(fill = freq), inherit.aes = F) +
  geom_point(data = hospital, aes(x = x, y = y), size = 1, colour = "black", inherit.aes = F) +
  coord_sf(xlim = c(minx, maxx),
           ylim = c(miny, maxy)) +
  facet_wrap(~ cohort) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral",
                       guide = guide_colourbar(barwidth = 10, direction = "horizontal")) +
  labs(title = "Postal address of cases and controls in London",
       fill = "Proportion of individuals")

ggsave("higher_level_map.png", plot = hl_map, path = here("output"), dpi = 320)
