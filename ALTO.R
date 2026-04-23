#ALTO Assessment

library(naturecounts)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)


ON_kba <- st_read("Data/KBA_ON.shp")
ON_kba<-ON_kba %>% distinct(Name_EN,  .keep_all = TRUE)

ALTO<-st_read("Data/ALTO.shp")
Road<-st_read("Data/Road.shp") 
Road <- st_make_valid(Road)  

gdb_path <- "Data/PriorityPlaces.gdb"
gdb_layers <- st_layers(gdb_path)
print(gdb_layers)

priori_place_polygons <- st_read(dsn = gdb_path, layer = "PriorityPlacesBoundary")

# Make sure both layers use the same CRS
priori_place_polygons <- st_transform(priori_place_polygons, st_crs(ON_kba))
ALTO   <- st_transform(ALTO,   st_crs(ON_kba))
Road   <- st_transform(Road,   st_crs(ON_kba))

# Find which features intersect which
overlap_kba   <- st_intersection(ALTO, ON_kba)                # ALTO ∩ KBA 
overlap_pp    <- st_intersection(ALTO, priori_place_polygons) # ALTO ∩ Priority Places 

kba_in_ALTO <- st_filter(ON_kba, ALTO, .predicate = st_intersects)
pp_in_ALTO  <- st_filter(priori_place_polygons, ALTO, .predicate = st_intersects)

# Get Ontario boundary as sf
map <- rnaturalearth::ne_states(country = "Canada",
                                    returnclass = "sf") |> dplyr::filter(name == "Ontario"|name== "Québec")                       

# Match CRS to your layers
map <- st_transform(map, st_crs(ON_kba))  

#Bounding Box
alto_bbox <- st_bbox(ALTO)

xpad <- (alto_bbox["xmax"] - alto_bbox["xmin"]) * 0.05
ypad <- (alto_bbox["ymax"] - alto_bbox["ymin"]) * 0.05

xmin_pad <- as.numeric(alto_bbox["xmin"] - xpad)
xmax_pad <- as.numeric(alto_bbox["xmax"] + xpad)
ymin_pad <- as.numeric(alto_bbox["ymin"] - ypad)
ymax_pad <- as.numeric(alto_bbox["ymax"] + ypad)


# Clip/crop roads to that bbox
roads_clip <- st_crop(
  Road,
  xmin = xmin_pad,
  ymin = ymin_pad,
  xmax = xmax_pad,
  ymax = ymax_pad
)

road_filter <- roads_clip %>% 
  dplyr::filter(NAT_CLASS %in% c("Freeway"))


kba_labels <- st_point_on_surface(kba_in_ALTO)
pp_labels  <- st_point_on_surface(pp_in_ALTO)

ggplot() +
  geom_sf(data = map, fill = "grey95", color = "grey70") +
  geom_sf(data = ON_kba, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = ALTO, fill = NA, color = "blue", linewidth = 0.6) +
  geom_sf(data = kba_in_ALTO, fill = "red", color = "red", alpha = 0.25) +
  geom_sf(data = pp_in_ALTO, fill = "yellow", color = "goldenrod2", alpha = 0.25) +
  
  geom_sf_text(
    data = kba_labels,
    aes(label = Name_EN),
    color = "black",
    size = 4.5,
    nudge_x = xpad * 0.08,
    nudge_y = -ypad * 0.08,
    hjust = 0
  ) +
  
  geom_sf_text(
    data = pp_labels,
    aes(label = NameEN),
    color = "black",
    size = 4.5,
    nudge_x = xpad * 0.08,
    nudge_y = -ypad * 0.08,
    hjust = 0
  ) +
  
  coord_sf(
    xlim = c(alto_bbox["xmin"] - xpad * 1.2, alto_bbox["xmax"] + xpad * 1.2),
    ylim = c(alto_bbox["ymin"] - ypad * 1.2, alto_bbox["ymax"] + ypad * 1.2),
    expand = FALSE
  )+
  theme_classic()

#use nc_counts to identify relevant collections for data download
bird <-nc_count(username = "dethier", region = list(bbox = c(left = xmin_pad, bottom = ymin_pad, right = xmax_pad, top =  ymax_pad)), years = c(2010,2025), timeout = 9000)

list<-unique(bird$collection)

col_list <- c(
  "AWSGS", "EBIRD-CA-ON", "EBIRD-CA-QC", "EBIRD-CA-SENS",
  "MMPBIRDS", "ONATLAS3BE_DO", "ONATLAS3BE_SAR",
  "ONATLAS3PC", "ONATLAS3SS", "QCOWLS", "SWIFTWATCH"
)

col_list<-c("QCATLAS2BE_DO", "QCATLAS2BE_RAW",  "QCATLAS2BE_SUMM", "QCATLAS2PC","QCATLAS2RC")

bird_list <- vector("list", length(col_list))
names(bird_list) <- col_list

for (i in seq_along(col_list)) {
  coll <- col_list[i]
  
  message("Starting collection: ", coll)
  
  bird_list[[coll]] <- tryCatch(
    {
      nc_data_dl(
        username = "dethier",
        region = list(
          bbox = c(
            left   = xmin_pad,
            bottom = ymin_pad,
            right  = xmax_pad,
            top    = ymax_pad
          )
        ),
        years = c(2020, 2025), #turn this off for QC data dl
        collections = coll,
        info = paste("ALTO Data Download for", coll, "using Bounding Box, 5 years"),
        timeout = 9000, 
        warn = FALSE
      )
    },
    error = function(e) {
      message("ERROR in ", coll, ": ", conditionMessage(e))
      return(NULL)
    }
  )
}

bird_list_ok <- Filter(Negate(is.null), bird_list)
bird_data<- bind_rows(bird_list_ok, .id = "collection")
readr::write_csv(bird_data, "bird_data_combined.csv")

##Read in the clean data
data<-read.csv("bird_data_combined_ONQC.csv")
dat<-data %>% select(collection, species_id, CommonName, statprov_code, SurveyAreaIdentifier, latitude, longitude, bcr, utm_square, survey_year, survey_month, survey_day)

#Make sure dat is an sf object
dat_sf <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"),  # use your exact column names
  crs = 4326                            # WGS84 lon/lat
)   
#Align coordinate reference systems
dat_sf  <- st_transform(dat_sf,  st_crs(ALTO))

#Clip (keep only features of dat inside ALTO)
dat_clip <- st_intersection(dat_sf, ALTO)  # strict intersection 
dat_clip<-dat_clip %>% filter(!(is.na(CommonName)))

#To assign species at risk
groups<-nc_query_table("Groups", username = "dethier")
groups<-groups %>% filter(category %in% c("cosewic", "iucn", "sara_status"))
groups<-groups %>% filter(popType==1)

sar<-dat_clip %>% select(CommonName, species_id, Id, gridcode, StudyArea, geometry) %>% distinct()
sar1 <- dplyr::left_join(groups, sar, by = c("speciesID" = "species_id"))

IUCN<-sar1 %>% filter(category == "iucn", groupName != "Least Concern") %>%
  select(groupName, speciesID, category, CommonName, Id, gridcode, StudyArea, geometry) %>% 
  filter(!is.na(StudyArea)) %>% 
  distinct()

IUCN_species <- IUCN %>%
  distinct(StudyArea, speciesID, CommonName, groupName)

IUCN_counts <- IUCN_species %>%
  group_by(StudyArea, groupName) %>%
  summarise(n_species = n(), .groups = "drop")

IUCN_nested <- IUCN_species %>%
  group_by(StudyArea) %>%
  summarise(
    species_table = list(
      select(cur_data(), speciesID, CommonName, groupName)
    ),
    n_species = n(),
    .groups = "drop"
  )

##SARA### HERE
SARA<-sar1 %>% filter(category == "sara_status", groupName %in% c("Threatened", "Endangered")) %>%
  select(groupName, speciesID, category, CommonName, Id, gridcode, StudyArea, geometry) %>% 
  filter(!is.na(StudyArea)) %>% 
  distinct()

SARA_species <- SARA %>%
  distinct(StudyArea, speciesID, CommonName, groupName)

SARA_counts <- SARA_species %>%
  group_by(StudyArea, groupName) %>%
  summarise(n_species = n(), .groups = "drop")

SARA_nested <- SARA_species %>%
  group_by(StudyArea) %>%
  summarise(
    species_table = list(
      select(cur_data(), speciesID, CommonName, groupName)
    ),
    n_species = n(),
    .groups = "drop"
  )

#Mapping SAR

# 1. Reproject to a projected CRS in metres
# Replace 3347 with a CRS appropriate for your study region if needed
SARA <- st_as_sf(SARA)
SARA_proj <- st_transform(SARA, 3347)

# 2. Create a grid over the study extent
grid <- st_make_grid(SARA_proj, cellsize = 5000, what = "polygons") |>
  st_as_sf() |>
  mutate(cell_id = row_number())

# 3A. Count ALL records in each grid cell
ix <- st_intersects(grid, SARA_proj)
grid$n_records <- lengths(ix)

# 3B. Or count DISTINCT species in each grid cell
grid$n_species <- sapply(ix, function(i) n_distinct(SARA_proj$speciesID[i]))

# 4. Keep only cells with at least 1 record if you want
grid_nonzero <- grid |> filter(n_species > 0)

# 5. Plot
ggplot() +
  geom_sf(data = map, fill = "grey95", color = "grey70") +
   geom_sf(data = grid_nonzero, aes(fill = n_species), color = NA) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    trans = "sqrt",
    name = "#SARA"
  ) +
  
    geom_sf(data = ALTO, fill = NA, color = "blue", linewidth = 0.4) +
  labs(fill = "Number of Species at Risk",
       title = "Avian Species at Risk Hotspot Map")+
  coord_sf(
    xlim = c(alto_bbox["xmin"] - xpad * 1.2, alto_bbox["xmax"] + xpad * 1.2),
    ylim = c(alto_bbox["ymin"] - ypad * 1.2, alto_bbox["ymax"] + ypad * 1.2),
    expand = FALSE
  )+
  theme_classic()
