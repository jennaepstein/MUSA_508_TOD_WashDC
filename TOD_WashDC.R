# SET UP
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(tigris)

options(scipen=999)
options(tigris_class = "sf")

# add census API key - make sure to replace this with your own API key!

census_api_key("41e1c0d912341017fa6f36a5da061d3b23de335e", overwrite = TRUE)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Here's a palette of two colors for TOD and Non-TOD, similar to what we used in lab.
paletteTOD <- c("#fdb863", "#80cdc1")


# Downloading & wrangling the Census data

# Creating a vector of census variables, since we have several.

acs_vars_DC <- c("B25026_001E", # Estimate!!Total population in occupied housing units
                "B02001_002E", # People describing themselves as "white alone"
                "B02001_003E", # People describing themselves as "black" or "african-american" alone
                "B15001_050E", # Females with bachelors degrees
                "B15001_009E", # Males with bachelors degrees
                "B19013_001E", # Median HH income
                "B25058_001E", # Median rent
                "B06012_002E") # Total poverty


# ---- Washington, DC - Census Data - 2009 -----
tracts2009 <- 
  get_acs(geography = "tract", 
          variables = acs_vars_DC, 
          year=2009, 
          state=11, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102685') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBlack = ifelse(TotalPop > 0, Blacks / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty)


# ---- Washington, DC - Census Data - 2017 -----

tracts2017 <- 
  get_acs(geography = "tract", 
          variables = acs_vars_DC, 
          year=2017, 
          state=11, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102685') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBlack = ifelse(TotalPop > 0, Blacks / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

# --- Combining 2009 and 2017 data ----

allTracts <- rbind(tracts2009,tracts2017)

# ---- Wrangling Transit Open Data -----

wmataStops <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/54018b7f06b943f2af278bbe415df1de_52.geojson") %>% 
      select(NAME, LINE)) %>%
  st_transform(st_crs(tracts2009))
  

wmataStops <- wmataStops %>%
  mutate(LINE = strsplit(as.character(LINE), ",")) %>%
  unnest(LINE) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(LINE = replace(LINE,LINE == "slvr", "silver"))%>%
  mutate(LINE = replace(LINE,LINE == "yllw", "yellow"))%>%
  mutate(LINE = replace(LINE,LINE == "grn", "green"))
wmataStops

# Visualize it

ggplot() + 
  geom_sf(data=st_union(tracts2009)) +
  geom_sf(data=wmataStops, 
          aes(colour = LINE),
          show.legend = "point", size= 2, alpha = 0.5) +
  scale_colour_manual(values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue", "silver" = "gray")) +
  labs(title="WMATA Stops", 
       subtitle="Washington, DC", 
       caption="Source: OpenDataDC") +
  mapTheme()

# Bringing in Lines, to use later for reference
wmataLines <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/a29b9dbb2f00459db2b0c3c56faca297_106.geojson") %>%
      select(NAME)) %>%
    st_transform(st_crs(tracts2009))
wmataLines


# --- Relating WMATA Stops and Tracts ----

# Create buffers (in feet - note the CRS) around WMATA stops -
# Both a buffer for each stop, and a union of the buffers, and then bind these objects together

wmataBuffers <- 
  rbind(
    st_buffer(wmataStops, 2640) %>% # projection is in feet
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(wmataStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# A small multiple "facet_wrap" plot showing both buffers

ggplot() +
  geom_sf(data=wmataBuffers) +
  geom_sf(data=wmataStops, show.legend = "point") +
  facet_wrap(~Legend) +
  mapTheme()

# ---- Spatial operations ----

# sf object with ONLY the unioned buffer
buffer <- filter(wmataBuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts by seeing which tracts intersect (st_intersection) with the buffer and clipping out only those areas
clip <- 
  st_intersection(buffer, tracts2009) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Spatial selection to see which tracts touch the buffer
selection <- 
  tracts2009[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Centroid-in-polygon join to see which tracts have their centroid in the buffer
selectCentroids <-
  st_centroid(tracts2009)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts2009, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# Examining the three different spatial selection types to further illustrate why select by centroids is best
# Using Total Population as the fill

allTracts.group.TotalPop <- 
  rbind(clip, selection, selectCentroids)
    
SpatialSelectionTypes <-
      ggplot(allTracts.group.TotalPop)+
      geom_sf(data = st_union(tracts2009))+
      geom_sf(aes(fill = TotalPop)) +
      labs(title = "Total Population within 1/2 mi of a WMATA station, 2009", subtitle = "Three types of spatial selection") +
      facet_wrap(~Selection_Type) +
      mapTheme() + 
      theme(plot.title = element_text(size=20))
SpatialSelectionTypes

# ---- INDICATOR MAPS ---- 

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't* join, and add them all together.
# Do this operation and then examine it.
# What represents the joins/doesn't join dichotomy?
# Note that this contains a correct 2009-2017 inflation calculation

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent)) 

# Time/Space Groups

TimeSpaceGrps <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  scale_fill_manual(values = paletteTOD)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
TimeSpaceGrps

# Median Rent

MedRent <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(MedRent.inf)), color = NA) +
  geom_sf(data = buffer, fill = "transparent",color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($)\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
MedRent
  
MedRentWmataLines <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(MedRent.inf)), color = NA) +
  geom_sf(data = buffer, fill = "transparent",color = "red")+
  geom_sf(data = wmataLines, color = "black")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($)\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
MedRentWmataLines
  

# --- TOD INDICATOR TABLES ---- if we end up choosing other variables, we can just swap things out

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Black = mean(pctBlack, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table caption")

# Let's make some comparisons and speculate about the willingness to pay and demographics in these areas 2009-2017 (see the 2000 data in the text too)

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Summary for All Tracts in Washington, DC")

# --- TOD INDICATOR PLOTS ------ if we end up choosing other variables, we can just swap things out

# Creating small multiple plots using the "gather" command to go from wide to long

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=3) +
  scale_fill_manual(values = paletteTOD) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

########################
# TASK: Create two graduated symbol maps of population and rent within 1/2 mi of each transit station. Google for more information, but a graduate symbol map represents quantities for each transit station proportionally.

# Getting centroids for tracts and adding them to dataframe
DC_tract_centroids <- sf::st_centroid(selectCentroids)

DC_tract_centroids <- DC_tract_centroids %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
DC_tract_centroids


# GRADUATED SYMBOL MAPS
# Population - likely need to adjust the breaks in the scale. Hard to tell difference between population over the two years also. Need to include circles for metro stops in legend.
PopulationSymbolMap <-
  ggplot(DC_tract_centroids) + 
    geom_sf()+
    geom_sf(data=allTracts)+
      aes()+
    geom_point(aes(x=lat, y=lon, size = TotalPop), data = DC_tract_centroids, color="blue", alpha=0.5) +
  facet_wrap(~year) +    
  scale_size_area() +
      geom_sf(data=wmataStops, size=1, color="black") + 
        aes() +
  labs(title="Population in Census Tracts within 1/2 mi. of WMATA Stops", 
     subtitle="Washington, DC", 
     caption="Data: US Census Bureau; opendata.dc.gov") +
  mapTheme()
PopulationSymbolMap

  
#MedRent - needs some styling help, also need the legend for medrent fixed (MedRent.inf wasn't working. and need to include metro stops circle in legend.
MedRentSymbolMap <-
    ggplot(DC_tract_centroids) + 
    geom_sf()+
    geom_sf(data = allTracts,
            aes(fill = q5(MedRent)), color = NA) +
              scale_fill_manual(values = palette5) +
    facet_wrap(~year)+
    geom_sf(data=wmataStops, size=1, color="black")+
    aes() +
    labs(title="Median Rent in Census Tracts within 1/2 mi. of WMATA Stops", 
         subtitle="Washington, DC", 
         caption="Source: OpenDataDC") +
    mapTheme()
MedRentSymbolMap


