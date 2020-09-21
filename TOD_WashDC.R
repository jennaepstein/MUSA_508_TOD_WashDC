# SET UP --------------------------------------------------------------------
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


# TASK 1: DATA WRANGLING -------------------------------------------------------

# Creating a vector of census variables, since we have several.

acs_vars_DC <- c("B02001_001E", # Estimate!!Total population by race
                "B02001_002E", # People describing themselves as "white alone"
                "B02001_003E", # People describing themselves as "black" or "african-american" alone
                "B15001_050E", # Females with bachelors degrees
                "B15001_009E", # Males with bachelors degrees
                "B19013_001E", # Median HH income
                "B25058_001E", # Median rent
                "B06012_002E", # Total poverty
                "B08301_001E", # People who have means of transportation to work
                "B08301_002E", # Total people who commute by car, truck, or van
                "B08301_010E", # Total people who commute by public transportation"
                "B03002_012E", # Estimate Total Hispanic or Latino by race
                "B19326_001E") # Median income in past 12 months (inflation-adjusted)


# ---- Washington, DC - Census Data - 2009 ----
tracts2009 <- 
  get_acs(geography = "tract", 
          variables = acs_vars_DC, 
          year=2009, 
          state=11, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102685') %>% # using the state plane of Maryland for a projection
  rename(TotalPop = B02001_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E,
         TotalCommute = B08301_001E,
         CarCommute = B08301_002E,
         PubCommute = B08301_010E,
         TotalHispanic = B03002_012E,
         MedInc = B19326_001E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBlack = ifelse(TotalPop > 0, Blacks / TotalPop,0),
         pctHis = ifelse(TotalPop >0, TotalHispanic/TotalPop,0),
         pctBlackorHis = ifelse (TotalPop>0, (Blacks+TotalHispanic)/TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         pctCarCommute = ifelse(TotalCommute > 0, CarCommute / TotalCommute,0),
         pctPubCommute = ifelse(TotalCommute > 0, PubCommute / TotalCommute,0),
         year = "2009") %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -CarCommute, -PubCommute, -TotalCommute, -TotalHispanic)

# ---- Washington, DC - Census Data - 2017 ----

tracts2017 <- 
  get_acs(geography = "tract", 
          variables = acs_vars_DC, 
          year=2017, 
          state=11, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102685') %>%
  rename(TotalPop = B02001_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E,
         TotalCommute = B08301_001E,
         CarCommute = B08301_002E,
         PubCommute = B08301_010E,
         TotalHispanic = B03002_012E,
         MedInc = B19326_001E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBlack = ifelse(TotalPop > 0, Blacks / TotalPop,0),
         pctHis = ifelse(TotalPop >0, TotalHispanic/TotalPop,0),
         pctBlackorHis = ifelse (TotalPop>0, (Blacks+TotalHispanic)/TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         pctCarCommute = ifelse(TotalCommute > 0, CarCommute / TotalCommute,0),
         pctPubCommute = ifelse(TotalCommute > 0, PubCommute / TotalCommute,0),
         year = "2017") %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -CarCommute, -PubCommute, -TotalCommute,-TotalHispanic)

# ---- Combining 2009 and 2017 data ----

allTracts <- rbind(tracts2009,tracts2017)

# ---- Wrangling Transit Open Data ----

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

# ---- Visualize wmata stops ----

ggplot() + 
  geom_sf(data=st_union(tracts2009)) +
  geom_sf(data=wmataStops, 
          aes(colour = LINE),
          show.legend = "point", size= 2, alpha = 0.5) +
  scale_colour_manual(values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue", "silver" = "gray")) +
  labs(title="WMATA Stops", 
       subtitle="Washington, DC", 
       caption="Source: opendata.dc.gov") +
  mapTheme()

# ---- Bringing in wmata Line data ----
wmataLines <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/a29b9dbb2f00459db2b0c3c56faca297_106.geojson") %>%
      select(NAME)) %>%
    st_transform(st_crs(tracts2009))
wmataLines


# --- Relating WMATA Stops and Tracts ----

# Creating buffers (in feet - note the CRS) around WMATA stops
# Both a buffer for each stop, and a union of the buffers, and then bind these objects together

wmataBuffers <- 
  rbind(
    st_buffer(wmataStops, 2640) %>% # projection is in feet
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(wmataStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# ---- Small multiple facet_wrap plot showing both buffers ----

ggplot() +
  geom_sf(data=wmataBuffers) +
  geom_sf(data=wmataStops, show.legend = "point") +
  facet_wrap(~Legend) +
  mapTheme()

# ---- Spatial operations ----

# sf object with ONLY the unioned buffer
buffer <- filter(wmataBuffers, Legend=="Unioned Buffer")

# Showing that we examine the three different spatial selection types 
# to further illustrate why the select by centroids method is best
# based on tracts2009

# Clip
clip <- 
  st_intersection(buffer, tracts2009) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Spatial selection
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


# Visualizing using Total Population as the fill

allTracts.group.TotalPop <- 
  rbind(clip, selection, selectCentroids)
    
SpatialSelectionTypes2009tracts <-
      ggplot(allTracts.group.TotalPop)+
      geom_sf(data = st_union(tracts2009))+
      geom_sf(aes(fill = TotalPop)) +
      labs(title = "Total Population within 1/2 mi of a WMATA station, 2009", subtitle = "Three types of spatial selection") +
      facet_wrap(~Selection_Type) +
      mapTheme() + 
      theme(plot.title = element_text(size=20))
SpatialSelectionTypes2009tracts

# Centroid joins as above, then "disjoin" to get the ones that *don't* join
# Then  add them all together
# Contains a correct 2009-2017 inflation calculation

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

# Visualize Time/Space Groups

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

# TASK 2 -----------------------------------------------------------------------
# Four small-multiple (2009 & 2017) visualizations comparing four selected Census variables across time

# TASK 2, PART A: MAPPING MEDIAN RENT
## NEED TO DO: change NA color/texture only

MedRent <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(MedRent.inf)), color = NA) +
  geom_sf(data = buffer, fill = "transparent",color = "orange", size = 1.5)+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($)\n(Quintile Breaks)") + 
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
MedRent

# USE THIS ONE for 2a, IT HAS WMATA LINES - LOOKS CLEANER
MedRentWmataLines <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(MedRent.inf)), color = NA) +
  geom_sf(data = buffer, fill = "transparent",color = "orange", size = 1.5)+
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($)\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
MedRentWmataLines

# TASK 2, PART B: MAPPING [PERCENT WHITE]

# TASK 2, PART C: MAPPING [VARIABLE]

# TASK 2, PART D: MAPPING [PUB COMMUTE] -- what is going on with the legend?! qbr again
pctPubCommuteWmataLines <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(pctPubCommute)), color = NA) +
  geom_sf(data = buffer, fill = "transparent",color = "red", size = 1.5)+
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctPubCommute"),
                    name = "Pct Public Transportation Commuters \n(Quintile Breaks)") +
  labs(title = "Percent of Commuters using Public Transportation, 2009-2017", subtitle = "Red border denotes areas close to WMATA stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
pctPubCommuteWmataLines

# TASKS 3 & 4: One grouped bar plot making these same comparisons
## NEED TO DO - swap out variables in TOD indicator group bar plots and tables once we determine final variables we are using

# TASK 4: Creating table -------------------------------------------------------
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Black = mean(pctBlack, na.rm = T),
            Percent_HispanicLatino = mean(pctHis, na.rm=T),
            Percent_BlackorHispanic = mean (pctBlackorHis, na.rom=T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T),
            Percent_CarCommute = mean(pctCarCommute, na.rm = T),
            Percent_PubCommute = mean(pctPubCommute, na.rm = T))
            
kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table caption")

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Summary for All Tracts in Washington, DC")

# TASK 3: Group bar plot making these same comparisons for TOD indicators ------
allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=3) +
  scale_fill_manual(values = paletteTOD) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

# TASK 5: Graduated symbol maps ------------------------------------------------

# More wrangling to get centroids for all tracts, both years, filtering by TOD

allTracts.group.TODonly <-
  filter(allTracts.group, TOD =="TOD")

allTracts.group.TODonly.centroids <- sf::st_centroid(allTracts.group.TODonly) %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
  
allTracts.group.TODonly.centroids

# TASK 5, PART A: Graduated symbol map of population within 1/2 mi of each wmata station
## Legend work needed - adjust breaks in population scale; add metro line. and do we want to include stations?
 
PopulationSymbolMap <-
  ggplot ()+
  geom_sf(data=allTracts.group, color="white", fill="gray")+
  geom_point(aes(x=lat, y=lon, size = TotalPop), data = allTracts.group.TODonly.centroids, color="blue", alpha=0.5) +
  facet_wrap(~year) +    
  scale_size_area() +
  geom_sf(data=wmataLines, size=1, color="black") + 
  aes() +
  geom_sf(data=wmataStops, size=1.75, shape=21, color="black", fill="yellow") + 
  aes() +
  labs(title="Population in Census Tracts within 1/2 mi. of WMATA Stops", 
       subtitle="Washington, DC", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  mapTheme()
PopulationSymbolMap
  

# TASK 5, PART B: Graduated symbol map of rent within 1/2 mi of each wmata station
##  Legend work still to do: fix breaks; include metro stops circle
MedRentSymbolMap <-
    ggplot(allTracts.group.TODonly) + 
    geom_sf()+
    geom_sf(data = allTracts,
            aes(fill = q5(MedRent)), color = NA) +
              scale_fill_manual(values = palette5) +
    facet_wrap(~year)+
    geom_sf(data=wmataStops, size=1.75, shape=21,color="black", fill="yellow")+
    aes() +
    labs(title="Median Rent in Census Tracts within 1/2 mi. of WMATA Stops", 
         subtitle="Washington, DC", 
         caption="Data: US Census Bureau; opendata.dc.gov") +
    mapTheme()
MedRentSymbolMap

# CRIME DATA ---------------------------

# Pulling in DC crime data in 2009 and 2017

# 2009
DC_2009_Crime <-
  rbind(
    st_read("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.geojson") %>% 
      select(OBJECTID, OFFENSE)) %>%
  st_transform(st_crs(tracts2009))

# getting lat lon 2009
DC_2009_Crime <- DC_2009_Crime %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
  
# 2017
DC_2017_Crime <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.geojson") %>% 
      select(OBJECTID, OFFENSE)) %>%
    st_transform(st_crs(tracts2017))

# getting lat lon 2017
DC_2017_Crime <- DC_2017_Crime %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])

# Mapping DC crime data 2009
ggplot(subset(DC_2009_Crime, OFFENSE =="ROBBERY")) + 
  geom_sf(data = tracts2009,
          aes(fill = q5(MedRent)), color = NA) +
  scale_fill_manual(values = palette5)+
  geom_point(aes(x=lat, y=lon), size=0.01) +
  geom_sf(data=wmataStops, size=3, shape=17, color="grey")+
  geom_sf(data=buffer, fill="transparent", color="red", size=1.5)+
  labs(title="DC crime incidents in 2009", 
       subtitle="Washington, DC", 
       caption="Source: opendata.dc.gov") +
  mapTheme()

# Mapping DC crime data 2017
ggplot(subset(DC_2017_Crime, OFFENSE =="ROBBERY")) + 
  geom_sf(data = tracts2017,
          aes(fill = q5(MedRent)), color = NA) +
  scale_fill_manual(values = palette5)+
  geom_point(aes(x=lat, y=lon), size=0.01)+
  geom_sf(data=wmataStops, size=3, shape=17, color="grey")+
  geom_sf(data=buffer, fill="transparent", color="red", size=1.5)+
  labs(title="DC crime incidents in 2017", 
       subtitle="Washington, DC", 
       caption="Source: opendata.dc.gov") +
  mapTheme()
