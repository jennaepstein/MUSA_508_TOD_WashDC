# SET UP --------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(tigris)
library(viridis)


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
# Discrete from YlGnBu colorbrewer palette
# This way also the lightest is yellow, won't get confused with NA
palette5.YlGnBu <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")

# Palette of two colors for TOD and Non-TOD, similar to what we used in lab.
paletteTOD <- c("#fdb863", "#80cdc1")

# TASK 1: DATA WRANGLING -------------------------------------------------------

# Creating a vector of census variables.

acs_vars_DC <- c("B02001_001E", # Estimate!!Total population by race -- ##let's double check that it's okay to use this as long as we justify it
                "B02001_002E", # People describing themselves as "white alone"
                "B02001_003E", # People describing themselves as "black" or "african-american" alone
                "B25058_001E", # Median rent
                ""
                "B08301_001E") # People who have means of transportation to work


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
         MedRent = B25058_001E,
         TransportToWork = B08301_001E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B2")) %>%
  mutate(pctWhite = (ifelse(TotalPop > 0, Whites / TotalPop,0))*100,
         pctBlack = (ifelse(TotalPop > 0, Blacks / TotalPop,0))*100,
         pctTransportToWork = (ifelse(TotalPop > 0, TransportToWork / TotalPop,0))*100,
         year = "2009") %>%
  dplyr::select(-Whites, -Blacks, -TransportToWork)

# ---- Washington, DC - Census Data - 2017 ----

tracts2017 <- 
  get_acs(geography = "tract", 
          variables = acs_vars_DC, 
          year=2017, 
          state=11, 
          geometry=T, 
          output="wide") %>%
  st_transform('ESRI:102685') %>% # using the state plane of Maryland for a projection
  rename(TotalPop = B02001_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         MedRent = B25058_001E,
         TransportToWork = B08301_001E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B2")) %>%
  mutate(pctWhite = (ifelse(TotalPop > 0, Whites / TotalPop,0))*100,
         pctBlack = (ifelse(TotalPop > 0, Blacks / TotalPop,0))*100,
         pctTransportToWork = (ifelse(TotalPop > 0, TransportToWork / TotalPop,0))*100,
         year = "2017") %>%
  dplyr::select(-Whites, -Blacks, -TransportToWork)
# ---- Combining 2009 and 2017 data ----

allTracts <- rbind(tracts2009,tracts2017)

# ---- Reading in DC boundary ----

boundaryDC2009 <-
    st_read("https://opendata.arcgis.com/datasets/7241f6d500b44288ad983f0942b39663_10.geojson") %>%
  st_transform(st_crs(tracts2009))%>%
  mutate(year = "2009")

boundaryDC2017 <-
  st_read("https://opendata.arcgis.com/datasets/7241f6d500b44288ad983f0942b39663_10.geojson") %>%
  st_transform(st_crs(tracts2009))%>%
  mutate(year = "2017")

boundaryDC <-
  rbind(boundaryDC2009,boundaryDC2017)


# ---- Dropping tracts ----

allTracts <- subset(allTracts, GEOID!="11001006202" & 
                      GEOID!="11001000201" & 
                      GEOID!= "11001008905" & 
                      GEOID!= "11001009809" & 
                      GEOID!= "11001008600" & 
                      GEOID!= "11001006804" & 
                      GEOID!= "11001001600" &
                      GEOID!= "11001000901")


# ---- Mapping census tracts ----
map.CensusTracts <-
  ggplot()+
  geom_sf(data=boundaryDC, fill="grey", color="white")+
  geom_sf(data = allTracts, fill="#253494", color="white", alpha=0.8)+
  labs(title = "Census Tracts", subtitle = "Gray tracts are not included in analysis") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
map.CensusTracts
  

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
## removing color for now - we just want ot see where the stops are, not different lines at this point
ggplot() + 
  geom_sf(data=st_union(tracts2009)) +
  geom_sf(data=wmataStops, 
          aes(),
          show.legend = "point", size= 2) +
  ## scale_colour_manual(values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue", "silver" = "gray")) +
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
# Then add them all together
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
# Small-multiple (2009 & 2017) visualizations comparing four selected Census variables across time

# TASK 2, PART A: MAPPING MEDIAN RENT
## NEED TO DO: add the wmata line to the legend

map.MedRent <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009), color="grey")+
  geom_sf(aes(fill = q5(MedRent.inf)), color = "grey", alpha = 0.75) +
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1.25)+
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($)\n(Quintile Breaks)",
                    na.value="#e7e7e6") +
  labs(title = "Median Rent by Census Tract, 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stops; \nBlack lines are the metrorail lines within the city") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
map.MedRent

# TASK 2, PART B: MAPPING [PERCENT WHITE]
map.PctWhite <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009), color="grey")+
  geom_sf(aes(fill=q5(pctWhite)), color = "grey", alpha = 0.75) +
  geom_sf(data = buffer, fill = "transparent",color = "red", size = 1.25) +
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "pctWhite"),
                     name = "Percent White") +
  labs(title = "Percent of Population that Identifies as White, 2009 & 2017", subtitle = "Red border denotes areas close to WMATA stops; \nBlack lines are the metrorail lines within the city") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
map.PctWhite


# TASK 2, PART C: MAPPING [PERCENT BLACK]
map.PctBlack <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009), color="grey")+
  geom_sf(aes(fill = q5(pctBlack)), color = "grey", alpha= 0.75) +
  geom_sf(data = buffer, fill = "transparent",color = "red", size = 1.25)+
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "pctBlack"),
                    name = "% Black") +
  labs(title = "Percent of Population that Identifies as \nBlack or African-American alone, 2009 & 2017", subtitle = "Red border denotes areas close to WMATA stops; \nBlack lines are the metrorail lines within the city") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
map.PctBlack

# TASK 2, PART D: MAPPING [PCT POVERTY]
map.pctTransportToWork <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts2009), color="grey")+
  geom_sf(aes(fill = q5(pctTransportToWork)), color = "grey", alpha= 0.75) +
  geom_sf(data = buffer, fill = "transparent",color = "red", size = 1.25)+
  geom_sf(data = wmataLines, color = "black", size = 1)+
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "pctTransportToWork"),
                    name = "% Transport to Work") +
  labs(title = "Percent of Population with Means of \nTransportation to Work, 2009 & 2017", subtitle = "Red border denotes areas close to WMATA stops; \nBlack lines are the metrorail lines within the city") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
map.pctTransportToWork

# TASKS 3 & 4: One grouped bar plot making these same comparisons

# TASK 4: Creating table -------------------------------------------------------
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Black = mean(pctBlack, na.rm = T),
            Percent_TransportToWork = mean(pctTransportToWork, na.rm = T))
            
            
kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "All Tracts Summary")

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

# some more data wrangling
wmataStopscoord <- wmataStops %>% 
                  st_transform(st_crs('ESRI:102685')) %>% 
                  dplyr::mutate(x = sf::st_coordinates(.)[,1], 
                                y = sf::st_coordinates(.)[,2]) %>%
                  distinct(NAME, .keep_all=TRUE) %>%
                  dplyr::select(-LINE)

wmataBuffersStops <- 
  st_transform(wmataStopscoord, ('ESRI:102685'))%>%
    st_buffer(2640) %>% # projection is in feet
      dplyr::select(NAME, geometry)

joinBuffersStopsTracts <-
  st_join(allTracts.group, wmataBuffersStops)%>%
  filter(!is.na(NAME)) 

# grouping by wmata stop - population
totalPopbyStop <- joinBuffersStopsTracts %>%
  group_by(NAME, year, .add=TRUE) %>%
  summarize(sumPop = sum(TotalPop))%>%
  st_drop_geometry() %>%
  right_join(wmataStopscoord, by="NAME")
  
# map
## legend work needed - merge the legend items so sumPop circles are
map.TotalPopbyStop <-
  ggplot()+
  geom_sf(data=allTracts.group, color="white", fill="gray", alpha=0.4)+
  geom_point(data=totalPopbyStop, aes(x=x, y=y, size=sumPop), color='#253494', alpha=0.3)+
  scale_size_area(max_size = 8) +
  guides(size=guide_legend("Sum of Population")) +
  geom_sf(data=wmataLines, size=0.5, color="black", alpha=0.8) + 
  aes() +
  labs(title="Sum of Population within 1/2 mi. of WMATA Stops, by Stop", 
       subtitle="Black lines are the metrorail lines within the city", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  facet_wrap(~year) +
  mapTheme()
map.TotalPopbyStop

# It is hard to see the changes with this map, so looking at a histogram helps tell the story of how many census tracts saw population change over the time period.
 
# How has sum of population changed over time?
ggplot(totalPopbyStop)+
  geom_histogram(aes(sumPop), binwidth = 5000, fill="#253494")+
  labs(
    title = "Change in Sum of Population per WMATA stop, by Number of Census Tracts, 2009 & 2017",
    caption = "Data: US Census Bureau",
    x="Sum of Population per Stop", 
    y="Number of tracts") +
  facet_wrap(~year)
 
# grouping by wmata stop - medrent
avgMedRentbyStop <- joinBuffersStopsTracts %>%
    group_by(NAME, year, .add=TRUE) %>%
    summarize(AvgRent = mean(MedRent.inf, na.rm = TRUE)) %>%
    st_drop_geometry() %>%
    right_join(wmataStopscoord, by="NAME")


# map
map.avgMedRentbyStop <-
  ggplot()+
  geom_sf(data=allTracts.group, color="white", fill="gray", alpha=0.4)+
  geom_point(data=avgMedRentbyStop, aes(x=x, y=y, size=AvgRent), color="#80cdc1", alpha=0.4)+
  scale_size_area(max_size = 8) +
  geom_sf(data=wmataLines, size=1, color="black") + 
  aes() +
  labs(title="Avg. Median Rent within 1/2 mi. of WMATA Stops, by Stop", 
       subtitle="Washington, DC", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  facet_wrap(~year) +
  mapTheme()
map.avgMedRentbyStop

# Graduated symbol by size does not show us changes, so a graduated color map helps see change over time in median rent price in the city:
map.avgMedRentbyStop <-
  ggplot()+
  geom_sf(data=allTracts.group, color="white", fill="gray", alpha=0.4)+
  geom_point(data=avgMedRentbyStop, aes(x=x, y=y, color=AvgRent, size=5))+
  scale_color_viridis(option="D", direction= -1) +
  guides(size = FALSE) +
  geom_sf(data=wmataLines, size=1, color="black") + 
  aes() +
  labs(title="Avg. Median Rent within 1/2 mi. of WMATA Stops, by Stop", 
       subtitle="Washington, DC", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  facet_wrap(~year) +
  mapTheme()
map.avgMedRentbyStop

# grouping by wmata stop - population AND medrent
joinPopRentbyStop <- joinBuffersStopsTracts %>%
  group_by(NAME, year, .add=TRUE) %>%
  summarize(sumPop = sum(TotalPop), AvgRent = mean(MedRent.inf, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  right_join(wmataStopscoord, by="NAME")

# map
## legend work needed - avg rent bar, put lowest on top, but keep it  yellow
map.gradsymbolcombo <-
  ggplot()+
  geom_sf(data=allTracts.group, color="white", fill="gray", alpha=0.4)+
  geom_point(data=joinPopRentbyStop, aes(x=x, y=y, size=sumPop, color=AvgRent))+
    scale_size_area(max_size = 8) +
    guides(size=guide_legend("Sum of Population"))+
    scale_color_viridis(option="D", direction= -1) +
  geom_sf(data=wmataLines, size=1, color="black") + 
  aes() +
  labs(title="Population and Avg. Median Rent within 1/2 mi. of WMATA Stops, by Stop", 
       subtitle="Black lines are the metrorail lines within the city ", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  facet_wrap(~year) +
  mapTheme()
map.gradsymbolcombo

# MULTIPLE RING BUFFER -----------------
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{distances <- seq(0, maxDistance, interval)
distancesCounter <- 2
numberOfRings <- floor(maxDistance / interval)
numberOfRingsCounter <- 1
allRings <- data.frame()

while (numberOfRingsCounter <= numberOfRings) 
{if(distances[distancesCounter] < 0 & distancesCounter == 2)
{ buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
buffer1_ <- st_difference(inputPolygon, buffer1)
thisRing <- st_cast(buffer1_, "POLYGON")
thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
thisRing$distance <- distances[distancesCounter]
}
  else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
  { buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
  buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
  thisRing <- st_difference(buffer2,buffer1)
  thisRing <- st_cast(thisRing, "POLYGON")
  thisRing <- as.data.frame(thisRing$geometry)
  thisRing$distance <- distances[distancesCounter]
  }
  
  else 
  { buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
  buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
  thisRing <- st_difference(buffer1,buffer1_)
  thisRing <- st_cast(thisRing, "POLYGON")
  thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
  thisRing$distance <- distances[distancesCounter]
  }  
  
  allRings <- rbind(allRings, thisRing)
  distancesCounter <- distancesCounter + 1
  numberOfRingsCounter <- numberOfRingsCounter + 1
}

allRings <- st_as_sf(allRings)
}

allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts.group, GEOID, year)),
          multipleRingBuffer(st_union(wmataStops), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts.group, GEOID, MedRent.inf, year),
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate("distance_miles" = distance / 5280)

allTracts.rings.group <- allTracts.rings %>%
  group_by(distance_miles,year, .add=TRUE) %>%
  summarise(AvgRent_dollars = mean(MedRent.inf, na.rm = TRUE))

ggplot(data=allTracts.rings.group,
       aes(x = distance_miles, y = AvgRent_dollars, colour = year)) +
  geom_line(size=2) +
  geom_point(size=3)


# CRIME DATA ---------------------------

# Pulling in DC crime data in 2009 and 2017

# 2009
DC_2009_Crime <-
  rbind(
    st_read("https://opendata.arcgis.com/datasets/73cd2f2858714cd1a7e2859f8e6e4de4_33.geojson") %>% 
      select(OBJECTID, OFFENSE)) %>%
  st_transform(st_crs(tracts2009))

# getting coordinates, 2009
DC_2009_Crime <- DC_2009_Crime %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])
  
# 2017
DC_2017_Crime <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.geojson") %>% 
      select(OBJECTID, OFFENSE)) %>%
    st_transform(st_crs(tracts2009))

# getting coordinates, 2017
DC_2017_Crime <- DC_2017_Crime %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2]) 

# Mapping DC crime data 2009
ggplot(subset(DC_2009_Crime, OFFENSE =="ROBBERY")) + 
  geom_sf(data = allTracts.group %>%
          filter(year=="2009"),
          aes(fill = q5(MedRent.inf)), color = NA, alpha=0.75) +
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($) \n(Quintile Breaks)")+
  geom_point(aes(x=x, y=y), size=0.025) +
  geom_sf(data=wmataStops, size=1.75, shape=22,color="black", fill="#999999")+
  geom_sf(data=buffer, fill="transparent", color="red", size=1.25)+
  labs(title="DC Crime: Robbery Incidents in 2009", 
       subtitle="Washington, DC", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  mapTheme()

# Mapping DC crime data 2017
ggplot(subset(DC_2017_Crime, OFFENSE =="ROBBERY")) + 
  geom_sf(data = allTracts.group %>%
          filter(year=="2017"),
          aes(fill = q5(MedRent.inf)), color = NA, alpha=0.75) +
  scale_fill_manual(values = palette5.YlGnBu,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent ($) \n(Quintile Breaks)")+
  geom_point(aes(x=x, y=y), size=0.025)+
  geom_sf(data=wmataStops, size=1.75, shape=22,color="black", fill="#999999")+
  geom_sf(data=buffer, fill="transparent", color="red", size=1.25)+
  labs(title="DC Crime: Robbery Incidents in 2017", 
       subtitle="Washington, DC", 
       caption="Data: US Census Bureau; opendata.dc.gov") +
  mapTheme()


# --- Crime data analysis

group.robbery.2009 <- DC_2009_Crime %>%
  filter(OFFENSE=="ROBBERY")
group.robbery.2017 <- DC_2017_Crime %>%
  filter(OFFENSE=="ROBBERY")

robbery.tract.2009 <-
  st_join(allTracts.group, group.robbery.2009) %>%
  group_by(GEOID, .add=TRUE)

robbery.tract.count.2009 <- count(as_tibble(robbery.tract.2009), GEOID, x, y, TOD, year)

robbery.tract.2017 <-
  st_join(allTracts.group, group.robbery.2017) %>%
  group_by(GEOID, .add=TRUE)

robbery.tract.count.2017 <- count(as_tibble(robbery.tract.2017), GEOID, x, y, TOD, year)


allRobberyTOD <- rbind(robbery.tract.count.2009, robbery.tract.count.2017) %>%
  group_by(TOD, year) %>%
  summarise(TotalRobberies = sum(n, na.rm=T))

bar.allRobbery <-
  ggplot(data=allRobberyTOD, aes(year, TotalRobberies, fill=TOD))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=paletteYears)
bar.allRobbery 

allRobbery.Summary <-
  unite(year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year, -TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Summary for All Tracts in Washington, DC")


#Robbery Incidents as a Function of Distance to a WMATA Stop, 2009 & 2017

