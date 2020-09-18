# SET UP
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

# add census API key
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

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Downloading & wrangling the Census data

# ---- Year 2009 Tracts Census Data-----

# We run our year 2000 code using 2009 ACS (and ACS variables from our 2017 list)
# This returns "long" data

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state=11, geometry=T) %>% 
  st_transform('ESRI:102728')

# Examine each variable and the elements of an sf object

tracts09[1:3,]

# Look at a table of variables

table(tracts09$variable)

# We create a new data frame consisting only of population

totalPop09 <-
  tracts09 %>%
  filter(variable == "B25026_001")

# Let's examine it

nrow(totalPop09)

names(totalPop09)

head(totalPop09)

glimpse(totalPop09)

# Use the base R plotting function to examine it visually

plot(totalPop09)

plot(totalPop09[,5])

# ---- Using ggplot to visualize census data with sf -----

# Each plot adds more and more nuance and information

# Consult the text to understand the symbology schemes

A <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = estimate)) +
  theme(plot.title = element_text(size=22))
A

B <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  theme(plot.title = element_text(size=22))
B

C <-
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Total\nPopulation\n(Quintile Breaks)") +
  theme(plot.title = element_text(size=22))
C

D <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "DC; 2009") +
  mapTheme() + theme(plot.title = element_text(size=22))
D

# Spread the data into wide form - each row becomes a census tract, and each column becomes a variable.

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)

st_drop_geometry(tracts09)[1:3,]

# Create new rate variables using mutate

tracts09 <- 
  tracts09 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)

# Tracts 2009 is now complete. Let's grab 2017 tracts and do a congruent set of operations

# ---- Year 2017 Tracts Census Data -----

# Notice that we are getting "wide" data here in the first place
# This saves us the trouble of using "spread"

tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state=11, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

# --- Combining 2009 and 2017 data ----

allTracts <- rbind(tracts09,tracts17)

# ---- Wrangling Transit Open Data -----

wmataStops <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/54018b7f06b943f2af278bbe415df1de_52.geojson") %>% 
      select(NAME, LINE)) %>%
  st_transform(st_crs(tracts09))
  

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
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=wmataStops, 
          aes(colour = LINE),
          show.legend = "point", size= 2, alpha = 0.5) +
  scale_colour_manual(values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue", "silver" = "gray")) +
  labs(title="WMATA Stops", 
       subtitle="Washington, DC", 
       caption="Source: OpenDataDC") +
  mapTheme()

# --- Relating WMATA Stops and Tracts ----

# Create buffers (in feet - note the CRS) around WMATA stops -
# Both a buffer for each stop, and a union of the buffers...
# and bind these objects together

wmataBuffers <- 
  rbind(
    st_buffer(wmataStops, 2640) %>% # projection is in feet
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(wmataStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=wmataBuffers) +
  geom_sf(data=wmataStops, show.legend = "point") +
  facet_wrap(~Legend) +
  mapTheme()

# ---- Spatial operations ----

# Consult the text to understand the difference between these three types of joins
# and discuss which is likely appropriate for this analysis

# Create an sf object with ONLY the unioned buffer
buffer <- filter(wmataBuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas
clip <- 
  st_intersection(buffer, tracts09) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Do a spatial selection to see which tracts touch the buffer
selection <- 
  tracts09[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# ---- Indicator Maps ----

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together.
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

# Can you try to create the maps seen in the text?
# The solutions are contained in "map_exercise.R"

TimeSpaceGrps <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
TimeSpaceGrps

MedRent <-
  ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars; Red border denotes areas close to WMATA stations ") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))
MedRent

# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# Let's make some comparisons and speculate about the willingness to pay
# and demographics in these areas 2009-2017 (see the 2000 data in the text too)

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

# --- TOD Indicator Plots ------ STILL PHILLY CODE FROM CLASS BEYOND THIS POINT

# Let's create small multiple plots
# We use the "gather" command (look this one up please)
# To go from wide to long
# Why do we do this??
# Notice we can "pipe" a ggplot call right into this operation!

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

# Examining three submarkets

centerCity <-
  st_intersection(
    st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union(),
    st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "Center City")

el <-
  st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "El")

broad.st <-
  st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "Broad Street")

threeMarkets <- rbind(el, broad.st, centerCity)

# You can then bind these buffers to tracts and map them or make small multiple plots

allTracts.threeMarkets <-
  st_join(st_centroid(allTracts), threeMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 

