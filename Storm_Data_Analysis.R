library(dplyr)

stormdata_filename <- "repdata_data_StormData.csv"
stormdata_filepath <- file.path("./data/", stormdata_filename)

if (!exists("stormdata")) {
  stormdata <- read.csv(stormdata_filepath)
}

#  Official event types
official_event_types <- c("ASTRONOMICAL LOW TIDE", "ASTRONOMICAL HIGH TIDE",
                          "AVALANCHE", "BLIZZARD", "COASTAL FLOOD", "COLD/WIND CHILL",
                          "DEBRIS FLOW", "DENSE FOG", "DENSE SMOKE", "DROUGHT", "DUST DEVIL",
                          "DUST STORM", "EXCESSIVE HEAT", "EXTREME COLD/WIND CHILL",
                          "FLASH FLOOD", "FLOOD", "FROST/FREEZE", "FUNNEL CLOUD",
                          "FREEZING FOG", "HAIL", "HEAT", "HEAVY RAIN", "HEAVY SNOW",
                          "HIGH SURF", "HIGH WIND", "HURRICANE (TYPHOON)", "ICE STORM",
                          "LANDSLIDE", "LAKE-EFFECT SNOW", "LAKESHORE FLOOD", "LIGHTNING",
                          "MARINE HAIL", "MARINE HIGH WIND", "MARINE STRONG WIND",
                          "MARINE THUNDERSTORM WIND", "RIP CURRENT", "SEICHE", "SLEET",
                          "STORM SURGE/TIDE", "STRONG WIND", "THUNDERSTORM WIND",
                          "TORNADO", "TROPICAL DEPRESSION", "TROPICAL STORM", "TSUNAMI",
                          "VOLCANIC ASH","WATERSPOUT", "WILDFIRE", "WINTER STORM",
                          "WINTER WEATHER")

replacement_list <- list(
  c("ASTRONOMICAL LOW TIDE"),
  c("ASTRONOMICAL HIGH TIDE"),
  c("AVALANCHE"),
  c("BLIZZARD"),
  c("COASTAL FLOOD", "COASTAL FLOODING"),
  c("COLD/WIND CHILL"),
  c("DEBRIS FLOW"),
  c("DENSE FOG", "FOG"),
  c("DENSE SMOKE"),
  c("DROUGHT"),
  c("DUST DEVIL"),
  c("DUST STORM"),
  c("EXCESSIVE HEAT"),
  c("EXTREME COLD/WIND CHILL", "EXTREME COLD"),
  c("FLASH FLOOD", "FLASH FLOODING", "FLOOD/FLASH FLOOD"),
  c("FLOOD", "URBAN/SML STREAM FLD", "URBAN FLOOD", "URBAN FLOODING", "FLOODING",
    "STREET FLOOD", "RIVER FLOOD"),
  c("FROST/FREEZE"),
  c("FUNNEL CLOUD"),
  c("FREEZING FOG"),
  c("HAIL"),
  c("HEAT"),
  c("HEAVY RAIN"),
  c("HEAVY SNOW", "SNOW", "LIGHT SNOW"),
  c("HIGH SURF", "HEAVY SURF/HIGH SURF"),
  c("HIGH WIND", "HIGH WINDS", "WIND"),
  c("HURRICANE (TYPHOON)", "HURRICANE"),
  c("ICE STORM"),
  c("LANDSLIDE"),
  c("LAKE-EFFECT SNOW"),
  c("LAKESHORE FLOOD"),
  c("LIGHTNING"),
  c("MARINE HAIL"),
  c("MARINE HIGH WIND"),
  c("MARINE STRONG WIND"),
  c("MARINE THUNDERSTORM WIND", "MARINE TSTM WIND"),
  c("RIP CURRENT"),
  c("SEICHE"),
  c("SLEET"),
  c("STORM SURGE/TIDE"),
  c("STRONG WIND", "STRONG WINDS"),
  c("THUNDERSTORM WIND", "TSTM WIND/HAIL", "TSTM WIND", "THUNDERSTORM WINDS",
    "THUNDERSTORM WINDSS", "THUNDERTORM WINDS","THUNDERSTORM WINDS LIGHTNING",
    "THUNDERSTORMS WINDS", "THUNDERSTORMS WIND", "THUNDERSTORM WIND/LIGHTNING",
    "THUNDERTSORM WIND", "THUNDERSTORM WINDS/HAIL"),
  c("TORNADO"),
  c("TROPICAL DEPRESSION"),
  c("TROPICAL STORM"),
  c("TSUNAMI"),
  c("VOLCANIC ASH"),
  c("WATERSPOUT"),
  c("WILDFIRE", "WILD/FOREST FIRE"),
  c("WINTER STORM"),
  c("WINTER WEATHER", "WINTER WEATHER/MIX")
)

lookup_table <- data.frame(CURRENT=c(), NEW=c())

for (i in c(1:length(replacement_list))) {
  rows <- data.frame(CURRENT=replacement_list[[i]], NEW=replacement_list[[i]][1])
  lookup_table <- rbind(lookup_table, rows)
}

newdata <- stormdata %>%
  mutate(EVTYPE=trimws(toupper(EVTYPE))) %>%
  left_join(lookup_table, by=(c("EVTYPE" = "CURRENT"))) %>%
  mutate(EVTYPE = if_else(!is.na(NEW), NEW, EVTYPE)) %>%
  #select(EVTYPE, NEW) %>%
  group_by(EVTYPE)

remaining <- newdata[!newdata$EVTYPE %in% official_event_types,] %>%
  group_by(EVTYPE) %>%
  summarise(count=n()) %>%
  arrange(desc(count), .by_group = TRUE) %>%
  print
