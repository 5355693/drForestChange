library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggrepel)
library(data.table)
#Import the files exported by Google Earth Engine
setwd("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001")
fl2001 <- fromJSON("drForestLossWeighted2001.csv", simplifyDataFrame = T)
fl2003 <- fromJSON("drForestLossWeighted2003.csv", simplifyDataFrame = T)
fl2003 <- fromJSON("drForestLossWeighted2003.csv", simplifyDataFrame = T)
fl2004 <- fromJSON("drForestLossWeighted2004.csv", simplifyDataFrame = T)
fl2005 <- fromJSON("drForestLossWeighted2005.csv", simplifyDataFrame = T)
fl2006 <- fromJSON("drForestLossWeighted2006.csv", simplifyDataFrame = T)
fl2007 <- fromJSON("drForestLossWeighted2007.csv", simplifyDataFrame = T)
fl2008 <- fromJSON("drForestLossWeighted2008.csv", simplifyDataFrame = T)
fl2009 <- fromJSON("drForestLossWeighted2009.csv", simplifyDataFrame = T)
fl2010 <- fromJSON("drForestLossWeighted2010.csv", simplifyDataFrame = T)
fl2011 <- fromJSON("drForestLossWeighted2011.csv", simplifyDataFrame = T)
fl2012 <- fromJSON("drForestLossWeighted2012.csv", simplifyDataFrame = T)
fl2013 <- fromJSON("drForestLossWeighted2013.csv", simplifyDataFrame = T)
fl2014 <- fromJSON("drForestLossWeighted2014.csv", simplifyDataFrame = T)
fl2015 <- fromJSON("drForestLossWeighted2015.csv", simplifyDataFrame = T)
fl2016 <- fromJSON("drForestLossWeighted2016.csv", simplifyDataFrame = T)

#Add a year variable to each file
fl2001$year <- 2001
fl2003$year <- 2003
fl2003$year <- 2003
fl2004$year <- 2004
fl2005$year <- 2005
fl2006$year <- 2006
fl2007$year <- 2007
fl2008$year <- 2008
fl2009$year <- 2009
fl2010$year <- 2010
fl2011$year <- 2011
fl2012$year <- 2012
fl2013$year <- 2013
fl2014$year <- 2014
fl2015$year <- 2015
fl2016$year <- 2016

#combine the files for each year into a single file:
forestLossWeighted <- rbind(fl2001,fl2003,fl2003, fl2004,fl2005,fl2006,fl2007,fl2008,fl2009,fl2010,fl2011,fl2012,fl2013,fl2014,fl2015,fl2016)

#Change the column name ('sum') to something meaningful:
colnames(forestLossWeighted)[2] <- "sqKmLost"

#Add columns for English and Spanish names of land-cover types:
forestLossWeighted <-
  forestLossWeighted %>%
  mutate(landCoverName = ifelse(landCoverCode == 1, "Dense conifer forest",
                                ifelse(landCoverCode == 2, "Open conifer forest",
                                       ifelse(landCoverCode == 4, "Wet broadleaf forest",
                                              ifelse(landCoverCode == 6, "Moist broadleaf forest",
                                                     ifelse(landCoverCode == 8, "Semi-moist broadleaf forest",
                                                            ifelse(landCoverCode == 9, "Dry forest",
                                                                   ifelse(landCoverCode == 12, "Temporarily flooded forested brackish wetlands",
                                                                          ifelse(landCoverCode == 13, "Permanently flooded forested brackish wetlands",
                                                                                 ifelse(landCoverCode == 14, "Forested freshwater wetlands",
                                                                                        ifelse(landCoverCode == 24, "Grassy brackish wetlands",
                                                                                               ifelse(landCoverCode == 33, "Cattail marsh",
                                                                                                      ifelse(landCoverCode == 26, "Grassy freshwater marsh",
                                                                                                             ifelse(landCoverCode == 16, "Broadleaf matorral",
                                                                                                                    ifelse(landCoverCode == 20, "Dry matorral",
                                                                                                                           ifelse(landCoverCode == 21, "Brackish wetland matorral",
                                                                                                                                  ifelse(landCoverCode == 39, "Rice",
                                                                                                                                         ifelse(landCoverCode == 38, "Sugar cane",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivated land",
                                                                                                                                                       ifelse(landCoverCode == 53, "Coffee and Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "African palm",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Coco Palm",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasture",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Subsistence agriculture or pasture",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Unvegetated or eroded areas",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Populated areas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lake",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Ocean",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Ocean",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Ocean", "NA")))))))))))))))))))))))))))))))

forestLossWeighted <-
  forestLossWeighted %>%
  mutate(landCoverNameSp = ifelse(landCoverCode == 1, "Bosque conifero denso",
                                ifelse(landCoverCode == 2, "Bosque conifero abierto",
                                       ifelse(landCoverCode == 4, "Bosque latifoliado nublado",
                                              ifelse(landCoverCode == 6, "Bosque latifoliado humedo",
                                                     ifelse(landCoverCode == 8, "Bosque latifoliado semi-humedo",
                                                            ifelse(landCoverCode == 9, "Bosque seco",
                                                                   ifelse(landCoverCode == 12, "Bosque humedales salobres temporalmente inundado",
                                                                          ifelse(landCoverCode == 13, "Bosque humedales salobres permanentemente inundado",
                                                                                 ifelse(landCoverCode == 14, "Bosque humedales de agua dulce",
                                                                                        ifelse(landCoverCode == 24, "Sabana de humedales salobres",
                                                                                               ifelse(landCoverCode == 33, "Eneal",
                                                                                                      ifelse(landCoverCode == 26, "Sabana humedales de agua aulce",
                                                                                                             ifelse(landCoverCode == 16, "Matorral latifoliado",
                                                                                                                    ifelse(landCoverCode == 20, "Matorral seco",
                                                                                                                           ifelse(landCoverCode == 21, "Matorral de humedales salobres",
                                                                                                                                  ifelse(landCoverCode == 39, "Arroz",
                                                                                                                                         ifelse(landCoverCode == 38, "Caña",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivos intensivos",
                                                                                                                                                       ifelse(landCoverCode == 53, "Café y Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "Palma Africana",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Palma de Coco",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasto",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Agricultura de Subsistencia y Pasto",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Escasa vegetación o area erosionada",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Areas pobladas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lagunos o lagos",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Agua semi-profunda",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Agua profunda",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Agua abierta baja", "NA")))))))))))))))))))))))))))))))


forestLossWeighted <-
  forestLossWeighted %>%
  mutate(landCoverCategory = ifelse(landCoverCode == 1, "Forest",
                                    ifelse(landCoverCode == 2, "Forest",
                                           ifelse(landCoverCode == 4, "Forest",
                                                  ifelse(landCoverCode == 6, "Forest",
                                                         ifelse(landCoverCode == 8, "Forest",
                                                                ifelse(landCoverCode == 9, "Forest",
                                                                       ifelse(landCoverCode == 12, "Wetland",
                                                                              ifelse(landCoverCode == 13, "Wetland",
                                                                                     ifelse(landCoverCode == 14, "Wetland",
                                                                                            ifelse(landCoverCode == 24, "Wetland",
                                                                                                   ifelse(landCoverCode == 33, "Wetland",
                                                                                                          ifelse(landCoverCode == 26, "Wetland",
                                                                                                                 ifelse(landCoverCode == 16, "Shrubland",
                                                                                                                        ifelse(landCoverCode == 20, "Shrubland",
                                                                                                                               ifelse(landCoverCode == 21, "Shrubland",
                                                                                                                                      ifelse(landCoverCode == 39, "Cultivated",
                                                                                                                                             ifelse(landCoverCode == 38, "Cultivated",
                                                                                                                                                    ifelse(landCoverCode == 37, "Cultivated",
                                                                                                                                                           ifelse(landCoverCode == 53, "Cultivated",
                                                                                                                                                                  ifelse(landCoverCode == 51, "Cultivated",
                                                                                                                                                                         ifelse(landCoverCode == 52, "Cultivated",
                                                                                                                                                                                ifelse(landCoverCode == 40, "Cultivated",
                                                                                                                                                                                       ifelse(landCoverCode == 59, "Cultivated",
                                                                                                                                                                                              ifelse(landCoverCode == 27, "Grassland",
                                                                                                                                                                                                     ifelse(landCoverCode == 35, "Unvegetated",
                                                                                                                                                                                                            ifelse(landCoverCode == 72, "Unvegetated",
                                                                                                                                                                                                                   ifelse(landCoverCode == 65, "Unvegetated",
                                                                                                                                                                                                                          ifelse(landCoverCode == 67, "Unvegetated",
                                                                                                                                                                                                                                 ifelse(landCoverCode == 66, "Unvegetated",
                                                                                                                                                                                                                                        ifelse(landCoverCode == 70, "Unvegetated", "NA")))))))))))))))))))))))))))))))




#Save combined file
write.csv(forestLossWeighted, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestLossWeighted.csv")
#Clean up
rm(fl2001,fl2003,fl2003, fl2004,fl2005,fl2006,fl2007,fl2008,fl2009,fl2010,fl2011,fl2012,fl2013,fl2014,fl2015,fl2016)

forestLossWeighted$landCoverCode <- factor(forestLossWeighted$landCoverCode)

forestLossWeighted <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestLossWeighted.csv", row.names = 1)
forestLossWeighted$landCoverCode <- factor(forestLossWeighted$landCoverCode)

forestLossWeighted %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(., aes(x = year, y = sqKmLost, color = landCoverName)) + geom_line() + ylab("Area lost (sq. km)") + 
  xlab("Year") + guides(color = guide_legend(title="Land-cover type"))

forestLossWeightedAll <- fromJSON("drForestLossWeighted.csv", simplifyDataFrame = T)
colnames(forestLossWeightedAll)[2] <- "sqKmLost"
forestGainWeightedAll <- fromJSON("drForestGainWeighted.csv", simplifyDataFrame = T)
colnames(forestGainWeightedAll)[2] <- "sqKmGained"
forestArea2000 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestArea2000.csv", header = T)
colnames(forestArea2000)[2] <- "sqKm2000"

forestChange <- inner_join(forestArea2000,forestLossWeightedAll, by = "landCoverCode")
forestChange <- inner_join(forestChange, forestGainWeightedAll, by = "landCoverCode")

forestChange <-
forestChange %>%
  mutate(netChange = sqKmGained-sqKmLost,
         area2016 = sqKm2000 - sqKmLost + sqKmGained,
         percentChange = (1-(sqKm2000/area2016))*100)
#Add columns for English and Spanish names of land-cover types:
forestChange <-
  forestChange %>%
  mutate(landCoverName = ifelse(landCoverCode == 1, "Dense conifer forest",
                                ifelse(landCoverCode == 2, "Open conifer forest",
                                       ifelse(landCoverCode == 4, "Wet broadleaf forest",
                                              ifelse(landCoverCode == 6, "Moist broadleaf forest",
                                                     ifelse(landCoverCode == 8, "Semi-moist broadleaf forest",
                                                            ifelse(landCoverCode == 9, "Dry forest",
                                                                   ifelse(landCoverCode == 12, "Temporarily flooded forested brackish wetlands",
                                                                          ifelse(landCoverCode == 13, "Permanently flooded forested brackish wetlands",
                                                                                 ifelse(landCoverCode == 14, "Forested freshwater wetlands",
                                                                                        ifelse(landCoverCode == 24, "Grassy brackish wetlands",
                                                                                               ifelse(landCoverCode == 33, "Cattail marsh",
                                                                                                      ifelse(landCoverCode == 26, "Grassy freshwater marsh",
                                                                                                             ifelse(landCoverCode == 16, "Broadleaf matorral",
                                                                                                                    ifelse(landCoverCode == 20, "Dry matorral",
                                                                                                                           ifelse(landCoverCode == 21, "Brackish wetland matorral",
                                                                                                                                  ifelse(landCoverCode == 39, "Rice",
                                                                                                                                         ifelse(landCoverCode == 38, "Sugar cane",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivated land",
                                                                                                                                                       ifelse(landCoverCode == 53, "Coffee and Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "African palm",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Coco Palm",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasture",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Subsistence agriculture or pasture",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Unvegetated or eroded areas",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Populated areas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lake",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Ocean",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Ocean",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Ocean", "NA")))))))))))))))))))))))))))))))

forestChange <-
  forestChange %>%
  mutate(landCoverNameSp = ifelse(landCoverCode == 1, "Bosque conifero denso",
                                  ifelse(landCoverCode == 2, "Bosque conifero abierto",
                                         ifelse(landCoverCode == 4, "Bosque latifoliado nublado",
                                                ifelse(landCoverCode == 6, "Bosque latifoliado humedo",
                                                       ifelse(landCoverCode == 8, "Bosque latifoliado semi-humedo",
                                                              ifelse(landCoverCode == 9, "Bosque seco",
                                                                     ifelse(landCoverCode == 12, "Bosque humedales salobres temporalmente inundado",
                                                                            ifelse(landCoverCode == 13, "Bosque humedales salobres permanentemente inundado",
                                                                                   ifelse(landCoverCode == 14, "Bosque humedales de agua dulce",
                                                                                          ifelse(landCoverCode == 24, "Sabana de humedales salobres",
                                                                                                 ifelse(landCoverCode == 33, "Eneal",
                                                                                                        ifelse(landCoverCode == 26, "Sabana humedales de agua aulce",
                                                                                                               ifelse(landCoverCode == 16, "Matorral latifoliado",
                                                                                                                      ifelse(landCoverCode == 20, "Matorral seco",
                                                                                                                             ifelse(landCoverCode == 21, "Matorral de humedales salobres",
                                                                                                                                    ifelse(landCoverCode == 39, "Arroz",
                                                                                                                                           ifelse(landCoverCode == 38, "Caña",
                                                                                                                                                  ifelse(landCoverCode == 37, "Cultivos intensivos",
                                                                                                                                                         ifelse(landCoverCode == 53, "Café y Cacao",
                                                                                                                                                                ifelse(landCoverCode == 51, "Palma Africana",
                                                                                                                                                                       ifelse(landCoverCode == 52, "Palma de Coco",
                                                                                                                                                                              ifelse(landCoverCode == 40, "Pasto",
                                                                                                                                                                                     ifelse(landCoverCode == 59, "Agricultura de Subsistencia y Pasto",
                                                                                                                                                                                            ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                   ifelse(landCoverCode == 35, "Escasa vegetación o area erosionada",
                                                                                                                                                                                                          ifelse(landCoverCode == 72, "Areas pobladas",
                                                                                                                                                                                                                 ifelse(landCoverCode == 65, "Lagunos o lagos",
                                                                                                                                                                                                                        ifelse(landCoverCode == 67, "Agua semi-profunda",
                                                                                                                                                                                                                               ifelse(landCoverCode == 66, "Agua profunda",
                                                                                                                                                                                                                                      ifelse(landCoverCode == 70, "Agua abierta baja", "NA")))))))))))))))))))))))))))))))



forestChange <-
  forestChange %>%
  mutate(landCoverName = ifelse(landCoverCode == 1, "Dense conifer forest",
                                ifelse(landCoverCode == 2, "Open conifer forest",
                                       ifelse(landCoverCode == 4, "Wet broadleaf forest",
                                              ifelse(landCoverCode == 6, "Moist broadleaf forest",
                                                     ifelse(landCoverCode == 8, "Semi-moist broadleaf forest",
                                                            ifelse(landCoverCode == 9, "Dry forest",
                                                                   ifelse(landCoverCode == 12, "Temporarily flooded forested brackish wetlands",
                                                                          ifelse(landCoverCode == 13, "Permanently flooded forested brackish wetlands",
                                                                                 ifelse(landCoverCode == 14, "Forested freshwater wetlands",
                                                                                        ifelse(landCoverCode == 24, "Grassy brackish wetlands",
                                                                                               ifelse(landCoverCode == 33, "Cattail marsh",
                                                                                                      ifelse(landCoverCode == 26, "Grassy freshwater marsh",
                                                                                                             ifelse(landCoverCode == 16, "Broadleaf matorral",
                                                                                                                    ifelse(landCoverCode == 20, "Dry matorral",
                                                                                                                           ifelse(landCoverCode == 21, "Brackish wetland matorral",
                                                                                                                                  ifelse(landCoverCode == 39, "Rice",
                                                                                                                                         ifelse(landCoverCode == 38, "Sugar cane",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivated land",
                                                                                                                                                       ifelse(landCoverCode == 53, "Coffee and Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "African palm",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Coco Palm",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasture",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Subsistence agriculture or pasture",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Unvegetated or eroded areas",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Populated areas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lake",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Ocean",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Ocean",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Ocean", "NA")))))))))))))))))))))))))))))))

forestChange <-
  forestChange %>%
  mutate(landCoverCategory = ifelse(landCoverCode == 1, "Forest",
                                  ifelse(landCoverCode == 2, "Forest",
                                         ifelse(landCoverCode == 4, "Forest",
                                                ifelse(landCoverCode == 6, "Forest",
                                                       ifelse(landCoverCode == 8, "Forest",
                                                              ifelse(landCoverCode == 9, "Forest",
                                                                     ifelse(landCoverCode == 12, "Wetland",
                                                                            ifelse(landCoverCode == 13, "Wetland",
                                                                                   ifelse(landCoverCode == 14, "Wetland",
                                                                                          ifelse(landCoverCode == 24, "Wetland",
                                                                                                 ifelse(landCoverCode == 33, "Wetland",
                                                                                                        ifelse(landCoverCode == 26, "Wetland",
                                                                                                               ifelse(landCoverCode == 16, "Shrubland",
                                                                                                                      ifelse(landCoverCode == 20, "Shrubland",
                                                                                                                             ifelse(landCoverCode == 21, "Shrubland",
                                                                                                                                    ifelse(landCoverCode == 39, "Cultivated",
                                                                                                                                           ifelse(landCoverCode == 38, "Cultivated",
                                                                                                                                                  ifelse(landCoverCode == 37, "Cultivated",
                                                                                                                                                         ifelse(landCoverCode == 53, "Cultivated",
                                                                                                                                                                ifelse(landCoverCode == 51, "Cultivated",
                                                                                                                                                                       ifelse(landCoverCode == 52, "Cultivated",
                                                                                                                                                                              ifelse(landCoverCode == 40, "Cultivated",
                                                                                                                                                                                     ifelse(landCoverCode == 59, "Cultivated",
                                                                                                                                                                                            ifelse(landCoverCode == 27, "Grassland",
                                                                                                                                                                                                   ifelse(landCoverCode == 35, "Unvegetated",
                                                                                                                                                                                                          ifelse(landCoverCode == 72, "Unvegetated",
                                                                                                                                                                                                                 ifelse(landCoverCode == 65, "Unvegetated",
                                                                                                                                                                                                                        ifelse(landCoverCode == 67, "Unvegetated",
                                                                                                                                                                                                                               ifelse(landCoverCode == 66, "Unvegetated",
                                                                                                                                                                                                                                      ifelse(landCoverCode == 70, "Unvegetated", "NA")))))))))))))))))))))))))))))))





write.csv(forestChange, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange$landCoverCode <- factor(forestChange$landCoverCode)
forestChange %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(.,aes(x = reorder(landCoverName, netChange), y = netChange)) + geom_col() + 
  ylab("Net change area (sq. km)") + xlab("Land-cover type") + theme(axis.text.x = element_text(angle = 340,
                                                                                                vjust = 0.5))

forestChange %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(.,aes(x = reorder(landCoverName, percentChange), y = percentChange)) + geom_col() + 
  ylab("Percent change area") + xlab("Land-cover type") + theme(axis.text.x = element_text(angle = 340,
                                                                                                vjust = 0.5))

forestLossWeighted %>%
  filter(landCoverCategory == "Forest"|landCoverCategory=="Shrubland") %>%
  ggplot(.,aes(x = year, y = sqKmLost, color = landCoverName)) + geom_line(size = 1.5) + 
  ylab("Area lost (sq. km)") + xlab("Year") + guides(color = guide_legend(title="Land-cover type")) + 
  scale_x_continuous(breaks = c(2000,2003,2004,2006,2008,2010,2012,2014,2016)) + 
  theme(axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 16))

#Create a file that shows annual area, by taking area in 2000 and subtracting amount lost each year
tmp <- filter(forestLossWeighted, year == 2001)
forestAreaAnnual <- mutate(forestArea2000, sqKm2001 = sqKm2000-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2003)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2003 = sqKm2001-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2003)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2003 = sqKm2003-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2004)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2004 = sqKm2003-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2005)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2005 = sqKm2004-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2006)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2006 = sqKm2005-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2007)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2007 = sqKm2006-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2008)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2008 = sqKm2007-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2009)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2009 = sqKm2008-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2010)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2010 = sqKm2009-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2011)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2011 = sqKm2010-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2012)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2012 = sqKm2011-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2013)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2013 = sqKm2012-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2014)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2014 = sqKm2013-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2015)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2015 = sqKm2014-tmp$sqKmLost)
tmp <- filter(forestLossWeighted, year == 2016)
forestAreaAnnual <- mutate(forestAreaAnnual, sqKm2016 = sqKm2015-tmp$sqKmLost)
rm(tmp)


forestAreaAnnual <- gather(data = forestAreaAnnual, key = "year", value = "sqKm",2:18)
forestAreaAnnual$year <- gsub(pattern = "sqKm", x = forestAreaAnnual$year, replacement = "")
forestAreaAnnual$year <- as.numeric(forestAreaAnnual$year)

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(landCoverCategory = ifelse(landCoverCode == 1, "Forest",
                                    ifelse(landCoverCode == 2, "Forest",
                                           ifelse(landCoverCode == 4, "Forest",
                                                  ifelse(landCoverCode == 6, "Forest",
                                                         ifelse(landCoverCode == 8, "Forest",
                                                                ifelse(landCoverCode == 9, "Forest",
                                                                       ifelse(landCoverCode == 12, "Wetland",
                                                                              ifelse(landCoverCode == 13, "Wetland",
                                                                                     ifelse(landCoverCode == 14, "Wetland",
                                                                                            ifelse(landCoverCode == 24, "Wetland",
                                                                                                   ifelse(landCoverCode == 33, "Wetland",
                                                                                                          ifelse(landCoverCode == 26, "Wetland",
                                                                                                                 ifelse(landCoverCode == 16, "Shrubland",
                                                                                                                        ifelse(landCoverCode == 20, "Shrubland",
                                                                                                                               ifelse(landCoverCode == 21, "Shrubland",
                                                                                                                                      ifelse(landCoverCode == 39, "Cultivated",
                                                                                                                                             ifelse(landCoverCode == 38, "Cultivated",
                                                                                                                                                    ifelse(landCoverCode == 37, "Cultivated",
                                                                                                                                                           ifelse(landCoverCode == 53, "Cultivated",
                                                                                                                                                                  ifelse(landCoverCode == 51, "Cultivated",
                                                                                                                                                                         ifelse(landCoverCode == 52, "Cultivated",
                                                                                                                                                                                ifelse(landCoverCode == 40, "Cultivated",
                                                                                                                                                                                       ifelse(landCoverCode == 59, "Cultivated",
                                                                                                                                                                                              ifelse(landCoverCode == 27, "Grassland",
                                                                                                                                                                                                     ifelse(landCoverCode == 35, "Unvegetated",
                                                                                                                                                                                                            ifelse(landCoverCode == 72, "Unvegetated",
                                                                                                                                                                                                                   ifelse(landCoverCode == 65, "Unvegetated",
                                                                                                                                                                                                                          ifelse(landCoverCode == 67, "Unvegetated",
                                                                                                                                                                                                                                 ifelse(landCoverCode == 66, "Unvegetated",
                                                                                                                                                                                                                                        ifelse(landCoverCode == 70, "Unvegetated", "NA")))))))))))))))))))))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(landCoverNameSp = ifelse(landCoverCode == 1, "Bosque conifero denso",
                                  ifelse(landCoverCode == 2, "Bosque conifero abierto",
                                         ifelse(landCoverCode == 4, "Bosque latifoliado nublado",
                                                ifelse(landCoverCode == 6, "Bosque latifoliado humedo",
                                                       ifelse(landCoverCode == 8, "Bosque latifoliado semi-humedo",
                                                              ifelse(landCoverCode == 9, "Bosque seco",
                                                                     ifelse(landCoverCode == 12, "Bosque humedales salobres temporalmente inundado",
                                                                            ifelse(landCoverCode == 13, "Bosque humedales salobres permanentemente inundado",
                                                                                   ifelse(landCoverCode == 14, "Bosque humedales de agua dulce",
                                                                                          ifelse(landCoverCode == 24, "Sabana de humedales salobres",
                                                                                                 ifelse(landCoverCode == 33, "Eneal",
                                                                                                        ifelse(landCoverCode == 26, "Sabana humedales de agua aulce",
                                                                                                               ifelse(landCoverCode == 16, "Matorral latifoliado",
                                                                                                                      ifelse(landCoverCode == 20, "Matorral seco",
                                                                                                                             ifelse(landCoverCode == 21, "Matorral de humedales salobres",
                                                                                                                                    ifelse(landCoverCode == 39, "Arroz",
                                                                                                                                           ifelse(landCoverCode == 38, "Caña",
                                                                                                                                                  ifelse(landCoverCode == 37, "Cultivos intensivos",
                                                                                                                                                         ifelse(landCoverCode == 53, "Café y Cacao",
                                                                                                                                                                ifelse(landCoverCode == 51, "Palma Africana",
                                                                                                                                                                       ifelse(landCoverCode == 52, "Palma de Coco",
                                                                                                                                                                              ifelse(landCoverCode == 40, "Pasto",
                                                                                                                                                                                     ifelse(landCoverCode == 59, "Agricultura de Subsistencia y Pasto",
                                                                                                                                                                                            ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                   ifelse(landCoverCode == 35, "Escasa vegetación o area erosionada",
                                                                                                                                                                                                          ifelse(landCoverCode == 72, "Areas pobladas",
                                                                                                                                                                                                                 ifelse(landCoverCode == 65, "Lagunos o lagos",
                                                                                                                                                                                                                        ifelse(landCoverCode == 67, "Agua semi-profunda",
                                                                                                                                                                                                                               ifelse(landCoverCode == 66, "Agua profunda",
                                                                                                                                                                                                                                      ifelse(landCoverCode == 70, "Agua abierta baja", "NA")))))))))))))))))))))))))))))))



forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(landCoverName = ifelse(landCoverCode == 1, "Dense conifer forest",
                                ifelse(landCoverCode == 2, "Open conifer forest",
                                       ifelse(landCoverCode == 4, "Wet broadleaf forest",
                                              ifelse(landCoverCode == 6, "Moist broadleaf forest",
                                                     ifelse(landCoverCode == 8, "Semi-moist broadleaf forest",
                                                            ifelse(landCoverCode == 9, "Dry forest",
                                                                   ifelse(landCoverCode == 12, "Temporarily flooded forested brackish wetlands",
                                                                          ifelse(landCoverCode == 13, "Permanently flooded forested brackish wetlands",
                                                                                 ifelse(landCoverCode == 14, "Forested freshwater wetlands",
                                                                                        ifelse(landCoverCode == 24, "Grassy brackish wetlands",
                                                                                               ifelse(landCoverCode == 33, "Cattail marsh",
                                                                                                      ifelse(landCoverCode == 26, "Grassy freshwater marsh",
                                                                                                             ifelse(landCoverCode == 16, "Broadleaf matorral",
                                                                                                                    ifelse(landCoverCode == 20, "Dry matorral",
                                                                                                                           ifelse(landCoverCode == 21, "Brackish wetland matorral",
                                                                                                                                  ifelse(landCoverCode == 39, "Rice",
                                                                                                                                         ifelse(landCoverCode == 38, "Sugar cane",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivated land",
                                                                                                                                                       ifelse(landCoverCode == 53, "Coffee and Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "African palm",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Coco Palm",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasture",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Subsistence agriculture or pasture",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Unvegetated or eroded areas",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Populated areas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lake",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Ocean",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Ocean",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Ocean", "NA")))))))))))))))))))))))))))))))




write.csv(forestAreaAnnual,"forestAreaAnnual.csv")
forestAreaAnnual <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/GEEanalysis/forestAreaAnnual.csv")

forestAreaAnnual %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(.,aes(x = year, y = sqKm, color = landCoverName)) + geom_line(size = 1.5) + 
  ylab("Area covered (sq. km)") + xlab("Year") + guides(color = guide_legend(title="Land-cover type")) + 
  scale_x_continuous(breaks = c(2000,2003,2004,2006,2008,2010,2012,2014,2016)) + 
  theme(axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 16))

## Bring in protected-area LC change and add it to the forestChange file.
## First, total area by LC in protected areas in 2000:
paForestArea2000 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestArea2000Output.csv")
paForestArea2000[,44] <- NULL #remove the geometry column
paForestArea2000 <- paForestArea2000[-c(1),] #remove the empty first line that had to be created in GEE
paForestArea2000 <- gather(paForestArea2000, key = 'landCover', value = 'sqKm2000',2:33)
paForestArea2000<-
  paForestArea2000 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                  ifelse(landCover == "X2", 2,
                                         ifelse(landCover == "X4", 4,
                                                ifelse(landCover == "X6", 6,
                                                       ifelse(landCover == "X8", 8,
                                                              ifelse(landCover == "X9", 9,
                                                                     ifelse(landCover == "X12", 12,
                                                                            ifelse(landCover == "X13", 13,
                                                                                   ifelse(landCover == "X14", 14,
                                                                                          ifelse(landCover == "X24", 24,
                                                                                                 ifelse(landCover == "X33", 33,
                                                                                                        ifelse(landCover == "X26", 26,
                                                                                                               ifelse(landCover == "X16", 16,
                                                                                                                      ifelse(landCover == "X20", 20,
                                                                                                                             ifelse(landCover == "X21", 21,
                                                                                                                                    ifelse(landCover == "X39", 39,
                                                                                                                                           ifelse(landCover == "X38", 38,
                                                                                                                                                  ifelse(landCover == "X37", 37,
                                                                                                                                                         ifelse(landCover == "X53", 53,
                                                                                                                                                                ifelse(landCover == "X51", 51,
                                                                                                                                                                       ifelse(landCover == "X52", 52,
                                                                                                                                                                              ifelse(landCover == "X40", 40,
                                                                                                                                                                                     ifelse(landCover == "X59", 59,
                                                                                                                                                                                            ifelse(landCover == "X27", 27,
                                                                                                                                                                                                   ifelse(landCover == "X35", 35,
                                                                                                                                                                                                          ifelse(landCover == "X72", 72,
                                                                                                                                                                                                                 ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                        ifelse(landCover == "X67",67,
                                                                                                                                                                                                                               ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                      ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))
paForestArea2000$sqKm2000[is.na(paForestArea2000$sqKm2000)] <- 0
paForestArea2000 <- paForestArea2000[,-c(2,5,6,7)]
paForestArea2000 <- paForestArea2000[,-c(7,8)]
write.csv(paForestArea2000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestArea2000.csv")
paForestArea2000 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestArea2000.csv")

paForestArea2000summary <-
  paForestArea2000 %>%
  group_by(landCoverCode) %>%
  summarize(sqKm2000inPA = sum(sqKm2000))
paForestArea2000summary <- na.omit(paForestArea2000summary)
paForestArea2000summary$landCoverCode <- factor(paForestArea2000summary$landCoverCode)
paForestArea2000summary <-
  paForestArea2000summary %>%
  filter(landCoverCode != "66") %>%
  filter(landCoverCode != "67") %>%
  filter(landCoverCode != "70") %>%
  droplevels()
forestChange<- inner_join(forestChange, paForestArea2000summary, by = "landCoverCode")
write.csv(forestChange, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange$landCoverCode <- factor(forestChange$landCoverCode)

## Create a "forestChange"-style data frame JUST for protected areas (we can merge this later with forestChange, too):
forestLossWeightedPA <- fromJSON("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossWeightedAllYears.csv", simplifyDataFrame = T)
colnames(forestLossWeightedPA)[2] <- "sqKmLostInPA"
forestGainWeightedPA <- fromJSON("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestGainWeightedAllYears.csv", simplifyDataFrame = T)
colnames(forestGainWeightedPA)[2] <- "sqKmGainedInPA"
forestLossWeightedPA$landCoverCode <- factor(forestLossWeightedPA$landCoverCode)
forestGainWeightedPA$landCoverCode <- factor(forestGainWeightedPA$landCoverCode)

forestChange <- inner_join(forestChange,forestLossWeightedPA, by = "landCoverCode")
forestChange <- inner_join(forestChange, forestGainWeightedPA, by = "landCoverCode")

forestChange <-
  forestChange %>%
  mutate(netChangeInPA = sqKmGainedInPA-sqKmLostInPA,
         area2016InPA = sqKm2000inPA - sqKmLostInPA + sqKmGainedInPA,
         percentChangeInPA = (1-(sqKm2000inPA/area2016InPA))*100)
forestChange <-
  forestChange %>%
  mutate(percentLCInPA  = (sqKm2000inPA/sqKm2000)*100)

forestChange <-
  forestChange %>%
  mutate(landCoverCategorySp = ifelse(landCoverCategory == "Forest", "Bosque",
                                      ifelse(landCoverCategory == "Shrubland", "Matorrales",
                                             ifelse(landCoverCategory == "Wetland", "Humedales",
                                                     ifelse(landCoverCategory == "Grassland", "Pastos",
                                                            ifelse(landCoverCategory == "Unvegetated", "Escasa vegetacion",
                                                                   ifelse(landCoverCategory == "Cultivated", "Cultivos","NA")))))))
forestChange[,c(1,2)] <- NULL # Did this to get rid of the index columns that are created each time the CSV is read back in as a d.f.
write.csv(forestChange, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv", row.names = 1)
forestChange$landCoverCode <- factor(forestChange$landCoverCode)


p<-forestChange%>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(., aes(x = landCoverNameSp, y = percentLCInPA)) + geom_col(fill="green") + xlab("Tipo de cobertura") + 
  ylab ("Porcentaje en areas protegidas") + theme(axis.text.x = element_text(angle = 340, size = 14),
                                                  axis.text.y = element_text(size = 14),
                                                  axis.title.x = element_text(size = 24),
                                                  axis.title.y  = element_text(size = 24)) + 
  scale_x_discrete(limit = c("Bosque conifero abierto", "Bosque conifero denso", "Bosque latifoliado nublado",
                             "Bosque latifoliado humedo", "Bosque latifoliado semi-humedo", "Bosque seco"))
  
q<-forestChange%>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(., aes(x = landCoverNameSp, y = percentChange)) + geom_col(fill = "red") + xlab("Tipo de cobertura") + 
  ylab ("Porcentaje cambio en cobertura") + theme(axis.text.x = element_text(angle = 340, size = 14),
                                                  axis.text.y = element_text(size = 14),
                                                  axis.title.x = element_text(size = 24),
                                                  axis.title.y  = element_text(size = 24)) + 
  scale_x_discrete(limit = c("Bosque conifero abierto", "Bosque conifero denso", "Bosque latifoliado nublado",
                             "Bosque latifoliado humedo", "Bosque latifoliado semi-humedo", "Bosque seco"))


grid.arrange(p,q, nrow = 1)
rm(p,q)

forestChange %>%
  filter(landCoverCategory != "Cultivated") %>%
  filter(landCoverCategory != "Unvegetated") %>%
  ggplot(., aes(x = percentLCInPA, y = percentChange, label = landCoverNameSp)) + geom_point() + 
  geom_text(aes(label = landCoverNameSp), vjust = 1)



### geom_label_repel
p <- forestChange %>%
  filter(landCoverCategory != "Cultivated") %>%
  filter(landCoverCategory != "Unvegetated") %>%
  ggplot(., aes(x = percentLCInPA, y = percentChange, color = landCoverCategorySp)) + geom_point(size = 4) + 
  xlab ("Porcentaje en areas protegidas") + ylab ("Porcentaje cambio en cobertura (2000 - 2016)") + guides(color = guide_legend(title = ""))


q <- p + 
  geom_label_repel(aes(label = landCoverNameSp),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic(base_size = 18)

png(filename = "cambioCoberturaVSporcentajeProtegida.png", res = 300,
    units = "in", height = 8, width = 12)
q
dev.off()

## Read in data for individual protected areas by year:
### 2001
paForestLoss2001 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2001.csv")
paForestLoss2001[,44] <- NULL #remove the geometry column
paForestLoss2001 <- paForestLoss2001[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2001 <- gather(paForestLoss2001, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2001 <-
  paForestLoss2001 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2001 <- paForestLoss2001[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2001$year <- 2001
paForestLoss2001$key <- paste0(paForestLoss2001$system.index,"_lc",paForestLoss2001$landCoverCode)
paForestLoss2001 <- filter(paForestLoss2001, !is.na(landCoverCode))
write.csv(paForestLoss2001, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2001.csv")
paForestLoss2001 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2001.csv", row.names = 1)
paForestLoss2001$landCoverCode <- factor(paForestLoss2001$landCoverCode)

##Area burned, 2001
paForestBurned2001 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2001.csv")
paForestBurned2001[,44] <- NULL #remove the geometry column
paForestBurned2001 <- paForestBurned2001[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2001 <- gather(paForestBurned2001, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2001 <-
  paForestBurned2001 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2001 <- paForestBurned2001[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2001$year <- 2001
paForestBurned2001$key <- paste0(paForestBurned2001$system.index,"_lc",paForestBurned2001$landCoverCode)
paForestBurned2001 <- filter(paForestBurned2001, !is.na(landCoverCode))
write.csv(paForestBurned2001, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2001.csv")
paForestBurned2001 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2001.csv", row.names = 1)
paForestBurned2001$landCoverCode <- factor(paForestBurned2001$landCoverCode)

paForestLostAll2001 <- paForestLoss2001 %>%
  left_join(select(paForestBurned2001, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2001, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2001.csv")
paForestLostAll2001 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2001.csv", row.names = 1)
paForestLostAll2001$landCoverCode <- factor(paForestLostAll2001$landCoverCode)

###2002
paForestLoss2002 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2002.csv")
paForestLoss2002[,44] <- NULL #remove the geometry column
paForestLoss2002 <- paForestLoss2002[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2002 <- gather(paForestLoss2002, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2002 <-
  paForestLoss2002 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2002 <- paForestLoss2002[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2002$year <- 2002
paForestLoss2002$key <- paste0(paForestLoss2002$system.index,"_lc",paForestLoss2002$landCoverCode)
paForestLoss2002 <- filter(paForestLoss2002, !is.na(landCoverCode))
write.csv(paForestLoss2002, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2002.csv")
paForestLoss2002 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2002.csv", row.names = 1)
paForestLoss2002$landCoverCode <- factor(paForestLoss2002$landCoverCode)


##Area burned, 2002
paForestBurned2002 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2002.csv")
paForestBurned2002[,44] <- NULL #remove the geometry column
paForestBurned2002 <- paForestBurned2002[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2002 <- gather(paForestBurned2002, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2002 <-
  paForestBurned2002 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2002 <- paForestBurned2002[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2002$year <- 2002
paForestBurned2002$key <- paste0(paForestBurned2002$system.index,"_lc",paForestBurned2002$landCoverCode)
paForestBurned2002 <- filter(paForestBurned2002, !is.na(landCoverCode))
write.csv(paForestBurned2002, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2002.csv")
paForestBurned2002 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2002.csv", row.names = 1)
paForestBurned2002$landCoverCode <- factor(paForestBurned2002$landCoverCode)

paForestLostAll2002 <- paForestLoss2002 %>%
  left_join(select(paForestBurned2002, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2002, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2002.csv")
paForestLostAll2002 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2002.csv", row.names = 1)
paForestLostAll2002$landCoverCode <- factor(paForestLostAll2002$landCoverCode)


###2003
paForestLoss2003 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2003.csv")
paForestLoss2003[,44] <- NULL #remove the geometry column
paForestLoss2003 <- paForestLoss2003[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2003 <- gather(paForestLoss2003, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2003 <-
  paForestLoss2003 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2003 <- paForestLoss2003[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2003$year <- 2003
paForestLoss2003$key <- paste0(paForestLoss2003$system.index,"_lc",paForestLoss2003$landCoverCode)
paForestLoss2003 <- filter(paForestLoss2003, !is.na(landCoverCode))
write.csv(paForestLoss2003, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2003.csv")
paForestLoss2003 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2003.csv", row.names = 1)
paForestLoss2003$landCoverCode <- factor(paForestLoss2003$landCoverCode)


##Area burned, 2003
paForestBurned2003 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2003.csv")
paForestBurned2003[,44] <- NULL #remove the geometry column
paForestBurned2003 <- paForestBurned2003[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2003 <- gather(paForestBurned2003, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2003 <-
  paForestBurned2003 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2003 <- paForestBurned2003[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2003$year <- 2003
paForestBurned2003$key <- paste0(paForestBurned2003$system.index,"_lc",paForestBurned2003$landCoverCode)
paForestBurned2003 <- filter(paForestBurned2003, !is.na(landCoverCode))
write.csv(paForestBurned2003, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2003.csv")
paForestBurned2003 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2003.csv", row.names = 1)
paForestBurned2003$landCoverCode <- factor(paForestBurned2003$landCoverCode)

paForestLostAll2003 <- paForestLoss2003 %>%
  left_join(select(paForestBurned2003, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2003, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2003.csv")
paForestLostAll2003 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2003.csv", row.names = 1)
paForestLostAll2003$landCoverCode <- factor(paForestLostAll2003$landCoverCode)

###2004
paForestLoss2004 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2004.csv")
paForestLoss2004[,44] <- NULL #remove the geometry column
paForestLoss2004 <- paForestLoss2004[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2004 <- gather(paForestLoss2004, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2004 <-
  paForestLoss2004 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2004 <- paForestLoss2004[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2004$year <- 2004
paForestLoss2004$key <- paste0(paForestLoss2004$system.index,"_lc",paForestLoss2004$landCoverCode)
paForestLoss2004 <- filter(paForestLoss2004, !is.na(landCoverCode))
write.csv(paForestLoss2004, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2004.csv")
paForestLoss2004 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2004.csv", row.names = 1)
paForestLoss2004$landCoverCode <- factor(paForestLoss2004$landCoverCode)


##Area burned, 2004
paForestBurned2004 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2004.csv")
paForestBurned2004[,44] <- NULL #remove the geometry column
paForestBurned2004 <- paForestBurned2004[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2004 <- gather(paForestBurned2004, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2004 <-
  paForestBurned2004 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2004 <- paForestBurned2004[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2004$year <- 2004
paForestBurned2004$key <- paste0(paForestBurned2004$system.index,"_lc",paForestBurned2004$landCoverCode)
paForestBurned2004 <- filter(paForestBurned2004, !is.na(landCoverCode))
write.csv(paForestBurned2004, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2004.csv")
paForestBurned2004 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2004.csv", row.names = 1)
paForestBurned2004$landCoverCode <- factor(paForestBurned2004$landCoverCode)

paForestLostAll2004 <- paForestLoss2004 %>%
  left_join(select(paForestBurned2004, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2004, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2004.csv")
paForestLostAll2004 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2004.csv", row.names = 1)
paForestLostAll2004$landCoverCode <- factor(paForestLostAll2004$landCoverCode)

###2005
paForestLoss2005 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2005.csv")
paForestLoss2005[,44] <- NULL #remove the geometry column
paForestLoss2005 <- paForestLoss2005[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2005 <- gather(paForestLoss2005, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2005 <-
  paForestLoss2005 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2005 <- paForestLoss2005[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2005$year <- 2005
paForestLoss2005$key <- paste0(paForestLoss2005$system.index,"_lc",paForestLoss2005$landCoverCode)
paForestLoss2005 <- filter(paForestLoss2005, !is.na(landCoverCode))
write.csv(paForestLoss2005, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2005.csv")
paForestLoss2005 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2005.csv", row.names = 1)
paForestLoss2005$landCoverCode <- factor(paForestLoss2005$landCoverCode)


##Area burned, 2005
paForestBurned2005 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2005.csv")
paForestBurned2005[,44] <- NULL #remove the geometry column
paForestBurned2005 <- paForestBurned2005[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2005 <- gather(paForestBurned2005, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2005 <-
  paForestBurned2005 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2005 <- paForestBurned2005[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2005$year <- 2005
paForestBurned2005$key <- paste0(paForestBurned2005$system.index,"_lc",paForestBurned2005$landCoverCode)
paForestBurned2005 <- filter(paForestBurned2005, !is.na(landCoverCode))
write.csv(paForestBurned2005, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2005.csv")
paForestBurned2005 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2005.csv", row.names = 1)
paForestBurned2005$landCoverCode <- factor(paForestBurned2005$landCoverCode)

paForestLostAll2005 <- paForestLoss2005 %>%
  left_join(select(paForestBurned2005, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2005, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2005.csv")
paForestLostAll2005 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2005.csv", row.names = 1)
paForestLostAll2005$landCoverCode <- factor(paForestLostAll2005$landCoverCode)

###2006
paForestLoss2006 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2006.csv")
paForestLoss2006[,44] <- NULL #remove the geometry column
paForestLoss2006 <- paForestLoss2006[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2006 <- gather(paForestLoss2006, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2006 <-
  paForestLoss2006 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2006 <- paForestLoss2006[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2006$year <- 2006
paForestLoss2006$key <- paste0(paForestLoss2006$system.index,"_lc",paForestLoss2006$landCoverCode)
paForestLoss2006 <- filter(paForestLoss2006, !is.na(landCoverCode))
write.csv(paForestLoss2006, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2006.csv")
paForestLoss2006 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2006.csv", row.names = 1)
paForestLoss2006$landCoverCode <- factor(paForestLoss2006$landCoverCode)


##Area burned, 2006
paForestBurned2006 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2006.csv")
paForestBurned2006[,44] <- NULL #remove the geometry column
paForestBurned2006 <- paForestBurned2006[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2006 <- gather(paForestBurned2006, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2006 <-
  paForestBurned2006 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2006 <- paForestBurned2006[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2006$year <- 2006
paForestBurned2006$key <- paste0(paForestBurned2006$system.index,"_lc",paForestBurned2006$landCoverCode)
paForestBurned2006 <- filter(paForestBurned2006, !is.na(landCoverCode))
write.csv(paForestBurned2006, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2006.csv")
paForestBurned2006 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2006.csv", row.names = 1)
paForestBurned2006$landCoverCode <- factor(paForestBurned2006$landCoverCode)

paForestLostAll2006 <- paForestLoss2006 %>%
  left_join(select(paForestBurned2006, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2006, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2006.csv")
paForestLostAll2006 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2006.csv", row.names = 1)
paForestLostAll2006$landCoverCode <- factor(paForestLostAll2006$landCoverCode)

###2007
paForestLoss2007 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2007.csv")
paForestLoss2007[,44] <- NULL #remove the geometry column
paForestLoss2007 <- paForestLoss2007[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2007 <- gather(paForestLoss2007, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2007 <-
  paForestLoss2007 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2007 <- paForestLoss2007[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2007$year <- 2007
paForestLoss2007$key <- paste0(paForestLoss2007$system.index,"_lc",paForestLoss2007$landCoverCode)
paForestLoss2007 <- filter(paForestLoss2007, !is.na(landCoverCode))
write.csv(paForestLoss2007, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2007.csv")
paForestLoss2007 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2007.csv", row.names = 1)
paForestLoss2007$landCoverCode <- factor(paForestLoss2007$landCoverCode)


##Area burned, 2007
paForestBurned2007 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2007.csv")
paForestBurned2007[,44] <- NULL #remove the geometry column
paForestBurned2007 <- paForestBurned2007[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2007 <- gather(paForestBurned2007, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2007 <-
  paForestBurned2007 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2007 <- paForestBurned2007[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2007$year <- 2007
paForestBurned2007$key <- paste0(paForestBurned2007$system.index,"_lc",paForestBurned2007$landCoverCode)
paForestBurned2007 <- filter(paForestBurned2007, !is.na(landCoverCode))
write.csv(paForestBurned2007, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2007.csv")
paForestBurned2007 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2007.csv", row.names = 1)
paForestBurned2007$landCoverCode <- factor(paForestBurned2007$landCoverCode)

paForestLostAll2007 <- paForestLoss2007 %>%
  left_join(select(paForestBurned2007, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2007, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2007.csv")
paForestLostAll2007 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2007.csv", row.names = 1)
paForestLostAll2007$landCoverCode <- factor(paForestLostAll2007$landCoverCode)

###2008
paForestLoss2008 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2008.csv")
paForestLoss2008[,44] <- NULL #remove the geometry column
paForestLoss2008 <- paForestLoss2008[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2008 <- gather(paForestLoss2008, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2008 <-
  paForestLoss2008 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2008 <- paForestLoss2008[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2008$year <- 2008
paForestLoss2008$key <- paste0(paForestLoss2008$system.index,"_lc",paForestLoss2008$landCoverCode)
paForestLoss2008 <- filter(paForestLoss2008, !is.na(landCoverCode))
write.csv(paForestLoss2008, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2008.csv")
paForestLoss2008 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2008.csv", row.names = 1)
paForestLoss2008$landCoverCode <- factor(paForestLoss2008$landCoverCode)


##Area burned, 2008
paForestBurned2008 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2008.csv")
paForestBurned2008[,44] <- NULL #remove the geometry column
paForestBurned2008 <- paForestBurned2008[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2008 <- gather(paForestBurned2008, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2008 <-
  paForestBurned2008 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2008 <- paForestBurned2008[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2008$year <- 2008
paForestBurned2008$key <- paste0(paForestBurned2008$system.index,"_lc",paForestBurned2008$landCoverCode)
paForestBurned2008 <- filter(paForestBurned2008, !is.na(landCoverCode))
write.csv(paForestBurned2008, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2008.csv")
paForestBurned2008 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2008.csv", row.names = 1)
paForestBurned2008$landCoverCode <- factor(paForestBurned2008$landCoverCode)

paForestLostAll2008 <- paForestLoss2008 %>%
  left_join(select(paForestBurned2008, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2008, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2008.csv")
paForestLostAll2008 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2008.csv", row.names = 1)
paForestLostAll2008$landCoverCode <- factor(paForestLostAll2008$landCoverCode)

###2009
paForestLoss2009 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2009.csv")
paForestLoss2009[,44] <- NULL #remove the geometry column
paForestLoss2009 <- paForestLoss2009[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2009 <- gather(paForestLoss2009, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2009 <-
  paForestLoss2009 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2009 <- paForestLoss2009[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2009$year <- 2009
paForestLoss2009$key <- paste0(paForestLoss2009$system.index,"_lc",paForestLoss2009$landCoverCode)
paForestLoss2009 <- filter(paForestLoss2009, !is.na(landCoverCode))
write.csv(paForestLoss2009, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2009.csv")
paForestLoss2009 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2009.csv", row.names = 1)
paForestLoss2009$landCoverCode <- factor(paForestLoss2009$landCoverCode)


##Area burned, 2009
paForestBurned2009 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2009.csv")
paForestBurned2009[,44] <- NULL #remove the geometry column
paForestBurned2009 <- paForestBurned2009[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2009 <- gather(paForestBurned2009, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2009 <-
  paForestBurned2009 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2009 <- paForestBurned2009[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2009$year <- 2009
paForestBurned2009$key <- paste0(paForestBurned2009$system.index,"_lc",paForestBurned2009$landCoverCode)
paForestBurned2009 <- filter(paForestBurned2009, !is.na(landCoverCode))
write.csv(paForestBurned2009, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2009.csv")
paForestBurned2009 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2009.csv", row.names = 1)
paForestBurned2009$landCoverCode <- factor(paForestBurned2009$landCoverCode)

paForestLostAll2009 <- paForestLoss2009 %>%
  left_join(select(paForestBurned2009, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2009, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2009.csv")
paForestLostAll2009 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2009.csv", row.names = 1)
paForestLostAll2009$landCoverCode <- factor(paForestLostAll2009$landCoverCode)

###2010
paForestLoss2010 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2010.csv")
paForestLoss2010[,44] <- NULL #remove the geometry column
paForestLoss2010 <- paForestLoss2010[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2010 <- gather(paForestLoss2010, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2010 <-
  paForestLoss2010 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2010 <- paForestLoss2010[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2010$year <- 2010
paForestLoss2010$key <- paste0(paForestLoss2010$system.index,"_lc",paForestLoss2010$landCoverCode)
paForestLoss2010 <- filter(paForestLoss2010, !is.na(landCoverCode))
write.csv(paForestLoss2010, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2010.csv")
paForestLoss2010 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2010.csv", row.names = 1)
paForestLoss2010$landCoverCode <- factor(paForestLoss2010$landCoverCode)


##Area burned, 2010
paForestBurned2010 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2010.csv")
paForestBurned2010[,44] <- NULL #remove the geometry column
paForestBurned2010 <- paForestBurned2010[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2010 <- gather(paForestBurned2010, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2010 <-
  paForestBurned2010 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2010 <- paForestBurned2010[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2010$year <- 2010
paForestBurned2010$key <- paste0(paForestBurned2010$system.index,"_lc",paForestBurned2010$landCoverCode)
paForestBurned2010 <- filter(paForestBurned2010, !is.na(landCoverCode))
write.csv(paForestBurned2010, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2010.csv")
paForestBurned2010 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2010.csv", row.names = 1)
paForestBurned2010$landCoverCode <- factor(paForestBurned2010$landCoverCode)

paForestLostAll2010 <- paForestLoss2010 %>%
  left_join(select(paForestBurned2010, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2010, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2010.csv")
paForestLostAll2010 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2010.csv", row.names = 1)
paForestLostAll2010$landCoverCode <- factor(paForestLostAll2010$landCoverCode)

###2011
paForestLoss2011 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2011.csv")
paForestLoss2011[,44] <- NULL #remove the geometry column
paForestLoss2011 <- paForestLoss2011[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2011 <- gather(paForestLoss2011, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2011 <-
  paForestLoss2011 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2011 <- paForestLoss2011[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2011$year <- 2011
paForestLoss2011$key <- paste0(paForestLoss2011$system.index,"_lc",paForestLoss2011$landCoverCode)
paForestLoss2011 <- filter(paForestLoss2011, !is.na(landCoverCode))
write.csv(paForestLoss2011, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2011.csv")
paForestLoss2011 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2011.csv", row.names = 1)
paForestLoss2011$landCoverCode <- factor(paForestLoss2011$landCoverCode)


##Area burned, 2011
paForestBurned2011 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2011.csv")
paForestBurned2011[,44] <- NULL #remove the geometry column
paForestBurned2011 <- paForestBurned2011[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2011 <- gather(paForestBurned2011, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2011 <-
  paForestBurned2011 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2011 <- paForestBurned2011[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2011$year <- 2011
paForestBurned2011$key <- paste0(paForestBurned2011$system.index,"_lc",paForestBurned2011$landCoverCode)
paForestBurned2011 <- filter(paForestBurned2011, !is.na(landCoverCode))
write.csv(paForestBurned2011, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2011.csv")
paForestBurned2011 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2011.csv", row.names = 1)
paForestBurned2011$landCoverCode <- factor(paForestBurned2011$landCoverCode)

paForestLostAll2011 <- paForestLoss2011 %>%
  left_join(select(paForestBurned2011, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2011, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2011.csv")
paForestLostAll2011 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2011.csv", row.names = 1)
paForestLostAll2011$landCoverCode <- factor(paForestLostAll2011$landCoverCode)

###2012
paForestLoss2012 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2012.csv")
paForestLoss2012[,44] <- NULL #remove the geometry column
paForestLoss2012 <- paForestLoss2012[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2012 <- gather(paForestLoss2012, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2012 <-
  paForestLoss2012 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2012 <- paForestLoss2012[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2012$year <- 2012
paForestLoss2012$key <- paste0(paForestLoss2012$system.index,"_lc",paForestLoss2012$landCoverCode)
paForestLoss2012 <- filter(paForestLoss2012, !is.na(landCoverCode))
write.csv(paForestLoss2012, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2012.csv")
paForestLoss2012 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2012.csv", row.names = 1)
paForestLoss2012$landCoverCode <- factor(paForestLoss2012$landCoverCode)


##Area burned, 2012
paForestBurned2012 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2012.csv")
paForestBurned2012[,44] <- NULL #remove the geometry column
paForestBurned2012 <- paForestBurned2012[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2012 <- gather(paForestBurned2012, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2012 <-
  paForestBurned2012 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2012 <- paForestBurned2012[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2012$year <- 2012
paForestBurned2012$key <- paste0(paForestBurned2012$system.index,"_lc",paForestBurned2012$landCoverCode)
paForestBurned2012 <- filter(paForestBurned2012, !is.na(landCoverCode))
write.csv(paForestBurned2012, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2012.csv")
paForestBurned2012 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2012.csv", row.names = 1)
paForestBurned2012$landCoverCode <- factor(paForestBurned2012$landCoverCode)

paForestLostAll2012 <- paForestLoss2012 %>%
  left_join(select(paForestBurned2012, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2012, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2012.csv")
paForestLostAll2012 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2012.csv", row.names = 1)
paForestLostAll2012$landCoverCode <- factor(paForestLostAll2012$landCoverCode)

###2013
paForestLoss2013 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2013.csv")
paForestLoss2013[,44] <- NULL #remove the geometry column
paForestLoss2013 <- paForestLoss2013[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2013 <- gather(paForestLoss2013, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2013 <-
  paForestLoss2013 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2013 <- paForestLoss2013[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2013$year <- 2013
paForestLoss2013$key <- paste0(paForestLoss2013$system.index,"_lc",paForestLoss2013$landCoverCode)
paForestLoss2013 <- filter(paForestLoss2013, !is.na(landCoverCode))
write.csv(paForestLoss2013, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2013.csv")
paForestLoss2013 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2013.csv", row.names = 1)
paForestLoss2013$landCoverCode <- factor(paForestLoss2013$landCoverCode)


##Area burned, 2013
paForestBurned2013 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2013.csv")
paForestBurned2013[,44] <- NULL #remove the geometry column
paForestBurned2013 <- paForestBurned2013[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2013 <- gather(paForestBurned2013, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2013 <-
  paForestBurned2013 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2013 <- paForestBurned2013[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2013$year <- 2013
paForestBurned2013$key <- paste0(paForestBurned2013$system.index,"_lc",paForestBurned2013$landCoverCode)
paForestBurned2013 <- filter(paForestBurned2013, !is.na(landCoverCode))
write.csv(paForestBurned2013, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2013.csv")
paForestBurned2013 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2013.csv", row.names = 1)
paForestBurned2013$landCoverCode <- factor(paForestBurned2013$landCoverCode)

paForestLostAll2013 <- paForestLoss2013 %>%
  left_join(select(paForestBurned2013, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2013, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2013.csv")
paForestLostAll2013 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2013.csv", row.names = 1)
paForestLostAll2013$landCoverCode <- factor(paForestLostAll2013$landCoverCode)

###2014
paForestLoss2014 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2014.csv")
paForestLoss2014[,44] <- NULL #remove the geometry column
paForestLoss2014 <- paForestLoss2014[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2014 <- gather(paForestLoss2014, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2014 <-
  paForestLoss2014 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2014 <- paForestLoss2014[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2014$year <- 2014
paForestLoss2014$key <- paste0(paForestLoss2014$system.index,"_lc",paForestLoss2014$landCoverCode)
paForestLoss2014 <- filter(paForestLoss2014, !is.na(landCoverCode))
write.csv(paForestLoss2014, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2014.csv")
paForestLoss2014 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2014.csv", row.names = 1)
paForestLoss2014$landCoverCode <- factor(paForestLoss2014$landCoverCode)


##Area burned, 2014
paForestBurned2014 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2014.csv")
paForestBurned2014[,44] <- NULL #remove the geometry column
paForestBurned2014 <- paForestBurned2014[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2014 <- gather(paForestBurned2014, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2014 <-
  paForestBurned2014 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2014 <- paForestBurned2014[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2014$year <- 2014
paForestBurned2014$key <- paste0(paForestBurned2014$system.index,"_lc",paForestBurned2014$landCoverCode)
paForestBurned2014 <- filter(paForestBurned2014, !is.na(landCoverCode))
write.csv(paForestBurned2014, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2014.csv")
paForestBurned2014 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2014.csv", row.names = 1)
paForestBurned2014$landCoverCode <- factor(paForestBurned2014$landCoverCode)

paForestLostAll2014 <- paForestLoss2014 %>%
  left_join(select(paForestBurned2014, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2014, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2014.csv")
paForestLostAll2014 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2014.csv", row.names = 1)
paForestLostAll2014$landCoverCode <- factor(paForestLostAll2014$landCoverCode)

###2015
paForestLoss2015 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2015.csv")
paForestLoss2015[,44] <- NULL #remove the geometry column
paForestLoss2015 <- paForestLoss2015[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2015 <- gather(paForestLoss2015, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2015 <-
  paForestLoss2015 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2015 <- paForestLoss2015[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2015$year <- 2015
paForestLoss2015$key <- paste0(paForestLoss2015$system.index,"_lc",paForestLoss2015$landCoverCode)
paForestLoss2015 <- filter(paForestLoss2015, !is.na(landCoverCode))
write.csv(paForestLoss2015, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2015.csv")
paForestLoss2015 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2015.csv", row.names = 1)
paForestLoss2015$landCoverCode <- factor(paForestLoss2015$landCoverCode)


##Area burned, 2015
paForestBurned2015 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2015.csv")
paForestBurned2015[,44] <- NULL #remove the geometry column
paForestBurned2015 <- paForestBurned2015[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2015 <- gather(paForestBurned2015, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2015 <-
  paForestBurned2015 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2015 <- paForestBurned2015[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2015$year <- 2015
paForestBurned2015$key <- paste0(paForestBurned2015$system.index,"_lc",paForestBurned2015$landCoverCode)
paForestBurned2015 <- filter(paForestBurned2015, !is.na(landCoverCode))
write.csv(paForestBurned2015, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2015.csv")
paForestBurned2015 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2015.csv", row.names = 1)
paForestBurned2015$landCoverCode <- factor(paForestBurned2015$landCoverCode)

paForestLostAll2015 <- paForestLoss2015 %>%
  left_join(select(paForestBurned2015, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2015, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2015.csv")
paForestLostAll2015 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2015.csv", row.names = 1)
paForestLostAll2015$landCoverCode <- factor(paForestLostAll2015$landCoverCode)

###2016
paForestLoss2016 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001/paForestLossOutput2016.csv")
paForestLoss2016[,44] <- NULL #remove the geometry column
paForestLoss2016 <- paForestLoss2016[-c(1),] #remove the empty first line that had to be created in GEE
paForestLoss2016 <- gather(paForestLoss2016, key = 'landCover', value = 'sqKmLost',2:33)
paForestLoss2016 <-
  paForestLoss2016 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))

paForestLoss2016 <- paForestLoss2016[,-c(2, 5, 6, 7, 11, 12)]
paForestLoss2016$year <- 2016
paForestLoss2016$key <- paste0(paForestLoss2016$system.index,"_lc",paForestLoss2016$landCoverCode)
paForestLoss2016 <- filter(paForestLoss2016, !is.na(landCoverCode))
write.csv(paForestLoss2016, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2016.csv")
paForestLoss2016 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLoss2016.csv", row.names = 1)
paForestLoss2016$landCoverCode <- factor(paForestLoss2016$landCoverCode)


##Area burned, 2016
paForestBurned2016 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurnedOutput2016.csv")
paForestBurned2016[,44] <- NULL #remove the geometry column
paForestBurned2016 <- paForestBurned2016[-c(1),] #remove the empty first line that had to be created in GEE
paForestBurned2016 <- gather(paForestBurned2016, key = 'landCover', value = 'sqKmBurned',2:33)
paForestBurned2016 <-
  paForestBurned2016 %>%
  mutate(landCoverCode = ifelse(landCover == "X1", 1,
                                ifelse(landCover == "X2", 2,
                                       ifelse(landCover == "X4", 4,
                                              ifelse(landCover == "X6", 6,
                                                     ifelse(landCover == "X8", 8,
                                                            ifelse(landCover == "X9", 9,
                                                                   ifelse(landCover == "X12", 12,
                                                                          ifelse(landCover == "X13", 13,
                                                                                 ifelse(landCover == "X14", 14,
                                                                                        ifelse(landCover == "X24", 24,
                                                                                               ifelse(landCover == "X33", 33,
                                                                                                      ifelse(landCover == "X26", 26,
                                                                                                             ifelse(landCover == "X16", 16,
                                                                                                                    ifelse(landCover == "X20", 20,
                                                                                                                           ifelse(landCover == "X21", 21,
                                                                                                                                  ifelse(landCover == "X39", 39,
                                                                                                                                         ifelse(landCover == "X38", 38,
                                                                                                                                                ifelse(landCover == "X37", 37,
                                                                                                                                                       ifelse(landCover == "X53", 53,
                                                                                                                                                              ifelse(landCover == "X51", 51,
                                                                                                                                                                     ifelse(landCover == "X52", 52,
                                                                                                                                                                            ifelse(landCover == "X40", 40,
                                                                                                                                                                                   ifelse(landCover == "X59", 59,
                                                                                                                                                                                          ifelse(landCover == "X27", 27,
                                                                                                                                                                                                 ifelse(landCover == "X35", 35,
                                                                                                                                                                                                        ifelse(landCover == "X72", 72,
                                                                                                                                                                                                               ifelse(landCover == "X65", 65,
                                                                                                                                                                                                                      ifelse(landCover == "X67",67,
                                                                                                                                                                                                                             ifelse(landCover == "X66",66,
                                                                                                                                                                                                                                    ifelse(landCover == "X70", 70, "NA")))))))))))))))))))))))))))))))



paForestBurned2016 <- paForestBurned2016[,-c(2, 5, 6, 7, 11, 12)]
paForestBurned2016$year <- 2016
paForestBurned2016$key <- paste0(paForestBurned2016$system.index,"_lc",paForestBurned2016$landCoverCode)
paForestBurned2016 <- filter(paForestBurned2016, !is.na(landCoverCode))
write.csv(paForestBurned2016, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2016.csv")
paForestBurned2016 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestBurned2016.csv", row.names = 1)
paForestBurned2016$landCoverCode <- factor(paForestBurned2016$landCoverCode)

paForestLostAll2016 <- paForestLoss2016 %>%
  left_join(select(paForestBurned2016, sqKmBurned, key), by = "key")
write.csv(paForestLostAll2016, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2016.csv")
paForestLostAll2016 <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAll2016.csv", row.names = 1)
paForestLostAll2016$landCoverCode <- factor(paForestLostAll2016$landCoverCode)
                                                                                                                                                                                                                                    
paForestLostAllYears <- rbind(paForestLostAll2001,paForestLostAll2002,paForestLostAll2003,paForestLostAll2003,paForestLostAll2004,
                              paForestLostAll2005, paForestLostAll2006, paForestLostAll2007, paForestLostAll2008,
                              paForestLostAll2009, paForestLostAll2010, paForestLostAll2011, paForestLostAll2012,
                              paForestLostAll2013, paForestLostAll2014, paForestLostAll2015, paForestLostAll2016)

paForestLostAllYears <-
  paForestLostAllYears %>%
  mutate(landCoverCategory = ifelse(landCoverCode == 1, "Forest",
                                    ifelse(landCoverCode == 2, "Forest",
                                           ifelse(landCoverCode == 4, "Forest",
                                                  ifelse(landCoverCode == 6, "Forest",
                                                         ifelse(landCoverCode == 8, "Forest",
                                                                ifelse(landCoverCode == 9, "Forest",
                                                                       ifelse(landCoverCode == 12, "Wetland",
                                                                              ifelse(landCoverCode == 13, "Wetland",
                                                                                     ifelse(landCoverCode == 14, "Wetland",
                                                                                            ifelse(landCoverCode == 24, "Wetland",
                                                                                                   ifelse(landCoverCode == 33, "Wetland",
                                                                                                          ifelse(landCoverCode == 26, "Wetland",
                                                                                                                 ifelse(landCoverCode == 16, "Shrubland",
                                                                                                                        ifelse(landCoverCode == 20, "Shrubland",
                                                                                                                               ifelse(landCoverCode == 21, "Shrubland",
                                                                                                                                      ifelse(landCoverCode == 39, "Cultivated",
                                                                                                                                             ifelse(landCoverCode == 38, "Cultivated",
                                                                                                                                                    ifelse(landCoverCode == 37, "Cultivated",
                                                                                                                                                           ifelse(landCoverCode == 53, "Cultivated",
                                                                                                                                                                  ifelse(landCoverCode == 51, "Cultivated",
                                                                                                                                                                         ifelse(landCoverCode == 52, "Cultivated",
                                                                                                                                                                                ifelse(landCoverCode == 40, "Cultivated",
                                                                                                                                                                                       ifelse(landCoverCode == 59, "Cultivated",
                                                                                                                                                                                              ifelse(landCoverCode == 27, "Grassland",
                                                                                                                                                                                                     ifelse(landCoverCode == 35, "Unvegetated",
                                                                                                                                                                                                            ifelse(landCoverCode == 72, "Unvegetated",
                                                                                                                                                                                                                   ifelse(landCoverCode == 65, "Unvegetated",
                                                                                                                                                                                                                          ifelse(landCoverCode == 67, "Unvegetated",
                                                                                                                                                                                                                                 ifelse(landCoverCode == 66, "Unvegetated",
                                                                                                                                                                                                                                        ifelse(landCoverCode == 70, "Unvegetated", "NA")))))))))))))))))))))))))))))))

paForestLostAllYears <-
  paForestLostAllYears %>%
  mutate(landCoverNameSp = ifelse(landCoverCode == 1, "Bosque conifero denso",
                                  ifelse(landCoverCode == 2, "Bosque conifero abierto",
                                         ifelse(landCoverCode == 4, "Bosque latifoliado nublado",
                                                ifelse(landCoverCode == 6, "Bosque latifoliado humedo",
                                                       ifelse(landCoverCode == 8, "Bosque latifoliado semi-humedo",
                                                              ifelse(landCoverCode == 9, "Bosque seco",
                                                                     ifelse(landCoverCode == 12, "Bosque humedales salobres temporalmente inundado",
                                                                            ifelse(landCoverCode == 13, "Bosque humedales salobres permanentemente inundado",
                                                                                   ifelse(landCoverCode == 14, "Bosque humedales de agua dulce",
                                                                                          ifelse(landCoverCode == 24, "Sabana de humedales salobres",
                                                                                                 ifelse(landCoverCode == 33, "Eneal",
                                                                                                        ifelse(landCoverCode == 26, "Sabana humedales de agua aulce",
                                                                                                               ifelse(landCoverCode == 16, "Matorral latifoliado",
                                                                                                                      ifelse(landCoverCode == 20, "Matorral seco",
                                                                                                                             ifelse(landCoverCode == 21, "Matorral de humedales salobres",
                                                                                                                                    ifelse(landCoverCode == 39, "Arroz",
                                                                                                                                           ifelse(landCoverCode == 38, "Caña",
                                                                                                                                                  ifelse(landCoverCode == 37, "Cultivos intensivos",
                                                                                                                                                         ifelse(landCoverCode == 53, "Café y Cacao",
                                                                                                                                                                ifelse(landCoverCode == 51, "Palma Africana",
                                                                                                                                                                       ifelse(landCoverCode == 52, "Palma de Coco",
                                                                                                                                                                              ifelse(landCoverCode == 40, "Pasto",
                                                                                                                                                                                     ifelse(landCoverCode == 59, "Agricultura de Subsistencia y Pasto",
                                                                                                                                                                                            ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                   ifelse(landCoverCode == 35, "Escasa vegetación o area erosionada",
                                                                                                                                                                                                          ifelse(landCoverCode == 72, "Areas pobladas",
                                                                                                                                                                                                                 ifelse(landCoverCode == 65, "Lagunos o lagos",
                                                                                                                                                                                                                        ifelse(landCoverCode == 67, "Agua semi-profunda",
                                                                                                                                                                                                                               ifelse(landCoverCode == 66, "Agua profunda",
                                                                                                                                                                                                                                      ifelse(landCoverCode == 70, "Agua abierta baja", "NA")))))))))))))))))))))))))))))))



paForestLostAllYears <-
  paForestLostAllYears %>%
  mutate(landCoverName = ifelse(landCoverCode == 1, "Dense conifer forest",
                                ifelse(landCoverCode == 2, "Open conifer forest",
                                       ifelse(landCoverCode == 4, "Wet broadleaf forest",
                                              ifelse(landCoverCode == 6, "Moist broadleaf forest",
                                                     ifelse(landCoverCode == 8, "Semi-moist broadleaf forest",
                                                            ifelse(landCoverCode == 9, "Dry forest",
                                                                   ifelse(landCoverCode == 12, "Temporarily flooded forested brackish wetlands",
                                                                          ifelse(landCoverCode == 13, "Permanently flooded forested brackish wetlands",
                                                                                 ifelse(landCoverCode == 14, "Forested freshwater wetlands",
                                                                                        ifelse(landCoverCode == 24, "Grassy brackish wetlands",
                                                                                               ifelse(landCoverCode == 33, "Cattail marsh",
                                                                                                      ifelse(landCoverCode == 26, "Grassy freshwater marsh",
                                                                                                             ifelse(landCoverCode == 16, "Broadleaf matorral",
                                                                                                                    ifelse(landCoverCode == 20, "Dry matorral",
                                                                                                                           ifelse(landCoverCode == 21, "Brackish wetland matorral",
                                                                                                                                  ifelse(landCoverCode == 39, "Rice",
                                                                                                                                         ifelse(landCoverCode == 38, "Sugar cane",
                                                                                                                                                ifelse(landCoverCode == 37, "Cultivated land",
                                                                                                                                                       ifelse(landCoverCode == 53, "Coffee and Cacao",
                                                                                                                                                              ifelse(landCoverCode == 51, "African palm",
                                                                                                                                                                     ifelse(landCoverCode == 52, "Coco Palm",
                                                                                                                                                                            ifelse(landCoverCode == 40, "Pasture",
                                                                                                                                                                                   ifelse(landCoverCode == 59, "Subsistence agriculture or pasture",
                                                                                                                                                                                          ifelse(landCoverCode == 27, "Sabana de pajon",
                                                                                                                                                                                                 ifelse(landCoverCode == 35, "Unvegetated or eroded areas",
                                                                                                                                                                                                        ifelse(landCoverCode == 72, "Populated areas",
                                                                                                                                                                                                               ifelse(landCoverCode == 65, "Lake",
                                                                                                                                                                                                                      ifelse(landCoverCode == 67, "Ocean",
                                                                                                                                                                                                                             ifelse(landCoverCode == 66, "Ocean",
                                                                                                                                                                                                                                    ifelse(landCoverCode == 70, "Ocean", "NA")))))))))))))))))))))))))))))))




paForestLostAllYears <-
  paForestLostAllYears  %>%
  mutate(landCoverCategorySp = ifelse(landCoverCategory == "Forest", "Bosque",
                                      ifelse(landCoverCategory == "Shrubland", "Matorrales",
                                             ifelse(landCoverCategory == "Wetland", "Humedales",
                                                    ifelse(landCoverCategory == "Grassland", "Pastos",
                                                           ifelse(landCoverCategory == "Unvegetated", "Escasa vegetacion",
                                                                  ifelse(landCoverCategory == "Cultivated", "Cultivos","NA")))))))


write.csv(paForestLostAllYears, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAllYears.csv")
paForestLostAllYears <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paForestLostAllYears.csv", row.names = 1)
paForestLostAllYears$landCoverCode <- factor(paForestLostAllYears$landCoverCode)

paAreaLC12000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '1') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC12000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC12000.csv")

paAreaLC22000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '2') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC22000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC22000.csv")
paAreaLC42000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '4') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC42000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC42000.csv")
paAreaLC62000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '6') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC62000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC62000.csv")
paAreaLC82000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '8') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC82000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC82000.csv")
paAreaLC92000 <- 
  paForestArea2000 %>%
  filter(landCoverCode == '9') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
write.csv(paAreaLC92000, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/paAreaLC92000.csv")

paAreaLatifoliado2000 <- rbind(paAreaLC42000, paAreaLC62000, paAreaLC82000)

paAreaLC42000 %>%
  filter(sqKm2000 > 1) %>%
ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc,  fill = IUCNCat)) + geom_col() +
  theme(axis.text.x = element_text(angle = 340)) + guides(fill=guide_legend(title="IUCN Category")) +
  xlab("Protected area") + ylab("Proportion of protected bosque nublado")


## Incorporating fire
areaBurnedAll <- fromJSON("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drLCLossToFire.csv", simplifyDataFrame = T)
colnames(areaBurnedAll)[2] <- 'sqKmBurned'
areaBurnedAll$landCoverCode <- factor(areaBurnedAll$landCoverCode)
forestChange <- inner_join(forestChange,areaBurnedAll, by = "landCoverCode")
forestChange <-
  forestChange %>%
  mutate(percentLossToBurn = (sqKmBurned/sqKmLost)*100)
write.csv(forestChange, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv")
forestChange <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestChange.csv", row.names = 1)
forestChange$landCoverCode <- factor(forestChange$landCoverCode)



setwd("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/drive-download-20180516T202531Z-001")
bu2001 <- fromJSON("drForestBurned2001.csv", simplifyDataFrame = T)
bu2003 <- fromJSON("drForestBurned2003.csv", simplifyDataFrame = T)
bu2003 <- fromJSON("drForestBurned2003.csv", simplifyDataFrame = T)
bu2004 <- fromJSON("drForestBurned2004.csv", simplifyDataFrame = T)
bu2005 <- fromJSON("drForestBurned2005.csv", simplifyDataFrame = T)
bu2006 <- fromJSON("drForestBurned2006.csv", simplifyDataFrame = T)
bu2007 <- fromJSON("drForestBurned2007.csv", simplifyDataFrame = T)
bu2008 <- fromJSON("drForestBurned2008.csv", simplifyDataFrame = T)
bu2009 <- fromJSON("drForestBurned2009.csv", simplifyDataFrame = T)
bu2010 <- fromJSON("drForestBurned2010.csv", simplifyDataFrame = T)
bu2011 <- fromJSON("drForestBurned2011.csv", simplifyDataFrame = T)
bu2012 <- fromJSON("drForestBurned2012.csv", simplifyDataFrame = T)
bu2013 <- fromJSON("drForestBurned2013.csv", simplifyDataFrame = T)
bu2014 <- fromJSON("drForestBurned2014.csv", simplifyDataFrame = T)
bu2015 <- fromJSON("drForestBurned2015.csv", simplifyDataFrame = T)
bu2016 <- fromJSON("drForestBurned2016.csv", simplifyDataFrame = T)

#Add a year variable to each file

bu2001$year <- 2001
bu2003$year <- 2003
bu2003$year <- 2003
bu2004$year <- 2004
bu2005$year <- 2005
bu2006$year <- 2006
bu2007$year <- 2007
bu2008$year <- 2008
bu2009$year <- 2009
bu2010$year <- 2010
bu2011$year <- 2011
bu2012$year <- 2012
bu2013$year <- 2013
bu2014$year <- 2014
bu2015$year <- 2015
bu2016$year <- 2016

#combine the files for each year into a single file:
lcBurnedAnnually <- rbind(bu2001,bu2003,bu2003, bu2004,bu2005,bu2006,bu2007,bu2008,bu2009,bu2010,bu2011,bu2012,bu2013,bu2014,bu2015,bu2016)
#Change the column name ('sum') to something meaningful:
colnames(lcBurnedAnnually)[2] <- "sqKmBurned"
lcBurnedAnnually$landCoverCode <- factor(lcBurnedAnnually$landCoverCode)
#Clean up yearly files:
rm(bu2001,bu2003,bu2003, bu2004,bu2005,bu2006,bu2007,bu2008,bu2009,bu2010,bu2011,bu2012,bu2013,bu2014,bu2015,bu2016)
write.csv(lcBurnedAnnually, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/lcBurnedAnnually.csv")
lcBurnedAnnually <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/lcBurnedAnnually.csv", row.names = 1)

## Most area burned was in 2005
lcBurnedAnnually %>%
  filter(landCoverCode == 1|landCoverCode == 2|landCoverCode == 4|landCoverCode == 6|landCoverCode == 8|landCoverCode == 9)%>%
  ggplot(., aes(y = sqKmBurned, x = year)) + geom_line() + facet_wrap(~landCoverCode)

forestLossWeighted$landCoverCode <- factor(forestLossWeighted$landCoverCode)
forestLossWeighted <- cbind(forestLossWeighted, lcBurnedAnnually$sqKmBurned)
colnames(forestLossWeighted)[7] <- 'sqKmBurned'
forestLossWeighted <-
  forestLossWeighted %>%
  mutate(sqKmLossNonFire = sqKmLost - sqKmBurned)
write.csv(forestLossWeighted, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestLossWeighted.csv")
forestLossWeighted <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestLossWeighted.csv", row.names = 1)
forestLossWeighted$landCoverCode <- factor(forestLossWeighted$landCoverCode)

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKm,0))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2000])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2001]),0)))
forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse (year == 2003, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2001])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2003]),0))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2003])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2003]),0)))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2003])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2004]),0))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2004])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2005]),0)))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2005])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2006]),0))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2006])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2007]),0)))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2007])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2008]),0))))))))))
                                                                       
forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2008])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2009]),0)))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2009])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2010]),0))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2010])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2011]),0)))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, forestAreaAnnual$sqKmNoFire,
                                                                                                          ifelse(year == 2012, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2011])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2012]),0))))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, forestAreaAnnual$sqKmNoFire,
                                                                                                          ifelse(year == 2012, forestAreaAnnual$sqKmNoFire,
                                                                                                                 ifelse(year == 2013, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2012])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2013]),0)))))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, forestAreaAnnual$sqKmNoFire,
                                                                                                          ifelse(year == 2012, forestAreaAnnual$sqKmNoFire,
                                                                                                                 ifelse(year == 2013, forestAreaAnnual$sqKmNoFire,
                                                                                                                        ifelse(year == 2014, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2013])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2014]),0))))))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, forestAreaAnnual$sqKmNoFire,
                                                                                                          ifelse(year == 2012, forestAreaAnnual$sqKmNoFire,
                                                                                                                 ifelse(year == 2013, forestAreaAnnual$sqKmNoFire,
                                                                                                                        ifelse(year == 2014, forestAreaAnnual$sqKmNoFire,
                                                                                                                               ifelse(year == 2015, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2014])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2015]),0)))))))))))))))))

forestAreaAnnual <-
  forestAreaAnnual %>%
  mutate(sqKmNoFire = ifelse(year == 2000, forestAreaAnnual$sqKmNoFire,
                             ifelse(year == 2001,forestAreaAnnual$sqKmNoFire,
                                    ifelse(year == 2003, forestAreaAnnual$sqKmNoFire,
                                           ifelse(year == 2003,forestAreaAnnual$sqKmNoFire,
                                                  ifelse(year == 2004, forestAreaAnnual$sqKmNoFire,
                                                         ifelse(year == 2005, forestAreaAnnual$sqKmNoFire,
                                                                ifelse(year == 2006, forestAreaAnnual$sqKmNoFire,
                                                                       ifelse(year == 2007,forestAreaAnnual$sqKmNoFire,
                                                                              ifelse(year == 2008, forestAreaAnnual$sqKmNoFire,
                                                                                     ifelse(year == 2009, forestAreaAnnual$sqKmNoFire,
                                                                                            ifelse(year == 2010, forestAreaAnnual$sqKmNoFire,
                                                                                                   ifelse(year == 2011, forestAreaAnnual$sqKmNoFire,
                                                                                                          ifelse(year == 2012, forestAreaAnnual$sqKmNoFire,
                                                                                                                 ifelse(year == 2013, forestAreaAnnual$sqKmNoFire,
                                                                                                                        ifelse(year == 2014, forestAreaAnnual$sqKmNoFire,
                                                                                                                               ifelse(year == 2015, forestAreaAnnual$sqKmNoFire,
                                                                                                                                      ifelse(year == 2016, as.numeric(forestAreaAnnual$sqKmNoFire[forestAreaAnnual$year == 2015])-as.numeric(forestLossWeighted$sqKmLossNonFire[forestLossWeighted$year == 2016]),0))))))))))))))))))

write.csv(forestAreaAnnual, "/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestAreaAnnual.csv")
forestAreaAnnual <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/EarthEngineOutput/forestAreaAnnual.csv", row.names = 1)
forestAreaAnnual$landCoverCode <- factor(forestAreaAnnual$landCoverCode)

forestAreaAnnual %>%
gather(., key = "lossType", value = "sqKm", sqKm, sqKmNoFire) %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(., aes(x = year, y = sqKm, color = lossType)) + geom_line() + facet_wrap(~landCoverName, scales = "free_y") + 
  xlab("Year") + ylab("Area covered (sq. km)") + scale_color_discrete(name = "Source of forest loss",
                         labels = c("All sources","All sources except fire"))
##Annual loss
png(filename = "figureAnnualLoss.png", width = 8, height = 8, units = "in",
    res = 300)  
forestAreaAnnual %>%
  filter(landCoverName == "Dense conifer forest"|landCoverName == "Open conifer forest"|landCoverName == "Wet broadleaf forest"|
           landCoverName == "Moist broadleaf forest"|landCoverName == "Semi-moist broadleaf forest"|landCoverName == "Dry forest")%>%
  mutate(labelName = ifelse(landCoverName == "Dense conifer forest", "Closed-canopy pine", 
                            ifelse(landCoverName == "Open conifer forest", "Open-canopy pine",
                                   ifelse(landCoverName == "Dry forest", "Dry", 
                                          ifelse(landCoverName == "Moist broadleaf forest", "Moist broadleaf",
                                                 ifelse(landCoverName == "Semi-moist broadleaf forest", "Semi-moist broadleaf",
                                                        ifelse(landCoverName == "Wet broadleaf forest", "Cloud", "NA"))))))) %>%
  mutate(labelName = factor(labelName, levels = c("Closed-canopy pine", "Open-canopy pine", "Cloud", "Moist broadleaf",
                                                  "Semi-moist broadleaf", "Dry"))) %>%
  group_by(labelName) %>%
  mutate(annualLoss = abs(sqKm - lag(sqKm))) %>%
  ggplot(., aes(x = year, y = annualLoss)) + geom_line() + facet_wrap(~labelName, scales = "free_y") + geom_smooth() + 
  labs(x = "Year", y = expression(paste("Forest area (", "km"^"2",")", " lost"))) + 
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18))
dev.off()

##Plotting it as forest lost, instead
png(filename = "Figure3.png", width = 8, height = 8, units = "in",
    res = 300)
forestAreaAnnual %>%
  group_by(landCoverCode) %>%
  mutate(year2000Extent = max(sqKm)) %>%
  group_by(landCoverCode, year) %>%
  mutate(cumLossAll = year2000Extent - sqKm,
         cumLossNoFire = year2000Extent - sqKmNoFire) %>%
  mutate(labelName = ifelse(landCoverName == "Dense conifer forest", "Closed-canopy pine", 
                            ifelse(landCoverName == "Open conifer forest", "Open-canopy pine",
                                   ifelse(landCoverName == "Dry forest", "Dry", 
                                          ifelse(landCoverName == "Moist broadleaf forest", "Moist broadleaf",
                                                 ifelse(landCoverName == "Semi-moist broadleaf forest", "Semi-moist broadleaf",
                                                        ifelse(landCoverName == "Wet broadleaf forest", "Cloud", "NA"))))))) %>%
  mutate(labelName = factor(labelName, levels = c("Closed-canopy pine", "Open-canopy pine", "Cloud", "Moist broadleaf",
                                                  "Semi-moist broadleaf", "Dry"))) %>%
  gather(., key = "lossType", value = "sqKm", cumLossAll, cumLossNoFire) %>%
  filter(landCoverCategory == "Forest") %>%
  ggplot(., aes(x = year, y = sqKm, color = lossType)) + geom_line() + facet_wrap(~labelName, scales = "free_y") + 
  labs(x = "Year", y = expression(paste("Cumulative area (", "km"^"2",")", " of forest lost"))) + scale_color_manual(name = "Source of forest loss",
                                                                      labels = c("All sources including fire","All sources except fire"),
                                                                      values = c("#E69F00", "#56B4E9")) + 
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18))
dev.off()

lcBurnedAnnually %>%
  filter(year == 2015) %>%
  group_by(landCoverCode) %>%
  summarize(sum = sum(sqKmBurned))

# Grouped
plot1 <-
forestChange %>%
  select(landCoverName, sqKm2000, area2016) %>%
  filter(landCoverName == "Dense conifer forest"|landCoverName == "Open conifer forest"|landCoverName == "Wet broadleaf forest"|
           landCoverName == "Moist broadleaf forest"|landCoverName == "Semi-moist broadleaf forest"|landCoverName == "Dry forest")%>%
  gather(., key = "Year", value = "Area", sqKm2000, area2016)

tiff(filename = "/Users/johnlloyd/Documents/GitHub/drForestChange/Figure1.tiff", width = 8, height = 6, units = "in", res = 300)
plot1 %>%
mutate(Year = ifelse(Year == "sqKm2000", "2000", "2016"))%>%
ggplot(., aes(fill=Year, y=Area, x=landCoverName)) +
  geom_bar(position="dodge", stat="identity") + scale_fill_grey(start = 0.4, end = 0.7) + theme_minimal() + theme(axis.text.x = element_text(angle = 340,
                                                                                                                                             vjust = 0.5),
                                                                                                                  axis.title.x = element_text(size = 18),
                                                                                                                  axis.title.y = element_text(size = 18),
                                                                                                                  axis.text.x.bottom = element_text(size = 10),
                                                                                                                  axis.text.y = element_text(size = 10)) + 
  xlab("Forest type") + ylab("Areal extent (sq. km)")

dev.off()

## Add variable for burned area within PAs, this taken from GEE analysis output:
forestChange$sqKmBurnedInPA <- c(73.12443770213224,36.824172388283,14.99157645569296,9.78010494840113,1.341022439299461,1.367332007870425,0.00820571890079274,0.10966606166848572,0,4.279997609192625,1.306993234085151,0.006794183349609374,0.004229908081054687,0.0008462817993164063,1.8013864541625972,0.0033859031982421874,0.08476447845458986,0.7949086199340815,0.01841126232359643,0,0.011781539548387714
                    ,0.0016922805175781249,0.04143538415527343,1.2763473043591058,9.952640274183805,0.016871465942382816,0.0008490761108398437)

tiff(filename = "/Users/johnlloyd/Documents/GitHub/drForestChange/Figure2.tiff", width = 6, height = 8, units = "in", res = 300)
panelA <- paAreaLC12000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Proportion of protected dense conifer") + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.035), size = 4) + scale_x_discrete(labels = c("Armando Bermúdez","Carmen Ramírez","Vallue Nuevo")) + 
  annotate("text", x = 0.75, y = 0.79, label = "A", size = 6)

panelB <- paAreaLC22000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Proportion of protected open conifer") + 
  scale_x_discrete(labels = c("Bahoruco","Carmen Ramírez","Vallue Nuevo")) + annotate("text", x = 0.75, y = 0.79, label = "B", size = 6) + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.035), size = 4) 

panelC <- paAreaLC42000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Proportion of protected cloud forest") + 
  scale_x_discrete(labels = c("Valle Nuevo","Armando Bermúdez",
                                "Bahoruco","Neiba")) + geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.035), size = 4) + 
  annotate("text", x = 0.75, y = 0.79, label = "C", size = 6)

panelD <- paAreaLC62000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc, group = 1)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6), axis.text.x.bottom = element_text(size = 6)) + xlab("Protected area") + ylab("Cumulative proportion of protected moist broadleaf forest") + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.022), size = 2) + scale_x_discrete(labels = c("Los Haitises","La Humeadora",
                                                                                                                 "Armando Bermúdez","Bahoruco",
                                                                                                                 "Martín Garcia","Luis Quin",
                                                                                                                 "Valle Nuevo", "Quita Espuela",
                                                                                                                 "Tavárez Justo","Carmen Ramírez",
                                                                                                                 "Caamaño","Hatillo")) + 
  annotate("text", x = 1.5, y = 0.79, label = "D", size = 6)

panelE <- paAreaLC82000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc, group = 1)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Cumulative proportion of protected semi-moist broadleaf forest") + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.035), size = 4) + scale_x_discrete(labels = c("Cotubanamá", "Bahoruco", "Jaragua",
                                                                                                                 "Punta Espada")) + 
  annotate("text", x = 0.75, y = 0.79, label = "E", size = 6)


panelF <- paAreaLC92000 %>%
  filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc, group = 1)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Cumulative proportion of protected dry forest") + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.022), size = 2) + scale_x_discrete(labels = c("Jaragua", "Bahoruco",
                                                                                                                   "Anacaona", "Cabeza de Toro", "Río Cana",
                                                                                                                   "Hatillo", "Caamaño", "Picky Lora")) + 
  annotate("text", x = 1.5, y = 0.79, label = "F", size = 6)
grid.arrange(panelA,panelB,panelC,panelD,panelE,panelF, ncol=2)
dev.off()

paForestLossTotaled <- paForestLostAllYears %>%
  filter(landCoverCode == 1|landCoverCode == 2|landCoverCode == 4|landCoverCode == 6|landCoverCode == 8|landCoverCode == 9) %>%
  filter(SUBCAT != "SANTUARIO MARINO") %>%
  filter(SUBCAT != "SANTUARIO DE MAMIFEROS MARINOS")%>%
  #group_by(NOMBRE, landCoverCode) %>%
  #mutate(totalLoss = sum(sqKmLost)) %>%
  #filter(!is.na(totalLoss)) %>%
  group_by(NOMBRE, landCoverCode,landCoverName) %>%
  summarize(totalLoss = sum(sqKmLost),totalBurned = sum(sqKmBurned))%>%
  filter(!is.na(totalLoss)) %>%
  mutate(key = paste0(NOMBRE,"_",landCoverCode))

lookupTable <- paForestArea2000 %>%
  filter(landCoverCode == 1|landCoverCode == 2|landCoverCode == 4|landCoverCode == 6|landCoverCode == 8|landCoverCode == 9)%>%
  filter(SUBCAT != "SANTUARIO MARINO") %>%
  filter(SUBCAT != "SANTUARIO DE MAMIFEROS MARINOS")%>%
  filter(sqKm2000 > 0) %>%
  mutate(key = paste0(NOMBRE,"_",landCoverCode))

paForestLossTotaled.dt <- as.data.table(paForestLossTotaled)  
lookupTable.dt <- as.data.table(lookupTable)
setkey(paForestLossTotaled.dt,"key")
setkey(lookupTable.dt,"key")
paForestLossTotaledWith2000Area <- paForestLossTotaled.dt[lookupTable.dt]  

paForestLossTotaledWith2000Area <- paForestLossTotaledWith2000Area %>%
  mutate(totalUnburnedLoss = totalLoss-totalBurned,
         stillForested = sqKm2000 - totalLoss)

#Add percentage of protected forest accounted for by each PA for each forest type
paForestLossTotaledWith2000Area <- paForestLossTotaledWith2000Area %>%
  group_by(landCoverCode) %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000))

#Summarize for table
paForestLossSummarized <- paForestLossTotaledWith2000Area %>%
  group_by(landCoverCode) %>%
  summarize(totalLoss = sum(totalLoss),
            totalBurnedLoss = sum(totalBurned),
            percLostToFire = totalBurnedLoss/totalLoss,
            area2000 = sum(sqKm2000),
            area2016 = area2000-totalLoss)

paAreaLC12000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '1') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))

paAreaLC12000With2000Area %>%
  filter(totalLoss > 8) %>%
  ggplot(., aes(x = reorder(NOMBRE,totalLoss), y = totalLoss, group = 1)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Cumulative proportion of protected semi-moist broadleaf forest") + 
  geom_text(aes(label=paste0(round(percLoss*100,1),"%"), y = totalLoss+5), size = 4) + annotate("text", x = 0.75, y = 0.79, label = "A", size = 6)

paAreaLC22000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '2') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
paAreaLC42000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '4') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
paAreaLC62000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '6') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
paAreaLC82000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '8') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))
paAreaLC92000With2000Area <- 
  paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == '9') %>%
  arrange(desc(sqKm2000)) %>%
  mutate(cumSum = cumsum(sqKm2000),
         cumPerc = cumSum/sum(sqKm2000),
         perc = sqKm2000/sum(sqKm2000),
         percLoss = totalLoss/sqKm2000,
         IUCNCat = ifelse(COD_UICN == "Ia - UICN", "Ia",
                          ifelse(COD_UICN == "Ib - UICN", "Ib",
                                 ifelse(COD_UICN == "II - UICN", "II",
                                        ifelse(COD_UICN == "III - UICN", "III",
                                               ifelse(COD_UICN == "IV - UICN", "IV",
                                                      ifelse(COD_UICN == "V - UICN", "V",
                                                             ifelse(COD_UICN == "VI - UICN", "VI", NA))))))))


paForestLostAllYearsTotaled <- paForestLostAllYears %>%
  filter(landCoverCode == 1|landCoverCode == 2|landCoverCode == 4|landCoverCode == 6|landCoverCode == 8|landCoverCode == 9) %>%
  filter(SUBCAT != "SANTUARIO MARINO") %>%
  filter(SUBCAT != "SANTUARIO DE MAMIFEROS MARINOS")%>%
  group_by(NOMBRE, landCoverCode,landCoverName) %>%
  summarize(totalLoss = sum(sqKmLost),
            totalBurned = sum(sqKmBurned)) %>%
  mutate(totalUnburnedLoss = totalLoss - totalBurned)

##Figures of loss v. intact for each forest type by top protected areas (those containing 75% of all protected forest of that type)
###Dense conifer
png(filename = "Figure4.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 1, cumPerc < 0.76) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected closed-canopy pine forest"))) + 
  scale_fill_manual(name = "Status of forest, 2016",
                    labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                    values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = c("Vallue Nuevo","Carmen Ramírez","Armando Bermúdez"))

dev.off()
###Open conifer
png(filename = "Figure5.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 2, cumPerc < 0.76) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected open-canopy pine forest"))) +
  scale_fill_manual(name = "Status of forest, 2016",
                    labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                    values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = rev(c("Bahoruco","Carmen Ramírez","Vallue Nuevo")))
dev.off()
###Cloud forest
png(filename = "Figure6.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 4, cumPerc < 0.95) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected cloud forest"))) + 
  scale_fill_manual(name = "Status of forest, 2016",
                    labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                    values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = rev(c("Valle Nuevo","Armando Bermúdez",
                                                                                             "Bahoruco","Neiba","Carmen Ramírez",
                                                                                             "Tavárez Justo","Nalga de Maco",
                                                                                             "Alto Bao")))
dev.off()
###Moist broadleaf
png(filename = "Figure7.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 6, cumPerc < 0.76) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected moist broadleaf forest"))) + 
  scale_fill_manual(name = "Status of forest, 2016",
                    labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                    values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = rev(c("Los Haitises","La Humeadora",
                                                                                           "Armando Bermúdez","Bahoruco",
                                                                                           "Martín Garcia","Luis Quin",
                                                                                           "Valle Nuevo", "Quita Espuela",
                                                                                           "Tavárez Justo","Carmen Ramírez",
                                                                                           "Caamaño","Hatillo")))
dev.off()
###Semi-moist broadleaf
png(filename = "Figure8.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 8, cumPerc < 0.76) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected semi-moist broadleaf forest"))) + 
  scale_fill_manual(name = "Status of forest, 2016",
                    labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                    values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = rev(c("Cotubanamá", "Bahoruco", "Jaragua",
                                                                                             "Punta Espada")))
dev.off()
###Dry forest
png(filename = "Figure9.png", width = 8, height = 8, units = "in", res = 300)
paForestLossTotaledWith2000Area %>%
  filter(landCoverCode == 9, cumPerc < 0.76) %>%
  gather(key = "lossSource", value = "sqKm", totalUnburnedLoss, totalBurned, stillForested)%>%
  #filter(lossSource != "stillForested") %>%
  ggplot(., aes(x = reorder(NOMBRE,desc(cumPerc)), y = sqKm)) + geom_bar(aes(fill = lossSource), stat = "identity", position = position_stack()) + 
  theme(axis.text.x = element_text(angle = 340, size = 14), axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) + 
  labs(x = "Protected area", y = expression(paste("Area (", "km"^"2",")", " of protected dry forest"))) + 
  scale_fill_manual(name = "Status of forest, 2016",
                      labels = c("Intact", "Deforested, fire", "Deforested, other cause"),
                      values = c("#999999", "#E69F00", "#56B4E9")) + scale_x_discrete(labels = rev(c("Jaragua", "Bahoruco",
                                                                                             "Anacaona", "Cabeza de Toro", "Río Cana",
                                                                                             "Hatillo", "Caamaño")))
dev.off()
                      

##
filter(cumPerc < 0.76) %>%
  ggplot(., aes(x = reorder(NOMBRE,cumPerc), y = cumPerc)) + geom_col() + scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.8)) + 
  theme(axis.text.x = element_text(angle = 340), axis.title.y = element_text(size = 6)) + xlab("Protected area") + ylab("Proportion of protected open conifer") + 
  scale_x_discrete(labels = c("Bahoruco","Carmen Ramírez","Vallue Nuevo")) + annotate("text", x = 0.75, y = 0.79, label = "B", size = 6) + 
  geom_text(aes(label=paste0(round(perc*100,1),"%"), y = cumPerc+0.035), size = 4) 

##Creating summary table for all protected areas and forest types
lookupTable2 <- forestChange %>%
  filter(landCoverCode == 1|landCoverCode == 2|landCoverCode == 4|landCoverCode == 6|landCoverCode == 8|landCoverCode == 9)%>%
  filter(sqKm2000 > 0) %>%
  select(sqKm2000, landCoverCode) %>%
  mutate(key = paste0("lcc_",landCoverCode))

paForestLossTotaledWith2000AreaSummaryTable <-
  paForestLossTotaledWith2000Area %>%
  mutate(key = paste0("lcc_",landCoverCode))

paForestLossTotaledWith2000AreaSummaryTable.dt <- as.data.table(paForestLossTotaledWith2000AreaSummaryTable)  
lookupTable2.dt <- as.data.table(lookupTable2)
setkey(paForestLossTotaledWith2000AreaSummaryTable.dt,"key")
setkey(lookupTable2.dt,"key")
paForestLossTotaledWith2000AreaSummaryTable <- paForestLossTotaledWith2000AreaSummaryTable.dt[lookupTable2.dt]  

summaryTable <- paForestLossTotaledWith2000AreaSummaryTable %>%
  mutate(percTypeProtected = sqKm2000/i.sqKm2000) %>%
  mutate(area2016 = sqKm2000 - totalLoss)%>%
  mutate(percLoss = 1-((sqKm2000-totalLoss)/sqKm2000))%>%
  mutate(percLostToFire = totalBurned/totalLoss)%>%
  select(NOMBRE, CATEGORIA, COD_UICN,SQ_KM,landCoverCode,
         landCoverName,sqKm2000,percTypeProtected,totalLoss,
         totalBurned,totalUnburnedLoss,area2016, percLoss,
         percLostToFire)

colnames(summaryTable) <- c("Name","National.category", "IUCN.category",
                            "Area.of.protected.area","Land.cover.code","Land.cover.name",
                            "Land.cover.area.2000","Percent.type.protected.by.area",
                            "Total.sq.km.lost","Total.sqkm.burned"
                            ,"Total.sq.km.lost.other","Land.cover.area.2016",
                            "Percent.lost","Percent.lost.to.fire")

col_order <-c("Name","National.category", "IUCN.category",
              "Area.of.protected.area","Land.cover.code","Land.cover.name",
              "Land.cover.area.2000","Percent.type.protected.by.area",
              "Total.sq.km.lost","Percent.lost","Total.sqkm.burned",
              "Percent.lost.to.fire","Total.sq.km.lost.other","Land.cover.area.2016")

summaryTable <- summaryTable[,col_order]
write.csv(summaryTable, "/Users/johnlloyd/Documents/GitHub/drForestChange/summaryTable.csv")
summaryTable <- read.csv("/Users/johnlloyd/Documents/GitHub/drForestChange/summaryTable.csv",row.names = 1)
