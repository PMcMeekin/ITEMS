library(sf)
library(ggplot2)
library(ggrastr)
library(tidyverse)

sf_nc <- sf::st_read(dsn="d:/GIS_ITEMS/LSOA_2019.shp", quiet = TRUE)
# Remove welsh ones
sf_nc <- sf_nc[substr(sf_nc$lsoa11cd, 1,1) !="W",]


STPS <- read.csv("d:/GIS_ITEMS/LSOA_TO_HEALTH.csv",)
STPS <- STPS[,c("LSOA11CD", "STP19CD",  "STP19NM")]
colnames(STPS) <- tolower(colnames(STPS))

#Add STP name and code
sf_nc = sf_nc %>% dplyr::left_join(STPS, by=c("lsoa11cd"="lsoa11cd"))

names <- unique(sf_nc$stp19nm)

for(nm in names){
	sf::st_write(sf_nc[sf_nc$stp19nm == nm, ], paste("d:/GIS_ITEMS/",nm,".shp",sep=""))
}


sf_nc <- sf::st_read(dsn="d:/GIS_ITEMS/Cumbria and North East.shp", quiet = TRUE)
towns <- read.csv("d:/GIS_ITEMS/Townsandcities.csv")
bounding <-st_bbox(sf_nc)

ltowns <- towns[ towns$LONG<=bounding$xmax & towns$LONG>=bounding$xmin &
                 towns$LAT<=bounding$ymax & towns$LAT>=bounding$ymin, ]
ltowns <- ltowns[,c("TCITY15NM", "LONG", "LAT")]
colnames(ltowns)=c("city","lng","lat")
flcities <- st_as_sf(ltowns, coords = c("lng", "lat"), crs=st_crs("WGS84"))

plot <- ggplot2::ggplot(sf_nc) +
  geom_sf(aes(color = IMDDecil)) +
  geom_sf(data = flcities)+
    geom_text(data = ltowns, aes(x = lng, y = lat, label = city), 
        size = 3.9, col = "white", fontface = "bold") 

plot
plotR <- rasterize(plot, layers='Point', dpi=50)
