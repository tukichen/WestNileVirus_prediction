library(readr) # Read competition data files:
library(ggmap) # For map plotting
library(geosphere)


distance <- function(longitude, latitude) {        
  #Euclidian distances aren't accurate, use Haversine distance : http://en.wikipedia.org/wiki/Haversine_formula        
  dist1 <- distHaversine(c(stations[1,]$Longitude,stations[1,]$Latitude),c(longitude,latitude))
  dist2 <- distHaversine(c(stations[2,]$Longitude,stations[2,]$Latitude),c(longitude,latitude))
  
  
  if(dist1<dist2){
    return(1)
  }
  return(2)
}
data_path= "/Users/qiaolinchen/Documents/data_challenge/sparkbeyond/data/"
mapdata <- readRDS(paste0(data_path, "mapdata_copyright_openstreetmap_contributors.rds") )
train <- read.csv(paste0(data_path, "train.csv") )

# extact year from date
train$Year <- as.numeric(substr( as.character(train$Date), 1,4))
# make Virus variable for present-absence
train$Virus <-ifelse(train$WnvPresent ==0,"Absent","Present")


# facet graph of infectious mosquitos
ggmap(mapdata)+geom_point(data=train, aes(x=Longitude, y=Latitude, colour=Virus,size=NumMosquitos),alpha=0.2)+
  scale_colour_manual(values =c("forestgreen","red"))+
  facet_grid(Virus~Year)+
  coord_equal(xlim = c(-87.95,-87.5),ylim = c(41.6,42.05))