library(DT)
library(rgdal)
library(raster)
library(leaflet)
library(tidyverse)
library(shinydashboard)

ecoz_na = c("5","11","12")
ecop_na = c("4.1","4.2","5.1","5.2","5.2","5.3","5.4","6.4","11.1","11.2","11.3","11.4","12.1","12.2","12.3","12.4","14.1","15.1")
ecor_na = c("50","51","52","53","54","55","56","57","58","59","60","61","62","63","66","68","69","70","71","72","73","74","75","76","77","78","79","80","82","84","85","87","89","95","104","105","106","107","108","109","112","113","114","115","116","136","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","199","200","215","216")

#indicators = c("CMI","GPP","LED","LCC","ForestBirds","BLBW","BTNW","CAWA","CMWA","OSFL","AllWaterfowl","CavityNesters","GroundNesters","OverwaterNesters","ForestBirdsCore","BLBW_CORE","BTNW_CORE","CAWA_CORE","CMWA_CORE","OSFL_CORE","ABDU","AGWT","AMWI","BUFF","BWTE","CANV","GADW","GGOL","GMER","GSCA","GSCO","MALL","NOPI","NSHO","REDH","RNDU","RUDU")
#indicators1 = c("CMI","GPP","LED","LCC")
indicators1 = c("ClimateMoisture","PrimaryProductivity","LakeEdgeDensity","LandCover")
#indicators2 = c("ForestBirds","BLBW","BTNW","CAWA","CMWA","OSFL")
indicators2 = c("ForestBirds","BLBW","BLPW","BTNW","CAWA","CMWA","HETH","LCSP","OCWA","OSFL","PISI","PUFI","RUBL","WIWA")
indicators3 = c("AllWaterfowl","CavityNesters","GroundNesters","OverwaterNesters")
#indicators4 = c("ForestBirdsCore","BLBW_CORE","BTNW_CORE","CAWA_CORE","CMWA_CORE","OSFL_CORE")
indicators4 = c("ForestBirdsCore","BLBW_CORE","BLPW_CORE","BTNW_CORE","CAWA_CORE","CMWA_CORE","HETH_CORE","LCSP_CORE","OCWA_CORE","OSFL_CORE","PISI_CORE","PUFI_CORE","RUBL_CORE","WIWA_CORE")
indicators5 = c("ABDU","AGWT","AMWI","BUFF","BWTE","CANV","GADW","GGOL","GMER","GSCA","GSCO","MALL","NOPI","NSHO","REDH","RNDU","RUDU")
indicators = c(indicators1, indicators2, indicators3, indicators4, indicators5)
x = read_csv("data/representation.csv")
x = x[x$indicator %in% indicators,]
#x$indicator = indicators
x$indicator = factor(x$indicator, levels=rev(indicators))
datasets = read_csv("docs/datasets.csv")

xz = read_csv("data/ecoz_rep.csv") %>%
    mutate(ClimateMoisture=CMI, PrimaryProductivity=GPP, LakeEdgeDensity=LED, LandCover=LCC, CMI=NULL, GPP=NULL, LED=NULL, LCC=NULL)
ecoz = shapefile('data/ecoz_rep.shp')
names(ecoz) = names(xz)
ecoz = ecoz[,c("Ecozone",indicators)] # indicators1, indicators2, indicators3)]
ecoz_tb = ecoz@data
ecoz$id = as.character(ecoz$Ecozone)

xp = read_csv("data/ecop_rep.csv") %>%
    mutate(ClimateMoisture=CMI, PrimaryProductivity=GPP, LakeEdgeDensity=LED, LandCover=LCC, CMI=NULL, GPP=NULL, LED=NULL, LCC=NULL)
ecop = shapefile('data/ecop_rep.shp')
names(ecop) = names(xp)
ecop = ecop[,c("Ecoprovince",indicators)] # indicators1, indicators2, indicators3)]
ecop_tb = ecop@data
ecop$id = as.character(ecop$Ecoprovince)

xr = read_csv("data/ecor_rep.csv") %>%
    mutate(ClimateMoisture=CMI, PrimaryProductivity=GPP, LakeEdgeDensity=LED, LandCover=LCC, CMI=NULL, GPP=NULL, LED=NULL, LCC=NULL)
ecor = shapefile('data/ecor_rep.shp')
names(ecor) = names(xr)
ecor = ecor[,c("Ecoregion",indicators)] # indicators1, indicators2, indicators3)]
ecor_tb = ecor@data
ecor$id = as.character(ecor$Ecoregion)

sfi = raster("data/sfi10.tif")
