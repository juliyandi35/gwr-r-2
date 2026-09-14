# Import data Peta
library(sf)
Map <- read_sf("Batas_Wilayah_KelurahanDesa_10K_AR.shp")
head(Map)
names(Map)
Serang <- Map[which(Map$WADMKK=="Serang"),]
names(Serang)
Serang <- Serang[,c(2,17,27)]
colnames(Serang) <- c("Desa","Kecamatan","geometry")

library(dplyr)
# Mengelompokkan data berdasarkan nama kecamatan
Kecamatan.Serang <- Serang %>%
  group_by(Kecamatan) %>%
  summarise(geometry = st_union(geometry))
Kecamatan.Serang
write_sf(Kecamatan.Serang,"Kecamatan Serang.shp")