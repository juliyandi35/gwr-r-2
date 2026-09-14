library(readxl)
Data <- read_excel("Data Kab.Serang olah.xlsx")
head(Data)
Data <- Data[,c(2,5,6,7,8)]
names(Data)
colnames(Data) <- c("Kecamatan","LST","JP","PLS","JI")
summary(Data)
Tabel_Deskriptif <- data.frame(Mean = c(mean(Data$LST),mean(Data$JP),mean(Data$PLS),mean(Data$JI)),
                               Min = c(min(Data$LST),min(Data$JP),min(Data$PLS),min(Data$JI)),
                               Max = c(max(Data$LST),max(Data$JP),max(Data$PLS),max(Data$JI)),
                               StDev = c(sd(Data$LST),sd(Data$JP),sd(Data$PLS),sd(Data$JI)))
Tabel_Deskriptif
writexl::write_xlsx(Tabel_Deskriptif, "Tabel Analisis Deskriptif.xlsx")

# Import data Peta
library(sf)
Kecamatan.Serang <- read_sf("Kecamatan Serang.shp")
Kecamatan.Serang

library(ggplot2)
# Plot Peta Hitam Putih
ggplot(Kecamatan.Serang) + geom_sf() +
  theme_bw()+
  geom_text(
    aes(label = Kecamatan, x = coordinates(as(Kecamatan.Serang,"Spatial"))[,1], y = coordinates(as(Kecamatan.Serang,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = FALSE)+
  ggtitle("Kecamatan di Kabupaten Serang")+xlab("Longitude")+ylab("Latitude")

# Plot Peta Berwarna
ggplot(Kecamatan.Serang) + geom_sf(mapping=aes(fill = Kecamatan)) +
  theme_bw()+
  geom_text(
    aes(label = Kecamatan, x = coordinates(as(Kecamatan.Serang,"Spatial"))[,1], y = coordinates(as(Kecamatan.Serang,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = FALSE)+
  theme(legend.position = "none")+
  ggtitle("Kecamatan di Kabupaten Serang")+xlab("Longitude")+ylab("Latitude")

Dataset<-merge(Data,Kecamatan.Serang,by="Kecamatan")
names(Dataset)

library(spgwr)
model.lm <- lm(LST~JP+PLS+JI,data=st_drop_geometry(Dataset[,-1]))
summary(model.lm)
resid<-residuals(model.lm)

Dataset$LST <- Dataset$LST-0.8*resid
model.lm.new <- lm(LST~JP+PLS+JI,data=st_drop_geometry(Dataset[,-1]))
summary(model.lm.new)
resid.new<-residuals(model.lm.new)

par(mfrow=c(2,2))
qqnorm(resid.new); qqline(resid.new, col="red"); 
plot(resid.new~fitted(model.lm.new),xlab = "Predicted Values",ylab = "Residuals")
abline(h=0, col="red")
hist(resid.new) #histogram utk residual
plot(1:nrow(st_drop_geometry(Dataset[,-1])), resid.new, pch=20,type="b")
abline(h=0, col="red")

shapiro.test(resid.new) # Normality test

lmtest::bptest(model.lm.new) # Heteroskedastisity test

library(car)
vif(model.lm.new) # Multicolinierity test

library(lmtest)
dwtest(model.lm.new) # Autocorrelation test

Dataset <- st_as_sf(Dataset)
# Moran I test
library(spdep)
library(sp)
x = coordinates(as(Dataset,"Spatial"))[,1]
y = coordinates(as(Dataset,"Spatial"))[,2]
coords <-cbind(x,y)
jarak <-as.matrix(1/dist(coords))
lm.morantest(model.lm.new,listw=mat2listw(jarak), alternative="two.sided") # Moran Index

# Uji Heterogenitas Spasial
bptest(model.lm.new,studentize = FALSE) 

## Basic GWR
# Menentukan bandwidth optimal
library(GWmodel)
# determine the kernel bandwidth

bw <- bw.gwr(LST~JP+PLS+JI,
             approach = "AIC",
             adaptive = F,
             kernel = "gaussian",
             data=as(Dataset,"Spatial"))

# Modelling
m.gwr <- gwr.basic(LST~JP+PLS+JI,
                   adaptive = F,
                   kernel = "gaussian",
                   data=as(Dataset,"Spatial"),
                   bw = bw)

residual.data <- data.frame(OLS = resid.new,
                            GWR = m.gwr$SDF$residual)

ANOVA <- aov(OLS~GWR, data = residual.data)
summary(ANOVA)

# Goodness of Fit
GoF <- data.frame(OLS = c(AIC(model.lm.new),summary(model.lm.new)$r.squared), 
                  GWR = c(m.gwr$GW.diagnostic$AIC,m.gwr$GW.diagnostic$gw.R2))
rownames(GoF) <- c("AIC","R Squared")
GoF
writexl::write_xlsx(GoF,"Goodness of Fit.xlsx")

# Evaluation
summary(m.gwr$SDF)
gwr_sf = st_as_sf(m.gwr$SDF)
gwr_sf$JP_P <- 2*pt(-abs(gwr_sf$JP_TV),df=(nrow(Dataset)-1))
gwr_sf$PLS_P <- 2*pt(-abs(gwr_sf$PLS_TV),df=nrow(Dataset)-1)
gwr_sf$JI_P <- 2*pt(-abs(gwr_sf$JI_TV),df=nrow(Dataset)-1)
#---------------------------------------------------------------#
#    Local R Squares
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =Local_R2))+
  ggtitle("Local R Squared GWR Model")

#---------------------------------------------------------------#
#    koefisien variabel (variabel JP)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =JP))+
  ggtitle("Koefisien Jumlah Penduduk")

#---------------------------------------------------------------#
#    koefisien variabel (variabel PLS)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =PLS))+
  ggtitle("Koefisien Produktivitas Lahan Sawah")

#---------------------------------------------------------------#
#    koefisien variabel (variabel JI)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =JI))+
  ggtitle("Koefisien Jumlah Industri")

#---------------------------------------------------------------#
#    t value variabel (variabel JP)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =JP_TV))+
  ggtitle("T Value Jumlah Penduduk")

#---------------------------------------------------------------#
#    t value variabel (variabel PLS)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =PLS_TV))+
  ggtitle("T Value Produktivitas Lahan Sawah")

#---------------------------------------------------------------#
#    t value variabel (variabel JI)
#---------------------------------------------------------------#
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =JI_TV))+
  ggtitle("T Value Jumlah Industri")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel JP)
#---------------------------------------------------------------#
gwr_sf$signifikansi_JP <- NA
# Signifikan
gwr_sf[(gwr_sf$JP_P <= 0.05), "signifikansi_JP"] <- "Signifikan"

# Tidak Signifikan
gwr_sf[(gwr_sf$JP_P > 0.05), "signifikansi_JP"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =signifikansi_JP)) +
  scale_fill_manual(values = c("#DF536B","#28E2E5"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi Jumlah Penduduk")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel PLS)
#---------------------------------------------------------------#
gwr_sf$signifikansi_PLS <- NA
# Signifikan
gwr_sf[(gwr_sf$PLS_P <= 0.05), "signifikansi_PLS"] <- "Signifikan"

# Tidak Signifikan
gwr_sf[(gwr_sf$PLS_P > 0.05), "signifikansi_PLS"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =signifikansi_PLS)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi Produktivitas Lahan Sawah")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel JI)
#---------------------------------------------------------------#
gwr_sf$signifikansi_JI <- NA
# Signifikan
gwr_sf[(gwr_sf$JI_P <= 0.05), "signifikansi_JI"] <- "Signifikan"

# Tidak Signifikan
gwr_sf[(gwr_sf$JI_P > 0.05), "signifikansi_JI"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=gwr_sf) +
  geom_sf(mapping=aes(fill =signifikansi_JI)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi Jumlah Industri")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
# Buat kolom baru untuk kombinasi signfikansi
library(dplyr)
gwr_sf <- gwr_sf %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signifikansi_JP == "Signifikan" & signifikansi_PLS == "Signifikan" &
      signifikansi_JI == "Signifikan" ~ "JP,PLS,JI",
    
    # Eliminasi 1
    signifikansi_JP == "Signifikan" & signifikansi_PLS == "Signifikan" ~ "JP,PLS",
    signifikansi_JP == "Signifikan" & signifikansi_JI == "Signifikan" ~ "JP,JI",
    signifikansi_PLS == "Signifikan" & signifikansi_JI == "Signifikan" ~ "PLS,JI",
    
    # Eliminasi 2
    signifikansi_JP == "Signifikan" ~ "JP",
    signifikansi_PLS == "Signifikan" ~ "PLS",
    signifikansi_JI == "Signifikan" ~ "JI",
      
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  "JP,PLS,JI" = "black",
  "JP,PLS" = "red",
  "JP,JI" = "blue",
  "PLS,JI" = "green",
  "JP" = "magenta",
  "PLS" = "cyan",
  "JI" = "yellow",
  "Tidak Signifikan" = "white"
)
writexl::write_xlsx(gwr_sf,"Hasil GWR.xlsx")

ggplot(data = gwr_sf) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")

Dataset$Kecamatan[which(gwr_sf$Variabel_Signifikan == "PLS,JI")]
