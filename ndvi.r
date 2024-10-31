###session 3: 29.11.2023

library(terra)

###setwd ctrl+shift+h

setwd("C:/MCCM_Remote sensing/wzm/New folder")

list.files()

###generate objects referring to the files

NDVI.FILES<-list.files(pattern="NDVI")
NDVI.FILES

EVI.FILES<-list.files(pattern="EVI")
EVI.FILES

PR.FILES<-list.files(pattern="pixel_reliability")
PR.FILES

###load rasters into 'R'
library(raster)

NDVI.RAST<-rast(NDVI.FILES)
EVI.RAST<-rast(EVI.FILES)
PR.RAST<-rast(PR.FILES)

NDVI.RAST

plot(NDVI.RAST[[10]])

plot(PR.RAST[[1]])

###use PR to mask NDVI and EVI

###first for one layer

plot(NDVI.RAST[[1]])
plot(PR.RAST[[1]])

NDVI.RAST[[1]][PR.RAST[[1]]>1]<-NA

plot(NDVI.RAST[[1]])

for(i in 1:nlyr(NDVI.RAST))
{
  NDVI.RAST[[i]][PR.RAST[[i]]>1]<-NA
  print(i)
}

###now all layers have been masked according to PR

SHAPE<-vect(list.files(pattern=".shp"))
SHAPE###not the same coordinate reference system!

###so, we have to reproject the data
###we will reproject the shape 
###since we don't loose information/ modify values there

SHAPE.P<-project(SHAPE,NDVI.RAST)

lines(SHAPE.P,lwd=3)##visualize the correct location of the shape

###we can now crop and mask our NDVI data to this polygon

NDVI.Prepared<-mask(crop(NDVI.RAST,SHAPE.P),SHAPE.P)

plot(NDVI.Prepared[[c(1,10)]])###visual inspection

###okay, data is prepared!

###extract a time-series with mean NDVI over all pixels for each time-step

MEAN.NDVI.TS<-global(NDVI.Prepared,"mean",na.rm=T)

MEAN.NDVI.TS

TIME<-as.POSIXct(substr(NDVI.FILES,35,41),format="%Y%j")

plot(MEAN.NDVI.TS[,1]/10000~TIME,type="l",
     lwd=2,xlab="",ylab="NDVI",
     cex.lab=1.5,cex.axis=1.5)###lower case l not 1

DOY<-substr(NDVI.FILES,39,41)

plot(MEAN.NDVI.TS[1:23,1]/10000~DOY[1:23],type="l",
     lwd=2,xlab="",ylab="NDVI",
     cex.lab=1.5,cex.axis=1.5)###lower case l not 1
lines(MEAN.NDVI.TS[24:46,1]/10000~DOY[24:46],
      lwd=2,col="red")
legend(x=0,y=0.8,legend=c("2017","2018"),
       fill=c("black","red"),cex=1.5)

MEAN.NDVI.2017<-app(NDVI.Prepared[[1:23]],"mean",na.rm=T)
MEAN.NDVI.2018<-app(NDVI.Prepared[[24:46]],"mean",na.rm=T)

plot(c(MEAN.NDVI.2017,MEAN.NDVI.2018)/10000)

###add some meaningful colors and make the map look a little nicer

COL.NDVI<-colorRampPalette(c("red","orange","grey","purple","blue"))

plot(c(MEAN.NDVI.2017,MEAN.NDVI.2018)/10000,
     breaks=seq(0,1,length=20),col=COL.NDVI(20),
     main=c(2017,2018),legend=F)

###project into WGS

MEAN.NDVI.P<-project(c(MEAN.NDVI.2017,MEAN.NDVI.2018)/10000,
                     "EPSG:4326")

par(oma=c(1,1,1,1))

plot(MEAN.NDVI.P,
     breaks=seq(0,1,length=20),col=COL.NDVI(20),
     main=c(2017,2018),legend=F,cex.main=1.5,
     pax=list(cex.axis=1.5))

###or plot a difference map to see the relative change over the years

plot(app(MEAN.NDVI.P,diff),
     breaks=seq(-0.15,0.15,length=20),col=COL.NDVI(20),
     main="delta 2017-2018",legend=F)

###maybe you just want to focus on the peak of growing seasons, 
###i.e. timesteps 11-18


MEAN.NDVI.2017.GS<-app(NDVI.Prepared[[11:18]],"mean",na.rm=T)
MEAN.NDVI.2018.GS<-app(NDVI.Prepared[[11:18+23]],"mean",na.rm=T)

###project into WGS

MEAN.NDVI.GS.P<-project(c(MEAN.NDVI.2017.GS,MEAN.NDVI.2018.GS)/10000,"EPSG:4326")

par(oma=c(1,1,1,1))

plot(MEAN.NDVI.GS.P,
     breaks=seq(0,1,length=20),col=COL.NDVI(20),
     main=c(2017,2018),legend=F,cex.main=1.5,
     pax=list(cex.axis=1.5))

###or plot a difference map to see the relative change over the years

plot(app(MEAN.NDVI.GS.P,diff),
     breaks=seq(-0.35,0.35,length=20),col=COL.NDVI(20),
     main="delta 2017-2018",legend=F)

###now the difference is much more pronounced
###reason: spring was favorable in 2018
###exclusion of spring time-steps 
###makes the drought-effect of 2018 more visible

###session 3 part 2: working with WZM-data

###first step: set wd --> ctrl+shift+h

###or manually [specify your personal work directory]
setwd("C:/MCCM_Remote sensing/wzm/New folder")

FILES.WZM<-list.files()

install.packages("raster")
library(raster)
rast <- raster(FILES.WZM)
install.packages(c("raster", "sp"))
library(raster)
FILES.WZM <- list.files(path = C://MCCM_Remote sensing//wzm//New folder"), full.names = TRUE)
rast <- raster(FILES.WZM)
# or
WZM.RAST <- stack(FILES.WZM)

rast <- raster(FILES.WZM[1])
# or
WZM.RAST <- stack(FILES.WZM[1])



WZM.RAST<-raster(FILES.WZM)

WZM.RAST

plot(WZM.RAST,breaks=seq(0.5,19.5,1),
     col=COL.NDVI(19),
     main=c("summer 2017","summer 2018", "summer 2019", "summer 2020"),
     legend=F,cex.main=1.5,
     pax=list(cex.axis=1.5))
warnings()
###statistical evaluation:

VALS.WZM<-values(WZM.RAST)

head(VALS.WZM)

boxplot(VALS.WZM, col=COL.NDVI(19))

wilcox.test(VALS.WZM[,1],VALS.WZM[,2], VALS.WZM[,3], VALS.WZM[,4], paired=T)
wilcox.test(VALS.WZM[,3], VALS.WZM[,4], paired=T)
# Assuming VALS.WZM is a data frame with columns Q_2017209, Q_2018209, Q_2019209, Q_2020209
friedman.test(VALS.WZM)

# Assuming VALS.WZM is a data frame with columns Q_2017209, Q_2018209, Q_2019209, Q_2020209
friedman.test(VALS.WZM, na.action = "na.omit")

library(raster)

# Assuming RasterLayer is your raster object
writeRaster(RasterLayer, filename = "path/to/your/outputfile.tif", format = "GTiff")

RasterLayer <- raster(VALS.WZM, col=COL.NDVI(19))

library(raster)

# Create a raster layer from VALS.WZM
RasterLayer <- raster(matrix(VALS.WZM, ncol = ncol(VALS.WZM)))

# Set a color palette
color_palette <- c("#FF0000", "#FF2400", "#FF4900", "#FF6E00", "#FF9200", "#F7A715", "#E9AD3F", "#DAB269", "#CCB893", "#BEBEBE", "#B79AC9", "#B077D4", "#AA54DF", "#A331EA", "#8E1CF1", "#6A15F4", "#470EF8", "#2307FB", "#0000FF")

# Plot the raster layer with the specified color palette
plot(RasterLayer, col = color_palette)

writeRaster(RasterLayer, filename = "\\MCCM_Remote sensing\\wzm", format = "GTiff")


