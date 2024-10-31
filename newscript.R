###session 3 part 2: working with WZM-data

###first step: set wd --> ctrl+shift+h

###or manually [specify your personal work directory]
setwd("C:/MCCM_Remote sensing/wzm/New folder")

FILES.WZM<-list.files()


# Load the 'raster' package
library(raster)

# Read the raster stack using stack() function
WZM.RAST <- stack(FILES.WZM)
WZM.RAST

COL.NDVI<-colorRampPalette(c("red","orange","grey","dodgerblue","blue"))


plot(WZM.RAST,breaks=seq(0.5,19.5,1),
     col=COL.NDVI(19),
     main=c("summer 2017","summer 2018", "summer 2019", "summer 2020"),
     legend=F,cex.main=1.5,
     pax=list(cex.axis=1.5))

# Set the color ramp palette
COL.NDVI <- colorRampPalette(c("red", "orange", "grey", "dodgerblue", "blue"))

# Plot the raster
plot(WZM.RAST, breaks = seq(0.5, 19.5, 1),
     col = COL.NDVI(19),
     main = c("summer 2017", "summer 2018", "summer 2019", "summer 2020"),
     legend = FALSE, cex.main = 1.5,
     cex.axis = 1.5)  # Set the size of axis labels directly here


###statistical evaluation:

VALS.WZM<-values(WZM.RAST)

head(VALS.WZM)

boxplot(VALS.WZM)

wilcox.test(VALS.WZM[,1],VALS.WZM[,2],paired=T)
wilcox.test(VALS.WZM[,1],VALS.WZM[,3],paired=T)
wilcox.test(VALS.WZM[,1],VALS.WZM[,4],paired=T)
wilcox.test(VALS.WZM[,2],VALS.WZM[,3],paired=T)
wilcox.test(VALS.WZM[,2],VALS.WZM[,4],paired=T)
wilcox.test(VALS.WZM[,3],VALS.WZM[,4],paired=T)

# Assuming VALS.WZM is a data frame with columns Q_2017209, Q_2018209, Q_2019209, Q_2020209
friedman.test(VALS.WZM)

# Assuming VALS.WZM is a data frame with columns Q_2017209, Q_2018209, Q_2019209, Q_2020209
friedman.test(VALS.WZM, na.action = "na.omit")
