#install packages
library(maptools)
library(geosphere)
library(spdep)
library(tigris) #install it
options(tigris_use_cache = TRUE)

#Tenth deliverable 
mtl<-rgdal::readOGR("tl_2012_us_state.shp") 
"%ni%" <- Negate("%in%") #negate funtion
mtl<-mtl[mtl$NAME %ni% c('Alaska','American Samoa','Commonwealth of the Northern Mariana Islands','Guam','Hawaii','United States Virgin Islands','Puerto Rico', 'District of Columbia'),]
plot(mtl)

#Alternatively
states <- states(cb = TRUE, class="sp")
"%ni%" <- Negate("%in%")
plot(states)

states<-states[states$NAME %ni% c('Alaska','American Samoa','Commonwealth of the Northern Mariana Islands','Guam','Hawaii','United States Virgin Islands','Puerto Rico', 'District of Columbia'),]
plot(states)

mtl<-states

sids_nbq<-poly2nb(mtl, queen=TRUE)#neighboring structure
list_neigh<- nb2listw(sids_nbq)#,zero.policy=T)
coords<-coordinates(mtl)
"col" <- ifelse(mtl@data$NAME=="Pennsylvania","magenta", "gray90")
#plot(mtl, col=col, bg=rgb(77,77,77, 77/1.5,max=77),border="white", lwd=1)
plot(mtl, border="gray20", bg="slategray", col=col)
plot(sids_nbq, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T)
title(main="State-level Neighboring Structure", cex.main=2, col.main="grey11", font.main=2, sub="Note that Pennsylvania has six neighbors with the specification employed",cex.sub=1.15, col.sub="grey11", font.sub=2,)

a<-read.csv("state_wealth.csv")
head(a)
mtl@data$wealth<-as.numeric(a$Rank[match(mtl@data$NAME,a$ID)])#preserves the structure

library(RColorBrewer)
library(classInt)

nclr <- 9
plotvar <- mtl@data$wealth
plotclr <- brewer.pal(nclr, "YlOrRd")
class <- classIntervals(plotvar, nclr, style = "quantile")
colcode <- findColours(class, plotclr, digits = 2)

plot(mtl, border="gray20", bg="slategray", col=colcode)
plot(sids_nbq, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T)
legend("bottomright", legend=names(attr(colcode, "table")), 
fill = attr(colcode, "palette"), title="Ranked Wealth") 
moran.test(mtl@data$wealth,list_neigh, na.action=na.exclude)
title(main=paste("States wealth distribution as of 2012 from neighbors\nMoran's I = ", round(moran.test(mtl@data$wealth,list_neigh, zero.policy=TRUE, na.action=na.exclude)$estimate[1],4), "(p<", round(moran.test(mtl@data$wealth,list_neigh, zero.policy=TRUE, na.action=na.exclude)$ p.value,10), ")"), sub="Data source: American Community Survey, 2021 estimates\nALEC-LAFFER STATE ECONOMIC COMPETITIVENESS INDEX\nAvailable from:https://www.alec.org/app/uploads/2011/11/RSPS_4th_Edition.pdf")

# install.packages('gtools')
library(gtools)
z<-(permutations(n=length(as.character(mtl@data$NAME)),r=2,v=as.character(mtl@data$NAME),repeats.allowed=T))

z<-z[z[,1]!=z[,2],]#remove self-selection
z<-as.data.frame(z)
head(z)
set.seed(47)
z<-z[sample(nrow(z), 500), ]

library(igraph)
g<-graph.data.frame(z,directed = FALSE)
g

z<-get.adjacency(g)
dim(z)
z<-as.matrix(z)
#z[z>1]<-1 Only for presence or absence
summary(rowSums(z))
g<-graph.adjacency(z)
g

z <-z /rowSums(z)
summary(rowSums(z))
#z[is.na(z)]<-0#this is for isolates
z <-z[order(rownames(z)), order(colnames(z))]

test.listwNew<-mat2listw(z)
summary(test.listwNew)
V(g)$weight<-1
g<-simplify(g)#
g
moran.test(mtl@data$wealth,test.listwNew, zero.policy=TRUE, na.action=na.exclude)

plot(mtl, border="gray20", bg="slategray", col=colcode)
plot(test.listwNew, coords, col = rgb(205, 204,0, 255/4,max=255), lwd = 2, add=T)
legend("bottomright", legend=names(attr(colcode, "table")), 
fill = attr(colcode, "palette"), title="Ranked Wealth") 
title(main=paste("States wealth distribution as of 2018 from random connections\nMoran's I = ", round(moran.test(mtl@data$wealth,test.listwNew, zero.policy=TRUE, na.action=na.exclude)$estimate[1],4), "(p<", moran.test(mtl@data$wealth,test.listwNew, zero.policy=TRUE, na.action=na.exclude)$ p.value, ")"), sub="Data source: US Census Shapefiles, 2018 estimates\nALEC-LAFFER STATE ECONOMIC COMPETITIVENESS INDEX\nAvailable from:https://www.alec.org/app/uploads/2011/11/RSPS_4th_Edition.pdf")

#Approaches based on points eleventh assignment 
url <- paste( 'http://nces.ed.gov/ipeds/datacenter/data/HD2010.zip')
url2 <- paste('http://nces.ed.gov/ipeds/datacenter/data/IC2010_AY.zip')
##This captures the working directory 
a<-getwd()
a
#Using the working directory information we download the data in that folder as follows
download.file(url, destfile = paste(a,"HD2010.zip",sep="/"))
#Loading the dataset 
download.file(url2, destfile = paste(a,"IC2010_AY.zip",sep="/"))
a <- read.csv(unz("HD2010.zip", "hd2010.csv")) 
names(a)
b<- read.csv(unz("IC2010_AY.zip", "ic2010_ay.csv")) 
names(b)
a<-a[a$SECTOR==1|a$SECTOR==2|a$SECTOR==4|a$SECTOR==5|a$SECTOR==6,]
#a<-a[a$STABBR=="GA",]
table(a$STABBR)
a<-a[a$LOCALE<14,] #Cities only
dim(a)
n<-names(b)
b<-cbind(rownames(b),b)
names(b)<-n
a<-merge(a,b[,c("UNITID","TUITION3")], by="UNITID")
head(a)
str(a$TUITION3)
a$TUITION3<-as.numeric(as.character(a$TUITION3))
head(a$TUITION3)
summary(a$TUITION3[a$SECTOR==1])
summary(a$TUITION3[a$SECTOR==2])
summary(a$TUITION3[a$SECTOR==4])
summary(a$TUITION3[a$SECTOR==5])
summary(a$TUITION3[a$SECTOR==6])
# a$TUITION3.1<-as.numeric(as.character(a$TUITION3))
# head(a[is.na(a$TUITION3.1),c("TUITION3","SECTOR","INSTNM")])
a<-a[!is.na(a$TUITION3),]
dim(a)#bounding box
a<-a[a$LONGITUD > -124.848 &
                 a$LONGITUD < -66.886 &
                 a$LATITUDE > 24.3964 &
                 a$LATITUDE < 49.3844, ]
dim(a)

#combine the first two columns to be coordinates:
coords<-cbind(a$LONGITUD,a$LATITUDE)
coordsPub<-cbind(a[a$SECTOR==1,]$LONGITUD,a[a$SECTOR==1,]$LATITUDE)
coordsPri<-cbind(a[a$SECTOR==2,]$LONGITUD,a[a$SECTOR==2,]$LATITUDE)

test.nb<-knn2nb(knearneigh(coords,k=1))
test.nb5<-knn2nb(knearneigh(coords,k=5))
test.nb10<-knn2nb(knearneigh(coords,k=10))
test.nb20<-knn2nb(knearneigh(coords,k=20))
test.nbdist<-dnearneigh(coords, 0, 80.4672, row.names = a$name, longlat = TRUE) #50 miles
#Now create a list, similar to an edgelist in spatial form
test.listw<-nb2listw(test.nb10, zero.policy=TRUE)
test.listwdist<-nb2listw(test.nbdist, zero.policy=TRUE)
#create the listw object to test for spatial dependence
moran.test(a$TUITION3,test.listw, na.action=na.omit, zero.policy =TRUE)
moran.test(a$TUITION3,test.listwdist, na.action=na.omit, zero.policy =TRUE)

table(a$SECTOR)

par(mfrow = c(2, 2))
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(mtl, border="gray20", bg="slategray", col="gray90")
plot(test.nb, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T, pch = 22)
points(coordsPub, col="#ff00669b", pch = 22, bg="#ff00669b")
points(coordsPri, col="#66ff009b", pch = 22, bg="#66ff009b")
title(cex.main=1.5, col.main="grey11", font.main=2, main="Closest neighbor specification",cex.sub=1.15, col.sub="grey11", font.sub=2,)
legend(title="Sector", "bottomright", legend=c("Public, n=734", "Private, n=1605"), fill=c("#ff00669b", "#66ff009b"), bty="n", cex=1.5, y.intersp=0.8)

plot(mtl, border="gray20", bg="slategray", col="gray90")
plot(test.nb5, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T, pch = 22)
points(coordsPub, col="#ff00669b", pch = 22, bg="#ff00669b")
points(coordsPri, col="#66ff009b", pch = 22, bg="#66ff009b")
title(cex.main=1.5, col.main="grey11", font.main=2, main="Five closest neighbors specification",cex.sub=1.15, col.sub="grey11", font.sub=2,)
legend(title="Sector", "bottomright", legend=c("Public, n=734", "Private, n=1605"), fill=c("#ff00669b", "#66ff009b"), bty="n", cex=1.5, y.intersp=0.8)

plot(mtl, border="gray20", bg="slategray", col="gray90")
plot(test.nb10, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T, pch = 22)
points(coordsPub, col="#ff00669b", pch = 22, bg="#ff00669b")
points(coordsPri, col="#66ff009b", pch = 22, bg="#66ff009b")
title(cex.main=1.5, col.main="grey11", font.main=2, main="Ten closest neighbors specification",cex.sub=1.15, col.sub="grey11", font.sub=2,)
legend(title="Sector", "bottomright", legend=c("Public, n=734", "Private, n=1605"), fill=c("#ff00669b", "#66ff009b"), bty="n", cex=1.5, y.intersp=0.8)

plot(mtl, border="gray20", bg="slategray", col="gray90")
plot(test.nb20, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T, pch = 22)
points(coordsPub, col="#ff00669b", pch = 22, bg="#ff00669b")
points(coordsPri, col="#66ff009b", pch = 22, bg="#66ff009b")
title(cex.main=1.5, col.main="grey11", font.main=2, main="Twenty closest neighbors specification",cex.sub=1.15, col.sub="grey11", font.sub=2,)
legend(title="Sector", "bottomright", legend=c("Public, n=734", "Private, n=1605"), fill=c("#ff00669b", "#66ff009b"), bty="n", cex=1.5, y.intersp=0.8)
mtext("Institution-level Neighboring Structures", outer = TRUE, cex = 2)

plot(mtl, border="gray20", bg="slategray", col="gray90")
plot(test.nbdist, coords, col = rgb(205, 204,0, 255,max=255), lwd = 2, add=T, pch = 22)
points(coordsPub, col="#ff00669b", pch = 22, bg="#ff00669b")
points(coordsPri, col="#66ff009b", pch = 22, bg="#66ff009b")
title(cex.main=1.5, col.main="grey11", font.main=2, main="Radius neighbor specification, 50 miles",cex.sub=1.15, col.sub="grey11", font.sub=2,)
legend(title="Sector", "bottomright", legend=c("Public, n=734", "Private, n=1605"), fill=c("#ff00669b", "#66ff009b"), bty="n", cex=1.5, y.intersp=0.8)


