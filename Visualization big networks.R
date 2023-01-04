a<-read.csv("G:\\My Drive\\SSNA 2017 Penn\\SSNA 2019\\Third Session\\Data formats\\ID904.csv")

#Keeping only column with author relationships
a<-as.data.frame(a[,2])
colnames(a)<-"AU"
a

#package for first split
install.packages("splitstackshape")
library(splitstackshape)
#As can be seen in the file the separator of interest is :
a1<-cSplit(a, splitCols = "AU", sep = ":", direction = "wide", drop = FALSE) #retain the matrix form version of the adjacency list input
#Here we just drop the original first column
a1<-a1[,-1]
fix(a1)
class(a1)

a1<-read.csv("G:\\My Drive\\SSNA 2017 Penn\\SSNA 2019\\Deliverables\\a1.csv",na.strings = "NA")

#read it as a matrix
mat <- as.matrix(a1)
mat

mat<-tolower(mat)

dim(mat)# the resulting column dimension is the number of times you will have to repeat the following procedure minus 1

#all combinations except first author with itself
edgelist1 <- cbind(mat[, 1], c(mat[, -1]))
edgelist1

edgelist2 <- cbind(mat[, 2], c(mat[, -c(1:2)]))
edgelist2

edgelist3 <- cbind(mat[, 3], c(mat[, -c(1:3)]))
edgelist3

edgelist4 <- cbind(mat[, 4], c(mat[, -c(1:4)]))
edgelist4

edgelist5 <- cbind(mat[, 5], c(mat[, -c(1:5)]))
edgelist5

edgelist6 <- cbind(mat[, 6], c(mat[, -c(1:6)]))
edgelist6

edgelist7 <- cbind(mat[, 7], c(mat[, -c(1:7)]))
edgelist7

edgelist8 <- cbind(mat[, 8], c(mat[, -c(1:8)]))
edgelist8

# edgelist9 <- cbind(mat[, 9], c(mat[, -c(1:9)]))
# edgelist9

edgelist <- rbind(edgelist1, edgelist2, edgelist3, edgelist4, edgelist5, edgelist6, edgelist7, edgelist8) 
dim(edgelist)

#if reading a1 from csv
# First approach
# # edgelist$V2[edgelist$V2==""]<-NA
# # #second approach
# # edgelist<-edgelist[edgelist[,2]!="",]

edgelist<-edgelist[!is.na(edgelist[,2]),]
dim(edgelist)


a1<-mat
edgelist1<-matrix(NA, 1, 2)#empty matrix two columns
for (i in 1:(ncol(a1)-1)) {
  edgelist11 <- cbind(a1[, i], c(a1[, -c(1:i)]))
  edgelist1 <- rbind(edgelist1,edgelist11)
  edgelist1<-edgelist1[!is.na(edgelist1[,2]),]
  edgelist1<-edgelist1[edgelist1[,2]!="",]
  }
dim(edgelist1)

install.packages("igraph")
library(igraph)
plot(graph.edgelist(edgelist1))

g<- graph.edgelist(edgelist1, directed = FALSE)

E(g)$weight	<- 1#must step
g.c <- simplify(g)
E(g.c)$weight 


#Adding color
library(networkD3)
g<-g.c
V(g)$name<-1:length(V(g)) #(1:length(V(g)))-1
links<-as.data.frame(get.edgelist(g))

links$V1<-as.numeric(as.character(links$V1))
links

links$V2<-as.numeric(as.character(links$V2))
str(links)

colnames(links)<-c("source","target")
links
links<-(links-1)

links$value <- E(g)$weight*5 #where 5 is a scalar
links
nodes <- data.frame(name=V(g)$name, group = edge.betweenness.community(g)$membership, size=betweenness(g,directed=F,normalized=T)*115) #so size isn't tiny

nodes

forceNetwork(Links = links, Nodes = nodes,
                  Source = 'source', Target = 'target',
                  NodeID = 'name',
                  Group = 'group', # color nodes by group calculated earlier
                  charge = -50, # node repulsion
                  linkDistance = 20,
                  opacity = 1,
				  Value = 'value',
				  Nodesize = 'size', # color nodes by group calculated earlier
                  zoom = T, 
                  fontSize=24,
				  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20)"))
				  