library(igraph)
library(scales)
a<- data.frame(Group=c("A","A","A","B","B","B"), Code=c("Code 1", "Code 2", "Code 3", "Code 1", "Code 2", "Code 3"), w=c(50, 25,30,20,50,25))
a
g1<-graph.data.frame(a)
V(g1)$type <- V(g1)$name %in% a[,1]
gm <- graph.adjacency(as.matrix((get.incidence(g1, attr="w"))) %*% (as.matrix(t(get.incidence(g1, attr="w")))>1), diag=F, mode="plus", weighted = TRUE)
g<-as.data.frame(cbind(get.edgelist(gm),as.numeric(E(gm)$weight)))
dg <- data.frame(id = colnames(t(get.incidence(g1, attr="w"))), dg = colSums(t(get.incidence(g1, attr="w"))))

detach(package:igraph)
library(ndtv)
library(networkDynamic) # load the dynamic extensions
# library(dplyr)
library(viridis)
net3 <- network(g[,c("V1", "V2")], matrix.type="edgelist", 
                loops=T, multiple=F, ignore.eval = T)
net3 %e% "weigth"<- as.numeric(as.character(g$V3))
#print(as.numeric(as.character(g$V3)))
#print(g)
#print(g$V3)
#print(net3 %e% "weigth")
net3 <- networkDynamic(base.net=net3)
net3 <- network.collapse(net3, onset=1, terminus=nrow(g)+1)

net3 %v% "labels" <- as.character(dg$id[match(net3 %v% "vertex.names", dg$id)])
net3 %v% "cex" <- (dg$dg[match(net3 %v% "vertex.names", dg$id)])

 render.d3movie(net3,
 usearrows = F, label.cex=.5,
 vertex.tooltip = paste("<b>Code name:</b>",(net3 %v% "labels"),"<br>", "<b>No. of unique connections:</b>",(net3 %v% "cex")),
 vertex.col = alpha("magenta", 0.8),
 label.col=rgb(250, 250, 250,max=255, 255/2),
 bg=rgb(11, 11, 11, max=255, 255/1), #222, 222, 222
 vertex.border="NA",
 vertex.cex = (net3 %v% "cex"/max(net3 %v% "cex")+.5),
 edge.lwd = log(net3 %e% "weigth"),
 edge.col = ifelse(net3 %e% "weigth"==max(net3 %e% "weigth"), "red", "snow"), #viridis( length(net3 %e% "weigth"), option="plasma",  direction = -1), 
edge.tooltip = paste("<b>No. of shared connections:</b>", (net3 %e% "weigth")),
 launchBrowser=T, filename="collapsed_analyses.html",
 main="")
# utils::browseURL(paste(getwd(),"/collapsed_analyses.html",sep=""))


  Group   Code  w
1     A Code 1 50
2     A Code 2 25
3     A Code 3 30
4     B Code 1 20
5     B Code 2 50
6     B Code 3 25

Matrix Representation
  Code 1 Code 2 Code 3
A     50     25     30
B     20     50     25
Tot   70     75     55

Transformation retaining codes
Code 1 Code 2 = 145 (70 + 75)
Code 1 Code 3 = 125 (70 + 55)
Code 2 Code 3 = 130 (75 + 55)



  

