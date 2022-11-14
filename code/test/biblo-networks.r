net=bibliometrix::networkPlot(NetMatrix, normalize="association", n = 50,
                Title = "Keyword Co-occurrences \nGISAID", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10,
                labelsize=5,label.cex=TRUE,label.n=30,edges.min=2, label.color = FALSE)
#E(net$graph)$color = "White"
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration \nGISAID",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=1)
net=networkPlot(NetMatrix,  n = 50, Title = "Institution collaboration\n(GISAID)",type = "auto", size=4,size.cex=F,edgesize = 3,labelsize=1)
net=networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Country collaboration",type = "circle", size=10,size.cex=T,edgesize = 1,labelsize=0.6, cluster="none")
