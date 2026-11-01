#code for elooking at nesting and foraging networks identified by the waterbird
#GB survey

#load in datasets
bbhav<-read.csv("C:\\Users\\Nathan\\OneDrive - Southern Illinois University\\GB_SDM\\GB_interaction_table.csv")


install.packages('igraph')
require(igraph)

head(bbhav)

nest<-data.frame(bbhav$Nest_from,bbhav$Nest_to)
forage<-data.frame(bbhav$Forage_from,bbhav$Forage_to)
forage<-forage[1:147,]

net <- graph_from_data_frame(nest, directed=F)
net <- graph_from_data_frame(forage, directed=F)

V(net)$name
length(V(net)$name)
V(net)$label <- V(net)$name
V(net)$degree <- igraph::degree(net)
net_s<-simplify(net)

set.seed(222)

hs <- hub_score(net)$vector
as <- authority.score(net)$vector
plot(as)
plot.new()

E(net_s)$weight = sapply(E(net_s), function(e) { 
    length(all_shortest_paths(net, from=ends(net_s, e)[1], to=ends(net_s, e)[2])$res) } )

plot.new()
plot(net_s, 
     vertex.size=5, 
     vertex.label.dist=0.5, 
     vertex.label.cex=0.8,
     vertex.label.degree=-pi/2, 
     edge.width=E(net_s)$weight,
     layout=layout_nicely)

dev.new()
png("C:\\Users\\Nathan\\OneDrive - Southern Illinois University\\GB_SDM\\nest_code.png",res=600,heigh=6,width=6,units="in")
set.seed(3) #1 for nesting, 3 for forage
l <- layout_with_graphopt(net_s, charge = 0.02, mass = 30)
plot(net_s,
     vertex.size=hs*10,
     vertex.label.dist = 1.5,
     main = 'Nesting Contact',
     vertex.color = categorical_pal(8),
     edge.arrow.size=0.1,
     layout = l,#layout_nicely,
	edge.width=E(net_s)$weight,
	edge.width=net$weight)
dev.off()

dev.new()
png("C:\\Users\\Nathan\\OneDrive - Southern Illinois University\\GB_SDM\\forage_code.png",res=600,heigh=6,width=6,units="in")
set.seed(5000)
l <- layout_with_graphopt(net_s, charge = 0.05, mass = 30)

plot(net_s,
     vertex.size=hs*10,
	vertex.label.dist = 1.5,
     main = 'Foraging Contact',
     vertex.color = categorical_pal(8),
     edge.arrow.size=0.1,
     layout = l,#layout_nicely,
	edge.width=E(net_s)$weight,
	edge.width=net$weight)
dev.off()

heatmap(as.matrix(net[]), Colv = NA, Rowv = NA, symm = TRUE)


net <- cluster_edge_betweenness(net_s)
set.seed(123)
min(hs)
soc_net<-plot(net_s,
     vertex.size=hs*5+5,
     vertex.label.cex = 0.8,
	edge.width=E(net_s)$weight,
	layout=layout_nicely)
summary(net_s)
plot(text("No Data"))

	plot(net_s,
	     vertex.size=hs*10,
	     vertex.label.cex = 1,
		edge.width=E(net_s)$weight,
		layout=layout_nicely)
