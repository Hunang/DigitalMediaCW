# http://konect.uni-koblenz.de/networks/sociopatterns-infectious

# The meaning of the columns in out.sociopatterns-infectious are: 
# First  column: ID of from node 
# Second column: ID of to node

library(igraph)

# Load data, create random dataset
out <- read.csv("sociopatterns-infectious/out.sociopatterns-infectious", row.names=NULL, sep="")
out <- read.csv("sociopatterns-infectious/newNonMultipleEdges.csv", header=TRUE, fileEncoding="UTF-8-BOM")

nodes = 410 
edges = 2765 
out.network <- graph.data.frame(out, directed = F)
random.network = erdos.renyi.game(nodes, edges, type = "gnm")

# Ploting
plot(out.network,    main = "INFECTIOUS", vertex.label = NA,  vertex.color="blue",  vertex.size = 3)
plot(random.network, main = "RANDOM",     vertex.label = NA,  vertex.color="blue",  vertex.size = 3)

# Degree distribution
out.degree = degree(out.network)
random.degree = degree(random.network)
out.degree.dist = degree.distribution(out.network)
random.degree.dist = degree.distribution(random.network)
random.degree.dist
out.degree.dist

summary(out.degree)
summary(random.degree)

plot(out.degree.dist,    main = "INFECTIOUS - Degree Distributions", xlab = "DEGREE", ylab = "FREQUENCY")
plot(random.degree.dist, main = "RANDOM - Degree Distributions",     xlab = "DEGREE", ylab = "FREQUENCY")

# Clustering coefficient
out.clustering    = transitivity(out.network,    type ="undirected")
random.clustering = transitivity(random.network, type ="undirected")
out.clustering 
random.clustering 

# Modularity
out.community     = cluster_walktrap(out.network)
random.community  = cluster_walktrap(random.network)

modularity(out.community)       # Prints modularity
modularity(random.community)

plot(out.community, out.network, main = "INFECTIOUS - Modularity", vertex.label = NA, vertex.size = 3)
plot(random.community, random.network, main = "RANDOM - Modularity", vertex.label = NA, vertex.size = 3)

# Betweeness
out.betweenness    = betweenness(out.network)
random.betweenness = betweenness(random.network) 

summary(out.betweenness)
summary(random.betweenness)

out.betweenness.edges = edge_betweenness(out.network) #Betweenness for each edge
random.betweenness.edges = edge_betweenness(random.network) #Betweenness for each edge

plot(out.network, main = "INFECTIOUS - Betweenness", vertex.label = NA, vertex.size = 0.001*betweenness(out.network), vertex.color = "red")
plot(random.network, main = "RANDOM - Betweenness", vertex.label = NA, vertex.size = 0.015*betweenness(random.network), vertex.color = "red")

