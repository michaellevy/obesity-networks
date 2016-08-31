# Taken from explore.R, this is the minimum to make and save the network
el = read.csv("data/Edgelist_8.8.csv")
att = read.csv("data/NodeAttributes2012_8.8.csv")
set.seed(510)

n = intergraph::asNetwork(igraph::graph.data.frame(el, vertices = att))
communities = igraph::cluster_edge_betweenness(intergraph::asIgraph(n))
n %v% "ebCommunity" = as.integer(igraph::membership(communities))
n %v% "comm12" = n %v% "ebCommunity"
set.vertex.attribute(n, "comm12", 3, which(n %v% "ebCommunity" > 2))  # Combine communities 3-7
n[n %v% "net.data" == 0, ] = NA  # Set unobserved ties (from non-participants) to NA.

# Make net-snack statistic: Difference between healthy and unhealthy snacks
n %v% "netSnackT1" = n %v% "fvservt1" - n %v% "usservt1"
n %v% "netSnackT2" = n %v% "fvservt2" - n %v% "usservt2"
n %v% "netSnackChange" = n %v% "netSnackT2" - n %v% "netSnackT1"

saveRDS(n, "data/net.RDS")
