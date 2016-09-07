library(statnet)
set.seed(78910)
n = readRDS("data/net.RDS")

# Define nice colors for plot
cols = RColorBrewer::brewer.pal(max(n %v% "ebCommunity"), "Accent")  
# Assign to the nodes based on the community they belong to
vCols = cols[n %v% "ebCommunity"]
# Replace all the size-1 communities with the same color
vCols[n %v% "ebCommunity" > 2] = "lightgray"

png("results/netPlot-noLabels.png", height = 800, width = 800)
plot(n
     # , displaylabels = TRUE
     , vertex.col = vCols
     # , vertex.cex = log(degree(n, cmode = "indegree") + 1) + .5
     , vertex.cex = ((n %v% "bmi.percentile")^-1 * 25)^-1
     , label.cex = 1.2
     , label.pos = 6
     , vertex.sides = c(4, 50)[as.factor(n %v% "Ethnicity")]
     , vertex.border = "net.data"
     # , main = "Nodes are colored by edge-betweenness community membership.\nShape reflects ethnicity and size BMI percentile"
)
dev.off()
