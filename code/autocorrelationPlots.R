library(sna)
library(numDeriv)
library(tidyverse)
theme_set(theme_bw())
source("code/functions/calcAlters.R")
n = readRDS("data/net.RDS")

# Filter to just the girls for whom we have network data (and check that it worked)
########################################################
par(mfrow = c(1, 2))
co = plot(n, vertex.col = "net.data", displaylabels = TRUE, label.cex = .7)
nr = which(!(n %v% "net.data"))
co = co[-nr, ]
delete.vertices(n, nr)
plot(n, coord = co, displaylabels = TRUE, label.cex = .7)

# Extract all nodal attributes
vat = netUtils::vAttrDF(n)

## Networks

cols = viridis::viridis(17)
makeCols = function(vec, pal = cols) {
  toCut = c(vec, 0, range(vec))
  cols = cut(toCut, breaks = length(pal))
  plotCols = cols[1:(length(cols) - 3)]
  legendCols = cols[(length(cols) - 2):length(cols)]
  return(list(plot = pal[plotCols], legend = pal[legendCols[c(2, 1, 3)]]))
}
paCols = makeCols(n %v% "TotalPAChange")
snackCols = makeCols(n %v% "netSnackChange")

png("results/ActivityAndSnackingChange_NetworkPlots.png", 1400, 600)
x = -17; y = -2
par(mfrow = 1:2, mar = c(1, 1, 2.5, 1), xpd = NA)
set.seed(324)
co = plot(n, vertex.cex = 2, vertex.col = paCols$plot)
mtext("Total Physical Activity Change", cex = 2)
legend(x = x, y = y, 
       legend = rev(c(paste0("Minimum: ", round(min(n %v% "TotalPAChange"), 0)),
                      "No change",
                      paste0("Maximum: ", round(max(n %v% "TotalPAChange"), 0)))),
       pt.bg = rev(paCols$legend), pch = 21, pt.cex = 3, bty = "o", cex = 1.5,
       title = "Activity Change")

plot(n, vertex.cex = 2, vertex.col = snackCols$plot, coord = co)
mtext("Overall Snack Quality Change", cex = 2)
legend(x = x, y = y, 
       legend = rev(c(paste0("Minimum: ", round(min(n %v% "netSnackChange"), 0)),
                      "No change",
                      paste0("Maximum: ", round(max(n %v% "netSnackChange"), 0)))),
       pt.bg = rev(snackCols$legend), pch = 21, pt.cex = 3, bty = "o", cex = 1.5,
       title = "Snack Change")
dev.off()


## Snacking vs. exercise behavior change

ggplot(vat, aes(x = TotalPAChange, y = netSnackChange)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm") +
  xlab("Change in Physical Activity (minutes / day)") +
  ylab("Change in Healthfulness of Snacks")
ggsave("results/dietVsExerciseChange.png", width = 5, height = 4)  


## Ego's behavior vs. average alter's behavior
ggpa = 
  data_frame(egoPA = n %v% "TotalPAChange",
             nominated = calcAlters("TotalPAChange", n, "out")
  ) %>%
  gather(key, value, -egoPA) %>%
  mutate(key = factor(key, key)) %>%
  ggplot(aes(y = egoPA, x = value)) + 
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  ylab("Change in physical activity") +
  xlab("Average change in friends' activity") 
ggsave(filename = "results/activityChangeVsFriends.png", width = 5, height = 4)

ggs = 
  data_frame(egoPA = n %v% "netSnackChange",
             nominated = calcAlters("netSnackChange", n, "out")
  ) %>%
  gather(key, value, -egoPA) %>%
  mutate(key = factor(key, key)) %>%
  ggplot(aes(y = egoPA, x = value)) + 
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  ylab("Change in snacking habits (positive = healthier)") +
  xlab("Average change in friends' snacking habits") 
ggsave(filename = "results/snackingChangeVsFriends.png", width = 5, height = 4)

ggsave(filename = "results/friendsVsSelf.png", 
       cowplot::plot_grid(ggpa, ggs),
       width = 9, height = 4)

