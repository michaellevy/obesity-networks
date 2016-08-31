library(dplyr)
library(statnet)
library(ggplot2)
library(stargazer)
el = read.csv("data/Edgelist_8.8.csv")
att = read.csv("data/NodeAttributes2012_8.8.csv")
set.seed(510)

# statnet's edgelist import functions are awful. Let's use igraph's instead, 
# and immediately convert to statnet with the intergraph package. 
# We read the vertex attributes in here too.
n = intergraph::asNetwork(igraph::graph.data.frame(el, vertices = att))

# Let's try to detect communities algorithmically so we don't have to do it subjectively. 
communities = igraph::cluster_edge_betweenness(intergraph::asIgraph(n))
# Get the community of each vertex and assign as vertex attribute:
n %v% "ebCommunity" = as.integer(igraph::membership(communities))
vCols = RColorBrewer::brewer.pal(max(n %v% "ebCommunity"), "Accent")  # Nice colors for plot
vCols = vCols[n %v% "ebCommunity"]

png("results/netPlot.png", height = 800, width = 800)
plot(n
     , displaylabels = TRUE
     , vertex.col = vCols
     # , vertex.cex = log(degree(n, cmode = "indegree") + 1) + .5
     , vertex.cex = ((n %v% "bmi.percentile")^-1 * 25)^-1
     , label.cex = 1.2
     , label.pos = 6
     , vertex.sides = c(4, 50)[as.factor(n %v% "Ethnicity")]
     , main = "Nodes are colored by edge-betweenness community membership.\nShape reflects ethnicity and size BMI percentile"
)
dev.off()

# Looking at this, I think perhaps we want differential homophily in communities 1 and 2 and none elsewhere.
# Implement this in model 18.
# Or, make everyone not in 1 or 2 in a 3rd community and have uniform homophily across all three.
# Model comparison (see 17.8 vs 18.1) favors the former.
# While I've got my head around it: 
## nodematch("comm12", diff = FALSE) makes three communities, all share a homophily effect
## nodematch("ebCommunity", diff = FALSE, keep = 1:2) makes three communities, the first two share a homophily effect

n %v% "comm12" = n %v% "ebCommunity"
set.vertex.attribute(n, "comm12", 3, which(n %v% "ebCommunity" > 2))  # Combine communities 3-7
# plot(n, vertex.col = "comm12", vertex.cex = 3)  # Make sure that worked right

# Set unobserved ties (from non-participants) to NA.
# Unfortunately, clustering algorithms don't handle missingness, so they have to be done with these assumed to be 0s
n[n %v% "net.data" == 0, ] = NA

# Examine distribution of bmi%ile and weight statuses
ggplot(att, aes(x = bmi.percentile)) + 
  geom_density(adjust = .2, fill = "gray") +
  geom_rug(sides = "b", aes(color = weight.status.3),
           data = data.frame(bmi.percentile = att$bmi.percentile + rnorm(nrow(att), 0, .5),
                             weight.status.3 = att$weight.status.3))
# Looks like the two-class division is reasonable. Overweight/obese divides similar girls.

# Let's try to fit some ERGMs
m1 = ergm(n ~ edges + mutual)
m2 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE))
m2.5 = ergm(n ~ edges + mutual + gwodegree(.5, fixed = TRUE))
m3 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + gwodegree(.5, fixed = TRUE))
m4 = ergm(n ~ edges + mutual + 
            gwidegree(.5, fixed = TRUE) + gwodegree(.5, fixed = TRUE) +
            nodeicov("bmi.percentile") + nodeocov("bmi.percentile"))
m4.5 = ergm(n ~ edges + mutual + 
              nodeicov("bmi.percentile") + nodeocov("bmi.percentile"))
stargazer(m1, m2, m2.5, m3, m4, m4.5, type = "text")  
# Strong evidence for reciprocity and a popularity effect.
# gwod controls for differential nominating, not clear whether it matters
# May be a positive relationship between bmi%ile and popularity, but it's collinear with a straight popularity effect (gwid)
# No differential nomination tendency with bmi%ile

# Let's add homophily by bmi%ile
m5 = ergm(n ~ edges + mutual + 
            gwidegree(.5, fixed = TRUE) + gwodegree(.5, fixed = TRUE) +
            nodeicov("bmi.percentile") + nodeocov("bmi.percentile") +
            absdiff("bmi.percentile"))
# Sampler didn't mix.

m6 = ergm(n ~ edges + mutual + 
            nodeicov("bmi.percentile") + nodeocov("bmi.percentile") +
            absdiff("bmi.percentile"))

m6.5 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
              nodeicov("bmi.percentile") + nodeocov("bmi.percentile") +
              absdiff("bmi.percentile"))
stargazer(m6, m6.5, type = "text")

m7 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + gwesp(.25, fixed = TRUE) + 
            nodeicov("bmi.percentile") + nodeocov("bmi.percentile") +
            absdiff("bmi.percentile"))
# Unconverged, singular Hessian

# Try categorical weight status instead of bmi%ile
m8 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
            nodeifactor("weight.status.2") + nodeofactor("weight.status.2"))

# Add uniform homophily
m9 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
            nodeifactor("weight.status.2") + nodeofactor("weight.status.2") +
            nodematch("weight.status.2", diff = FALSE))

# And differential homophily
m10 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             nodeifactor("weight.status.2") + nodeofactor("weight.status.2") +
             nodematch("weight.status.2", diff = TRUE))
# mcmc.diagnostics(m10)   # Poor convergence. Overspecified, I think.
# How about combining the node-in and node-out factors?
m10.1 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               nodefactor("weight.status.2") +
               nodematch("weight.status.2", diff = TRUE))
# Poor convergence
# mcmc.diagnostics(m10.1)   # Not sure why this is performing poorly. Must be overspecd
gof10.1 = gof(m10.1)
par(mfrow = c(2, 2))
plot(gof10.1)  # Not getting transitivity. 
summary(m10.1)

m11 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             gwesp(.1, fixed = TRUE) + 
             nodefactor("weight.status.2") +
             nodematch("weight.status.2", diff = TRUE))
# MCMC not mixing. Maybe the nodefactor and gwi popularity effects are colinear. Try ditching gwid

m12 = ergm(n ~ edges + mutual + gwesp(.1, fixed = TRUE) + 
             nodeifactor("weight.status.2") + nodeofactor("weight.status.2") +
             nodematch("weight.status.2", diff = TRUE),
           control = control.ergm(MCMC.samplesize = 1e4))
# Really is computationally singular. Don't see colinearity in params though.
summary(m12)
m12$covar
plot(gof(m12))

# Maybe with three weight categories?
m13 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             nodefactor("weight.status.3") +
             nodematch("weight.status.3", diff = FALSE))
summary(m13)

m14 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             gwesp(.2, fixed = TRUE) + 
             nodefactor("weight.status.3") +
             nodematch("weight.status.3", diff = FALSE))
# Nope

m13.1 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               nodefactor("weight.status.3") +
               nodematch("weight.status.3", diff = TRUE))
# Aha! Can get differential homophily, just not with GWESP.
gof13.1 = gof(m13.1)
plot(gof13.1)  # Really missing those ESPs though.

m14.1 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) + 
               nodefactor("weight.status.3") +
               nodematch("weight.status.3", diff = TRUE))
# Of course, nope.

m15 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             gwesp(0, fixed = TRUE) + 
             nodefactor("weight.status.3") +
             nodematch("weight.status.3", diff = TRUE))
# nope

m16 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             nodematch("comm12", diff = TRUE) + 
             nodefactor("weight.status.2") +
             nodematch("weight.status.2", diff = TRUE))
# nope

m16.1 = ergm(n ~ edges + mutual + 
               nodematch("comm12", diff = TRUE) + 
               nodefactor("weight.status.2") +
               nodematch("weight.status.2", diff = TRUE))
# nope

m17 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
             gwesp(.2, fixed = TRUE) +
             nodematch("comm12", diff = TRUE) + 
             nodefactor("weight.status.2") +
             nodematch("weight.status.2", diff = TRUE))
# nope
gof17 = gof(m17)
par(mfrow = c(2,2))
plot(gof17)
mcmc.diagnostics(m17)

m17.1 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = TRUE) + 
               nodefactor("weight.status.2") +
               nodematch("weight.status.2", diff = FALSE))


m17.2 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = FALSE) + 
               nodefactor("weight.status.2") +
               nodematch("weight.status.2", diff = TRUE))
# nope

m17.3 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = FALSE) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m17.3)
gof17.3 = gof(m17.3)
plot(gof17.3)

m17.4 = ergm(n ~ edges + mutual + gwidegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = TRUE) + 
               nodematch("weight.status.2", diff = TRUE))
# nope. weird: no warning, but nope.

m17.5 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = FALSE) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m17.5)
gof17.5 = gof(m17.5)
plot(gof17.5)

m17.6 = ergm(n ~ edges + mutual + isolates + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("comm12", diff = FALSE) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m17.6)
gof17.6 = gof(m17.6)
plot(gof17.6)

m17.7 = ergm(n ~ edges + mutual + isolates + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("comm12", diff = TRUE) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m17.7)
# nope

summary(n ~ edges + nodefactor("Ethnicity") + nodematch("Ethnicity", TRUE))
table(n %v% "Ethnicity", n %v% "comm12")
m17.8 = ergm(n ~ edges + mutual + isolates + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("comm12", diff = FALSE) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m17.8)
gof17.8 = gof(m17.8)
plot(gof17.8)

# load("models/ergmsGOFsetc.RData")  # Everything to this point

things = structure(lapply(ls(), get), names = ls())
models = things[sapply(things, class) == "ergm"]
# Keep just the models that converged:
models = models[sapply(models, function(m) !all(is.na(m[["est.cov"]])))]
# Order by model-name number
models = models[order(as.numeric(sapply(names(models), function(x) substr(x, 2, nchar(x)))))]
stargazer(models, type = "text", column.labels = names(models))

# m17.8 minimizes A/BIC. Good! Has differential homophily by weight status, but no node-factors
summary(m17.8)

m18 = ergm(n ~ edges + mutual + isolates + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.2, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = TRUE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
# nope

m18.1 = ergm(n ~ edges + mutual + isolates + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
               nodematch("weight.status.2", diff = TRUE))
summary(m18.1)

stargazer(m17.8, m18.1, type = "text")  
# xIC slightly prefers not having homophily in the "third community" 

# Want: 
## bump gwesp alpha to get 2-esps
## add nodefactor for weight status
## same for ethnicity
## Keep isolates?
## Need endo communities?
## Add physical activity

m19 = ergm(n ~ edges + mutual + isolates + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("comm12", diff = FALSE) + 
             nodematch("weight.status.2", diff = TRUE))
gof19 = gof(m19)
par(mfrow = c(2, 2))
plot(gof19)  # Really helps with the 1 vs 2 ESPs. Gets the upper distribution too. Nice.
stargazer(m17.8, m18.1, m19, type = "text")
par(mfcol = c(2, 4))
plot(gof17.8)
plot(gof19)
# Hmm, bigger gwesp-alpha helps with ESPs but seems to hurt with degree dists. And xIC favors smaller. 

m20 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("comm12", diff = FALSE) + 
             nodematch("weight.status.2", diff = TRUE))
stargazer(m17.8, m19, m20, type = "text")

m17.9 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("comm12", diff = FALSE) + 
               nodematch("weight.status.2", diff = TRUE))

m21 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))

m22 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = TRUE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
# nope

m23 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.2, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("comm12", diff = TRUE) + 
               nodematch("weight.status.2", diff = TRUE))
# nope

stargazer(m17.8, m18.1, m19, m20, m21, type = "text")

# Okay, those are small xIC differences. I like nodematch("ebCommunity", diff = FALSE, keep = 1:2):
# Found two major clusters, let there be one homophily force acting within but not outside them.
# How about GWESP alpha and isoaltes for that:
m24 = ergm(n ~ edges + mutual + isolates +
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
stargazer(m18.1, m21, m24, type = "text")
par(mfrow = c(2, 2))
gof21 = gof(m21)
plot(gof21)

m25 = ergm(n ~ edges + mutual + isolates +
             gwidegree(1, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
gof25 = gof(m25)
plot(gof25)   # Increasing gwd-decay and adding isolates doesn't help degdist or xIC
stargazer(m21, m25, type = "text")


m26 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodefactor("weight.status.2") + 
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
# nope. computationally singular. weight status main effect seems to be redundent

m27 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodecov("bmi.percentile") + 
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
stargazer(m21, m27, type = "text")
# So besides the overspecification problem above, there doesn't seem to be any
# main effect of weight status (here as bmi%ile) and including it doesn't improve
# the model per xIC.

m28 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodefactor("Ethnicity") + 
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
stargazer(m21, m28, type = "text")
# Maybe Latina girls form more friends, but it's not strong and xIC slightly prefers without it

m29 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodefactor("Ethnicity") + 
             nodematch("Ethnicity", diff = FALSE) + 
             # nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
             nodematch("weight.status.2", diff = TRUE))
stargazer(m21, m28, m29, type = "text")  
# Ha! xiC likes having endo-defined communities, for obvious reasons, and ethnicity doesn't replace them.
# m21 still looks the best, though a few others are in the same neighborhood.

# Oh, what about age homophily?
m21.1 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
               nodematch("weight.status.2", diff = TRUE) +
               absdiff("T1Age"))
stargazer(m21, m21.1, type = 'text')  # Yeah, age homophily is important. Good.

# To what extent are the communities age separation?
ggplot(data.frame(com = n %v% "ebCommunity",
                  age = n %v% "T1Age") %>%
         filter(com < 3), 
       aes(x = as.factor(com), y = age)) + 
  geom_boxplot(fill = "gray")
# To a large extent. Does xIC prefer just age homophily with endo-communities?
# No: It likes having both. There is overlap of age in the communities and they have separate effects.
m21.2 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               # nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
               nodematch("weight.status.2", diff = TRUE) +
               absdiff("T1Age"))

# What about a main effect of age?
m21.3 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               # nodematch("ebCommunity", diff = FALSE, keep = 1:2) + 
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age"))

m21.4 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age"))
stargazer(m21, m21.1, m21.2, m21.3, m21.4, type = "text")
# m21.4 is winner. Main effect of age and age-homophily beyond the community structure.
# AHA! And there was masking: Revealed a weight-status homophily effect!
# Let's make sure we can't have a weight main effect in there too.

m21.5 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
               nodefactor("weight.status.2") +
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age"))
# nope, we can't.

# What about these camp activity teams?
m21.6 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age") +
               nodematch("team", diff = FALSE))

m21.7 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               # nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age") +
               nodematch("team", diff = TRUE, keep = 2:4))
summary(n ~ nodematch("team", diff = TRUE))  # No ties in team=3 so exclude that
# from the differential team-homophily estimate. This is unfortunate beacuse we'd 
# like to keep that negative effect but fixing it to -Inf is harsh and wrecks xIC.
# Anyway, overspecified with differential homophily for the other three teams.
# Tried it without endo communities; still over-specd. So, stick with one
# team-homophily effect (m21.6).
stargazer(m21.4, m21.7, m21.6, type = "text")

# Add exercise and dietary habits
summary(n %v% "TotalPAChange")  # Wow -- that's some variance! Nice.

m30 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("TotalPAChange"))

m31 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("TotalPA2"))

m32 = ergm(n ~ edges + mutual + 
                gwidegree(.5, fixed = TRUE) + 
                gwodegree(.5, fixed = TRUE) + 
                gwesp(.5, fixed = TRUE) +
                nodematch("Ethnicity", diff = FALSE) + 
                nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
                nodematch("weight.status.2", diff = TRUE) +
                nodecov("T1Age") + 
                absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("MVPA2"))

m33 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("TotalPA1"))

m34 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("TotalPA1") +
             nodecov("TotalPAChange"))

stargazer(m21.4, m21.6, m30, m31, m32, m33, m34, type = "text")
# Total exercise, vigerous exercise, and change in exercise don't seem to matter much as main effects.
# What about homophily?

m30.1 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             absdiff("TotalPAChange"))

m31.1 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             absdiff("TotalPA2"))

m32.1 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             absdiff("MVPA2"))

m33.1 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             absdiff("TotalPA1"))

m34.1 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             absdiff("TotalPA1") +
             absdiff("TotalPAChange"))

stargazer(m30, m31, m32, m33, m34, m30.1, m31.1, m32.1, m33.1, m34.1, type = "text")
# No homophily there; tiny effects are just main effects recast. Just to be extra sure:

m35 = ergm(n ~ edges + mutual + 
               gwidegree(.5, fixed = TRUE) + 
               gwodegree(.5, fixed = TRUE) + 
               gwesp(.5, fixed = TRUE) +
               nodematch("Ethnicity", diff = FALSE) + 
               nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
               nodematch("weight.status.2", diff = TRUE) +
               nodecov("T1Age") + 
               absdiff("T1Age") +
               nodematch("team", diff = FALSE) +
               nodecov("MVPA2") +
               absdiff("MVPA2"))
summary(m35)  # Nothing there.

# What about diet?
dev.off()
par(mfrow = c(2, 2))
plot(n %v% "fvservt1", n %v% "usservt1")
plot(n %v% "fvservt2", n %v% "usservt2")
plot(n %v% "fvservt1", n %v% "fvservt2")
plot(n %v% "FVChange", n %v% "USChange")

# That outlier who was eating 12 unhealthy snacks a day at t1 might need to be dealt with, that point has a lot of leverage
m36 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("usservt1")
)

m37 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("fvservt1")
)

m38 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("fvservt1") +
             nodecov("usservt1")
)

stargazer(m21.6, m36, m37, m38, type = "text")
# Maybe a little evidence for unhealthy eaters being a little more popular, but unclear
# Homophily in eating?

m39 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("usservt1") +
             absdiff("usservt1")
)
stargazer(m36, m39, type = "text")
# Hmm, minor evidence for homophily in unhealthy snacks.

m40 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("fvservt1") +
             absdiff("fvservt1") +
             nodecov("usservt1") +
             absdiff("usservt1")
)
stargazer(m36, m39, m40, type = "text")
# Similar story for healthy snacks, weaker but with similar (im)precision
# What if we combined them and called it the net healthy snack score?

n %v% "netSnackT1" = n %v% "fvservt1" - n %v% "usservt1"

m41 = ergm(n ~ edges + mutual + 
       gwidegree(.5, fixed = TRUE) + 
       gwodegree(.5, fixed = TRUE) + 
       gwesp(.5, fixed = TRUE) +
       nodematch("Ethnicity", diff = FALSE) + 
       nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
       nodematch("weight.status.2", diff = TRUE) +
       nodecov("T1Age") + 
       absdiff("T1Age") +
       nodematch("team", diff = FALSE) +
       nodecov("netSnackT1") +
       absdiff("netSnackT1")
)
stargazer(m21.6, m41, type = "text")
summary(m41)
# Similar story, prefered by xIC, but effect is still imprecise. May just be noisy effect.

# What about dietary change?
m42 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackT1") +
             absdiff("netSnackT1") +
             nodecov("USChange") +
             absdiff("USChange")
)
stargazer(m41, m42, type = "text")
# Interesting: Adding change in diet during camp clarified the homophily of dietary 
# choices and overweight-homophily effects (but both only slightly). Can we use just
# unhealthy snacks instead of my created variable?

m43 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("usservt1") +
             absdiff("usservt1") +
             nodecov("USChange") +
             absdiff("USChange")
)
stargazer(m41, m42, m43, type = "text")
# It doesn't work as well. So, what about constructing a net-change statistic?
n %v% "netSnackT2" = n %v% "fvservt2" - n %v% "usservt2"
n %v% "netSnackChange" = n %v% "netSnackT2" - n %v% "netSnackT1"

m44 = ergm(n ~ edges + mutual + 
                gwidegree(.5, fixed = TRUE) + 
                gwodegree(.5, fixed = TRUE) + 
                gwesp(.5, fixed = TRUE) +
                nodematch("Ethnicity", diff = FALSE) + 
                nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
                nodematch("weight.status.2", diff = TRUE) +
                nodecov("T1Age") + 
                absdiff("T1Age") +
                nodematch("team", diff = FALSE) +
                nodecov("netSnackT1") +
                absdiff("netSnackT1") +
                nodecov("netSnackChange") +
                absdiff("netSnackChange")
)

m45 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange")
)
stargazer(m21.6, m39, m41, m42, m43, m44, m45, type = "text")

# Let's try adding physical activity back in on top of that:
m46 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange") +
             nodecov("MVPA2") +
             absdiff("MVPA2")
)
summary(m46)

m47 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange") +
             nodecov("TotalPAChange") +
             absdiff("TotalPAChange")
)

m48 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange") +
             nodecov("MVPA1") +
             absdiff("MVPA1") +
             nodecov("MVPAChange") +
             absdiff("MVPAChange")
)

m49 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange") +
             nodecov("MVPAChange") +
             absdiff("MVPAChange")
)

m50 = ergm(n ~ edges + mutual + 
             gwidegree(.5, fixed = TRUE) + 
             gwodegree(.5, fixed = TRUE) + 
             gwesp(.5, fixed = TRUE) +
             nodematch("Ethnicity", diff = FALSE) + 
             nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
             nodematch("weight.status.2", diff = TRUE) +
             nodecov("T1Age") + 
             absdiff("T1Age") +
             nodematch("team", diff = FALSE) +
             nodecov("netSnackChange") +
             absdiff("netSnackChange") +
             absdiff("MVPAChange")
)

stargazer(m45, m46, m47, m48, m49, m50, type = "text")
stargazer(m41, m45, m44, type = "text")
save.image("models/manyModels.RData")

dd = broom::tidy(m50)[c(1:7, 10:12, 8:9, 13:15), -4]
dd = mutate(dd, sig = ifelse(p.value < .01, "***",
                        ifelse(p.value < .05, "**",
                               ifelse(p.value < .1, "*", ""))))
knitr::kable(dd, format = "markdown", digits = 3)
