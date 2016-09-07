library(statnet)
library(stargazer)
set.seed(303290)
n = readRDS("data/net.RDS")
load("models/ERGMs.RDA")

# m1 =  # The basic social model: density, reciprocity, and triadic closure. Won't converge.
#   ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE)
#   )

# m1 =  # Just structural features. This really doesn't want to converge. Maybe with init from m104
#   ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
#          gwidegree(.5, fixed = TRUE) + 
#          gwodegree(.5, fixed = TRUE) 
#   )

m1 =  # Just density and "controls"
  ergm(n ~ edges + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE) 
  )

m2 =  # Structure plus basic controls 
  ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
         gwidegree(.5, fixed = TRUE) + 
         gwodegree(.5, fixed = TRUE) + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE)
  )

m3 =  # Show that the endogenously-detected communities really do help
  ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
         gwidegree(.5, fixed = TRUE) + 
         gwodegree(.5, fixed = TRUE) + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE) +
         nodematch("ebCommunity", diff = FALSE, keep = 1:2)
  )

m4 =  # Add weight-status homophily
  ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
         gwidegree(.5, fixed = TRUE) + 
         gwodegree(.5, fixed = TRUE) + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE) +
         nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
         nodematch("weight.status.2", diff = TRUE)
  )

m5 =  # Add main effect of diet
  ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
         gwidegree(.5, fixed = TRUE) + 
         gwodegree(.5, fixed = TRUE) + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE) +
         nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
         nodematch("weight.status.2", diff = TRUE) +
         nodecov("netSnackT1") 
  )

m6 =  # And homophily of diet and exercise-change. Same as m104.
  ergm(n ~ edges + mutual + gwesp(.5, fixed = TRUE) +
         gwidegree(.5, fixed = TRUE) + 
         gwodegree(.5, fixed = TRUE) + 
         nodematch("Ethnicity", diff = FALSE) + 
         nodecov("T1Age") + 
         absdiff("T1Age") +
         nodematch("team", diff = FALSE) +
         nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
         nodematch("weight.status.2", diff = TRUE) +
         nodecov("netSnackT1") +
         absdiff("netSnackT1") +
         absdiff("MVPAChange")
  )

m7 = # This is what a logit could do. Just non-structural features. 
     # To show biased estimates we'd get without the fancy ERGMs.
ergm(n ~ edges + 
       nodematch("Ethnicity", diff = FALSE) + 
       nodecov("T1Age") + 
       absdiff("T1Age") +
       nodematch("team", diff = FALSE) +
       nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
       nodecov("netSnackT1") +
       absdiff("netSnackT1") +
       absdiff("MVPAChange")
)

modelBuild = list(m1, m2, m3, m4, m5, m6, m7)
saveRDS(modelBuild, "models/modelBuild.RDS")

knitr::knit(text = stargazer(modelBuild[c(1, 2, 3, 4, 6)], 
                             type = "html", digits = 2, model.numbers = FALSE, 
                             dep.var.caption = "", dep.var.labels.include = FALSE,
                             covariate.labels = 
                               c("Density", "Reciprocity", "Triangles (GW-ESP, $\\theta_{T} = 0.5)",
                                 "Anti-Popularity (GW-Indegree, $\\theta_{S} = 0.5)",
                                 "Balance of Nominations (GW-Outdegree, $\\theta_{S} = 0.5)",
                                 "Ethnicity Homophily", "Age", "Age Heterophily",
                                 "Team Homophily", "Community Homophily", 
                                 "Weight-status Homophily (Healthy)",
                                 "Weight-status Homophily (Overweight/Obese)",
                                 "Healthy Snacking", "Healthy Snacking Heterophily",
                                 "Exercise Change Heterophily")),
            output = "results/ergmTable-build.html")