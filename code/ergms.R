library(statnet)
library(stargazer)
set.seed(95616)
n = readRDS("data/net.RDS")

m100 = ergm(n ~ edges + mutual + 
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

m101 = ergm(n ~ edges + mutual + 
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
              absdiff("TotalPAChange")
)

m102 = ergm(n ~ edges + mutual + 
              gwidegree(.5, fixed = TRUE) + 
              gwodegree(.5, fixed = TRUE) + 
              gwesp(.5, fixed = TRUE) +
              nodematch("Ethnicity", diff = FALSE) + 
              nodematch("ebCommunity", diff = FALSE, keep = 1:2) +
              nodematch("weight.status.2", diff = FALSE) +
              nodecov("T1Age") + 
              absdiff("T1Age") +
              nodematch("team", diff = FALSE) +
              nodecov("netSnackChange") +
              absdiff("netSnackChange") +
              absdiff("MVPAChange")
)

m103 = ergm(n ~ edges + mutual + 
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
              absdiff("MVPA1")
)

m104 = ergm(n ~ edges + mutual + 
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
              absdiff("MVPAChange")
)

gof100 = gof(m100)
png("results/m100_goodness-of-fit.png", height = 600, width = 600)
par(mfrow = c(2, 2))
plot(gof100)
dev.off()

png("results/m100_mcmc.png", height = 1800, width = 800)
par(mfrow = c(length(m100$coef), 2))
plot(m100$sample, ask = FALSE, auto = FALSE)
dev.off()

knitr::knit(text = stargazer(m100, m101, m102, m103, m104, 
                             type = "html", digits = 2, model.numbers = FALSE, 
                             dep.var.caption = "", dep.var.labels.include = FALSE,
                             order = c(1:7, 13, 11, 12, 10, 8, 9, 14:15, 18:19, 16:17, 20)),
            output = "results/ergmTable.html")

# Or just one model
dd = broom::tidy(m100)[c(1:7, 10:12, 8:9, 13:15), -4]
dd = mutate(dd, sig = ifelse(p.value < .01, "***",
                             ifelse(p.value < .05, "**",
                                    ifelse(p.value < .1, "*", ""))))
knitr::knit(text = knitr::kable(dd, format = "html", digits = 3),
            output = "ergmSummay.html")