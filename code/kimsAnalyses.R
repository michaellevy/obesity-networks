#Creating my network from my files

rm(list = ls())

#load network files
library(network)
library(sna)
library(statnet)

#Set working directory
setwd("~/Dropbox/Research/Dissertation/Defense/Analyses/Stats Camp/Practice with my Data")

#Creating my adjacency matrix from scratch
m <-matrix(0, nrow=42, ncol=42)
m

m[1,3] <-1

m[2,4] <-1
m[2,13] <-1

m[3,28] <-1
m[3,37] <-1
m[3,6] <-1
m[3,22] <-1

m[4,13] <-1
m[4,17] <-1
m[4,19] <-1

m[5,14] <-1
m[5,10] <-1
m[5,9] <-1

m[6,8] <-1
m[6,28] <-1
m[6,37] <-1

m[7,8] <-1
m[7,12] <-1
m[7,6] <-1

m[8,6] <-1

m[9,13] <-1
m[9,35] <-1
m[9,19] <-1
m[9,23] <-1
m[9,24] <-1
m[9,5] <-1
m[9,10] <-1
m[9,30] <-1
m[9,14] <-1
m[9,38] <-1
m[9,4] <-1

m[10,11] <-1
m[10,38] <-1
m[10,14] <-1
m[10,30] <-1
m[10,9] <-1
m[10,17] <-1

m[11,21] <-1
m[11,13] <-1
m[11,4] <-1
m[11,17] <-1

m[12,7] <-1

m[13,4] <-1
m[13,19] <-1
m[13,17] <-1
m[13,30] <-1

m[14,5] <-1
m[14,23] <-1
m[14,9] <-1
m[14,10] <-1

m[15,32] <-1
m[15,28] <-1
m[15,6] <-1

m[16,40] <-1
m[16,18] <-1
m[16,14] <-1

m[17,30] <-1

m[18,34] <-1
m[18,19] <-1
m[18,24] <-1
m[18,25] <-1
m[18,4] <-1
m[18,2] <-1

m[19,4] <-1
m[19,13] <-1
m[19,24] <-1
m[19,34] <-1
m[19,35] <-1
m[19,25] <-1
m[19,30] <-1
m[19,9] <-1

m[20,16] <-1
m[20,33] <-1
m[20,37] <-1
m[20,26] <-1

m[21,24] <-1
m[21,31] <-1

m[22,33] <-1
m[22,20] <-1

m[23,14] <-1
m[23,34] <-1
m[23,9] <-1

m[24,21] <-1
m[24,25] <-1
m[24,18] <-1
m[24,17] <-1

m[25,31] <-1
m[25,19] <-1
m[25,34] <-1
m[25,13] <-1

m[27,17] <-1

m[28,37] <-1
m[28,20] <-1
m[28,33] <-1
m[28,6] <-1
m[28,3] <-1
m[28,8] <-1

m[29,35] <-1

m[30,17] <-1
m[30,34] <-1

m[31,25] <-1
m[31,24] <-1
m[31,34] <-1
m[31,19] <-1

m[32,41] <-1
m[33,20] <-1
m[33,22] <-1

m[34,18] <-1
m[34,23] <-1
m[34,19] <-1

m[35,11] <-1
m[35,10] <-1
m[35,5] <-1

m[37,28] <-1
m[37,33] <-1
m[37,22] <-1
m[37,20] <-1
m[37,8] <-1

m[38,10] <-1
m[38,23] <-1
m[38,37] <-1

#Adding in ties that are NA and not absent from girls without network data
m[26, ] <-NA
m[36, ] <-NA
m[39, ] <-NA
m[41, ] <-NA

m
#Create a network object with my adjacency edgelist and name it girls
?network

girls <-network(m, matrix.type="adjacency", directed = TRUE)
girls
summary(girls)

#Read in node attribute data (okay to leave it as a data frame)
#Vertex is synonym for node
#Create an object for it, called NodeInfo
NodeInfo<-read.csv("NodeAttributes2012_8.8.csv", header=TRUE, stringsAsFactors = FALSE)
NodeInfo

#Assigning NodeInfo (node attributes) to the network object girls
#using a "for loop"
for(i in 1:ncol(NodeInfo))
  girls%v%colnames(NodeInfo)[i]<-NodeInfo[,i]

summary(girls)

network.naedgecount(girls)
#DID IT SUCCESSFULLY! CAN SEE IN PRINTOUT THAT IT DETECTS MISSING EDGES

#look at a table of ties
table(girls[,])
#divide by 2 bc undirected
table(girls[,])/2
#look at as edgelist
as.edgelist(girls)
plot(as.edgelist(girls))

#Setting name of nodes to the nodeIDs I created in node file
network.vertex.names(girls) <- (girls%v%"NodeID")
network.vertex.names(girls) #Still producing #s assigned by R and not NodeIDs
plot(girls, label="name", displayisolates=TRUE, displaylabels=TRUE)
plot(girls, label="nodeID", displayisolates=TRUE,displaylabels=TRUE) #correct NodeIDs

###########################################################
#Description and Visualization of Small Network (no missing)

summary(girls) # Get an overall summary
network.dyadcount(girls) # How many dyads in girls?
network.edgecount(girls) # How many edges are present?
network.size(girls) # How large is the network?
as.sociomatrix(girls) # Show it as a sociomatrix
isolates(girls, diag=FALSE)

isol <-isolates(girls)
isol
network.vertex.names(girls)[isol] #showing R's #s and not NodeIDs

###How many mutual ties? How many triangles?
summary(girls~edges +mutual +triangle)


team <- girls %v% 'team' # %v% references vertex attributes
team
summary(team) # summarize the distribution of teams

table(component.dist(girls)$csize)

#Degree
degree(girls) # Default: total degree
ideg <- degree(girls, cmode="indegree") # Indegree
odeg <- degree(girls, cmode="outdegree") # Outdegree
all(degree(girls) == ideg+odeg) # In + out = total?

#Plotting network without colors
plot(girls, label="name", vertex.col="blue", displayisolates=TRUE, displaylabels=TRUE)

#Assigning colors to nodes based on 3 weight categories
par(mar=c(0,0,0,0))
plot(girls, vertex.col="weight.status.3", label="nodeID", displayisolates=T, displaylabels=T)
#Yay it worked! Correct IDs! Adding legend
vals<-sort(unique(girls%v%"weight.status.3"))
legend("topleft", fill=1:length(vals), legend=vals, bty = "n")

#Attempting to plot with colors showing race/ethnicity explore if this
#explains grouping
plot(girls, vertex.col="Ethnicity", label="nodeID", displayisolates=T, displaylabels=T)
vals<-sort(unique(girls%v%"Ethnicity"))
legend("topleft", fill=1:length(vals), legend=vals, bty = "n")
#Do not appear to be clustering based on ethnicity

#Plot by team
plot(girls, vertex.col="team", label="nodeID", displayisolates=T, displaylabels=T)
vals<-sort(unique(girls%v%"team"))
legend("topleft", fill=1:length(vals), legend=vals, bty = "n")
#legend colors incorrect for some reason... 3's are green, 4's are dark blue,
#5's are light blue, 6's are pink

################################################################

#Mixing matrices
mms<-mixingmatrix(girls,"weight.status.3")    # Compute w/a network object and
mms                                             # See what we have...
names(mms)                                      # This is a complex object
class(mms)
mms$matrix                                      # Extract the mixing matrix

# Can crudely visualize using plot.sociomatrix:
plot.sociomatrix(mms$matrix)                    # Must use matrix, not object
#darker plots have increased ties

##########################################################################
#ERGM

#####Creating a null model
model0<-ergm(girls~edges)
summary(model0)
network.density(girls)
#Testing goodness of fit for null model
plot(simulate(model0),vertex.col="weight.status.3")
gof0<-gof(model0~odegree + distance, verbose=FALSE)
plot(gof0)

model0.gof.global <- gof(model0 ~ odegree + idegree + esp + distance)
model0.gof.global
plot(model0.gof.global)
par(mfrow=c(1,3))

##### Model 1 - Adding reciprocity which can be hard to fit
summary(girls~edges +mutual)

model1 <-ergm(girls~edges + mutual)
summary(model1)
#Testing goodness of fit for this model
plot(simulate(model1),vertex.col="weight.status.3")
gof1<-gof(model1~odegree + distance, verbose=FALSE)
plot(gof1)

#### Model 2 - Adding transitive type (triangles) to assist in model fit
summary(girls~edges +mutual +triangle)
model2 <-ergm(girls~edges + mutual + triangle, control = control.ergm(MCMC.samplesize = 100000,
                       MCMC.burnin = 1000000, MCMC.interval = 1000))
###Got errors so dropping triangle term. Discard model.

#### New Model 2: Adding GWESP instead of triangle
model2 <-ergm(girls~edges + mutual + gwesp)
summary(model2)
gof2<-gof(model2~odegree + distance, verbose=FALSE)
plot(gof2)
plot(simulate(model2),vertex.col="weight.status.3")
model2.gof.global <- gof(model2 ~ odegree + idegree + esp + distance)
model2.gof.global
plot(model2.gof.global)
#Examine MCMC diagnostics
mcmc.diagnostics(model2)

####Overall appears to be converging, now want to find best fitting alpha
#Will set seed at 1 to be able to reproduce these estimates as well
gc()
model2a <-ergm(girls~edges + mutual + gwesp(.1,T),
               control=control.ergm(seed=1))
summary(model2a)
model2b <-ergm(girls~edges + mutual + gwesp(.2,T),
               control=control.ergm(seed=1))
summary(model2b)
model2c <-ergm(girls~edges + mutual + gwesp(.3,T),
               control=control.ergm(seed=1))
summary(model2c)
model2d <-ergm(girls~edges + mutual + gwesp(.4,T),
               control=control.ergm(seed=1))
summary(model2d)
model2e <-ergm(girls~edges + mutual + gwesp(.5,T),
               control=control.ergm(seed=1))
summary(model2e)
model2f <-ergm(girls~edges + mutual + gwesp(.6,T),
               control=control.ergm(seed=1))
summary(model2f)

## Chose model 2c where alpha=.3 Now go back and examine fit of this model.
mcmc.diagnostics(model2c)

##### Model 3 - Adding in component structure here hoping it helps with fit
model3 <-ergm(girls~edges + mutual + gwesp(.3,T) + nodefactor("component"),
     control=control.ergm(seed=1))
summary(model3)
#could not calculate stats! discard model

##### New Model 3: Adding main effects -- #weight status, PA, diet
model3 <-ergm(girls~edges + mutual + gwesp(.3,T) + nodefactor("weight.status.3") + nodecov("MVPA1")
              + nodecov("fvservt1") + nodecov("usservt1"),
                      control=control.ergm(seed=1))
summary(model3)
#converges, calculates

### Model 4: Adding interaction effects
model4 <-ergm(girls~edges + mutual + gwesp(.3,T) + nodefactor("weight.status.3") +
                nodecov("MVPA1") + nodecov("fvservt1") + nodecov("usservt1") +
                nodematch("weight.status.3", diff=FALSE) + absdiff("MVPA1") +
                absdiff("fvservt1") + absdiff("usservt1"),
              control=control.ergm(seed=1))
summary(model4)
#converges, calculates

### Model 5: Removing the main effects and interaction effects that weren't significant except
#for weight status related ones as those are of interest
model5 <-ergm(girls~edges + mutual + gwesp(.3,T) + nodefactor("weight.status.3") +
                nodematch("weight.status.3", diff=FALSE),
              control=control.ergm(seed=1))
summary(model5)
#converges, calculates

mcmc.diagnostics(model5)



