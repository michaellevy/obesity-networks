calcAlters = function(at, net, tieDir = "either") {
  # Returns a vector of vertices' alters' average attribute values
  # at is a vertex attribute
  # net is a network object
  # tieDir is "either", "in", or "out" specifying whether to return ego's nominated or nominated-by alters' values
  
  sapply(seq_len(network.size(net)), function(i) {
    
    # Find i's alters:
    if(!is.directed(net) | tieDir == "either") {
      alters = net[i, ] | net[, i]
    } else if(tieDir == "in") {
      alters = as.logical(net[, i])
    } else if(tieDir == "out") {
      alters = as.logical(net[i, ])  # Outgoing on rows
    } else stop("tieDir should be either (default), in or out")
    
    # Get their attribute values
    alterAvg = if(!sum(alters)) # If no alters, give them the overall mean
      mean((net %v% at)) else 
        mean((net %v% at)[alters])
    
    return(alterAvg)
  })
}