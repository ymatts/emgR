moduleFinder = function(obj,plot.it=TRUE,plot.type=c("member","graph")){
  net = obj$coh$network
  el = net$el[,-3]
  G = net$net
  oc = linkcomm::getOCG.clusters(el,verbose = FALSE)
  if(plot.it){
    if(plot.type == "graph"){
      p = par(mfcol=c(1,2),mar=c(1,1,1,1))
      plot(oc$igraph,vertex.size=1,main="Orignal coherent muscle network")
      plot(oc,type="graph",main="Coherent muscle modules")
      par(p)
    }else if(plot.type == "member")
      plot(oc,type="members")
  }

  # clique = igraph::max_cliques(G,min = 3)
  # cliqueGraph = lapply(clq,function(x)igraph::induced_subgraph(G,vids = as.numeric(x)))

  # p = par(mfcol=c(4,4),mar=c(1,1,1,1))
  # for(i in seq_along(clqGraph)){
  #   g = cliqueGraph[[i]]
  #   V(g)$label.cex = 1.5
  #   plot(g,vertex.size = 4)
  # }
  # par(p)

  obj$network$module = list(module = oc$nodeclusters,
                                oc = oc)
  obj
}

