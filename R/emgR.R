emgList = function(x,t,name = colnames(x),freq=1000){
  if(class(x) != "matrix"){x = as.matrix(x)}
  require(signal)
  require(ggplot2)
  require(reshape2)
  require(mixtools)
  require(pheatmap)
  require(RColorBrewer)
  require(igraph)
  require(linkcomm)
  require(NMF)
  require(data.table)

  size = list()
  size$nrow = nrow(x)
  size$ncol = ncol(x)
  xx = data.frame(x,check.names = F)
  tt = t
  tt2 = tt - tt[1]
  if(is.null(freq)){
    freq = getFreq(tt2)
  }
  colnames(xx) = name
  obj = list(emg=xx,
             size = size,
             time=tt2,
             name = name,
             frequency=freq,
             start = tt[1],
             end = tail(tt,1))
  class(obj) = "emgR"
  return(obj)
}


print.emgR = function(obj){
  cat(class(obj),"\n")
  cat("size:",obj$size$nrow,"rows",obj$size$ncol,"columns","\n")
  cat("frequency:",obj$frequency,"Hz","\n")
  cat("Downsample:",obj$downsample,"Hz","\n")
  cat("names:",obj$name,"\n")
  cat("filter type:",obj$filter$param$filtType,"\n")
  cat("Normalize:",obj$filter$param$normalizeType,"\n")
  cat("Similarity:",obj$coh$cohType,"\n")
#  cat(str(obj))
}


