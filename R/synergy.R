muscleSynergy2D.rankEst = function(obj){
  xx = obj$filter$normalizedEmg
  tt = obj$time
  core = detectCores()
  nupper = round(ncol(xx) / 2)
  estimRank = nmf(xx,rank = 2:nupper,seed=123,.options=paste0("p",core))
  plot(estimRank)
}


muscleSynergy2D = function(obj,rank=4){
  xx = obj$filter$normalizedEmg
  tt = obj$time
  core = detectCores()
  nmfResult = nmf(xx,rank = rank,seed=123,.options=paste0("p",core))
  W = nmfResult@fit@W
  H = nmfResult@fit@H
  obj$synergy = nmfResult
}
