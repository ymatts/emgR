muscleSynergy2D.rankEst = function(obj,ranks=NULL){
  xx = obj$filter$emg
  tt = obj$time
  core = detectCores()
  if(is.null(ranks)){
    nupper = round(ncol(xx) / 2)
    ranks = 1:nupper
  }
  estimRank = nmf(xx,rank = ranks,seed=123,.options=paste0("p",core))
  obj$synergy$estimRank = estimRank
  obj
}


muscleSynergy2D = function(obj,rank=4){
  xx = obj$filter$normalizedEmg
  tt = obj$time
  core = detectCores()
  nmfResult = nmf(xx,rank = rank,seed=123,.options=paste0("p",core))
  W = nmfResult@fit@W
  H = nmfResult@fit@H
  obj$synergy$W = nmfResult@fit@W
  obj$synergy$H = nmfResult@fit@H
  obj$synergy$rank = rank
  obj

}
