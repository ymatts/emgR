muscleSynergy2D = function(obj){
  xx = obj$downsample
  tt = obj$time
  core = detectCores()
  #estimRank = nmf(xx,2:3,seed=123,.Options=paste0("p",core))
  estimRank = nmf(xx,2:6,seed=123)
}
