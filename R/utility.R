getFreq = function(t){
  tt = t
  if(tt[1] != 0){
    tt = tt - tt[1]
  }
  n = length(tt)
  elapsedTime = tail(tt,1)
  freq = round(n / elapsedTime, 0)
  freq
}

rectify = function(x){
  xx = x
  if(class(xx)!="matrix"){xx = as.matrix(xx)}
  rxx = apply(xx,2,function(x)abs(x - mean(x)))
  rxx
}

downSample = function(obj,freq = 250){
  xx = obj$filter$emg
  tt = obj$time
  nn = obj$size$nrow * (freq / obj$frequency)
  approxOut = apply(obj$emg,2,function(x)approx(tt,x,method = "linear",n = nn))
  ttout = sapply(approxOut,function(x)x$x)[,1]
  xxout = sapply(approxOut,function(x)x$y)
  rownames(xxout) = 1:nrow(xxout)
  colnames(xxout) = colnames(xx)

  # obj$time = ttout
  # obj$emg = xxout
  # obj$size$nrow = nrow(obj$emg)
  # obj$size$ncol = ncol(obj$emg)
  # obj$frequency = freq
  # obj$start = ttout[1]
  # obj$end = tail(ttout,1)
  # obj$downsample = freq
  # obj
}

addAnnotation = function(obj,ids=NULL,annoFile){

  anno = fread(annoFile,header = TRUE,data.table=F)
  tt = obj$time
  freq = obj$frequency

  anno$startIdx = round(anno$start*freq,0)
  anno$endIdx = round(anno$end*freq - 1,0)

  obj$annotation = anno
  obj
}


