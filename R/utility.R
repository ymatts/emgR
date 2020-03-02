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

downSample = function(obj,freq = 200){
  xx1 = obj$filter$emg
  xx2 = obj$filter$normalizedEmg

  tt = obj$time
  nn = round(obj$size$nrow * (freq / obj$frequency))

  approxOut1 = apply(xx1,2,function(x)approx(tt,x,method = "linear",n = nn))
  ttout1 = sapply(approxOut1,function(x)x$x)[,1]
  xxout1 = sapply(approxOut1,function(x)x$y)

  approxOut2 = apply(xx2,2,function(x)approx(tt,x,method = "linear",n = nn))
  ttout2 = sapply(approxOut2,function(x)x$x)[,1]
  xxout2 = sapply(approxOut2,function(x)x$y)

  rownames(xxout1) = rownames(xxout2) = 1:nrow(xxout1)
  colnames(xxout1) = colnames(xxout2) = colnames(xx1)

  obj$time = ttout
  obj$filter$emg = xxout1
  obj$filter$normalizedEmg = xxout2
  obj$size$nrow = nrow(obj$emg)
  obj$size$ncol = ncol(obj$emg)
  obj$frequency = freq
  obj$start = ttout[1]
  obj$end = tail(ttout,1)
  obj$downsample = freq
  obj
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


