filter = function(obj,
                  method = "butterworth",
                  param = list(order_high = 4,
                              order_low = order_high,
                              filtFreq = c(20,50))){

  if(method %in% c("butterworth","btw")){
    emgR::btw(obj,
        order_high = param$order_high,
        order_low = param$order_low,
        filtFreq = param$filtFrea)
  }else if(method %in% "arv"){
    emgR::arv(obj,
        k = param$k)
  }else if(method %in% "rmv"){
    emgR::rms(obj,
        k = param$k)
  }

}

bandpass = function(obj,
               order = 4,
               filtFreq = c(5,50)){
  xx = obj$emg
  tt = obj$time
  freq = obj$frequency
  normFreq = 2 * filtFreq / freq

  pf1 = butter(order,normFreq,type = "pass",plane = "z")

  filt = apply(xx,2,function(x)filtfilt(pf,x))

  filt = abs(filt)
  filt[filt < 0] = min(filt[filt>0])
  rownames(filt) = rownames(xx)
  colnames(filt) = colnames(xx)
  filt = data.frame(filt,check.names = F)
  obj$filter = list(emg = filt, param=list(order=order,filtFreq = filtFreq,filtType = "Butterwoth"))
  obj
}


envelop = function(obj,
                   order = 4,
                   filtFreq = c(5,50)){
  xx = obj$emg
  tt = obj$time
  freq = obj$frequency

  fn = filtFreq / (freq/2)            # Normalise by the Nyquist frequency (f/2)
  names(fn) = c("low","high")

  HP = butter(order, fn["high"], type="high")
  LP = butter(order, fn["low"], type="low")

  filtHp = apply(xx,2,function(x)filtfilt(HP,x))
  filtHp = abs(filtHp)
  filtHpLp = apply(filtHp,2,function(x)filtfilt(LP,x))

  envelope = filtHpLp
  envelope[envelope < 0] = min(envelope[envelope > 0])
  envelope = data.frame(envelope,check.names = F)
  obj$filter = list(emg = envelope, param=list(order=order,filtFreq = filtFreq,filtType = "envelope"))
  obj
}


arv = function(obj, k = 10){
  xx = obj$emg
  tt = obj$time
  freq = obj$frequency
  if(k < 2){stop("Nearest neighbor k is less than 2. Set more larger r")}
  w = rep(1/k, k)
  rxx = rectify(xx)
  filt = apply(rxx,2,function(x)stats::filter(x, filter = w, sides = 2, method = "convolution"))
  filt[filt < 0] = min(filt[filt>0])
  filt = data.frame(filt,check.names = F)
  rownames(filt) = rownames(xx)
  colnames(filt) = colnames(xx)

  obj$filter = list(emg = filt,param=list(knn = k),filtType = "ARV")
  obj
}


rms = function(obj,k = 10){
  xx = obj$emg
  tt = obj$time
  freq = obj$frequency
  if(k < 2){stop("moving average time point is less than 2. Set more larger r")}
  w = rep(1/k, k)
  rxx = rectify(xx)

  filt = apply(rxx*rxx,2,function(x)stats::filter(x, filter = w, sides = 2, method = "convolution"))
  rownames(filt) = rownames(xx)
  colnames(filt) = colnames(xx)
  filt[filt < 0] = min(filt[filt>0])
  filt = data.frame(filt,check.names = F)

  obj$filter = list(emg = filt,param=list(knn = k,filtType = "RMS"))

  obj
}



