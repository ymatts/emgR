normalize = function(obj,method="MVC",p = 0.99){
  xx = obj$filter$emg

  if(method=="MVC"){

    mvc = apply(xx,2,function(x)x / max(x))
    obj$filter$normalizedEmg = mvc
    obj$filter$param$normalizeType = "MVC"

  }else if(method=="rMVC"){

    mixObj = apply(xx,2,function(x)normalmixEM(x = x,
                                               k = 2,
                                               mean.constr = c(0,0),
                                               sigma = c(1,2),
                                               maxit = 5000))

    sigmas = sapply(mixObj,function(x)max(x$sigma))
    bound = sapply(sigmas,function(x)qnorm(p,
                                           mean = 0,
                                           sd = x))
    xx2 = sapply(1:ncol(xx),function(ii)replace(xx[,ii],
                                                abs(xx[,ii]) >= bound[ii],
                                                bound[ii]))
    #xx2 = sapply(1:ncol(xx),function(ii)replace(xx[,ii],abs(xx[,ii]) >= bound[ii], xx[,ii]*bound[ii] / max(xx[,ii])))
    rownames(xx2) = rownames(xx)
    colnames(xx2) = colnames(xx)

    mvc = apply(xx2,2,function(x)x / max(x))

    obj$filter$normalizedEmg = mvc
    obj$filter$param$normalizeType = "rMVC"
  }
  obj
}
