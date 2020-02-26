coherence = function(obj){
  xx = obj$filter$emg
  tt = obj$time
  freq = obj$frequency
  param = list(spans = c(3,5),taper = F,fast = F,plot = T)
  pg = spec.pgram(xx,
                  spans = param$spans,
                  taper = param$taper,
                  fast = param$fast,
                  plot = FALSE)
  coh = pg$coh
  ff = seq(1,freq/2000,length=nrow(coh))
  ii = 1:(ncol(xx) - 1)
  cohName = paste("coh",1:ncol(coh),sep="_")
  cohName = cbind(cohName,source=NA,target=NA)
  for(i in ii){
    for(j in (i + 1):ncol(xx)){
      cohName[i + (j - 1)*(j - 2) / 2,2] = colnames(xx)[i]
      cohName[i + (j - 1)*(j - 2) / 2,3] = colnames(xx)[j]
    }
  }
  colnames(coh) = cohName[,1]
  coh = as.data.frame(coh)
  out = list(coh=coh,name=cohName,param = param,cohType="Linear coherence")

  obj$coh = out
  obj
}

aggrCoh = function(obj,
                   cohRange,
                   name = NULL,
                   plot.it = TRUE,
                   ulim=0.5){

  if(is.null(name)){
    name = obj$name
  }

  coh = getCoh(obj,name = name)$coh
  cohName = getCoh(obj,name = name)$names

  freq = obj$frequency
  ran = cohRange

  colnames(coh) = paste(cohName[,2],cohName[,3],sep="-")

  ff = seq(0,freq/2,length.out = nrow(coh))
  selectCoh = (ff > ran[1] & ff <= ran[2])
  coh = coh[selectCoh,]
  ff = ff[selectCoh]

  aggr = apply(coh,2,function(x)mean(x,na.rm = T))
  src = sapply(strsplit(names(aggr),"-"),function(x)x[1])
  tgt = sapply(strsplit(names(aggr),"-"),function(x)x[2])
  aggr = data.frame(aggr,source=src,target=tgt,stringsAsFactors = F)

  aggrOut = aggr[,-1]
  aggrOut$weight = aggr[,1]

  aggr2 = aggr
  aggr2$source = aggr$target
  aggr2$target = aggr$source
  aggr = rbind(aggr,aggr2)

  rname = sort(unique(src))
  cname = sort(unique(tgt))
  cohMat = matrix(NA,nrow=length(rname),ncol=length(cname))
  rownames(cohMat) = colnames(cohMat) = rname

  for(i in 1:(nrow(cohMat)-1)){
    for(j in (i+1):(ncol(cohMat))){
      cohMat[i,j] = aggr[(aggr$source %in% rname[i]) & (aggr$target %in% cname[j]),1]
    }
  }
  diag(cohMat) = NA
  cohMat[lower.tri(cohMat)] = cohMat[upper.tri(cohMat)]

  breaks = c(seq(0,ulim,by=0.1))
  cols = colorRampPalette(brewer.pal(n = 7,name = "PuBuGn"))(length(breaks)+1)

  ph = pheatmap(cohMat,cluster_rows = F,cluster_cols = F,breaks = breaks,color = cols,silent = !plot.it)
  obj$coh$aggregate = list(adj=cohMat,thres=cohRange,heatmap=ph,egdelist = aggrOut)
  obj
}

cohNet = function(obj,
                  mode = c("bin","weighted"),
                  thres = 0.35,
                  plot.it = TRUE){
  el = obj$coh$aggregate$egdelist
  newEl = el
  coh = obj$coh$aggregate$adj
  diag(coh) = 1
  if(mode =="bin"){
    adj = coh
    adj[coh >= thres] = 1
    adj[coh < thres] = 0
    g = graph_from_adjacency_matrix(adj,weighted = T,mode = "undirected")
    newEl = el[el$weight>=thres,]
  }else if(mode == "weighted"){
    adj = coh
    g = graph_from_adjacency_matrix(adj,mode = "undirected")
  }

  g = simplify(g)

  if(plot.it){
    plot(g)
  }
  obj$coh$network = list(net = g, adj = adj, thres = thres, coh = coh,el = newEl)
  obj
}

getCoh = function(obj,name,rename = FALSE){
  coh = obj$coh$coh
  cohName = obj$coh$name
  targetName = name
  refName = obj$coh$name
  missing = !(targetName %in% refName)
  if(any(missing)){
    stop(cat(targetName[missing],
             "are missing! please specify correct names!\n"))
  }
  selectIdx = refName[,2] %in% targetName & refName[,3] %in% targetName
  selectCoh = coh[,selectIdx]
  selectCohName = cohName[selectIdx,]
  out = list(coh = selectCoh,names = selectCohName)
  out
}

