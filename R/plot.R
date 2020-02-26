plot.emgR = function(obj,
                     type=c("emg","coh"),
                     files="emgR.jpeg",
                     what = c("raw","filtered"),
                     name=NULL,
                     cohRange=c(0,50),...){
  if(type == "emg"){
    if(what == "raw"){
      df = data.frame(time=obj$time,obj$emg)
    }else if(what == "filtered"){
      df = data.frame(time=obj$time,obj$filter$emg)
    }else{
      stop("Please specify \"raw\" or \"filtered\" for argument \"what\".")
    }
    df = melt(df,id.vars="time",variable.name = "muscle",value.name = "vol")
    gg = ggplot(df,aes(x = time,y = vol,color = muscle)) + geom_line()
    gg = gg + facet_wrap(facets = muscle ~.,ncol=2) + theme_bw()
    dev = unlist(strsplit(files,"\\."))[2]
    cat("saving to",files,"\n")
    ggsave(filename = files,plot = gg,device = dev)
  }

  if(type == "coh"){
    if(is.null(name)){
      name = obj$name
    }
    coh = getCoh(obj,name = name)$coh
    cohName = getCoh(obj,name = name)$names
    freq = obj$frequency
    ran = cohRange

    colnames(coh) = paste(cohName[,2],cohName[,3],sep="-")


    #ff = seq(0,freq/2000,length.out = nrow(coh))
    ff = seq(0,freq/2,length.out = nrow(coh))
    selectCoh = (ff > ran[1] & ff <= ran[2])
    coh = coh[selectCoh,]
    ff = ff[selectCoh]


    df = data.frame(frequency=ff,coh,check.names = F)
    df = melt(df,id.vars = "frequency",variable.name = "muscle",value.name = "coherence")
    df$source = sapply(strsplit(as.character(df$muscle),"-"),function(x)x[1])
    df$target = sapply(strsplit(as.character(df$muscle),"-"),function(x)x[2])
    df2 = df
    df2$source = df$target
    df2$target = df$source
    df = rbind(df,df2)
    gg = ggplot(df,aes(x = frequency, y = coherence)) +
      geom_point(alpha=0.05,shape=16,size=0.8) +
      geom_smooth() +
      ylim(c(0,1))

    gg = gg + facet_grid(facets = source ~ target) + theme_bw()
    dev = unlist(strsplit(files,"\\."))[2]
    cat("saving to",files,"\n")
    ggsave(gg,filename = files,device = dev,width = 15,height = 15)
  }
}



