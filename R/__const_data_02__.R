
home = "~/BMHI Dropbox/Matsui Yusuke/matsui/git/emgR/"
setwd(home)
library(data.table)

load("inst/data/CYCLE_TIMES.RData")
load("inst/data/RAW_EMG.RData")
cycs = 10
#npoints = 200
emg = vector("list",length(RAW_EMG))
ids = gsub("RAW_EMG_","",names(RAW_EMG))
pid = sapply(strsplit(ids,"_"),function(x)x[1])
tid = sapply(strsplit(ids,"_"),function(x)x[2])
names(emg) = ids
for(id in seq_along(RAW_EMG)){
  emg_data = RAW_EMG[[id]]
  emg_data$t = 0:(nrow(emg_data)-1)
  c_time = CYCLE_TIMES[[id]]
  freq <- round(nrow(emg_data)/(tail(emg_data$Time, 1)+emg_data$Time[2]), 0)
  cyc_stance <- cyc_swing <-
    matrix(NA,nrow=cycs,ncol=4,
           dimnames = list(NULL,c("st","ed","cycle","phase")))
  cyc_stance = as.data.frame(cyc_stance)
  for(jj in  1:cycs){
    # Stance
    t1   <- round(c_time[jj,1]*freq+1, 0)
    t2   <- round((c_time[jj,1]+c_time[jj,2])*freq+1, 0)
    if (t1>nrow(emg_data) || t2>nrow(emg_data)) {
      cycs <- jj-1
      break
    } else {
      #temp  <- emg_data[t1:t2,]
      cyc_stance[jj,]  <- c(t1,t2,jj,"stance")
    }
    # Swing
    t1   <- (c_time[jj,1]+c_time[jj,2])*freq+1
    t2   <- c_time[jj+1,1]*freq+1
    cyc_swing[jj,] = c(t1,t2,jj,"swing")
    # Check if there is data
    if (sum(t1:t2, na.rm=T)==0) next
  }
  cycle_anno = rbind(cyc_stance,cyc_swing)
  cycle_emg = vector("list",cycs)
  names(cycle_emg) = 1:cycs
  for(i in seq_along(cycle_emg)){
    anno = cycle_anno[cycle_anno$cycle==i,]
    idx1 = anno[anno$phase=="stance",1:2]
    idx2 = anno[anno$phase=="swing",1:2]
    stance = emg_data[idx1$st:idx1$ed,]
    swing = emg_data[idx2$st:idx2$ed,]
    #x1 = apply(stance,2,function(x)approx(x,method="linear",n = npoints/2))
    #x2 = apply(swing,2,function(x)approx(x,method="linear",n = npoints/2))
    x1 = data.frame(pid = pid[id],tid=tid[id],seqid=idx1$st:idx1$ed,cycle=i,phase="stance",Time = stance$Time,stance[,!colnames(stance)%in%c("Time","t")])
    x2 = data.frame(pid = pid[id],tid=tid[id],seqid=idx2$st:idx2$ed,cycle=i,phase="swing",Time = swing$Time,swing[,!colnames(swing)%in%c("Time","t")])
    cycle_emg[[i]] = rbind(x1,x2)
  }
  emg[[id]] = do.call(rbind,cycle_emg)
  cat(id,"\n")
}
emg = do.call(rbind,emg)
rownames(emg) = NULL
select_tid = "01"
emg = emg[emg$tid==select_tid,]
#select_pid = unique(emg$pid)[1:10]
#emg = emg[emg$pid%in%select_pid,]
save(emg,file="inst/data/emg.rda")
