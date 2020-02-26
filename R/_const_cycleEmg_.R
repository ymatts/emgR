home = "~/Dropbox/matsui/gitR/EMG/data/Example/"
setwd(home)
library(data.table)

cycleTime = fread("CYCLE_TIMES_P0001_01.dat",header = F,data.table = F)

stanceAnno = swingAnno = gaitAnno = matrix(NA,nrow=nrow(cycleTime),ncol=4)
colnames(stanceAnno) = colnames(swingAnno) = c("id","type","start","end")

stanceAnno[,"id"] = swingAnno[,"id"] = 1:nrow(cycleTime)
stanceAnno[,"type"] = "Stance"
swingAnno[,"type"] = "Swing"

for(i in 1:nrow(cycleTime)){

  #contact (stance start time)
  contact = cycleTime[i,2]

  #stance end time & swing start time
  stanceEnd = contact + cycleTime[i,3]

  stanceAnno[i,"start"] = contact
  stanceAnno[i,"end"] = stanceEnd

  #swing (end time)
  if(i < nrow(cycleTime)){
    swingEnd = cycleTime[i+1,2]
    swingAnno[i,"start"] = stanceEnd
    swingAnno[i,"end"] = swingEnd
  }

}

anno = data.frame(rbind(stanceAnno,swingAnno),check.names = F,stringsAsFactors = F)

fwrite(anno,file="~/Dropbox/matsui/git/emgR/misc/gaitAnno.txt",row.names = F,col.names = T,sep = "\t")


save(anno,file="~/Dropbox/matsui/git/emgR/data/gaitAnno.rda")
