library(data.table)
library(igraph)
mbAdj = fread("misc/mnet.txt",header = TRUE,drop = 1,data.table = F)
rownames(mbAdj) = fread("misc/mnet.txt",header = TRUE,select = 1,data.table = F)[,1]
mbAdj = mbAdj[,-271]
mbAdj[is.na(mbAdj)] = 0

elidx = which(mbAdj!=0,arr.ind = TRUE)
el = cbind(rownames(mbAdj)[elidx[,1]],"connect to",colnames(mbAdj)[elidx[,2]])

mbAdj = as.matrix(mbAdj)
bbAdj = mbAdj %*% t(mbAdj)
mmAdj = t(mbAdj) %*% mbAdj

diag(mmAdj) = 0
diag(bbAdj) = 0

# save(mmAdj,file="data/mmAdj.rda")
# save(bbAdj,file="data/bbAdj.rda")

mbBinAdj = mbAdj
mmBinAdj = mmAdj
bbBinAdj = bbAdj
mbBinAdj[mbAdj > 0] = 1
mmBinAdj[mmAdj > 0] = 1
bbBinAdj[bbAdj > 0] = 1

# save(mmBinAdj,file="data/mmBindj.rda")
# save(bbBinAdj,file="data/bbBinAdj.rda")

mName = colnames(mbAdj)
bName = rownames(mbAdj)

mAbbr = sapply(strsplit(mName," "),function(x)paste(toupper(substr(tail(x,2),1,1)),collapse = ""))
muscleDb = cbind(desc = mName,abbr = mAbbr)

bAbbr = sapply(strsplit(bName," "),function(x)paste(toupper(substr(tail(x,2),1,1)),collapse = ""))
boneDb = cbind(desc = bName,abbr = bAbbr)

hsa.skmn = list(sif = el,
                muscle_name = muscleDb,
                bone_name = boneDb,
                muscle_bone_adj = mbAdj,
                muscle_muscle_adj = mmAdj,
                bone_bone_adj = bbAdj,
                muscle_bone_bin_adj = mbBinAdj,
                muscle_muslce_bin_adj = mmBinAdj,
                bone_bone_bin_adj = bbBinAdj
                )

save(hsa.skmn,file="data/hsa.skmn.rda")
