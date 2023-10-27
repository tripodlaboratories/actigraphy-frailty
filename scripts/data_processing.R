#script to process data given to me by nima
library(openxlsx)
library(missForest)

datapath = "./actigraphy-frailty/"

temp = read.xlsx(sprintf('%s/data/BD STANFORD 20_06_01.xlsx', datapath), 1)
ids=temp[,1]
data=temp[,-1]
rownames(data) = ids

##frailty scores
frailtyscores=c("FRIED_SCORE", "ROCKWOOD_INDEX", "FTS", 'SHORT_FTS5_SCORE')

##frailty classification
frailclass = c("FRIED_CLASIFICATION","SHORT_FTS5_CLASIFIC")

##risk factors
risks=colnames(temp)[which(colnames(temp)=="NUMBER_DRUGS"):which(colnames(temp)=="FERRITIN")]


#################################################
### organizing data into respective matrices ####
#################################################

##Read actigraphy data
temp=read.csv(sprintf('%s/data/actigraph_export/hi_DailyDetailed.csv', datapath))
agdids=as.vector(temp[,2])
for(i in seq(length(agdids))){
    if(substr(agdids[i],1,1)=='h'){
        agdids[i]=gsub("^hi1 (.*)\\.agd$", "\\1", agdids[i])
    }    
    if(substr(agdids[i],1,1)!='h'){
        agdids[i]=gsub("^(.*)\\.agd$", "\\1", agdids[i])
    }    
}
agddata=temp

##Select patients who have both kinds of data
iids=intersect(agdids, ids)

##matrix of random clinical variables (age, sex, etc.)
clinmat=matrix(NA, length(iids), 12)
for (i in seq(length(iids))){
    clinmat[i,]=unlist(data[which(iids[i]==ids),1:12])
}
colnames(clinmat)=colnames(data)[1:12]
rownames(clinmat) = iids

##matrix of risk factors
risksmat=matrix(NA, length(iids), length(risks))
for (i in seq(length(iids))){
    risksmat[i,]=as.numeric(unlist(data[which(iids[i]==ids),risks]))
}
colnames(risksmat) = risks
rownames(risksmat) = iids

##matrix of frailty measurements
frailtymat=matrix(NA, length(iids), length(frailtyscores))
for (i in seq(length(iids))){
    frailtymat[i,]=as.numeric(unlist(data[which(iids[i]==ids),frailtyscores]))
}
colnames(frailtymat)=frailtyscores
rownames(frailtymat) = iids

##matrix of frailty classifications
classmat= matrix(NA, length(iids), length(frailclass))
for( i in seq(length(iids))){
     classmat[i,] = as.numeric(unlist(data[which(iids[i] == ids), frailclass]))
}
colnames(classmat) = frailclass
rownames(classmat) = iids


temp=read.xlsx(sprintf('%s/data/BD STANFORD 20_06_01.xlsx', datapath), 2)
id=temp[,1]
temp=temp[,-c(1,2,38,39)]
breakmat=matrix(NA, length(iids), ncol(temp))
for (i in seq(length(iids))){
    breakmat[i,]=as.numeric(unlist(temp[which(iids[i]==ids),]))
}
colnames(breakmat)=colnames(temp)
rownames(breakmat) = iids


temp=read.xlsx(sprintf('%s/data/BD STANFORD 20_06_01.xlsx', datapath), 3)
id=temp[,1]
temp=temp[,-c(1,2,8)]
fts5.breakmat=matrix(NA, length(iids), ncol(temp))
for (i in seq(length(iids))){
    fts5.breakmat[i,]=as.numeric(unlist(temp[which(iids[i]==ids),]))
}
colnames(fts5.breakmat)=colnames(temp)
rownames(fts5.breakmat) = iids

   
#cleaning up the agd (from acti life) matrix.      
agdmat=matrix(NA, length(iids), 51)
for (i in seq(length(iids))){
    temp=agddata[which(agdids==iids[i]),10:ncol(agddata)]
    temp=matrix(as.numeric(unlist(temp)), nrow=nrow(temp))
    agdmat[i,]=colMeans(temp)
}
colnames(agdmat)=colnames(agddata)[10:ncol(agddata)]
mat=agdmat
rownames(agdmat) = iids
rownames(mat) = iids

#############################################
### imputing missing data via missForest ####
#############################################


#imputing missing variables into clinical matrix (0.1% NAs)
set.seed(2017)
ms=missForest(clinmat)
clinmat=ms$ximp

#imputing missing variables into risk factor matrix (1.9% NAs, meadian NA count 8 among columns)
set.seed(2017)
ms=missForest(risksmat)
risksmat=ms$ximp

#8.5% NAs, median NA count 4 among columns - dominanted by Albumin/Albumin_score being completely NA, ignored in imputation
set.seed(2017)
ms=missForest(breakmat)
breakmat=ms$ximp

#1.6% NAs, median NA count 6.5 among columns
set.seed(2017)
ms=missForest(fts5.breakmat)
fts5.breakmat=ms$ximp

#5.6% NAs, median NA count 17 among columns
set.seed(2017)
ms=missForest(frailtymat)
frailtymat=ms$ximp

#############################################
### saving all data into single location ####
#############################################

save(file=sprintf("%s/data/organized_data.rda",datapath), classmat, clinmat, risksmat, breakmat, fts5.breakmat, mat, frailtymat, agdmat)
