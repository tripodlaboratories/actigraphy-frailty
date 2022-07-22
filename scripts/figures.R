#script to generate scripts from results
library(ggplot2)
library(ggpmisc)
library(ggthemes)
library(reshape2)

datapath = "./submission_materials"

load(sprintf("%s/results/all_results.rda", datapath))
#load in previously processed data
load(sprintf("%s/data/organized_data.rda",datapath))


#list of features to separate based on continuous, binary, multiclass

#note nutrition is actually 3 classes but no one presented is malnurished
multiclass <- c("ScholorLevel", "EducativeLevel","EDUCATION_LEVEL","SMOKER_CURRENT","ALCOHOL")
binary <- c("Sex","SEX","Frail1","NUTRITION","GDS_DIAG","MORTALITY","FTS_VALID_","FTS5_clas" )


##############################################
### building bar plots for mean prediction ###
##############################################

#simply listing frailty pred scores
-log10(modelresults_frailty_mean[,"spear.pvs"])
modelresults_frailty_mean[,"spear.pvs"]
modelresults_frailty_mean[,"spear.rho"]

###########
# FRAILTY #
###########

long.frail <- melt(modelresults_frailty_mean)
levels(long.frail$Var1) <- c("Fried Score", "Rockwood Index", "FTS", "FTS5")

plot_obj = ggplot(data=NULL ,aes(x=long.frail[long.frail$Var2 == "spear.pvs",]$Var1, y=-log10(long.frail[long.frail$Var2 == "spear.pvs",]$value))) +
geom_bar(position="dodge", stat="identity", fill="grey") + xlab("Frailty Measures") + ylab("-log10(P-Value)") + theme_classic() +
theme(axis.text.x= element_text(angle=45), axis.text= element_text(size=25, hjust=1, colour="black"), axis.title = element_text(size=30, face="bold")) 

ggsave(file=sprintf("%s/figures/frailty_spear_pvs.pdf", datapath), plot=plot_obj, width=5, height=10)


###############
# FRAILTY RHO #
###############

plot_obj = ggplot(data=NULL ,aes(x=long.frail[long.frail$Var2 == "spear.rho",]$Var1, y=long.frail[long.frail$Var2 == "spear.rho",]$value)) +
geom_bar(position="dodge", stat="identity", fill="grey") + xlab("Frailty Measures") + ylab("Rho") + theme_classic() +
theme(axis.text.x= element_text(angle=45), axis.text= element_text(size=25, hjust=1, colour="black"), axis.title = element_text(size=30, face="bold")) 

ggsave(file=sprintf("%s/figures/frailty_spear_rho.pdf", datapath), plot=plot_obj, width=5, height=10)

#####################################################
### looking at acti prediction vs FTS correlation ###
#####################################################

long.frail <- melt(modelresults_frailty_mean)
long.risk <- melt(modelresults_risk_mean)

long.frail$Var1 <- as.character(long.frail$Var1)
long.risk$Var1 <- as.character(long.risk$Var1)

long.frail <- long.frail[long.frail$Var2 == "spear.pvs",]
long.risk <- long.risk[long.risk$Var2 == "spear.pvs",]

#organized predictions into single vector
activec <- c(long.frail$value,  long.risk$value)
names(activec) <- c(long.frail$Var1,  long.risk$Var1)
activec <- activec[order(names(activec))]

load(sprintf("%s/results/univariate_analysis.rda", datapath))


#organize frailty measures into vector
frailvec <- cor.matrix["FTS",dimnames(cor.matrix)[[2]] != "FTS","spear.pvs"]

frailvec <- frailvec[names(frailvec) %in% intersect(names(frailvec),names(activec))]
frailvec <- frailvec[order(names(frailvec))]

#remove frailty measure from predicted vector
activec <- activec[which(names(activec) != "FTS")]

activec <- activec[which(names(activec) != "WBTOT_FAT_PRE")]
frailvec <- frailvec[which(names(frailvec) != "WBTOT_FAT_PRE")]

#checking frailvec values that are displayed in paper
frailvec[c("MVC","MCHC", "GAIT_SPEED", "GaitSpeed", "SPPB_TOT_SCORE", "FOLLOW_UP_TIME_MORTALITY_WEEKS" )]

#remove binary/mutliclass responses
activec = activec[!(names(activec) %in% c(binary, multiclass))]

#check to see vectors are aligned
setdiff(names(activec), names(frailvec))
identical(names(activec), names(frailvec))

#plot comparison of only risksmat features

frailvec <- cor.matrix["FTS",dimnames(cor.matrix)[[2]] != "FTS","spear.pvs"]
frailvec <- frailvec[which(names(frailvec) %in% colnames(risksmat))]
frailvec <- frailvec[order(names(frailvec))]

activec <- activec[which(names(activec) %in% colnames(risksmat))]
activec <- activec[order(names(activec))]

activec <- activec[which(names(activec) != "WBTOT_FAT_PRE")]
frailvec <- frailvec[which(names(frailvec) != "WBTOT_FAT_PRE")]

#remove binary/mutliclass responses
activec = activec[!(names(activec) %in% c(binary, multiclass))]

identical(names(activec), names(frailvec))


plot_obj = ggplot(data=NULL, aes(x=-log10(activec), y=-log10(frailvec))) + geom_point(shape=1 ,size=6) +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Acitgraphy P-values")+ylab("Frailty Score P-values")+
theme_classic() + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold")) 

ggsave(file=sprintf("%s/figures/acti_pred_vs_FTS-risks.pdf",  datapath),plot=plot_obj, width=8,height=8)


plot_obj = ggplot(data=NULL, aes(x=-log10(activec), y=-log10(frailvec))) + geom_point(shape=1 ,size=6) +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Acitgraphy P-values")+ylab("Frailty Score P-values")+
theme_classic() + geom_text(aes(label=names(activec)),hjust=0, vjust=0)+
theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold")) 

ggsave(file=sprintf("%s/figures/acti_pred_vs_FTS-risks-named.pdf",  datapath),plot=plot_obj, width=8,height=8)

######################################################
### looking at acti prediction vs FTS5 correlation ###
######################################################

long.frail <- melt(modelresults_frailty_mean)
long.risk <- melt(modelresults_risk_mean)

long.frail$Var1 <- as.character(long.frail$Var1)
long.risk$Var1 <- as.character(long.risk$Var1)

long.frail <- long.frail[long.frail$Var2 == "spear.pvs",]
long.risk <- long.risk[long.risk$Var2 == "spear.pvs",]

#organized predictions into single vector
activec <- c(long.frail$value,  long.risk$value)
names(activec) <- c(long.frail$Var1,  long.risk$Var1)
activec <- activec[order(names(activec))]

load(sprintf("%s/results/univariate_analysis.rda", datapath))

#organize frailty measures into vector
frailvec <- cor.matrix["SHORT_FTS5_SCORE",dimnames(cor.matrix)[[2]] != "SHORT_FTS5_SCORE","spear.pvs"]
frailvec <- frailvec[order(names(frailvec))]

#remove frailty measure from predicted vector
activec <- activec[which(names(activec) != "SHORT_FTS5_SCORE")]

#remove binary/mutliclass responses
activec = activec[!(names(activec) %in% c(binary, multiclass))]

#plot comparison of only risksmat features
frailvec <- frailvec[which(names(frailvec) %in% colnames(risksmat))]
activec <- activec[which(names(activec) %in% colnames(risksmat))]

#check to see vectors are aligned
identical(names(activec), names(frailvec))

plot_obj = ggplot(data=NULL, aes(x=-log10(activec), y=-log10(frailvec))) + geom_point(shape=1, size=4) +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Acitgraphy P-values")+ylab("Frailty Score P-values")+
theme_classic() + theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold")) + xlim(0, 65) 

ggsave(file=sprintf("%s/figures/pred_vs_FTS5-risks.pdf", datapath), plot=plot_obj, width=8, height=8)


###################################################################
### acti prediction of FTS5 vs actual FTS5 correlation to risks ###
###################################################################

load(sprintf("%s/results/%s_results.rda", datapath, "SHORT_FTS5_SCORE"))
load(sprintf("%s/data/organized_data.rda",datapath))

actual.fts5 <-  frailtymat[, "SHORT_FTS5_SCORE"]
pred.fts5 <- colMeans(modelpreds, na.rm=TRUE)


#get correlation of predicted vs actual FTS5 scores
realfts5.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(actual.fts5, risksmat[,x], method="spearman")$p.value)
	})

predfts5.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(pred.fts5, risksmat[,x], method="spearman")$p.value)
	})



#plot comparison
plot_obj = ggplot(data=NULL, aes(y=-log10(realfts5.cors), x=-log10(predfts5.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Frailty P-values")+ylab("Frailty Score P-values")+
theme_classic() + xlim(0, max(-log10(realfts5.cors)+1, na.rm=TRUE)) +
ylim(0, max(-log10(realfts5.cors)+1, na.rm=TRUE)) + theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold")) 

ggsave(file=sprintf("%s/figures/predicted_FTS5_vs_FTS5-risks.pdf",  datapath), plot=plot_obj, width=8, height=8)


#################################################################
### acti prediction of FTS vs actual FTS correlation to risks ###
#################################################################

load(sprintf("%s/results/%s_results.rda", datapath, "FTS"))
load(sprintf("%s/data/organized_data.rda",datapath))

actual.fts <-  frailtymat[, "FTS"]
pred.fts <- colMeans(modelpreds, na.rm=TRUE)



#get correlation of predicted vs actual FTS scores
realfts.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(actual.fts, risksmat[,x], method="spearman")$p.value)
	})

predfts.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(pred.fts, risksmat[,x], method="spearman")$p.value)
	})
names(realfts.cors) <- colnames(risksmat)
names(predfts.cors) <- colnames(risksmat)


realfts.cors <- realfts.cors[which(names(realfts.cors) != "WBTOT_FAT_PRE")]
predfts.cors <- predfts.cors[which(names(predfts.cors) != "WBTOT_FAT_PRE")]


#plot comparison
plot_obj = ggplot(data=NULL, aes(y=-log10(realfts.cors), x=-log10(predfts.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Frailty P-values")+ylab("Frailty Score P-values")+
theme_classic() + xlim(0, max(-log10(realfts5.cors)+1, na.rm=TRUE)) +
ylim(0, max(-log10(realfts.cors), na.rm=TRUE)) + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold")) 

ggsave(file=sprintf("%s/figures/predicted_FTS_vs_FTS-risks.pdf",  datapath), width=8, height=8)


###########################################################################
### acti prediction of ROCKWOOD vs actual ROCKWOOD correlation to risks ###
###########################################################################


load(sprintf("%s/results/%s_results.rda", datapath, "ROCKWOOD_INDEX"))
load(sprintf("%s/data/organized_data.rda",datapath))


actual.rockwood <-  frailtymat[, "ROCKWOOD_INDEX"]
pred.rockwood <- colMeans(modelpreds, na.rm=TRUE)


#get correlation of predicted vs actual FTS scores
realrockwood.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(actual.rockwood, risksmat[,x], method="spearman")$p.value)
	})

predrockwood.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(pred.rockwood, risksmat[,x], method="spearman")$p.value)
	})
names(realrockwood.cors) <- colnames(risksmat)
names(predrockwood.cors) <- colnames(risksmat)



#plot comparison
plot_obj = ggplot(data=NULL, aes(y=-log10(realrockwood.cors), x=-log10(predrockwood.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Rockwood P-values")+ylab("Rockwood P-values")+
theme_classic() + xlim(0, max(-log10(realrockwood.cors)+1, na.rm=TRUE)) +
ylim(0, max(-log10(realrockwood.cors), na.rm=TRUE)) + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold"))


ggsave(file=sprintf("%s/figures/predicted_rockwood_vs_rockwood-risks.pdf", datapath),plot=plot_obj,  width=8, height=8)


#prediction with names
plot_obj = ggplot(data=NULL, aes(y=-log10(realrockwood.cors), x=-log10(predrockwood.cors), label = names(realrockwood.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Rockwood P-values")+ylab("Rockwood P-values")+
theme_classic() + xlim(0, max(-log10(realrockwood.cors)+1, na.rm=TRUE)) +geom_text(hjust=0, vjust=0)+
ylim(0, max(-log10(realrockwood.cors), na.rm=TRUE)) + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold"))


ggsave(file=sprintf("%s/figures/predicted_rockwood_vs_rockwood-risks_named.pdf", datapath), plot= plot_obj,  width=8, height=8)




#####################################################################
### acti prediction of FRIED vs actual FRIED correlation to risks ###
#####################################################################

load(sprintf("%s/results/%s_results.rda", datapath, "FRIED_SCORE"))
load(sprintf("%s/data/organized_data.rda",datapath))


actual.fried <-  frailtymat[, "FRIED_SCORE"]
pred.fried <- colMeans(modelpreds, na.rm=TRUE)


#get correlation of predicted vs actual FTS scores
realfried.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(actual.fried, risksmat[,x], method="spearman")$p.value)
	})

predfried.cors <- sapply(seq(ncol(risksmat)), FUN=function(x){
	return(cor.test(pred.fried, risksmat[,x], method="spearman")$p.value)
	})
names(realfried.cors) <- colnames(risksmat)
names(predfried.cors) <- colnames(risksmat)


#plot comparison
plot_obj = ggplot(data=NULL, aes(y=-log10(realfried.cors), x=-log10(predfried.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Fried P-values")+ylab("Fried P-values")+
theme_classic() + xlim(0, max(-log10(realfried.cors)+1, na.rm=TRUE)) +
ylim(0, max(-log10(realfried.cors), na.rm=TRUE)) + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold"))


ggsave(file=sprintf("%s/figures/predicted_fried_vs_fried-risks.pdf", datapath), plot=plot_obj, width=8, height=8)


#plot comparison
plot_obj = ggplot(data=NULL, aes(y=-log10(realfried.cors), x=-log10(predfried.cors), label=names(predfried.cors))) + geom_point(shape=1 ,size=6)  +
geom_abline(slope=1, intercept=0, linetype="longdash", colour="red")+xlab("Predicted Fried P-values")+ylab("Fried P-values")+
theme_classic() + xlim(0, max(-log10(realfried.cors)+1, na.rm=TRUE)) + geom_text(hjust=0, vjust=0) +
ylim(0, max(-log10(realfried.cors), na.rm=TRUE)) + theme(axis.text= element_text(size=30), axis.title = element_text(size=35, face="bold"))


ggsave(file=sprintf("%s/figures/predicted_fried_vs_fried-risks_named.pdf", datapath), plot=plot_obj, width=8, height=8)


#########################################################
### looking at acti prediction vs Frailty correlation ###
#########################################################

load(sprintf("%s/data/organized_data.rda",datapath))

#################
### FTS Plots ###
#################

resp = "FTS"

load(file=sprintf("%s/results/%s_results.rda", datapath, resp))

#looking at all predictions averaged to a single value across all predictions
avg.pred <- colMeans(modelpreds, na.rm=TRUE)

-log10(cor.test(avg.pred, frailtymat[,resp], method="spearman")$p.value)

plot_obj = ggplot(data=NULL, aes(x=avg.pred, y=frailtymat[, resp])) + geom_point() + stat_smooth(method="lm", col="red")+
theme_classic() + xlab("Predicted FTS") + ylab("Actual FTS") + 
theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold"))

ggsave(file = sprintf("%s/figures/predicted_vs_actual_FTS.pdf",datapath), plot=plot_obj, width=8, height=8)

############################
### Rockwood index Plots ###
############################

resp = "ROCKWOOD_INDEX"

load(file=sprintf("%s/results/%s_results.rda", datapath, resp))

#looking at all predictions averaged to a single value across all predictions
avg.pred <- colMeans(modelpreds, na.rm=TRUE)

-log10(cor.test(avg.pred, frailtymat[,resp], method="spearman")$p.value)

plot_obj = ggplot(data=NULL, aes(x=avg.pred, y=frailtymat[, resp])) + geom_point() + stat_smooth(method="lm", col="red")+
theme_classic() + xlab("Predicted Rockwood Index") + ylab("Actual Rockwood Index") +
theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold"))

ggsave(file = sprintf("%s/figures/predicted_vs_actual_rockwood_index.pdf" ,datapath), width=8, height=8)


##################
### FTS5 Plots ###
##################

resp = "SHORT_FTS5_SCORE"

load(file=sprintf("%s/results/%s_results.rda", datapath, resp))

#looking at all predictions averaged to a single value across all predictions
avg.pred <- colMeans(modelpreds, na.rm=TRUE)

model <- lm(avg.pred ~ frailtymat[,resp])
summary(model)$r.squared

eq <- substitute(~~italic(R)^2~"="~r2,
		list(r2= format(summary(model)$r.squared, digits=3)))
label <- as.character(as.expression(eq))

plot_obj = ggplot(data=NULL, aes(x=avg.pred, y=frailtymat[, resp])) + geom_point() +
stat_smooth(method="lm", col="red", level=0.95) + theme_classic() + xlab("Predicted FTS5") +
ylab("Actual FTS5") + #geom_text(x = 20, y = 10, label = "label", parse = TRUE) +
theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold"))


ggsave(file = sprintf("%s/figures/predicted_vs_actual_FTS5.pdf",datapath), plot=plot_obj, width=8, height=8)


#########################
### FRIED SCORE Plots ###
#########################

resp = "FRIED_SCORE"

load(file=sprintf("%s/results/%s_results.rda", datapath, resp))

#looking at all predictions averaged to a single value across all predictions
avg.pred <- colMeans(modelpreds, na.rm=TRUE)

model <- lm(avg.pred ~ frailtymat[,resp])
summary(model)$r.squared

eq <- substitute(~~italic(R)^2~"="~r2, 
		list(r2= format(summary(model)$r.squared, digits=3)))
label <- as.character(as.expression(eq))

plot_obj = ggplot(data=NULL, aes(x=avg.pred, y=frailtymat[, resp])) + geom_point() +
stat_smooth(method="lm", col="red", level=0.95) + theme_classic() + xlab("Predicted Fried Score") +
ylab("Actual Fried Score") + #geom_text(x = 20, y = 10, label = "label", parse = TRUE) +
theme(axis.text= element_text(size=25), axis.title = element_text(size=30, face="bold")) 

ggsave(file = sprintf("%s/figures/predicted_vs_actual_FRIED_SCORE.pdf",datapath), plot = plot_obj, width=8, height=8)



#######################################################################
## looking at frailty class via FTS5 to compare to pre existing work ##
#######################################################################


resp = "SHORT_FTS5_CLASIFIC"
load(file=sprintf("%s/results/%s_results.rda", datapath, resp))

#looking at all predictions averaged to a single value across all predictions
avg.pred <- colMeans(modelpreds, na.rm=TRUE)

library(pROC)
auroc = roc(classmat[!is.na(avg.pred),resp]~avg.pred[!is.na(avg.pred)], dir="<")

pdf(sprintf("%s/figures/fts5_class_roc.pdf",datapath), height=7, width=7, useDingbats=FALSE)
plot(auroc,xlab = "FPR", ylab = "TPR", cex.axis=2, cex.lab=2)
title("FTS5 Frailty Class", adj = 0, line = 2.5, cex.main=2)
text(0.4, 0.4, paste0("AUC: ", round(auroc$auc,3)), pos=4, cex=2)
dev.off()
