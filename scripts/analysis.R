#script for analyzing frailty data via repeated train/test XGboost
library(xgboost)
library(caret)
library(Metrics)

datapath = "./actigraphy-frailty"

#load in previously processed data
load(sprintf("%s/data/organized_data.rda",datapath))

#analysis parameters
repeats = 50

#load raw data to parse for indices of missing data so they can properly be removed for responses
library(openxlsx)

#read sheets and concantenate
data = list()
for(i in seq(3)){
    #read ith sheet
    temp = read.xlsx(sprintf('%s/data/BD STANFORD 20_06_01.xlsx', datapath), i)
    ids=temp[,1]
    data[[i]]=temp[,-1]
    rownames(data[[i]]) = ids
}

#check order of rows for each sheet
identical(rownames(data[[1]]), rownames(data[[2]]))
identical(rownames(data[[2]]), rownames(data[[3]]))

#compress into a single matrix
is_here = !is.na(do.call(cbind, data))
is_here = is_here[rownames(frailtymat),] #align with other strucs

###############################################
### model prediction of frailty measurements ##
###############################################

#need to give 'mat' rownames...
rownames(mat) = rownames(agdmat)

# generate train/test splits prior to analysis
test <- array(data=NA, dim=c(repeats, nrow(mat)/2))
train <- array(data=NA, dim=c(repeats,ceiling(nrow(mat) - nrow(mat)/2)))

set.seed(2020)
for(i in seq(repeats)){
	test[i,] <- sample(rownames(mat), nrow(mat)/2, replace=FALSE)
	train[i,] <- setdiff(rownames(mat), test[i,])
}


modelresults_frailty <- array(data=NA, dim=c(repeats, ncol(frailtymat) ,5) )
dimnames(modelresults_frailty)[[2]] <- colnames(frailtymat)
dimnames(modelresults_frailty)[[3]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs", "RMSE")

modelresults_frailty_mean <- array(data=NA, dim=c( ncol(frailtymat) ,5) )
dimnames(modelresults_frailty_mean)[[1]] <- colnames(frailtymat)
dimnames(modelresults_frailty_mean)[[2]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs", "RMSE")


#iterate over every response
#NOTE: models may perform slighlt different due to change in random seed between R versions, however this should be a negligible difference
set.seed(2020)
for(resp in colnames(frailtymat)){
	print(sprintf("Models running for %s",resp))
	Y <- frailtymat[,resp]

	#model fitting loop, saving each iterations model
	modelpreds <- array(data=NA, dim=c(repeats, nrow(mat)))
    colnames(modelpreds) <- rownames(mat)

	modelresults <- array(data=NA, dim=c(repeats,5) )
	dimnames(modelresults)[[2]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs", "RMSE")

	models <- list()

	#for a given response fot 'repeats' number of train/test splits
	for(i in seq(repeats)){
        #remove imputed values from process
        train_iter <- intersect(train[i,], rownames(is_here)[is_here[,resp]])
        test_iter <- intersect(test[i,], rownames(is_here)[is_here[,resp]])

		model <- xgboost(mat[train_iter,], label=Y[train_iter], nrounds=50, verbose=0)

		pred <- predict(model, mat[test_iter, ])

		#record model/result information
		modelpreds[i,test_iter] <- pred

		models[[i]] <- model

		modelresults[i,"spear.pvs"] <- cor.test(Y[test_iter], pred, method="spearman")$p.value
		modelresults[i,"spear.rho"] <- cor.test(Y[test_iter], pred, method="spearman")$estimate

		modelresults[i,"pear.pvs"] <- cor.test(Y[test_iter], pred, method="pearson")$p.value
		modelresults[i,"pear.rho"] <- cor.test(Y[test_iter], pred, method="pearson")$estimate

		modelresults[i,"RMSE"] <- rmse(Y[test_iter], pred)

		#save model/results objects
		save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
			models, modelpreds, modelresults)
	}

	modelresults_frailty[,resp,] <- modelresults

	modelresults_frailty_mean[resp,"spear.pvs"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$p.value
	modelresults_frailty_mean[resp,"spear.rho"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$estimate

	modelresults_frailty_mean[resp,"pear.pvs"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="pearson")$p.value
	modelresults_frailty_mean[resp,"pear.rho"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="pearson")$estimate

	modelresults_frailty_mean[resp,"RMSE"] <- RMSE(Y, colMeans(modelpreds, na.rm=TRUE))



	meanpreds = -log10(cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$p.value)

	cat(sprintf("-log10 pvalues\nmean prediction p-Value: %s\n\n", meanpreds))

	#save model/results objects
	save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
		models, modelpreds, modelresults)
}




##########################################################
### model prediction for FTS and FRIED frailty classes ###
##########################################################
library(pROC)

modelresults_class <- array(data=NA, dim=c(repeats, ncol(classmat) ,2) )
dimnames(modelresults_class)[[2]] <- colnames(classmat)
dimnames(modelresults_class)[[3]] <- c("wilcox.pvs", "AURPC")

modelresults_class_mean <- array(data=NA, dim=c( ncol(classmat) ,2) )
dimnames(modelresults_class_mean)[[1]] <- colnames(classmat)
dimnames(modelresults_class_mean)[[2]] <- c("wilcox.pvs", "AUROC")

set.seed(2020)
#iterate over every respo2se
for(resp in colnames(classmat)){
	print(sprintf("models running for %s",resp))
	Y <- classmat[,resp]

    #transform fried to non-frail vs frail+pre-frail
    if(resp == "FRIED_CLASIFICATION"){
            Y[Y == 3] = 2
    }

	#model fitting loop, saving each iterations model
	modelpreds <- array(data=NA, dim=c(repeats, nrow(mat)))
    colnames(modelpreds) <- rownames(mat)

	modelresults <- array(data=NA, dim=c(repeats,2) )
	dimnames(modelresults)[[2]] <- c("wilcox.pvs", "AUROC")

	models <- list()

	#for a given response fot 'repeats' number of train/test splits
	for(i in seq(repeats)){
        #remove imputed values from process
        train_iter <- intersect(train[i,], rownames(is_here)[is_here[,resp]])
        test_iter <- intersect(test[i,], rownames(is_here)[is_here[,resp]])

        model <- xgboost(mat[train_iter,], label=Y[train_iter]-1, nrounds=50, verbose=0, objective="binary:logistic", eval_metric="logloss")
        pred <- predict(model, mat[test_iter, ])#select the vector with most prominent label

		#record model/result information
		modelpreds[i,test_iter] <- pred

		models[[i]] <- model


        modelresults[i,"wilcox.pvs"] <- wilcox.test(pred, Y[test_iter], paired=FALSE)$p.value

        modelresults[i,"AUROC"] <- roc(Y[test_iter], pred, dir="<", quiet=TRUE)$auc

        #save model/results objects
		save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
			models, modelpreds, modelresults)
	}

	modelresults_class[,resp,] <- modelresults

	modelresults_class_mean[resp,"wilcox.pvs"] <- wilcox.test(Y, colMeans(modelpreds, na.rm=TRUE), paired=FALSE)$p.value
	modelresults_class_mean[resp,"AUROC"] <- roc(Y, colMeans(modelpreds, na.rm=TRUE),dir="<", quiet=TRUE)$auc


	meanpreds = -log10(wilcox.test(Y, colMeans(modelpreds, na.rm=TRUE), paired=FALSE)$p.value)

	cat(sprintf("-log10 pvalues\nmean prediction: %s\n\n", meanpreds))

    #save model/results objects
	save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
		models, modelpreds, modelresults)
}


########################################
### model prediction of risk factors ###
########################################

#NOTE: nutrition is actually 3 classes but no one presented is malnurished
multiclass <- c("ScholorLevel", "EducativeLevel","EDUCATION_LEVEL","SMOKER_CURRENT","ALCOHOL")
binary <- c("Sex","SEX","Frail1","NUTRITION","GDS_DIAG","MORTALITY","FTS_VALID_","FTS5_clas" )

modelresults_risk <- array(data=NA, dim=c(repeats, ncol(risksmat) ,7) )
dimnames(modelresults_risk)[[2]] <- colnames(risksmat)
dimnames(modelresults_risk)[[3]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs", "wilcox.pvs", "kruskal.pvs", "RMSE")

modelresults_risk_mean <- array(data=NA, dim=c( ncol(risksmat) ,7) )
dimnames(modelresults_risk_mean)[[1]] <- colnames(risksmat)
dimnames(modelresults_risk_mean)[[2]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs",  "wilcox.pvs", "kruskal.pvs","RMSE")

set.seed(2020)
#iterate over every response
for(resp in colnames(risksmat)){
	print(sprintf("Models running for %s",resp))
	Y <- risksmat[,resp]

	#model fitting loop, saving each iterations model
	modelpreds <- array(data=NA, dim=c(repeats, nrow(mat)))
    colnames(modelpreds) = rownames(mat)

	modelresults <- array(data=NA, dim=c(repeats,7) )
	dimnames(modelresults)[[2]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs",  "wilcox.pvs", "kruskal.pvs","RMSE")

	models <- list()

	#for a given response fot 'repeats' number of train/test splits
	for(i in seq(repeats)){
		train_iter <- intersect(train[i,], rownames(is_here)[is_here[,resp]])
        test_iter <- intersect(test[i,], rownames(is_here)[is_here[,resp]])

        if(resp %in% binary){
            #binary classification response training call
		    model <- xgboost(mat[train_iter,], label=as.integer(as.factor(Y[train_iter]))-1, nrounds=50, verbose=0, objective="binary:logistic", eval_metric="logloss")
            pred <- predict(model, mat[test_iter, ])#select the vector with most prominent label

		     #record model/result information
		     modelpreds[i,test_iter] <- pred

        }else if(resp %in% multiclass){
            if(length(table(Y[train_iter])) == 2){
                next #skip iterations with
            }

            #mutliclass response training call
		    model <- xgboost(mat[train_iter,], label=Y[train_iter]-1, nrounds=50, verbose=0, num_class = length(unique(Y[train_iter])), objective="multi:softprob", eval_metric="mlogloss")
            pred <- predict(model, mat[test_iter, ], reshape=T)[,which(min(table(Y[train_iter])) == table(Y[train_iter]))[1]]

		     #record model/result information
		    modelpreds[i,test_iter] <- pred
            modelresults[i,"kruskal.pvs"] <- kruskal.test(pred, as.factor(Y[test_iter]))$p.value
        }else{
            #continuous response training call
		    model <- xgboost(mat[train_iter,], label=Y[train_iter], nrounds=50, verbose=0, objective="reg:squarederror")
            pred <- predict(model, mat[test_iter, ])

            #record model/result information
		    modelpreds[i,test_iter] <- pred
        }

		models[[i]] <- model

        if(resp %in% binary){
            modelresults[i,"wilcox.pvs"] <- wilcox.test(pred, Y[test_iter], paired=FALSE)$p.value

        }else if(resp %in% multiclass){
            # apply kruskal.test a non-parametric test that extends mann-whitney to more than 2 groups
            modelresults[i,"kruskal.pvs"] <- kruskal.test(pred, as.factor(Y[test_iter]))$p.value

        }else{
            modelresults[i,"spear.pvs"] <- cor.test(Y[test_iter], pred, method="spearman")$p.value
		    modelresults[i,"spear.rho"] <- cor.test(Y[test_iter], pred, method="spearman")$estimate

		    modelresults[i,"pear.pvs"] <- cor.test(Y[test_iter], pred, method="pearson")$p.value
		    modelresults[i,"pear.rho"] <- cor.test(Y[test_iter], pred, method="pearson")$estimate

            modelresults[i,"RMSE"] <- rmse(Y[test_iter], pred)
        }



		#save model/results objects
		save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
			models, modelpreds, modelresults)
	}

	modelresults_risk[,resp,] <- modelresults

    if(resp %in% binary){
        modelresults_risk_mean[resp,"wilcox.pvs"] <- wilcox.test(colMeans(modelpreds, na.rm=TRUE), Y, paired=FALSE)$p.value

        meanpreds = -log10(wilcox.test(colMeans(modelpreds, na.rm=TRUE), Y, paired=FALSE)$p.value)
    }else if(resp %in% multiclass){
        # apply kruskal.test a non-parametric test that extends mann-whitney to more than 2 groups
        modelresults_risk_mean[resp,"kruskal.pvs"] <- kruskal.test(colMeans(modelpreds,na.rm=TRUE), as.factor(Y))$p.value

        meanpreds = -log10(kruskal.test(colMeans(modelpreds,na.rm=TRUE), as.factor(Y))$p.value)
    }else{
        modelresults_risk_mean[resp,"spear.pvs"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$p.value
        modelresults_risk_mean[resp,"spear.rho"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$estimate

        modelresults_risk_mean[resp,"pear.pvs"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="pearson")$p.value
        modelresults_risk_mean[resp,"pear.rho"] <- cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="pearson")$estimate

        modelresults_risk_mean[resp,"RMSE"] <- RMSE(Y, colMeans(modelpreds, na.rm=TRUE))


        meanpreds = -log10(cor.test(Y, colMeans(modelpreds, na.rm=TRUE), method="spearman")$p.value)
    }



	#cat(sprintf("-log10 pvalues\nmean prediction: %s\n\n", meanpreds))

	#save model/results objects
	save(file = sprintf("%s/results/%s_results.rda", datapath, resp),
		models, modelpreds, modelresults)
}



save(file= sprintf("%s/results/all_results.rda", datapath), modelresults_frailty, modelresults_risk, modelresults_class,
	modelresults_frailty_mean, modelresults_risk_mean, modelresults_class_mean)

##############################################################
### univariate analysis between frailty measures and risks ###
##############################################################

#NOTE: While Fried is an integer score and can be considered ordinal we
#treat it as continuous due to the continuous nature of the underlying phenomena
#being measured (frailty) and if predictable by activity data all distinct scores
#should indicate distinct levels of predicted frailty, ie high rank-sum statistic and
#high spearman rho (monotonically increasing predicted value when ordered by Fried score)


combined.data <- cbind(risksmat, frailtymat, breakmat, fts5.breakmat)

#remove categorical data
combined.data = combined.data[,!(colnames(combined.data) %in% c(multiclass, binary))]

cor.matrix <- array(data=NA, dim=c(ncol(combined.data),ncol(combined.data),4))
dimnames(cor.matrix)[[1]] <- colnames(combined.data)
dimnames(cor.matrix)[[2]] <- colnames(combined.data)
dimnames(cor.matrix)[[3]] <- c("spear.rho", "spear.pvs", "pear.rho", "pear.pvs")


for(i in colnames(combined.data)){

	for(j in colnames(combined.data)){

		if(i != j){
			cor.matrix[i, j, "spear.pvs"] <- cor.test(combined.data[,i], combined.data[,j], method="spearman")$p.value
			cor.matrix[i, j, "spear.rho"] <- cor.test(combined.data[,i], combined.data[,j], method="spearman")$estimate
			
			cor.matrix[i, j, "pear.pvs"] <- cor.test(combined.data[,i], combined.data[,j], method="pearson")$p.value
			cor.matrix[i, j, "pear.rho"] <- cor.test(combined.data[,i], combined.data[,j], method="pearson")$estimate
		}

	}
}

save(file = sprintf("%s/results/univariate_analysis.rda", datapath), cor.matrix)

