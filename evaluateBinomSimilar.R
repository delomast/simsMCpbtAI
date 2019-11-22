# evalute simulations of an additional binomial variable with similar values
#  between hatchery and wild populations

library(ggplot2)
library(cowplot)
library(tidyverse)


################
## Sample Rate
###############

load("./rdaOutputs/addBinomVarSimilar.rda")

## trueComp not calculated in script, so need to load from previous rda file
trueComp <- sapply("./rdaOutputs/sampRateLarge.rda", function(x){
	load(x) #load within function provides temporary environment, so nothing is overwritten
	return(trueComp)
})
trueComp <- trueComp[,1]

## now need to combine trueComp with true proportions for the binary variable
trueCompList <- list()
for(i in 1:length(trueVarMat)){
	tempMat <- trueVarMat[[i]] * trueComp
	trueCompList[[i]] <- c(tempMat[,1], tempMat[,2])
	names(trueCompList[[i]]) <- c(paste0(names(trueComp), "_cat1"), paste0(names(trueComp), "_cat2"))
}

### looking at SD and MLE

# sampling variation means sometimes groups that are present are not sampled
# replacing these "NA" estimates with 0 for looking at error of point estimates

srSD_mean <- apply(srSD_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})
srSD_tag100 <- apply(srSD_tag100, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})
srSD_spibetrFALSE[,grepl("GSIgroup", colnames(srSD_spibetrFALSE))] <- apply(srSD_spibetrFALSE[,grepl("GSIgroup", colnames(srSD_spibetrFALSE))], 2, 
																									 function(x){
	x[is.na(x)] <- 0
	return(x)
})
srMLE_mean <- apply(srMLE_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})


###################
## MSE
###################

# SD 100%
SE <- mapply(function(t,e){
		(t - e)^2
	}, trueCompList, split(srSD_tag100, row(srSD_tag100)))
SE <- as.tibble(t(SE))
MSE_srSD_tag100 <- SE %>% gather("group", "estimate", c(2:ncol(SE))) %>% group_by(group) %>% summarize(NoEx = mean(estimate))

# SD SPIBETR=FALSE
SE <- mapply(function(t,e){
		(t - e)^2
	}, trueCompList, split(srSD_spibetrFALSE, row(srSD_spibetrFALSE)))
SE <- as.tibble(t(SE))
MSE_srSD_spibetrFALSE <- SE %>% gather("group", "estimate", c(2:ncol(SE))) %>% group_by(group) %>% summarize(TotEx = mean(estimate))

# SD normal
SE <- mapply(function(t,e){
		(t - e)^2
	}, trueCompList, split(srSD_mean, row(srSD_mean)))
SE <- as.tibble(t(SE))
MSE_srSD_mean <- SE %>% gather("group", "estimate", c(2:ncol(SE))) %>% group_by(group) %>% summarize(Ac = mean(estimate))

# MLE
SE <- mapply(function(t,e){
		(t - e)^2
	}, trueCompList, split(srMLE_mean, row(srMLE_mean)))
SE <- as.tibble(t(SE))
MSE_srMLE_mean <- SE %>% gather("group", "estimate", c(2:ncol(SE))) %>% group_by(group) %>% summarize(MLE = mean(estimate))

# now summarize data
allMSE <- MSE_srSD_tag100 %>% left_join(MSE_srSD_spibetrFALSE, by=c("group")) %>%
	left_join(MSE_srSD_mean, by=c("group")) %>% 
	left_join(MSE_srMLE_mean, by=c("group")) %>% gather("type", "MSE", 2:5)


ggplot(data = allMSE, aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))

PBTselect <- c(paste0(c("pbtGroup8", "pbtGroup11", "pbtGroup25"), "_cat1"), paste0(c("pbtGroup8", "pbtGroup11", "pbtGroup25"), "_cat2"))
GSIselect <- c(paste0(c("GSIgroup3", "GSIgroup9"), "_cat1"), paste0(c("GSIgroup3", "GSIgroup9"), "_cat2"))


MSE_out <- allMSE %>% filter(group %in% c(PBTselect, GSIselect)) %>% filter(type != "TotEx") %>% 
	mutate(MSE = round(MSE, 0)) %>% spread(type, MSE) %>% 
	select(group, MLE, Ac, NoEx)


## MSE table for select groups for ms
write.table(MSE_out, "tables/selectBinomSimilarMSE.txt", sep = "\t", row.names = F, quote = F)

## MSE table for all groups for supplementary
write.table(mutate(allMSE, MSE = round(MSE, 0)), "tables/allBinomSimilarMSE.txt", sep = "\t", row.names = F, quote = F)

