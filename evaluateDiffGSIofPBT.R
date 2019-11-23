# evalute simulations with PBT groups having similar
#   GSI compositions

library(tidyverse)
library(cowplot)

################
## Sample Rate
###############

load("./rdaOutputs/diffGSIofPBT.rda")

# check if MLE failed to converge on any simulations
table(convergeMLE, useNA="ifany")

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
SE <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_tag100[,x])^2)
colnames(SE) <- names(trueComp)
SE <- data.frame(sr = as.factor(srRec), SE)
MSE_srSD_tag100 <- SE %>% gather("group", "estimate", c(2:36)) %>% group_by(sr, group) %>% summarize(NoEx = mean(estimate))

# SD SPIBETR=FALSE
SE <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_spibetrFALSE[,x])^2)
colnames(SE) <- names(trueComp)
SE <- data.frame(sr = as.factor(srRec), SE)
MSE_srSD_spibetrFALSE <- SE %>% gather("group", "estimate", c(2:36)) %>% group_by(sr, group) %>% summarize(TotEx = mean(estimate))

# SD normal
SE <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_mean[,x])^2)
colnames(SE) <- names(trueComp)
SE <- data.frame(sr = as.factor(srRec), SE)
MSE_srSD_mean <- SE %>% gather("group", "estimate", c(2:36)) %>% group_by(sr, group) %>% summarize(Ac = mean(estimate))

# MLE
SE <- sapply(names(trueComp), function(x) (trueComp[x] - srMLE_mean[,x])^2)
colnames(SE) <- names(trueComp)
SE <- data.frame(sr = as.factor(srRec), SE)
MSE_srMLE_mean <- SE %>% gather("group", "estimate", c(2:36)) %>% group_by(sr, group) %>% summarize(MLE = mean(estimate))

# now summarize data
allMSE <- MSE_srSD_tag100 %>% left_join(MSE_srSD_spibetrFALSE, by=c("sr", "group")) %>%
	left_join(MSE_srSD_mean, by=c("sr", "group")) %>% 
	left_join(MSE_srMLE_mean, by=c("sr", "group")) %>% gather("type", "MSE", 3:6)


GSIselect <- c("GSIgroup3", "GSIgroup9")
PBTselect <- c("pbtGroup8", "pbtGroup11", "pbtGroup25")


MSE_out <- allMSE %>% filter(group %in% c(PBTselect, GSIselect) & sr %in% seq(.1, .4, .1)) %>% filter(type != "TotEx") %>% 
	mutate(MSE = round(MSE, 0)) %>% spread(type, MSE) %>% 
	select(sr, group, MLE, Ac, NoEx) %>% arrange(sr, group)


## MSE table for select groups for ms
write.table(MSE_out, "tables/selectDiffGSIofPBTMSE.txt", sep = "\t", row.names = F, quote = F)

## MSE table for all groups for supplementary
write.table(mutate(allMSE, MSE = round(MSE, 0)), "tables/allDiffGSIofPBTMSE.txt", sep = "\t", row.names = F, quote = F)

ggplot(data = allMSE, aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = filter(allMSE, type %in% c("Ac", "MLE")), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1))
