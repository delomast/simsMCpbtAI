# evalute simulations of variable sampling rate
library(tidyverse)
library(cowplot)

################
## Sample Rate
###############

load("./rdaOutputs/sampRateLarge.rda")

### looking at SD and MLE

# check if MLE failed to converge on any simulations
table(convergeMLE, useNA="ifany")


# sampling varaition means sometimes groups that are present are not sampled
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
## some representative graphs of estimates with MLE and all versions of SD vs true value
###################


## all groups for a given sample rate
tempSR <- .20
pdf("graphs/allGroupsSR20.pdf")
for(group in names(trueComp)){
	
	dataPlot <- data.frame(estim = srMLE_mean[srRec == tempSR, group], type = "MLE")
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_mean[srRec == tempSR, group], type = "Ac"))
	if(grepl("GSIgroup", group)) dataPlot <- rbind(dataPlot, data.frame(estim = srSD_spibetrFALSE[srRec == tempSR, group], type = "TotEx"))
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_tag100[srRec == tempSR, group], type = "NoEx"))
	
	p <- ggplot(dataPlot, aes(x=type, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75), fill="#A4A4A4") + 
	theme(legend.position = "none")
	
	#make the plot and add mean and true line
	print(
		p + stat_summary(fun.y=mean, geom = "point", color="red", size = 5) + 
			geom_hline(aes(yintercept = trueComp[group]), linetype = "dashed", color = "blue", size = 2) +
			# theme(text = element_text(size = 30)) + 
			xlab("Estimate type") + ylab("Population size") + ggtitle(paste(group, "sample rate:", tempSR)) +
			ylim(.95 * min(srSD_mean[,group], srSD_tag100[,group], srSD_spibetrFALSE[,group], na.rm = T), 
				  1.05 * max(srSD_mean[,group], srSD_tag100[,group], srSD_spibetrFALSE[,group], na.rm = T))
	)

}
dev.off()


PBTselect <- c("pbtGroup8", "pbtGroup11", "pbtGroup25")
graphs <- list()
for(group in PBTselect){
	dataPlot <- data.frame(estim = srMLE_mean[srRec == tempSR, group], type = "MLE")
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_mean[srRec == tempSR, group], type = "Ac"))
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_tag100[srRec == tempSR, group], type = "NoEx"))
	
	graphs[[group]] <- ggplot(dataPlot, aes(x=type, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75), fill="#A4A4A4") +
	theme(legend.position = "none", axis.text = element_text(size=12, colour = "black"), axis.title = element_text(size=12, colour = "black")) + 
		stat_summary(fun.y=mean, geom = "point", color="red", size = 3) +
		# ggtitle(paste(group)) +
			ylim(.95 * min(dataPlot$estim),
				  1.05 * max(dataPlot$estim)) + ylab("Population size") + 
	scale_y_continuous(breaks = seq(round(min(dataPlot$estim), -1), round(max(dataPlot$estim), -1), 
											  by = round((max(dataPlot$estim) - min(dataPlot$estim)) / 4, -1)))
}
## some manipulations that weren't possible in the loop
graphs[[1]] <- graphs[[1]] + geom_hline(aes(yintercept = trueComp["pbtGroup8"]), linetype = "dashed", color = "blue", size = 1) +
				theme(axis.line.x=element_blank(),axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x = element_blank()) 
graphs[[2]] <- graphs[[2]] + geom_hline(aes(yintercept = trueComp["pbtGroup11"]), linetype = "dashed", color = "blue", size = 1) +
	 theme(axis.line.x=element_blank(),axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x = element_blank()) 
graphs[[3]] <- graphs[[3]] + geom_hline(aes(yintercept = trueComp["pbtGroup25"]), linetype = "dashed", color = "blue", size = 1) +
	xlab("Estimate Type") 

save_plot("graphs/selectPBTsampRate.pdf", plot_grid(plotlist = graphs, nrow = 3, labels ="AUTO"), base_asp = .4, base_height = 9.5)


GSIselect <- c("GSIgroup3", "GSIgroup9")
graphs <- list()
for(group in GSIselect){
	dataPlot <- data.frame(estim = srMLE_mean[srRec == tempSR, group], type = "MLE")
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_mean[srRec == tempSR, group], type = "Ac"))
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_spibetrFALSE[srRec == tempSR, group], type = "TotEx"))
	dataPlot <- rbind(dataPlot, data.frame(estim = srSD_tag100[srRec == tempSR, group], type = "NoEx"))
	
	graphs[[group]] <- ggplot(dataPlot, aes(x=type, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75), fill="#A4A4A4") +
	theme(legend.position = "none", axis.text = element_text(size=12, colour = "black"), axis.title = element_text(size=12, colour = "black")) + 
		stat_summary(fun.y=mean, geom = "point", color="red", size = 3) +
		# ggtitle(paste(group)) +
			ylim(.95 * min(dataPlot$estim),
				  1.05 * max(dataPlot$estim)) + ylab("Population size") + 
	scale_y_continuous(breaks = seq(round(min(dataPlot$estim), -1), round(max(dataPlot$estim), -1), 
											  by = round((max(dataPlot$estim) - min(dataPlot$estim)) / 4, -1)))
}
## some manipulations that weren't possible in the loop
graphs[[1]] <- graphs[[1]] + geom_hline(aes(yintercept = trueComp["GSIgroup3"]), linetype = "dashed", color = "blue", size = 1) +
				theme(axis.line.x=element_blank(),axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x = element_blank()) 
graphs[[2]] <- graphs[[2]] + geom_hline(aes(yintercept = trueComp["GSIgroup9"]), linetype = "dashed", color = "blue", size = 1) +
	xlab("Estimate Type") 

save_plot("graphs/selectGSIampRate.pdf", plot_grid(plotlist = graphs, nrow = 2, labels ="AUTO"), base_asp = .4, base_height = 9.5)


## now all groups across sampling rates
for(type in c("MLE", "Ac", "TotEx", "NoEx")){
	pdf(paste0("./graphs/allGroupsAllSamps_", type, ".pdf"))
	for(group in names(trueComp)){
		if (type == "MLE") dataPlot <- data.frame(estim = srMLE_mean[,group], sr = as.factor(srRec))
		if (type == "Ac") dataPlot <- data.frame(estim = srSD_mean[,group], sr = as.factor(srRec))
		if (type == "TotEx") dataPlot <- data.frame(estim = srSD_spibetrFALSE[,group], sr = as.factor(srRec))
		if (type == "NoEx") dataPlot <- data.frame(estim = srSD_tag100[,group], sr = as.factor(srRec))
		
		if(type == "TotEx" && grepl("pbt", group)) next
		
		p <- ggplot(dataPlot, aes(x=sr, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75)) + 
				theme(legend.position = "none")
		#make the plot and add mean and true line
		print(
			p + stat_summary(fun.y=mean, geom = "point", color="red", size = 2) + 
				geom_hline(aes(yintercept = trueComp[group]), linetype = "dashed", color = "blue", size = 1) +
				geom_vline(aes(xintercept = 3.5), linetype = "solid", color = "black", size = 1)+
				xlab("Sampling rate") + ylab("Population size") + ggtitle(paste(type, group)) +
				ylim(.95 * min(srMLE_mean[,group], srSD_mean[,group], srSD_spibetrFALSE[,group], srSD_tag100[,group]), 
					  1.05 * max(srMLE_mean[,group], srSD_mean[,group], srSD_spibetrFALSE[,group], srSD_tag100[,group]))
		)
	}
	dev.off()
}

rm(dataPlot)
rm(graphs)
rm(p)

###################
## MSE
###################
trueComp # simulated values

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


ggplot(data = filter(allMSE, sr == .05), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = filter(allMSE, sr == .1), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = filter(allMSE, sr == .2), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = filter(allMSE, sr == .3), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = filter(allMSE, sr == .4), aes(x=group, y=MSE, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))


MSE_out <- allMSE %>% filter(group %in% c(PBTselect, GSIselect) & sr %in% seq(.1, .4, .1)) %>% filter(type != "TotEx") %>% 
	mutate(MSE = round(MSE, 0)) %>% spread(type, MSE) %>% 
	select(sr, group, MLE, Ac, NoEx) %>% arrange(sr, group)


## MSE table for select groups for ms
write.table(MSE_out, "tables/selectSampRateMSE.txt", sep = "\t", row.names = F, quote = F)

## MSE table for all groups for supplementary
write.table(mutate(allMSE, MSE = round(MSE, 0)), "tables/allSampRateMSE.txt", sep = "\t", row.names = F, quote = F)


###################
## coverage
###################

## SD
cov_SD <- sapply(names(trueComp), function(x){
	srSD_lower[,x] <= trueComp[x] & srSD_upper[,x] >= trueComp[x]
})
cov_SD <- data.frame(sr = as.factor(srRec), cov_SD)
head(cov_SD)
covSum_SD <- as.tibble(cov_SD) %>% gather("group", "covBool", -sr) %>% group_by(sr, group) %>% 
	summarise(covSD = sum(covBool, na.rm = TRUE)/sum(!is.na(covBool)) ) %>% mutate(covSD = round(covSD, 3))

## MLE
cov_MLE <- sapply(names(trueComp), function(x){
	srMLE_lower[,x] <= trueComp[x] & srMLE_upper[,x] >= trueComp[x]
})
cov_MLE <- data.frame(sr = as.factor(srRec), cov_MLE)
head(cov_MLE)
covSum_MLE <- as.tibble(cov_MLE) %>% gather("group", "covBool", -sr) %>% group_by(sr, group) %>% 
	summarise(covMLE = sum(covBool, na.rm = TRUE)/sum(!is.na(covBool)) ) %>% mutate(covMLE = round(covMLE, 3))

## now put together
allCovSum <- covSum_SD %>% left_join(covSum_MLE, by = c("sr", "group")) 

## write out all for supplemental
write.table(allCovSum, "tables/allSampRateCoverage.txt", sep = "\t", row.names = F, quote = F)

##  and summarise for table
allCovSumMS <- allCovSum %>% gather("type", "cov", 3:4) %>% group_by(sr, type) %>% 
	summarise(mean = round(mean(cov), 3), sd = round(sd(cov), 3), u85 = sum(cov < .85), o95 = sum(cov > .95), min = min(cov), max = max(cov))

## write out for ms
write.table(allCovSumMS, "tables/summarySampRateCoverage.txt", sep = "\t", row.names = F, quote = F)


###################
## look at distribution of raw error
###################

# SD 100%
ER <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_tag100[,x]))
colnames(ER) <- names(trueComp)
ER <- as.tibble(data.frame(sr = as.factor(srRec), ER))
ER_srSD_tag100 <- ER %>% gather("group", "estimate", c(2:ncol(ER))) %>% group_by(sr, group) %>% summarize(NoEx = mean(estimate))

# SD SPIBETR=FALSE
ER <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_spibetrFALSE[,x]))
colnames(ER) <- names(trueComp)
ER <- as.tibble(data.frame(sr = as.factor(srRec), ER))
ER_srSD_spibetrFALSE <- ER %>% gather("group", "estimate", c(2:ncol(ER))) %>% group_by(sr, group) %>% summarize(TotEx = mean(estimate))

# SD normal
ER <- sapply(names(trueComp), function(x) (trueComp[x] - srSD_mean[,x]))
colnames(ER) <- names(trueComp)
ER <- as.tibble(data.frame(sr = as.factor(srRec), ER))
ER_srSD_mean <- ER %>% gather("group", "estimate", c(2:ncol(ER))) %>% group_by(sr, group) %>% summarize(Ac = mean(estimate))

# MLE
ER <- sapply(names(trueComp), function(x) (trueComp[x] - srMLE_mean[,x]))
colnames(ER) <- names(trueComp)
ER <- as.tibble(data.frame(sr = as.factor(srRec), ER))
ER_srMLE_mean <- ER %>% gather("group", "estimate", c(2:ncol(ER))) %>% group_by(sr, group) %>% summarize(MLE = mean(estimate))

allER <- ER_srSD_tag100 %>% left_join(ER_srSD_spibetrFALSE, by=c("sr", "group")) %>%
	left_join(ER_srSD_mean, by=c("sr", "group")) %>% 
	left_join(ER_srMLE_mean, by=c("sr", "group")) %>% gather("type", "ER", 3:6)

ER_out <- allER %>% filter(group %in% c(PBTselect, GSIselect) & sr %in% seq(.1, .4, .1)) %>% filter(type != "TotEx") %>% 
	mutate(ER = round(ER, 0)) %>% spread(type, ER) %>% 
	select(sr, group, MLE, Ac, NoEx)


## ER table for select groups for ms
write.table(ER_out, "tables/selectSampRateER.txt", sep = "\t", row.names = F, quote = F)

## ER table for all groups for supplementary
write.table(mutate(allER, ER = round(ER, 0)), "tables/allSampRateDiffER.txt", sep = "\t", row.names = F, quote = F)
