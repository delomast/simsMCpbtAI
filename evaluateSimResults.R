# evalute simulations



# sample rate
load("./rdaOutputs/small_sampRate_symprior5.rda")
load("./rdaOutputs/large_sampRateSimsOutput_sym5_multiThread.rda") #this has SD and MLE estimates to use for large scenario
load("./rdaOutputs/large_sampRateSimsOutput_sym1divN.rda")
load("./rdaOutputs/large_sampRateSimsOutput_sym1divN_all_gsiSep.rda")
load("./rdaOutputs/large_sampRateSimsOutput_sym1divN_all.rda")
load("./rdaOutputs/small_sampRate_1divN.rda")

# replace NA with 0
sr_mean <- apply(sr_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})
srSD_mean <- apply(srSD_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})

srMLE_mean <- apply(srMLE_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})

# sr_upper <- apply(sr_upper, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })
# srSD_upper <- apply(srSD_upper, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })
# 
# sr_lower <- apply(sr_lower, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })
# srSD_lower <- apply(srSD_lower, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })

# make some box plots

pdf("large_sampRate.pdf")
for(group in names(trueComp)){
	boxplot(sr_mean[,group]/trueComp[group] ~ srRec, main = paste("mcmc", group), ylim = c(.5,1.5))
	abline(h = 1)
	# boxplot(srSD_mean[,group] ~ srRec, main = paste("SD", group))
	# abline(h = trueComp[group])
	# boxplot(srMLE_mean[,group]/trueComp[group] ~ srRec, main = paste("MLE", group), ylim = c(.5,1.5))
	# abline(h = 1)
	
	# boxplot(sr_mean[,group]/trueComp[group], main = paste("mcmc", group), ylim = c(.5,1.5))
	# abline(h = 1)
	# boxplot(srSD_mean[,group]/trueComp[group], main = paste("SD", group), ylim = c(.5,1.5))
	# abline(h = 1)
	# temp <- cbind(sr_mean[,group]/trueComp[group], srSD_mean[,group]/trueComp[group])
	# colnames(temp) <- c("mcmc", "SD")
	# boxplot(temp, main = paste(group), ylim = c(.5,1.5))
	# abline(h = 1)
}
dev.off()

#replicate in ggplot with a marker for the mean
library(ggplot2)
group = "pbtGroup11"

boxplot(srSD_mean[,group] ~ srRec, main = paste("SD", group))
abline(h = trueComp[group])


dataPlot <- data.frame(estim = srSD_mean[,group], sr = as.character(srRec))

#boxplot
p <- ggplot(dataPlot, aes(x=sr, y=estim)) + geom_boxplot() +
		theme(legend.position = "none") + scale_fill_brewer(palette="Set1")
#violin plot
p <- ggplot(dataPlot, aes(x=sr, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75)) + 
		theme(legend.position = "none") + scale_fill_brewer(palette="Set1")
#make the plot and add mean and true line
p + stat_summary(fun.y=mean, geom = "point", color="red", size = 2) + 
	geom_hline(aes(yintercept = trueComp[group]), linetype = "dashed", color = "blue", size = 1) +
	geom_vline(aes(xintercept = 6.5), linetype = "solid", color = "black", size = 1)

#now make these for a subset of groups to illustrate

groups <- c("pbtGroup1", "pbtGroup6", "pbtGroup11", "pbtGroup16", "pbtGroup20", "pbtGroup25", "GSIgroup1", "GSIgroup4", "GSIgroup7", "GSIgroup10")

pdf("SD_selection.pdf")
for(group in groups){
	dataPlot <- data.frame(estim = srSD_mean[,group], sr = as.character(srRec))

	# #boxplot
	# p <- ggplot(dataPlot, aes(x=sr, y=estim)) + geom_boxplot() +
	# 		theme(legend.position = "none") + scale_fill_brewer(palette="Set1")
	#violin plot
	p <- ggplot(dataPlot, aes(x=sr, y=estim)) + geom_violin(draw_quantiles = c(.25, .5, .75)) + 
			theme(legend.position = "none") + scale_fill_brewer(palette="Set1")
	#make the plot and add mean and true line
	print(
		p + stat_summary(fun.y=mean, geom = "point", color="red", size = 2) + 
			geom_hline(aes(yintercept = trueComp[group]), linetype = "dashed", color = "blue", size = 1) +
			geom_vline(aes(xintercept = 6.5), linetype = "solid", color = "black", size = 1)+
			xlab("Sampling rate") + ylab("Estimated number of fish") + ggtitle(paste(group))
	)
}
dev.off()






pdf("testSmall1divN.pdf")
for(group in names(trueComp)){
	boxplot(sr_mean[,group]/trueComp[group] ~ srRec, main = paste("mcmc", group), ylim = c(.5,1.5))
	abline(h = 1)
	boxplot(srSD_mean[,group]/trueComp[group] ~ srRec, main = paste("SD", group), ylim = c(.5,1.5))
	abline(h = 1)
	boxplot(srMLE_mean[,group]/trueComp[group] ~ srRec, main = paste("MLE", group), ylim = c(.5,1.5))
	abline(h = 1)
	
	# boxplot(sr_mean[,group]/trueComp[group], main = paste("mcmc", group), ylim = c(.5,1.5))
	# abline(h = 1)
	# boxplot(srSD_mean[,group]/trueComp[group], main = paste("SD", group), ylim = c(.5,1.5))
	# abline(h = 1)
	# temp <- cbind(sr_mean[,group]/trueComp[group], srSD_mean[,group]/trueComp[group])
	# colnames(temp) <- c("mcmc", "SD")
	# boxplot(temp, main = paste(group), ylim = c(.5,1.5))
	# abline(h = 1)
}
dev.off()


group <- "pbtGroup2"
boxplot(sr_mean[,group]/trueComp[group] ~ srRec, main = paste("mcmc", group), ylim = c(.5,1.5))
abline(h = 1)
hist(sr_mean[srRec == .05,group]/trueComp[group])
abline(v = mean(sr_mean[srRec == .05,group]/trueComp[group]))
abline(v = median(sr_mean[srRec == .05,group]/trueComp[group]))



table(convergeMLE) #never converged


sum(sr_upper[,"pbtGroup10"] >= trueComp["pbtGroup10"] & sr_lower[,"pbtGroup10"] <= trueComp["pbtGroup10"]) / nrow(sr_mean)
sum(sr_upper[,"pbtGroup25"] >= trueComp["pbtGroup25"] & sr_lower[,"pbtGroup25"] <= trueComp["pbtGroup25"]) / nrow(sr_mean)

sum(sr_upper[,"GSIgroup3"] >= trueComp["GSIgroup3"] & sr_lower[,"GSIgroup3"] <= trueComp["GSIgroup3"]) / nrow(sr_mean)
sum(sr_upper[,"GSIgroup10"] >= trueComp["GSIgroup10"] & sr_lower[,"GSIgroup10"] <= trueComp["GSIgroup10"]) / nrow(sr_mean)

sum(srSD_upper[,"pbtGroup10"] >= trueComp["pbtGroup10"] & srSD_lower[,"pbtGroup10"] <= trueComp["pbtGroup10"]) / nrow(srSD_mean)
sum(srSD_upper[,"pbtGroup25"] >= trueComp["pbtGroup25"] & srSD_lower[,"pbtGroup25"] <= trueComp["pbtGroup25"]) / nrow(srSD_mean)

sum(srSD_upper[,"GSIgroup3"] >= trueComp["GSIgroup3"] & srSD_lower[,"GSIgroup3"] <= trueComp["GSIgroup3"]) / nrow(srSD_mean)
sum(srSD_upper[,"GSIgroup10"] >= trueComp["GSIgroup10"] & srSD_lower[,"GSIgroup10"] <= trueComp["GSIgroup10"]) / nrow(srSD_mean)

