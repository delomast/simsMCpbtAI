# evalute simulations



# sample rate
load("./rdaOutputs/small_sampRate_symprior5.rda")
load("./rdaOutputs/large_sampRateSimsOutput_sym5_multiThread.rda")
load("./rdaOutputs/large_sampRateSimsOutput_sym1divN.rda")

# replace NA with 0
# sr_mean <- apply(sr_mean, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })
# srSD_mean <- apply(srSD_mean, 2, function(x){
# 	x[is.na(x)] <- 0
# 	return(x)
# })
# 
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

pdf("test.pdf")
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

pdf("test2.pdf")
for(group in names(trueComp)){
	boxplot(sr_mean[,group]/trueComp[group] ~ srRec, main = paste("mcmc", group), ylim = c(.5,1.5))
	abline(h = 1)
	# boxplot(srSD_mean[,group]/trueComp[group] ~ srRec, main = paste("SD", group), ylim = c(.5,1.5))
	# abline(h = 1)
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



table(convergeMLE) #never converged


sum(sr_upper[,"pbtGroup10"] >= trueComp["pbtGroup10"] & sr_lower[,"pbtGroup10"] <= trueComp["pbtGroup10"]) / nrow(sr_mean)
sum(sr_upper[,"pbtGroup25"] >= trueComp["pbtGroup25"] & sr_lower[,"pbtGroup25"] <= trueComp["pbtGroup25"]) / nrow(sr_mean)

sum(sr_upper[,"GSIgroup3"] >= trueComp["GSIgroup3"] & sr_lower[,"GSIgroup3"] <= trueComp["GSIgroup3"]) / nrow(sr_mean)
sum(sr_upper[,"GSIgroup10"] >= trueComp["GSIgroup10"] & sr_lower[,"GSIgroup10"] <= trueComp["GSIgroup10"]) / nrow(sr_mean)

sum(srSD_upper[,"pbtGroup10"] >= trueComp["pbtGroup10"] & srSD_lower[,"pbtGroup10"] <= trueComp["pbtGroup10"]) / nrow(srSD_mean)
sum(srSD_upper[,"pbtGroup25"] >= trueComp["pbtGroup25"] & srSD_lower[,"pbtGroup25"] <= trueComp["pbtGroup25"]) / nrow(srSD_mean)

sum(srSD_upper[,"GSIgroup3"] >= trueComp["GSIgroup3"] & srSD_lower[,"GSIgroup3"] <= trueComp["GSIgroup3"]) / nrow(srSD_mean)
sum(srSD_upper[,"GSIgroup10"] >= trueComp["GSIgroup10"] & srSD_lower[,"GSIgroup10"] <= trueComp["GSIgroup10"]) / nrow(srSD_mean)

