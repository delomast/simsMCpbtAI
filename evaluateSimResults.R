# evalute simulations



# sample rate
load("./rdaOutputs/small_sampRate_symprior5.rda")
# replace NA with 0
sr_mean <- apply(sr_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})
srSD_mean <- apply(srSD_mean, 2, function(x){
	x[is.na(x)] <- 0
	return(x)
})

# make some box plots

pdf("small_symPrior5.pdf")
for(group in names(trueComp)){
	boxplot(sr_mean[,group]/trueComp[group] ~ srRec, main = paste("mcmc", group), ylim = c(.5,1.5))
	abline(h = 1)
	boxplot(srSD_mean[,group]/trueComp[group] ~ srRec, main = paste("SD", group), ylim = c(.5,1.5))
	abline(h = 1)
	boxplot(sr_mean[,group]/trueComp[group], main = paste("mcmc", group), ylim = c(.5,1.5))
	abline(h = 1)
	boxplot(srSD_mean[,group]/trueComp[group], main = paste("SD", group), ylim = c(.5,1.5))
	abline(h = 1)
	# temp <- cbind(sr_mean[,group]/trueComp[group], srSD_mean[,group]/trueComp[group])
	# colnames(temp) <- c("mcmc", "SD")
	# boxplot(temp, main = paste(group), ylim = c(.5,1.5))
	# abline(h = 1)
}
dev.off()




