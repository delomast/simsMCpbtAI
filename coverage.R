load("./rdaOutputs/archiveRDA/large_MLEonly.rda")
load("./rdaOutputs/temp0.05.rda")

head(srMLE_lower)
head(srMLE_upper)

sr05 <- c()
for(g in colnames(srMLE_lower)){
	# print(g)
	# print(sum(trueComp[g] >= srMLE_lower[,g] & trueComp[g] <= srMLE_upper[,g], na.rm = TRUE) / sum(!is.na(srMLE_lower[,g]))
	# )
	sr05 <- c(sr05, sum(trueComp[g] >= srMLE_lower[,g] & trueComp[g] <= srMLE_upper[,g], na.rm = TRUE) / sum(!is.na(srMLE_lower[,g])))
}

summary(sr05)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.8473  0.8742  0.8820  0.8850  0.8930  0.9604 
sort(sr05)

sr05SD <- c()
for(g in colnames(srSD_lower)){
	# print(g)
	# print(sum(trueComp[g] >= srMLE_lower[,g] & trueComp[g] <= srMLE_upper[,g], na.rm = TRUE) / sum(!is.na(srMLE_lower[,g]))
	# )
	sr05SD <- c(sr05SD, sum(trueComp[g] >= srSD_lower[,g] & trueComp[g] <= srSD_upper[,g], na.rm = TRUE) / sum(!is.na(srSD_lower[,g])))
}

summary(sr05SD)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.8411  0.8739  0.8840  0.8856  0.8940  0.9604 
sort(sr05SD)





load("./rdaOutputs/temp0.075.rda")

head(srMLE_lower)
head(srMLE_upper)

sr075 <- c()
for(g in colnames(srMLE_lower)){
	# print(g)
	# print(sum(trueComp[g] >= srMLE_lower[,g] & trueComp[g] <= srMLE_upper[,g], na.rm = TRUE) / sum(!is.na(srMLE_lower[,g]))
	# )
	sr075 <- c(sr075, sum(trueComp[g] >= srMLE_lower[,g] & trueComp[g] <= srMLE_upper[,g], na.rm = TRUE) / sum(!is.na(srMLE_lower[,g])))
}

summary(sr075)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.8517  0.8810  0.8840  0.8865  0.8955  0.9298
sort(sr075)



## now looking at error between the two methods

plot(srMLE_mean[,1], srSD_mean[,1])
abline(0,1)

plot(srMLE_mean[,30], srSD_mean[,30])
abline(0,1)

#Mean square error
mleMSE <- sapply(colnames(srMLE_mean),function(x){
	mean((srMLE_mean[,x] - trueComp[x])^2, na.rm = TRUE)
	
})

sdMSE <- sapply(colnames(srSD_mean),function(x){
	mean((srSD_mean[,x] - trueComp[x])^2, na.rm = TRUE)
	
})

library(ggplot2)
data <- data.frame(values = c(mleMSE, sdMSE), groups = names(trueComp), type =c(rep("MLE", 35), rep("SD", 35)))
ggplot(data = data, aes(x=groups, y=values, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + 
	 theme(axis.text.x = element_text(angle = 90, hjust = 1))
## very little difference

summary(sdMSE - mleMSE)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -21.43453  -0.07089   0.38017  10.68866  16.92607  92.06562
## MLE does a little better

hist(sdMSE - mleMSE)

#MLE convergence
table(convergeMLE, useNA = "always")
