# Organize all small tables into tables for the MS
dir.create("MS_tables")
# load tables

dir("./tables", "select.+txt", full.names = TRUE)
#  [1] "./tables/selectBinomDiffER.txt"       
#  [2] "./tables/selectBinomDiffMSE.txt"      
#  [3] "./tables/selectBinomLowTagDiffER.txt" 
#  [4] "./tables/selectBinomSimilarER.txt"    
#  [5] "./tables/selectBinomSimilarMSE.txt"   
#  [6] "./tables/selectDiffGSIofPBTER.txt"    
#  [7] "./tables/selectDiffGSIofPBTMSE.txt"   
#  [8] "./tables/selectLowTagBinomDiffMSE.txt"
#  [9] "./tables/selectSampRateER.txt"        
# [10] "./tables/selectSampRateMSE.txt"       
# [11] "./tables/selectSimilarGSIofPBTER.txt" 
# [12] "./tables/selectSimilarGSIofPBTMSE.txt"


###########################################
# MSE and ER for sample rate and diff vs similar GSI of PBT
MSE_samp <- read.table("./tables/selectSampRateMSE.txt", header = T, stringsAsFactors = F)
MSE_diff <- read.table("./tables/selectDiffGSIofPBTMSE.txt", header = T, stringsAsFactors = F)
MSE_sim <- read.table("./tables/selectSimilarGSIofPBTMSE.txt", header = T, stringsAsFactors = F)

MSE_samp$scenario <- "Base"
MSE_diff$scenario <- "2"
MSE_sim$scenario <- "1"

combined <- rbind(MSE_samp, MSE_sim, MSE_diff) %>% select(sr, scenario, group, MLE, Ac, NoEx) 
colnames(combined)[4:6] <- paste0("MSE_", colnames(combined)[4:6])

ER_samp <- read.table("./tables/selectSampRateER.txt", header = T, stringsAsFactors = F)
ER_diff <- read.table("./tables/selectDiffGSIofPBTER.txt", header = T, stringsAsFactors = F)
ER_sim <- read.table("./tables/selectSimilarGSIofPBTER.txt", header = T, stringsAsFactors = F)

ER_samp$scenario <- "Base"
ER_diff$scenario <- "2"
ER_sim$scenario <- "1"

ER_sim$sr <- .2
ER_diff$sr <- .2

combinedER <- rbind(ER_samp, ER_sim, ER_diff) %>% select(sr, scenario, group, MLE, Ac, NoEx) 
colnames(combinedER)[4:6] <- paste0("ER_", colnames(combinedER)[4:6])

combined <- combined %>% left_join(combinedER, by = c("sr", "scenario", "group"))

#change groups names to make ms easier to read
tempSwitch <- function(x){
	y <- rep(NA, length(x))
	y[x == "GSIgroup3"] <- "GSI A"
	y[x == "GSIgroup6"] <- "GSI B"
	y[x == "GSIgroup9"] <- "GSI C"
	y[x == "pbtGroup8"] <- "PBT A"
	y[x == "pbtGroup24"] <- "PBT B"
	y[x == "pbtGroup11"] <- "PBT C"
	y
}
combined <- combined %>% mutate(group = tempSwitch(group)) %>% arrange(scenario, sr, group)
combined <- rbind(combined[combined$scenario == "Base",], combined[combined$scenario != "Base",]) #put base first in table


write.table(combined, "./MS_tables/sampRate_s1_s2_MSE_ER.txt", row.names = F, col.names = T, quote = F, sep = "\t")


#######################################
# MSE and ER for binom var scenarios
MSE_lowtag <- read.table("./tables/selectLowTagBinomDiffMSE.txt", header = T, stringsAsFactors = F)
MSE_diffBin <- read.table("./tables/selectBinomDiffMSE.txt", header = T, stringsAsFactors = F)
MSE_simBin <- read.table("./tables/selectBinomSimilarMSE.txt", header = T, stringsAsFactors = F)

MSE_lowtag$scenario <- "5"
MSE_diffBin$scenario <- "4"
MSE_simBin$scenario <- "3"

combined2 <- rbind(MSE_simBin, MSE_diffBin, MSE_lowtag) %>% select(scenario, group, MLE, Ac, NoEx) 
colnames(combined2)[3:5] <- paste0("MSE_", colnames(combined2)[3:5])

ER_lowtag <- read.table("./tables/selectBinomLowTagDiffER.txt", header = T, stringsAsFactors = F)
ER_diffBin <- read.table("./tables/selectBinomDiffER.txt", header = T, stringsAsFactors = F)
ER_simBin <- read.table("./tables/selectBinomSimilarER.txt", header = T, stringsAsFactors = F)

ER_lowtag$scenario <- "5"
ER_diffBin$scenario <- "4"
ER_simBin$scenario <- "3"

combinedER <- rbind(ER_simBin, ER_diffBin, ER_lowtag) %>% select(scenario, group, MLE, Ac, NoEx) 
colnames(combinedER)[3:5] <- paste0("ER_", colnames(combinedER)[3:5])

combined2 <- combined2 %>% left_join(combinedER, by = c("scenario", "group")) %>% filter(grepl("cat2", group))

combined2 <- combined2 %>% mutate(group = gsub("_cat2", "", group)) %>% mutate(group = tempSwitch(group)) %>% arrange(scenario, group)

write.table(combined2, "./MS_tables/s3_s4_s5_MSE_ER.txt", row.names = F, col.names = T, quote = F, sep = "\t")


######################################
# Coverage samp rate
cov <- read.table("./tables/summarySampRateCoverage.txt", header = T, stringsAsFactors = F)
cov$type <- gsub("cov", "", cov$type)
cov$type[cov$type == "SD"] <- "Ac"
cov$m_sd <- paste0(cov$mean, " +/- ", cov$sd)
cov$range <- paste0(cov$min, " - ", cov$max)
cov <- select(cov, sr, type, m_sd, u85, o95, range) %>% arrange(type)

write.table(cov, "./MS_tables/coverage_sampRate.txt", row.names = F, col.names = T, quote = F, sep = "\t")
