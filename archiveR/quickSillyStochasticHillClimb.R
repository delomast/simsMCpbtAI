## stochastic hill climbing quickly

hikeMtPBT <- function(STARTpiTot, STARTpiGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi){
	
	logLikelihood <- function(piTot, piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi){
		# now, calculate the log likelihood
		llh <- 0
		# first ohnc part
		if(nPBT > 0) llh <- sum(ohnc[1:nPBT] * log(piTot[1:nPBT] * t[1:nPBT]))
		# then utGSI part
		untag <- 1 - t
		for(j in 1:nGSI){
			llh <- llh + log(sum(piTot * untag * piGSI[,j])) * utGSI[j]
		}
		# then ohnc GSI part
		if(nPBT > 0){
			for(i in 1:nPBT){
				llh <- llh + sum(ohnc_gsi[i,] * log(piGSI[i,]))
			}
		}
		return(llh)
	}
	
	proposal_piTot <- function(bigstdev, alphas){
		rec <- MCMCpack::rdirichlet(1, alphas)
		rec <- rec[1,] - rep(1/length(alphas), length(alphas))
		return(rec * rnorm(1,0,bigstdev))
	}

	
	stdev <- .1
	piTot <- STARTpiTot
	piGSI <- STARTpiGSI
	reps <- 0
	
	pressOn <- TRUE
	while(pressOn){
		
		keepSD <- TRUE
		count <- 0
		
		while(keepSD){
			if(count >= 4000) keepSD <- FALSE
			skip <- FALSE
			reps <- reps + 1
			piTotNew <- piTot + proposal_piTot(stdev, rep(1, length(piTot)))
			if(sum(piTotNew < 0) > 0){
				count <- count + 1
				next
			}
			piGSInew <- piGSI
			for(i in 1:nPBT){
				piGSInew[i,] <- piGSInew[i,] + proposal_piTot(stdev, rep(1, nGSI))
				if(sum(piGSInew[i,] < 0) > 0){
					count <- count + 1
					skip <- TRUE
					break
				}
			}
			if(skip) next

			if(logLikelihood(piTotNew, piGSInew, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi) > logLikelihood(piTot, piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)){
				piTot <- piTotNew
				piGSI <- piGSInew
				count <- 0
			} else {
				count <- count + 1
			}
			if(reps %% 100 == 0) print(logLikelihood(piTot, piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi))
			
			
		}
		
		stdev <- stdev/2
		print(stdev)

		if(stdev < .00001) pressOn <- FALSE
	}
	
	return(list(piTot = piTot, piGSI = piGSI, reps = reps))
}







	input <- prepStrata(trapData, tags, "GSI", "GenParentHatchery", "StrataVar", variableCols = c(), variableColsOth = c(), "AdClip",
									AI = TRUE, GSIgroups = NA,
										 variableValues = NA, variableValuesOth = NA, verbose = FALSE, symPrior = 0.5)
input <- input[[2]]
	#pull values out of input
		ohnc <- input$ohnc
		nPBT <- input$nPBT
		t <- input$t
		ohnc_gsi <- input$ohnc_gsi
		utGSI <- c()
		for(g in input$GSI_values){
			utGSI <- c(utGSI, sum(input$gsiUT == g))
		}

		#define some values used in the function
		nGSI <- length(input$groups) - nPBT
		
		


		
		# determine reasonable starting values
		#for piTot
		start_piTot <- c()
		if(nPBT > 0) start_piTot <- ohnc[1:nPBT] / t[1:nPBT] #scale PBT by tag rates
		start_piTot <- c(start_piTot, utGSI) #just use observed GSI
		start_piTot <- start_piTot/sum(start_piTot)

		#for piGSI
		start_piGSI <- matrix(nrow=0, ncol=nGSI)
		for(i in 1:nrow(ohnc_gsi)){
			temp <- ohnc_gsi[i,] #just use ohnc assignments
			temp[temp < 1] <- .1
			start_piGSI <- rbind(start_piGSI, temp/sum(temp))
		}
		start_piGSI <- rbind(start_piGSI, diag(1,nGSI))
		
		
results <- hikeMtPBT(start_piTot, start_piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)

mlepoit <- MLEwrapper(trapData[trapData$StrataVar == 18,], tags = tags, GSIcol = "GSI", PBTcol = "GenParentHatchery", 
												  strataCol = "StrataVar", adFinCol = "AdClip", AI = TRUE, optimMethod = "Nelder-Mead", variableCols = NULL, 
												  control = list(maxit=10000))[[1]]

logLikelihood(mlepoit$piTot, mlepoit$piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)
logLikelihood(results$piTot, results$piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)


###########
# Now writign a gradient function for the likelihood function to use in MLE


###############
#### NOTE: moved this development to devMCpbt - the below function may be outdated
###############


#this returns gradient with respect to piTot and piGSI
test_gradient <- function(piTot, piGSItemp, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi){
	untag <- 1 - t
	
	#now calculate the gradient
	gradient <- rep(0, length(piTot) + nPBT*nGSI)
	
	#piTot
	#first, PBT groups observed part
	if(nPBT > 0) gradient[1:nPBT] <- ohnc[1:nPBT] / piTot[1:nPBT]
	#then all the groups unobserved part
	for(i in 1:(nPBT + nGSI)){
		temp <- 0
		for(j in 1:nGSI){
			if(utGSI[j] == 0) next
			temp <- temp + 
				(
					(utGSI[j] * untag[i] * piGSItemp[i,j]) /
					sum(untag * piGSItemp[,j] * piTot)	
				)
		}
		gradient[i] <- gradient[i] + temp
	}
	
	#piGSI
	if (nPBT > 0){
		pos <- (nPBT + nGSI + 1) #position in the gradient vector to assign values
		for(i in 1:nPBT){
			for(j in 1:nGSI){
				temp <- 0
				temp <- ((utGSI[j] * piTot[i] * untag[i]) /
					sum(untag * piGSItemp[,j] * piTot))
				if(ohnc_gsi[i,j] > 0) temp <- temp + (ohnc_gsi[i,j] / piGSItemp[i,j])
				gradient[pos] <- temp
				pos <- pos + 1
			}
		}
	}
	# print(gradient)
	return(-gradient)
}

#now a function to calculate the gradient with respect to params

params_grad <- function(params, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi){
	# first, unpack params
	#piTot
	piTot <- params[1:(nPBT + nGSI)]
	piTot <- piTot / sum(piTot) #transform into proportions
	if(sum(piTot < 0 | piTot > 1) != 0) return(Inf)
	#piGSI
	# piGSItemp <- matrix(params[(nPBT + nGSI):length(params)], nrow = (nPBT), ncol = (nGSI-1), byrow = TRUE)
	subParams <- params[(nPBT + nGSI + 1):length(params)]
	piGSItemp <- matrix(0, nrow = (nPBT), ncol = (nGSI)) #initiate with zeros
	if(nPBT > 0){
		for(i in 1:nPBT){
			piGSItemp[i,] <- subParams[1:nGSI]
			subParams <- subParams[(nGSI + 1):length(subParams)] #bump entries forward
			piGSItemp[i,] <- piGSItemp[i,] / sum(piGSItemp[i,]) #normalize
			if(sum(piGSItemp[i,] < 0 | piGSItemp[i,] > 1) != 0) return(Inf) #make sure all entries are valid
		}
	}
	piGSItemp <- rbind(piGSItemp, diag(nGSI)) #add GSI groups as fixed 100%
	
	#get partial derivs with respect to piTot and piGSI
	firstLevel <- test_gradient(piTot, piGSItemp, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)
	
	gradient <- rep(NA, length(params))
	#calc gradient of piTot with respect to params
	par_piTot <- params[1:(nPBT + nGSI)]
	sumPar <- sum(par_piTot)
	for(i in 1:(nPBT+nGSI)){
		temp <- 0
		for(j in 1:(nPBT+nGSI)){
			if(i == j){
				temp <- temp + (firstLevel[j] * ((sumPar - par_piTot[j]) / (sumPar^2)))
			} else {
				temp <- temp + (firstLevel[j] * (-par_piTot[j] / (sumPar^2)))
			}
		}
		gradient[i] <- temp
	}
	
	#now gradient of piGSI
	if (nPBT > 0){
		pos <- (nPBT + nGSI + 1) #position in the gradient vector to assign values
		for(i in 1:nPBT){
			paramsTemp <- params[pos:(pos + nGSI - 1)]
			firstLevelTemp <- firstLevel[pos:(pos + nGSI - 1)]
			sumPar <- sum(paramsTemp)
			for(j in 1:nGSI){
				temp <- 0
				for(k in 1:nGSI){
					if(j == k){
						temp <- temp + (firstLevelTemp[k] * ((sumPar - paramsTemp[k]) / (sumPar^2)))
					} else {
						temp <- temp + (firstLevelTemp[k] * (-paramsTemp[k] / (sumPar^2)))
					}
				}
				gradient[pos] <- temp
				pos <- pos + 1
			}
		}
	}
	return(gradient)
}


test_llh <- function(u, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi){

	piTot <- u[[1]]
	piGSItemp <- u[[2]]
	# now, calculate the log likelihood
	llh <- 0
	# first ohnc part
	if(nPBT > 0) llh <- sum(ohnc[1:nPBT] * log(piTot[1:nPBT] * t[1:nPBT]))
	# then utGSI part
	untag <- 1 - t
	for(j in 1:nGSI){
		llh <- llh + log(sum(piTot * untag * piGSItemp[,j])) * utGSI[j]
	}
	# then ohnc GSI part
	if(nPBT > 0){
		for(i in 1:nPBT){
			llh <- llh + sum(ohnc_gsi[i,] * log(piGSItemp[i,]))
		}
	}
	
	# returning negative log-likelihood for minimization
	return(-llh)
}


test_gradient(start_piTot, start_piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)

numDeriv::grad(function(u) test_llh(u, nPBT = nPBT, nGSI = nGSI, ohnc = ohnc, t = t, utGSI = utGSI, ohnc_gsi = ohnc_gsi), 
								list(start_piTot, start_piGSI))

test_gradient(start_piTot, start_piGSI, nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)[1:length(start_piTot)]

c(start_piTot, as.vector(t(start_piGSI)))
  
params_grad(c(start_piTot, as.vector(t(start_piGSI))), nPBT, nGSI, ohnc, t, utGSI, ohnc_gsi)

