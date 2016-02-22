# Bayesian Vowel Modeling
 # Version 1.0 (October 2012)
 
# Aaron Albin
 # Department of Linguistics and Department of Second Language Studies, Indiana University - Bloomington
 
# Wil Rankinen
 # Department of Linguistics, Indiana University - Bloomington
 
# Please send bug reports and feature requests to aaalbin [AT-SIGN] indiana [DOT] edu
 
#"This program is free software: you can redistribute it and/or modify
 # it under the terms of the GNU General Public License as published by
 # the Free Software Foundation, either version 3 of the License, or
 # (at your option) any later version.
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 # GNU General Public License for more details.
 # You should have received a copy of the GNU General Public License
 # along with this program. If not, see ."
 #--------------------------------------------------------------------------------------------------------------------------------------------------------------
 #--------------------------------------------------------------------------------------------------------------------------------------------------------------
 # CODE BEGINS HERE
 #--------------------------------------------------------------------------------------------------------------------------------------------------------------
 #--------------------------------------------------------------------------------------------------------------------------------------------------------------
 
# Close all open windows and clear out all previous R objects
 graphics.off()
 rm(list=ls(all=TRUE))
 
# Determine whether the packages 'tcltk' and 'rjags' are installed.
 InstalledPackageNames = rownames(installed.packages())
 tcltkInstalled = as.logical(sum( InstalledPackageNames=="tcltk" ))
 rjagsInstalled = as.logical(sum( InstalledPackageNames=="rjags" ))
 
# If one or both are not installed, issue an error message as appropriate
 if(!tcltkInstalled & rjagsInstalled){stop("This script requires the package 'tcltk' to be installed.\nPlease see the following website for installation instructions:\nhttp://mypage.iu.edu/~wrankine/bvm/")}
 if( tcltkInstalled & !rjagsInstalled){stop("This script requires the package 'rjags' to be installed.\nPlease see the following website for installation instructions:\nhttp://mypage.iu.edu/~wrankine/bvm/")}
 if(!tcltkInstalled & !rjagsInstalled){stop("This script requires the packages 'rjags' and 'tcltk' to be installed.\nPlease see the following website for installation instructions:\nhttp://mypage.iu.edu/~wrankine/bvm/")}
 
# Only run the rest of the code if both packages are installed
 if(tcltkInstalled & rjagsInstalled){
 
# Bring in the two required packages
 # Use 'library()' rather than 'require()' since the former issues an error and will stop computation if it is not found.
 library(tcltk)
 library(rjags)
 
# Set up error messages for doing things in the wrong order.
 DataNotImportedError = "You have not imported any data yet.\n\nFirst go to File > Import data, and then try this again."
 AnalysisNotRunError = "You have not run the analysis yet.\n\nFirst Select 'Run analysis', and then try this again."
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
 # READING IN THE DATA FILE
 
# Begin the function to import a data file on the user's computer
 ImportData = function(){
 
# Using TclTk, open an interactive window to choose the desired file
 OpenedFilePath = tclvalue(tkgetOpenFile(filetypes = "{{Text file} {.txt}} {{All files} *}"))
 
# Only continue executing code if the user actually selected something (and did not just hit 'cancel' when selecting the file)
 if(!OpenedFilePath==""){
 
# Check if the first line is in the correct format.
 FirstLine = scan(file=OpenedFilePath,n=1,what="character",sep="\n",quiet=TRUE)
 
# If not, issue an error and stop executing code
 if(FirstLine != "SpeakerGroup\tSpeaker\tTarget\tF1\tF2"){stop("The header (first line) of the selected file is not in the correct format.\nPlease see the following website for instructions on how the data file should be structured:\n")
 }else{
 
# Read in the second line of the data file
 SecondLine_Together = scan(file=OpenedFilePath,n=2,what="character",sep="\n",quiet=TRUE)[2]
 
# Verify whether there are 4 tabs (after ignoring all trailing whitespace at the end of the line), suggesting the existence of 5 tab-delimited columns
 SecondLine_Split = strsplit(SecondLine_Together,split="")[[1]]
 IsTab = SecondLine_Split == "\t"
 IsSpace = SecondLine_Split == " "
 LastNonWhitespacePosition = max(which(!IsTab & !IsSpace))
 nTabsBeforeTrailingWhitespace = sum( SecondLine_Split[1:LastNonWhitespacePosition] =="\t")
 
# If there are not 4 tabs, issue an error and stop executing code
 if(nTabsBeforeTrailingWhitespace != 4){stop("The data need to be structured in five columns separated by tabs.\n\nPlease see the following website for instructions on how the data file should be structured:\nhttp://mypage.iu.edu/~wrankine/bvm/")
 }else{
 
# If all of the above checks have been passed, move on to the 'read.table()' function. Hopefully, because of the above checks, this should not yield any error messages.
 Dataframe = read.table(file=OpenedFilePath,header=TRUE,sep="\t")
 
# Calculate the number of rows in the dataframe
 nRows = nrow(Dataframe)
 
# Create a dataframe containing all unique pairings of speaker and group, then sort the dataframe and revert the row names back to their defaults
 GroupAffiliations_Initial = unique(data.frame(Speaker=Dataframe$Speaker,Group=Dataframe$SpeakerGroup))
 GroupAffiliations = GroupAffiliations_Initial[order(GroupAffiliations_Initial$Speaker),]
 rownames(GroupAffiliations) <- NULL
 
# Now compile various relevant pieces of information about the speaker-groups
 assign("GroupVector_Character", value=as.character(GroupAffiliations$Group), envir=.GlobalEnv)
 assign("GroupVector_Integer", value=as.integer(as.factor(GroupVector_Character)), envir=.GlobalEnv)
 assign("UniqueGroupNames", value=sort(unique(GroupVector_Character)), envir=.GlobalEnv)
 assign("nGroups", value=length(UniqueGroupNames), envir=.GlobalEnv)
 
# Do the same thing for information on individual speakers
 assign("SpeakerVector_Character", value=as.character(Dataframe$Speaker), envir=.GlobalEnv)
 assign("SpeakerVector_Integer", value=as.integer(as.factor(SpeakerVector_Character)), envir=.GlobalEnv)
 assign("UniqueSpeakerNames", value=sort(unique(SpeakerVector_Character)), envir=.GlobalEnv)
 assign("nSpeakers", value=length(UniqueSpeakerNames), envir=.GlobalEnv)
 
# Do the same thing for information on vowel-targets
 assign("TargetVector_Character", value=as.character(Dataframe$Target), envir=.GlobalEnv)
 assign("TargetVector_Integer", value=as.integer(as.factor(TargetVector_Character)), envir=.GlobalEnv)
 assign("UniqueTargetNames", value=sort(unique(TargetVector_Character)), envir=.GlobalEnv)
 assign("nTargets", value=length(UniqueTargetNames), envir=.GlobalEnv)
 
# Calculate the grand mean and grand standard deviation (SD) for F1 and for F2
 assign("F1GrandMean", value=mean(Dataframe$F1), envir=.GlobalEnv)
 assign("F2GrandMean", value=mean(Dataframe$F2), envir=.GlobalEnv)
 assign("F1GrandSD", value=sd(Dataframe$F1), envir=.GlobalEnv)
 assign("F2GrandSD", value=sd(Dataframe$F2), envir=.GlobalEnv)
 
# Use these pieces of information to 'standardize' (i.e. z-score) F1 and F2 (separately)
 assign("StandardizedF1Coordinates", value=(Dataframe$F1 - F1GrandMean)/F1GrandSD, envir=.GlobalEnv)
 assign("StandardizedF2Coordinates", value=(Dataframe$F2 - F2GrandMean)/F2GrandSD, envir=.GlobalEnv)
 
# Specify the data in as a list, hence compatible with BRugs:
 DataList = list( nRows = nRows,
 GroupVector_Integer = GroupVector_Integer,
 nGroups = nGroups,
 SpeakerVector_Integer = SpeakerVector_Integer,
 nSpeakers = nSpeakers,
 TargetVector_Integer = TargetVector_Integer,
 nTargets = nTargets,
 FormantCoordinates = cbind(F1=StandardizedF1Coordinates,F2=StandardizedF2Coordinates)
 ) # End specification of 'DataList'
 
assign("DataList", value=DataList, envir=.GlobalEnv)
 assign("DataSuccessfullyImported", value=TRUE, envir=.GlobalEnv)
 
cat("\n\nThe data have successfully been imported.\n\n\n")
 
} # End 'if the second line of the file is in the correct format'
 } # End 'if the first line of the file is in the correct format'
 } # End 'if the user did not just hit cancel when opening the file'
 } # End function 'ImportData()'
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
 
RunAnalysis = function(){
 if(!DataSuccessfullyImported){tkmessageBox(title="Error", message=DataNotImportedError, icon="error")}
 if(DataSuccessfullyImported){
 
# Define a 'precision' function to simplify code below
 precision=function(input,na.rm=FALSE){1/(sd(input,na.rm=na.rm))^2}
 
# Cycle through each target and calculate the precision (in both F1 and F2) and the F1-F2 correlation for each of them
 PrecisionF1 <- PrecisionF2 <- Correlation <- vector(mode="numeric")
 for(EachTarget in 1:nTargets){ # EachTarget=1
 CurrentTargetFilter = ( TargetVector_Integer==EachTarget )
 CurrentF1 = StandardizedF1Coordinates[CurrentTargetFilter]
 CurrentF2 = StandardizedF2Coordinates[CurrentTargetFilter]
 PrecisionF1[EachTarget] <- precision(CurrentF1)
 PrecisionF2[EachTarget] <- precision(CurrentF2)
 Correlation[EachTarget] <- cor(CurrentF1,CurrentF2)
 } # End 'EachTarget' loop
 
# Calculate each individual speaker's Center (in both F1 and F2), then calculate the mean and precision of those according to group
 F1Center_Individual = aggregate(x=StandardizedF1Coordinates,by=list(SpeakerVector_Character),FUN=mean)[,"x"]
 F2Center_Individual = aggregate(x=StandardizedF2Coordinates,by=list(SpeakerVector_Character),FUN=mean)[,"x"]
 F1Center_GroupMean = mean(F1Center_Individual)
 F2Center_GroupMean = mean(F2Center_Individual)
 F1Center_GroupPrecision = precision(F1Center_Individual)
 F2Center_GroupPrecision = precision(F2Center_Individual)
 
# Do the same for center and expansion
 F1Expansion_Individual = aggregate(x=StandardizedF1Coordinates,by=list(SpeakerVector_Character),FUN=sd)[,"x"]
 F2Expansion_Individual = aggregate(x=StandardizedF2Coordinates,by=list(SpeakerVector_Character),FUN=sd)[,"x"]
 F1Expansion_GroupMean = mean(F1Expansion_Individual)
 F2Expansion_GroupMean = mean(F2Expansion_Individual)
 F1Expansion_GroupPrecision = precision(F1Expansion_Individual)
 F2Expansion_GroupPrecision = precision(F2Expansion_Individual)
 
# Determine mean F1 and F2 qualities for each target+speaker combination
 # (If there are any 'holes' in the dataset, they will show up here as NAs.)
 UnstandardizedF1Coordinates_Individual = tapply(X=StandardizedF1Coordinates,INDEX=list(Speaker=SpeakerVector_Character,Target=TargetVector_Character),FUN=mean)
 UnstandardizedF2Coordinates_Individual = tapply(X=StandardizedF2Coordinates,INDEX=list(Speaker=SpeakerVector_Character,Target=TargetVector_Character),FUN=mean)
 
# Duplicate the individual-speaker Center and Expansion vectors so that they form a matrix of the same size as the formant-coordinate matrices just calculated
 F1CenterMatrix_Individual = matrix(rep(F1Center_Individual,times=nTargets),nrow=nSpeakers,ncol=nTargets,byrow=FALSE)
 F2CenterMatrix_Individual = matrix(rep(F2Center_Individual,times=nTargets),nrow=nSpeakers,ncol=nTargets,byrow=FALSE)
 F1ExpansionMatrix_Individual = matrix(rep(F1Expansion_Individual,times=nTargets),nrow=nSpeakers,ncol=nTargets,byrow=FALSE)
 F2ExpansionMatrix_Individual = matrix(rep(F2Expansion_Individual,times=nTargets),nrow=nSpeakers,ncol=nTargets,byrow=FALSE)
 
# Standardize (i.e. z-score) the data using these new matrices
 F1Quality_Individual = (UnstandardizedF1Coordinates_Individual - F1CenterMatrix_Individual) / F1ExpansionMatrix_Individual
 F2Quality_Individual = (UnstandardizedF2Coordinates_Individual - F2CenterMatrix_Individual) / F2ExpansionMatrix_Individual
 
# Summarize these 'quality' matrices with a mean and precision for each group (in F1 and F2)
 F1Quality_GroupMean <- F2Quality_GroupMean <- F1Quality_GroupPrecision <- F2Quality_GroupPrecision <- matrix(nrow=nGroups,ncol=nTargets,dimnames=list(UniqueGroupNames,UniqueTargetNames))
 for(EachGroup in 1:nGroups){ # EachGroup=2
 GroupFilter = (EachGroup==GroupVector_Integer)
 F1Quality_GroupMean[EachGroup,] = apply(F1Quality_Individual[GroupFilter,],MARGIN=2,FUN="mean",na.rm=TRUE)
 F2Quality_GroupMean[EachGroup,] = apply(F2Quality_Individual[GroupFilter,],MARGIN=2,FUN="mean",na.rm=TRUE)
 F1Quality_GroupPrecision[EachGroup,] = apply(F1Quality_Individual[GroupFilter,],MARGIN=2,FUN="precision",na.rm=TRUE)
 F2Quality_GroupPrecision[EachGroup,] = apply(F2Quality_Individual[GroupFilter,],MARGIN=2,FUN="precision",na.rm=TRUE)
 } # End 'each group' filter
 
# Use all the values calculated thus far for 'intelligent initialization' of the MCMC sampling (based off the maximum-likelihood estimates from the data)
 InitialValueList = list( PrecisionF1 = PrecisionF1,
 PrecisionF2 = PrecisionF2,
 Correlation = Correlation,
 F1Center_GroupMean = F1Center_GroupMean,
 F1Center_GroupPrecision = F1Center_GroupPrecision,
 F1Expansion_GroupMean = F1Expansion_GroupMean,
 F1Expansion_GroupPrecision = F1Expansion_GroupPrecision,
 F1Quality_GroupMean = F1Quality_GroupMean,
 F1Quality_GroupPrecision = F1Quality_GroupPrecision,
 F2Center_GroupMean = F2Center_GroupMean,
 F2Center_GroupPrecision = F2Center_GroupPrecision,
 F2Expansion_GroupMean = F2Expansion_GroupMean,
 F2Expansion_GroupPrecision = F2Expansion_GroupPrecision,
 F2Quality_GroupMean = F2Quality_GroupMean,
 F2Quality_GroupPrecision = F2Quality_GroupPrecision )
 
# Specify the parameters to sample. At present, all parameters are sampled
 ParametersToSample = c("PrecisionF1", "PrecisionF2",
 "Correlation",
 "F1Center_GroupMean", "F1Center_GroupPrecision",
 "F1Expansion_GroupMean", "F1Expansion_GroupPrecision",
 "F1Quality_GroupMean", "F1Quality_GroupPrecision",
 "F2Center_GroupMean", "F2Center_GroupPrecision",
 "F2Expansion_GroupMean", "F2Expansion_GroupPrecision",
 "F2Quality_GroupMean", "F2Quality_GroupPrecision")
 
# Specify the number of steps to 'tune' the samplers
 nAdaptionSteps = 500
 
# Specify the number of steps to 'burn-in' the samplers
 nBurnInSteps = 500
 
# Specify the number of chains to run
 nChains = 5
 
# Specify the total number of steps in chains to save
 nSavedSteps = 1000
 
# Specify the number of steps to 'thin'. (Setting this to '1' means no thinning will be applied.)
 ThinningFactor=1
 
# Based on the above-set parameters, calculate the number of steps per chain.
 nPerChain = ceiling( ( nSavedSteps * ThinningFactor ) / nChains )
 
# STEP I: Creat, initialize, and adapt the model
 cat("\n\nStep I: Creating, initializing, and adapting the model...\n\n\n");flush.console()
 JAGS_Model = jags.model(file="Model specification- Comparing speaker-groups.txt", data=DataList, inits=InitialValueList, n.chains=nChains, n.adapt=nAdaptionSteps)
 alarm();Sys.sleep(0.5)
 
# STEP II: Burn-in the MCMC chain
 cat("\n\nStep II: Burning-in the MCMC chain...\n\n\n");flush.console()
 update(JAGS_Model, n.iter=nBurnInSteps)
 alarm();Sys.sleep(0.5);alarm()
 
# STEP III: Sample the final MCMC chain
 cat("\n\nStep III: Sampling final MCMC chain...\n\n\n");flush.console()
 MCMC_List = coda.samples(JAGS_Model, variable.names=ParametersToSample, n.iter=nPerChain, thin=ThinningFactor)
 alarm();Sys.sleep(0.5);alarm();Sys.sleep(0.5);alarm()
 
cat("\n\nThe analysis is now complete.\n\n\n");flush.console()
 
# Concatenate the various chains stored in 'list' format in MCMC_List into one long chain in a matrix.
 for(EachChain in 1:nChains){ # EachChain=1
 if(EachChain==1){MCMC_Chain=MCMC_List[[1]]
 }else{
 MCMC_Chain = rbind(MCMC_List,MCMC_List[[EachChain]])
 } # End 'if/else'
 } # End 'each chain' loop
 
# Extract out the Centers and Expansions in unconstrained form
 UnconstrainedF1Center <- as.numeric(MCMC_Chain[,"F1Center_GroupMean"])
 UnconstrainedF1Expansion <- as.numeric(MCMC_Chain[,"F1Expansion_GroupMean"])
 UnconstrainedF2Center <- as.numeric(MCMC_Chain[,"F2Center_GroupMean"])
 UnconstrainedF2Expansion <- as.numeric(MCMC_Chain[,"F2Expansion_GroupMean"])
 
# Extract out the Quality in unconstrained form
 UnconstrainedF1Quality <- UnconstrainedF2Quality <- array(dim=c(nTargets,nGroups,nSavedSteps))
 for(EachTarget in 1:nTargets){ # EachTarget = 1
 for(EachGroup in 1:nGroups){ # EachGroup = 1
 UnconstrainedF1Quality[EachTarget,EachGroup,] <- MCMC_Chain[,paste("F1Quality_GroupMean[",EachGroup,",",EachTarget,"]",sep="")]
 UnconstrainedF2Quality[EachTarget,EachGroup,] <- MCMC_Chain[,paste("F2Quality_GroupMean[",EachGroup,",",EachTarget,"]",sep="")]
 } # End 'each speaker' loop
 } # End 'each target' loop
 
# Convert these into constrained (but still standardized) estimates
 StandardizedF1Coordinates <- StandardizedF2Coordinates <- array(dim=c(nTargets,nGroups,nSavedSteps))
 for(EachStep in 1:nSavedSteps){ # EachStep = 1
 F1CenterMatrix = matrix(rep(UnconstrainedF1Center[EachStep],times=nTargets),nrow=nTargets,ncol=nGroups,byrow=FALSE)
 F1ExpansionMatrix = matrix(rep(UnconstrainedF1Expansion[EachStep],times=nTargets),nrow=nTargets,ncol=nGroups,byrow=FALSE)
 F2CenterMatrix = matrix(rep(UnconstrainedF2Center[EachStep],times=nTargets),nrow=nTargets,ncol=nGroups,byrow=FALSE)
 F2ExpansionMatrix = matrix(rep(UnconstrainedF2Expansion[EachStep],times=nTargets),nrow=nTargets,ncol=nGroups,byrow=FALSE)
 F1QualityMatrix = UnconstrainedF1Quality[,,EachStep]
 F2QualityMatrix = UnconstrainedF2Quality[,,EachStep]
 StandardizedF1Coordinates[,,EachStep] <- F1CenterMatrix + F1ExpansionMatrix * F1QualityMatrix
 StandardizedF2Coordinates[,,EachStep] <- F2CenterMatrix + F2ExpansionMatrix * F2QualityMatrix
 } # End 'each step' loop
 
# Finally, de-standardize the measurements in order to put them back onto the raw scale
 UnstandardizedF1Coordinates = StandardizedF1Coordinates * F1GrandSD + F1GrandMean
 UnstandardizedF2Coordinates = StandardizedF2Coordinates * F2GrandSD + F2GrandMean
 
# Compute one global mean (=Center) and SD (=Expansion) for each step in the chain (margin 3)
 # In doing so, use c() to collapse across all groups (margin 1) and targets (margin 2)
 F1Center = apply(UnstandardizedF1Coordinates,MARGIN=c(3),FUN=function(x){mean(c(x))})
 F2Center = apply(UnstandardizedF2Coordinates,MARGIN=c(3),FUN=function(x){mean(c(x))})
 
F1Expansion = apply(UnstandardizedF1Coordinates,MARGIN=c(3),FUN=function(x){sd(c(x))})
 F2Expansion = apply(UnstandardizedF2Coordinates,MARGIN=c(3),FUN=function(x){sd(c(x))})
 
# Stack the center and expansion matrices to get back to 3 dimensions
 F1CenterMatrixStack <- F1ExpansionMatrixStack <- F2CenterMatrixStack <- F2ExpansionMatrixStack <- array(dim=c(nTargets,nGroups,nSavedSteps))
 for(EachTarget in 1:nTargets){ # EachTarget = 1
 for(EachGroup in 1:nGroups){ # To insert the same overall values for both groups
 F1CenterMatrixStack[EachTarget,EachGroup,] <- F1Center
 F2CenterMatrixStack[EachTarget,EachGroup,] <- F2Center
 F1ExpansionMatrixStack[EachTarget,EachGroup,] <- F1Expansion
 F2ExpansionMatrixStack[EachTarget,EachGroup,] <- F2Expansion
 } # End 'EachGroup' loop
 } # End 'each target' loop
 
# Use these stacked matrices to 'normalize out' the center and expansion
 F1Quality = ( UnstandardizedF1Coordinates - F1CenterMatrixStack )/ F1ExpansionMatrixStack
 F2Quality = ( UnstandardizedF2Coordinates - F2CenterMatrixStack )/ F2ExpansionMatrixStack
 
# Name rows and columns
 dimnames(F1Quality) <- dimnames(F2Quality) <- list(UniqueTargetNames,UniqueGroupNames,NULL)
 
for(EachGroup in 1:nGroups){ # EachGroup=1
 CurrentF1Matrix = t(F1Quality[,EachGroup,])
 CurrentF2Matrix = t(F2Quality[,EachGroup,])
 colnames(CurrentF1Matrix) <- paste(UniqueGroupNames[EachGroup],colnames(CurrentF1Matrix),sep="_")
 colnames(CurrentF2Matrix) <- paste(UniqueGroupNames[EachGroup],colnames(CurrentF2Matrix),sep="_")
 if(EachGroup==1){
 F1QualityPosterior = CurrentF1Matrix
 F2QualityPosterior = CurrentF2Matrix
 }else{
 F1QualityPosterior = cbind(F1QualityPosterior,CurrentF1Matrix)
 F2QualityPosterior = cbind(F2QualityPosterior,CurrentF2Matrix)
 } # End 'if/else'
 } # End 'each group' loop
 
assign("F1QualityPosterior", value=F1QualityPosterior, envir=.GlobalEnv)
 assign("F2QualityPosterior", value=F2QualityPosterior, envir=.GlobalEnv)
 
assign("AnalysisSuccessfullyRun", value=TRUE, envir=.GlobalEnv)
 
} # End 'if data have been imported and analysis has been run' statement
 } # End function 'RunAnalysis()'
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
 
ExportParameterEstimates = function(){
 
if(!DataSuccessfullyImported){tkmessageBox(title="Error", message=DataNotImportedError, icon="error")}
 if(!AnalysisSuccessfullyRun ){tkmessageBox(title="Error", message=AnalysisNotRunError, icon="error")}
 if(DataSuccessfullyImported & AnalysisSuccessfullyRun){
 write.table(x=F1QualityPosterior,file="F1QualityPosterior.txt",quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE)
 write.table(x=F2QualityPosterior,file="F2QualityPosterior.txt",quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE)
 cat("The group-level F1 and F2 quality estimates from the posterior have\nsuccessfully been exported to the directory where your input data file\nis located.\n\n\n")
 
} # End 'if data have been imported and analysis has been run' statement
 } # End function 'ExportParameterEstimates()'
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
 
# Create the top-level window
 TopLevelWindow = tktoplevel(width=400,height=100)
 
# Create the overall menu bar and store it in an R object
 MenuBar = tkmenu(TopLevelWindow)
 
# Configure the window to be associated with this menu bar
 tkconfigure(TopLevelWindow, menu = MenuBar)
 
# Set up the 'File' menu
 Menu_File = tkmenu(MenuBar, tearoff = FALSE)
 tkadd(MenuBar, "cascade", label="File", menu=Menu_File)
 tkadd(Menu_File, "command", label = "Import data", command = ImportData)
 tkadd(Menu_File, "separator")
 tkadd(Menu_File, "command", label = "Quit", command = function(){tkdestroy(TopLevelWindow)})
 
# Set up the 'Run analysis' menu button
 Menu_RunAnalysis = tkmenu(MenuBar, tearoff = FALSE)
 tkadd(MenuBar, "command", label="Run analysis", command=RunAnalysis)
 
# Set up the 'Export parameter estimates' menu button
 Menu_ExportParameterEstimates = tkmenu(MenuBar, tearoff = FALSE)
 tkadd(MenuBar, "command", label="Export parameter estimates", command=ExportParameterEstimates)
 
# Finally, put the top-level window into focus:
 tkfocus(TopLevelWindow)
 
# Set the flag for the data being imported to FALSE... until it is changed to TRUE when 'ImportData()' is called.
 assign("DataSuccessfullyImported", value=FALSE, envir=.GlobalEnv)
 assign("AnalysisSuccessfullyRun", value=FALSE, envir=.GlobalEnv)
 
} # End "if the 'tcltk' and 'rjags' packages are installed" statement
