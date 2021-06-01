library(data.table)
library(dplyr)
library(psych)

#Read in the dataset 
allphenos <- fread('Phenotypes_with_scz_bip_sa_rm_v2.csv', data.table=FALSE)
allphenos$ID <- as.factor(allphenos$ID)

#Remove individuals who aren't of european ancestry
##Read in the qc'd sample which includes people of european ancestry
euranc <- fread('unrelated_european_ancestries.fam')
colnames(euranc)[1] <- 'ID'
euranc$ID <- as.factor(euranc$ID)

##Inner join to all phenos so only IDs that match between the two files are retained. 
eurphenos <- inner_join(euranc, allphenos , by='ID')

#Take the phenotypes and ID only
UpdatedPhenos <- eurphenos[,-c(3:6)]

UpdatedPhenos <- fread("Phenotypes_unrelated_screened_european_ancestry.csv", data.table=FALSE)
colnames(UpdatedPhenos)[c(1,2)] <- c("fid","iid")
UpdatedPhenos$fid <- as.factor(UpdatedPhenos$fid)
UpdatedPhenos$iid <- as.factor(UpdatedPhenos$iid)

#Create individual phenotype files that follows convention in example - FID, IID, cases=1, controls=0, missings are removed.

for (i in 1:32){
	subset.of.pheno <- UpdatedPhenos[,c("fid","iid",paste("Pheno",i,sep=""))]
	#Rearrange dataframe so its got the right 
	colnames(subset.of.pheno) <- c("fid","iid","pheno")
	#remove all missing values from the file
	subset.of.pheno <- subset(subset.of.pheno, pheno!=999)
	print(dim(subset.of.pheno))
	write.table(subset.of.pheno, paste("Pheno",i,".txt",sep=""), row.names=F, quote=FALSE, sep="\t")
}

###################################################################################################################################################

#Create the covariate files for each phenotype
covariates <- fread('covariates.txt', data.table=FALSE)
colnames(covariates)[1] <- 'fid'
covariates$fid <- as.factor(covariates$fid)

#Inner join to the updated phenotype file so only considering the covariates for unrelated people of european ancestry
covandphen <- inner_join(UpdatedPhenos, covariates, By='fid')

#Remove phenotype variables
covariates <- covandphen[,-c(3:35)]

#Dummy code assessment centre and batch
rownames(covariates) <- covariates[,1]
batch_dummy <- dummy.code(covariates$batch)
covariates <- data.frame(covariates,batch_dummy)

assessment_centre_dummy <- dummy.code(covariates$assessment_centre)
covariates <- data.frame(covariates, assessment_centre_dummy)

#Remove assessment_centre and batch factors
covariates <- covariates[,-c(9,10)]

#Save down the full covariates file before alignment
write.table(covariates, "covariates_adjusted.cov", quote=FALSE, row.names=F, sep="\t")
	
covariates <- fread('covariates_adjusted.cov', data.table=FALSE) 	
covariates$fid <- as.factor(covariates$fid)
covariates$iid <- as.factor(covariates$iid)

#Align to the IDs of individuals for each phenotype
for (i in 1:32){
	pheno <- read.table(paste('Pheno',i,".txt",sep=""), sep="\t",header=T)
	pheno$fid <- as.factor(pheno$fid)
	pheno$iid <- as.factor(pheno$iid)
	covandpheno <- inner_join(pheno, covariates)
	#Remove phenotype
	cov <- covandpheno[,-3]
	print(dim(cov))
	write.table(cov, paste("Covar",i,".cov",sep=""), row.names=F, quote=FALSE, sep="\t")
}