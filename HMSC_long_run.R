######################################################################
######################################################################
#code adapted from:
#https://www.r-bloggers.com/2020/06/guide-to-using-the-hmsc-package-for-the-production-of-joint-species-distribtuion-models/
install.packages("Hmsc")
install.packages("snow")
install.packages("corrplot")

require(Hmsc)
require(snow)
require(corrplot)

rm(list=ls())


##read in occupancy data
#data1<-read.csv("G:\\GB_SDM\\Hmsc_Data_2km_0_occ_removed.csv")
head(data1)
dim(data1)
#data<-read.csv(choose.files()) #bbs_hmsc_occ_env
data<-read.csv(choose.files())
data<-read.csv("G:\\GB_SDM\\HMSC\\updated_HMSC\\community_data_5km.csv")
data<-read.csv("D:\\GB_SDM\\community_data_2km_hmsc_2026.csv")
data<-read.csv("G:\\GB_SDM\\community_data_500m_hmsc_2026.csv")

#data<-read.csv("G:\\GB_SDM\\HMSC\\updated_HMSC\\community_data_500m_pivottable.csv")

head(data[,1:10])
dim(data)
data<-data[-1,]
data<-data[-1,]

###Change occupancy to binary
data[is.na(data)]<-0
head(data)
colnames(data)[1]<-"Site"
site_names<-as.numeric(data$Site)

data[data > 0] <- 1 
data[,1]<-site_names
head(data)

colnames(data)
#remove columns that are not relevant 33 = NA, 51 = Total
colnames(data)
head(data)
#remove the grand total row at the bottom
dim(data)
data[723,]
#data<-data[-337,]
data<-data[-723,]
names(data)[1]<-"Site"

data[992,]
head(data)
dim(data)
#read in predictor data

jsdm_site_params<-read.csv("D:/GB_SDM/env_data_2km_2026.csv")
jsdm_site_params<-read.csv("G:/GB_SDM/env_data_500m_2026.csv")


dim(jsdm_site_params)
head(jsdm_site_params)
names(jsdm_site_params)
names(jsdm_site_params)[2]<-"Site"

jsdm_site_params<-jsdm_site_params[,-1]
jsdm_site_params$Site

names(jsdm_site_params)
head(jsdm_site_params)

com_data<-merge(jsdm_site_params,data,by="Site")

colnames(jsdm_site_params)
colnames(data)
head(com_data)


colnames(com_data)
#phylo<-read.csv(choose.files()) #phyloHMSC.csv phylogenetic file
#Tr<-read.csv("G:\\GB_SDM\\HMSC\\trait_data_march2025.csv")

Tr<-read.csv("D:\\GB_SDM\\trait_data_March2026.csv")

sp_names<-Tr[,1:3]
head(Tr)
head(sp_names)
dim(data)
dim(Tr)
rownames(Tr)
head(Tr)


Tr$species.code
Tr<-Tr[is.element(Tr$species.code,colnames(data)),]

dim(data)
names(data)

#2km
dim(data)
dim(com_data)
colnames(com_data)
head(com_data)
summary_rows<-(com_data[,14:84])
class(summary_rows)
head(com_data)
test<-com_data[,14:84]

require(dplyr)

str(com_data[, 14:84])
com_data[, 14:84] <- lapply(com_data[, 14:84], as.numeric)
str(com_data[, 14:84])

data<-(com_data[which(rowSums(com_data[,14:84],na.rm=TRUE)>0),])
dim(data)
head(data)
data[is.na(data)]<-0

#500m
data<-(com_data[which(rowSums(com_data[,13:73])>0),])

head(data)
dim(data)
######################################################################
######################################################################
#Environmental predictor selection and manipulation

#select the environmental datasets that will be used in the analysis
head(data)
cov<-data[,c(2:13)]
head(cov)
cov_sc<-data.frame(scale(cov))
#check normality of all variables and transform if necessary
names(cov)
qqnorm((cov[,2]))

X<-cov
head(X)
dim(X)

#phragmites had NA, so replace with 0
X$Phragmites[is.na(X$Phragmites)]<-0
X$Slope[is.na(X$Slope)]<-0
X$DepthtoWaterTable[is.na(X$DepthtoWaterTable)]<-0
X$Dist2Coast[is.na(X$Dist2Coast)]<-0
X$hfp2022[is.na(X$hfp2022)]<-0


dim(X)
#2km has 438 sites retained

#check distributions again
qqnorm(((X[,11])))
names(X)
X_sc<-data.frame(scale(X))
head(X_sc)
qqnorm(X_sc[,11])
#create a formula. The below only has addative effects, 
#but you can create interactions between env variables


XFormula<- ~Slope + SandPercent + DepthtoWaterTable + 
	FloodFreq + DrainClass +PercentImpervious +PercentCanopy + hfp2022 +
	TempWarmQ + + PrecipWetQ + 
	Dist2Coast + Phragmites 

######################################################################
######################################################################
#Species occurrence box
head(data)
dim(data)
colnames(data)[1]<-"Site"
#select species occurrences
Y<-as.matrix(data[,13:60])
Y<-as.matrix(data[,14:84])
Y<-as.matrix(data[,14:73])

colnames(Y)

######################################################################
######################################################################
#Study Design
#provide the sampling design structure (e.g., year, site, etc).

studyDesign<-data[,c("Site")]

#reassign the numeric values to be factors for both site and plot
studyDesign<-data.frame(as.factor(studyDesign))
#,as.factor(studyDesign[,2]))
colnames(studyDesign)<-c("site") #,"plot")
class(studyDesign[,1])

#now structure the variables. These will be HMSC random effects
site<-HmscRandomLevel(units=studyDesign$ID)
ranlevels<-list(site = site)#,plot = plot)
ranlevels

######################################################################
######################################################################
#Phylogenetic Matrix
#now transform the phylogenetic matrix

#C<-as.matrix(phylo)

######################################################################
######################################################################
#Phylogenetic Matrix
#now transform the traits matrix

colnames(Tr)
head(Tr)

#assign species as rownames and remove the species code
#species names have to match object Y above
rownames(Tr)<-Tr[,3]
head(Tr)
Tr<-Tr[,-1]
Tr<-Tr[,-1]
Tr<-Tr[,-1]
Tr<-Tr[,-23]
dim(Tr)
head(Tr)


#ensure that the names are the same (adjust in case of typos)
colnames(Y)
rownames(Tr)

rownames(Tr)<-colnames(Y)


#now create a traits formula that is similar to the X formula above
colnames(Tr)
TrFormula<- ~as.factor(inland) +
		as.factor(shore) +
		as.factor(shallow) +
		as.factor(surface) +
		as.factor(shallow_dive) +
		as.factor(deep_dive) +
		as.factor(vegetation) +
		as.factor(insects) +
		as.factor(herps) +
		as.factor(mollusks) +
		as.factor(crustaceans) +
		as.factor(fish) +
		as.factor(small.mammals) +
		as.factor(aves) +
		as.factor(scavenger) +
		as.factor(marsh) +
		as.factor(meadow) +
		as.factor(trees) +
		as.factor(open..exposed) +
		as.factor(human.influenced) +
		as.factor(flocking) +
		as.factor(colonial.nesting)

######################################################################
######################################################################
#now we can create and run the model

#assemble the previously created objects into a formual

simul <- Hmsc(Y=Y, XData = X,
		#Y are the species occurrences and X is the env params

              XFormula = XFormula,
		#XFormula is how the env params interact

              TrData = Tr,
		#Tr are the trait data

              TrFormula = TrFormula , 
		#TrFormula is how the traits interact

            #  C = C,
		#C is the phylogenetic structure

              studyDesign = studyDesign,
		#studyDesign are the data for the random effects

              ranLevels  = ranlevels,
		#ranLevels specifies which data are random

              distr = "probit")  
		#probit used for presence/absence

simul_sc <- Hmsc(Y=Y, XData = X_sc,
		#Y are the species occurrences and X is the env params

              XFormula = XFormula,
		#XFormula is how the env params interact

              TrData = Tr, 
		#Tr are the trait data

              TrFormula = TrFormula , 
		#TrFormula is how the traits interact

            #  C = C,
		#C is the phylogenetic structure

              studyDesign = studyDesign,
		#studyDesign are the data for the random effects

              ranLevels  = ranlevels,
		#ranLevels specifies which data are random

              distr = "probit")  
		#presume a normal distribution rather than a logit model


######################################################################
######################################################################
#set Bayesian parameters. Transient is the burn-in
thin<-10000
samples<-1000000
nChains<-3
transient<-10000


mod_HMSC <- sampleMcmc(simul,
                      samples = samples,
                      thin = thin,
                      transient = transient,
                      nChains = nChains, 
                      nParallel = nChains)


save(mod_HMSC,file="D:\\GB_SDM\\HMSC_2km_noTransformation_2026_longer.Rdata")

save(mod_HMSC,file="C:\\Users\\Nathan\\OneDrive - University of Illinois - Urbana\\Desktop\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_5km_noTransformation.Rdata")
save(mod_HMSC,file="G:\\GB_SDM\\updated_models\\HMSC\\HMSC_2km_noTransformation_2026.Rdata")
save(mod_HMSC,file="C:\\Users\\Nathan\\OneDrive - University of Illinois - Urbana\\Desktop\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_500m_noTransformation.Rdata")
save(mod_HMSC,file="G:\\GB_SDM\\updated_models\\HMSC\\HMSC_500m_noTransformation_2026.Rdata")

save(mod_HMSC,file="G:\\Avian_Disease_postdoc\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_2km_noTransformation.Rdata")
save(mod_HMSC,file="G:\\Avian_Disease_postdoc\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_500m_noTransformation.Rdata")

mod_HMSC_sc <- sampleMcmc(simul_sc,
                      samples = samples,
                      thin = thin,
                      transient = transient,
                      nChains = nChains, 
                      nParallel = nChains)
save(mod_HMSC_sc,file="C:\\Users\\Nathan\\OneDrive - University of Illinois - Urbana\\Desktop\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_5km_scale.Rdata")
save(mod_HMSC_sc,file="C:\\Users\\Nathan\\OneDrive - University of Illinois - Urbana\\Desktop\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_2km_scale.Rdata")

######################################################################
######################################################################
#Check chain convergence
setwd(choose.dir())
getwd()
rm(list=ls())
load("D:\\GB_SDM\\HMSC_2km_noTransformation_2026_longer.Rdata")
load("G:\\Avian_Disease_postdoc\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_500m_noTransformation.Rdata")
load("E:\\Avian_Disease_postdoc\\Cat Island Data\\HMSC\\updated_HMSC\\HMSC_out\\HMSC_2km_noTransformation.Rdata")

load("G:\\GB_SDM\\updated_models\\HMSC\\HMSC_2km_noTransformation_2026.Rdata")

load("G:\\GB_SDM\\updated_models\\HMSC\\HMSC_500m_noTransformation_2026.Rdata")

## Convergence tests
mcoda <- convertToCodaObject(mod_HMSC)


layout(matrix(1:72,ncol=12))
#Visual chain tests for different coefficients of interest 
plot(mcoda$Beta)
(mcoda$Beta)
plot(mcoda$Gamma[,1:5])



# Gelman's diagnosis, which should be at most close to 1.0 for good convergence.
gelman.diag(mcoda$Beta[,1:14])$psrf
max(gelman.diag(mcoda$Beta)$psrf)
min(gelman.diag(mcoda$Beta)$psrf)

boxplot(effectiveSize(mcoda$Beta))
boxplot(gelman.diag(mcoda$Beta)$psrf)


#2.2074 Upper CI for TempWarmestQ Meleagris gallopavo, but mean is 1.29 at 2km
#1.144 Upper CI for Charadrius vociferus phragmites, but mean is 1.06 at 500m
#multivariate psrf is 1.31 at 500m and 1.51 for 2km.

length(mod_HMSC$postList)

partition<-createPartition(mod_HMSC,nfolds=10,column="site")

pred_params<-pcomputePredictedValues(mod_HMSC,partition=partition, 
	start=1000, thin=1000,mcmcStep = 1000, nChains=3, nParallel = 3, alignPost = TRUE,verbose=TRUE)
mod_ fit<-evaluateModelFit(mod_HMSC,pred_params)

#modfit has RMSE, AUC, and TjurR2
mod_HMSC$spNames
mod_fit$AUC
mod_fit$TjurR2
mod_fit$RMSE

#2km
#Anas carolinensis (green-winged teal), Egretta thula (snowy egret), Gallinula gallinula (Common gallinule), and
#Recurvirostra americana (American avocet) did not resolve AUC nor Tjur R2

#500m
#Calidris alpina and Calidrid pusilla did not resolve for AUC nor TJUR2 at 500m. Note that only 60 species are included at 500m


mod_fit_500m<-cbind(mod_HMSC$spNames,as.numeric(mod_fit$RMSE),as.numeric(mod_fit$AUC),as.numeric(mod_fit$TjurR2))
colnames(mod_fit_500m)<-c("Species","RMSE","AUC","TjurR2")
head(mod_fit_500m)
plot(mod_fit_500m$RMSE)
write.csv(mod_fit_500m,"G:\\GB_SDM\\updated_models\\HMSC\\HMSC_500m_noTransformation_2026_modelFit.csv")


mod_fit_2km<-cbind(as.factor(mod_HMSC$spNames,as.numeric(mod_fit$RMSE),as.numeric(mod_fit$AUC),as.numeric(mod_fit$TjurR2))
colnames(mod_fit_2km)<-c("Species","RMSE","AUC","TjurR2")
head(mod_fit_2km)
plot(mod_fit_2km$RMSE)
write.csv(mod_fit_2km,"G:\\GB_SDM\\updated_models\\HMSC\\HMSC_2km_noTransformation_2026_modelFit.csv")

mod_fit_2km<-read.csv("G:\\GB_SDM\\updated_models\\HMSC\\HMSC_2km_noTransformation_2026_modelFit.csv")
head(mod_fit_2km)
mod_fit_2km<-mod_fit_2km[,-1]

mod_fit_500m<-read.csv("G:\\GB_SDM\\updated_models\\HMSC\\HMSC_500m_noTransformation_2026_modelFit.csv")
head(mod_fit_500m)
mod_fit_500m<-mod_fit_500m[,-1]

mod_fit_2km<-cbind(mod_fit_2km[,1],rep("1km",nrow(mod_fit_2km)),mod_fit_2km[,2:4])
colnames(mod_fit_2km)<-c("Species","Radius","RMSE","AUC","TjurR2")

mod_fit_500m<-cbind(mod_fit_500m[,1],rep("250m",nrow(mod_fit_500m)),mod_fit_500m[,2:4])
colnames(mod_fit_500m)<-c("Species","Radius","RMSE","AUC","TjurR2")

mod_fit_scale<-rbind(mod_fit_2km,mod_fit_500m)


require(reshape)
mod_fit_m<-melt(mod_fit_scale)
head(mod_fit_m)

require(ggplot2)
JSDM_modfit<-
	ggplot(na.omit(mod_fit_m), aes(x=Species,y=value,group=variable,color=Radius)) + 
	geom_point(size=2)+
	scale_color_manual(values=c("deepskyblue3","goldenrod3"))+
	theme_classic()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
	xlab("Species")+facet_wrap(~variable,ncol=1,scales="free_y")


ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\jsdm_fit.png", units="in", width=6, height=8, dpi=600)#, compression = 'lzw')
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\jsdm_fit.tif", units="in", width=6, height=8, dpi=600, compression = 'lzw')

#find out which species are not in the 500m compared to the 2km models

require(dplyr) 

anti_join(mod_fit_2km,mod_fit_500m,by="Species")
#at the 500m, we do not have Accipiter cooperii, Anas carolinensis, Botaurus exilis, Chlidonias niger,
#Egretta thula, Gallinula chloropus, Melanitta deglandi, Phalaropus tricolor, Podiceps grisegena,
#Recurvirostra americana, and Xanthocephalus xanthocephalus	

mod_fit_500m


pred_params_cv<-computePredictedValues(mod_HMSC,partition=partition)
mod_fit_cv<-evaluateModelFit(mod_HMSC,pred_params_cv)

partition<-createPartition(mod_HMSC_sc,nfolds=2,column="site")
pred_params<-computePredictedValues(mod_HMSC_sc)
mod_fit<-evaluateModelFit(mod_HMSC_sc,pred_params)
pred_params_cv<-computePredictedValues(mod_HMSC_sc,partition=partition)
mod_fit_cv<-evaluateModelFit(mod_HMSC_sc,pred_params_cv)

#######
#Environmental contributions visualization
postBeta = getPostEstimate(mod_HMSC, parName = "Beta")
names(postBeta)
quantile(postBeta$mean[,1])

plotBeta(mod_HMSC,
         post = postBeta, 
         plotTree = F,
         spNamesNumbers = c(T,F))
dev.new()
names(mod_HMSC)
sp_test<-list(colnames(mod_HMSC$Y))


dev.new(width=5,height=10)
png("G:\\GB_SDM\\updated_models\\updated_figs\\jsdm_support_500.png",res=1200,width=6,height=10,unit="in",bg="white")
png("D:\\jsdm_support_2km_longer.png",res=1200,width=5,height=11,unit="in",bg="white")

#plotBeta is not very flexible, so we need to add margins. We create blank plots in layout
#to accommodate labels

plotBeta(mod_HMSC, 
         post = postBeta,
	   SpeciesOrder = "Original", #"Vector"
         param = "Support",
         plotTree = F,  
	  # SpVector = match(1:mod_HMSC$ns, as.numeric(as.factor(colnames(mod_HMSC$Y)))),
         spNamesNumbers = c(T,F),
	   supportLevel = 0.75,
	   colorLevels=9,
	   colors=colorRampPalette(c("goldenrod4","goldenrod3","goldenrod2","goldenrod1",
			"white",
			"deepskyblue1","deepskyblue2","deepskyblue3","deepskyblue4")),
	   #cex = c(.8, .8, .8),
	  # mar= c(2.1, 2.1, 4.1, 2.1),
	   newplot=T)

dev.off()

par("mar")
plotBeta(mod_HMSC_sc, 
         post = postBeta,
         param = "Support",
         plotTree = F,  
         spNamesNumbers = c(T,F),
	   supportLevel = 0.75)

########
#Trait based contributions
dev.new()
par(mar=c(5,11,2.5,0))
postGamma = getPostEstimate(mod_HMSC, parName = "Gamma")



dev.new()
png("G:\\GB_SDM\\updated_models\\updated_figs\\jsdm_trait_500m.png",res=1200,width=6,height=6,unit="in",bg="white")
png("D:\\jsdm_trait_2km_longer.png",res=1200,width=6,height=6,unit="in",bg="white")

plotGamma(mod_HMSC,
	 post = postGamma,
	 param = "Support",
	supportLevel = 0.75,
	    colorLevels=9,
	   colors=colorRampPalette(c("goldenrod4","goldenrod3","goldenrod2","goldenrod1",
			"white",
			"deepskyblue1","deepskyblue2","deepskyblue3","deepskyblue4")),
	   cex = c(.7, .5, 0.8),
	   mar= c(5.1, 5.1, 4.1, 2.1))
dev.off()

colnames(mod_HMSC$TrData)


#####
#Variance partitioning
VP <- computeVariancePartitioning(mod_HMSC)
dev.new()
par(mar=c(4,4,4,4))
plotVariancePartitioning(mod_HMSC, VP = VP,
                         las = 2, horiz=F)
graph2ppt(file="G:\\GB_SDM\\updated_models\\updated_figs\\HMSC_2km_noTransformation_VarPart_sign_75.ppt",width=6,height=8)
graph2doc(file="G:\\GB_SDM\\updated_models\\updated_figs\\HMSC_2kmm_noTransformation_VarPart_sign_75.doc",width=10,height=11)
graph2svg(file="G:\\GB_SDM\\updated_models\\updated_figs\\HMSC_2km_noTransformation_VarPart_sign_75.svg",width=6,height=8)


####
#species richness by env
names(mod_HMSC)
Gradient = constructGradient(mod_HMSC,
                             focalVariable = "Phragmites")
head(Gradient$XDataNew)

predY = predict.Hmsc(mod_HMSC,
                XData = Gradient$XDataNew, 
                studyDesign = Gradient$studyDesignNew,
                ranLevels = Gradient$rLNew,
                expected = TRUE)

### Species richness
plotGradient(mod_HMSC,
             Gradient,
             pred=predY,
             measure="S",
             showData = TRUE)

##single species response
#index=species row
plotGradient(mod_HMSC,
             Gradient,
             pred=predY,
             measure="Y",
             index = 8,
             showData = TRUE)

##traitbased response
### Species traits
dev.new()
plotGradient(mod_HMSC, 
             Gradient,
             pred=predY,
             measure="T",
             index = 2, 
             showData = TRUE)
###species co-occurrence
dev.new()
OmegaCor = computeAssociations(mod_HMSC)
(OmegaCor)
install.packages("pheatmap")
require(pheatmap)
pheatmap(OmegaCor[[1]]$mean)
supportLevel = 0.95

dev.new()
png("G:\\GB_SDM\\updated_models\\updated_figs\\500m_co_occ.png",res=1200,width=8,height=8,unit="in",bg="white")
png("D:\\2km_co_occ_longer.png",res=1200,width=8,height=8,unit="in",bg="white")

for (r in 1:mod_HMSC$nr){
plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
toPlot = ((OmegaCor[[r]]$support>supportLevel) +
(OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean
par(xpd=T)
colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot))
corrplot(toPlot[plotOrder,plotOrder], method = "color",
col=colorRampPalette(c("goldenrod4","white","deepskyblue4"))(200),
col.lim = c(0,1),
title="",type="lower",tl.col="black",tl.cex=.5, mar=c(0,0,6,0))
}
dev.off()

