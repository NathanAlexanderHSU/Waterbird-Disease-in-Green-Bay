options(java.parameters = "-Xmx8000m")

rm(list=ls())
options(java.parameters = "-Xmx8000m")

require(sf)
require(terra)

GB_counties<-vect("G:\\GB_SDM\\political\\GB_Counties.shp")
##Get predictor parameters
##Currently using this, although WI has 1m resolution that we will substitute in
require(geodata)
elev<-elevation_30s('USA',download=TRUE,path="G:\\GB_SDM\\Elevation\\", mask=TRUE)
plot(elev)

cr.elev<-crop(elev,GB_counties, mask=TRUE)

names(cr.elev)<-"Elevation"
plot(cr.elev)

slope<-terrain(cr.elev,v='slope',unit='degrees',neighbors=8)
names(slope)<-"Slope"
plot(slope)

#############################################
#############################################
##Soil properties
#All soil properties were created from gNATSGO 
#in ArcMap 10.8.1 usinjg the Soil Data Development Toolbox
#in Sept 2024 

#sand percent
sand_MI<-rast("G:\\GB_SDM\\Soil_datasets\\sand_MI_84.tif")
activeCat(sand_MI)<-6
sand_MI
sand_WI<-rast("G:\\GB_SDM\\Soil_datasets\\sand_WI_84.tif")
ext(sand_MI)
sand_WI_re<-resample(sand_WI,sand_MI)
activeCat(sand_MI)<-6
activeCat(sand_WI_re)<-6

WI_test<-catalyze(sand_WI_re,index=6)
sand_WI<-WI_test[[6]]
MI_test<-catalyze(sand_MI,index=6)
sand_MI<-MI_test[[6]]
sand<-merge(sand_WI,sand_MI)
writeRaster(sand,"G:/GB_SDM/Soil_datasets/sand_percent.tif")

sand<-rast("G:\\GB_SDM\\Soil_datasets\\sand_percent.tif")

#soil drainage; merged in ArcMap 10.8
soil_drain<-rast("G:\\GB_SDM\\Soil_datasets\\soil_drainage_MI_WI.tif")

sd_WI<-catalyze(soil_drain)
sd_drain<-sd_WI[[2]]
plot(c(soil_drain,sd_drain))
writeRaster(sd_drain,"G:/GB_SDM/Soil_datasets/drainage.tif")

##water table
water_table_MI<-rast("G:\\GB_SDM\\Soil_datasets\\depth_to_watertable_MI_Apr_Sept2.tif")
water_table_WI<-rast("G:\\GB_SDM\\Soil_datasets\\depth_to_watertable_WI_Apr_Sept.tif")


water_table_MI_84<-resample(water_table_MI,sand_MI)
water_table_WI_re<-resample(water_table_WI,sand_WI)
plot(water_table_WI_re)
activeCat(water_table_MI_84) <- 3
plot(water_table_WI_re)
plot(water_table_MI_84,add=TRUE)
wt_MI<-catalyze(water_table_MI_84)
wt_MI_dep<-wt_MI[[3]]
plot(wt_MI_dep)
wt_WI<-catalyze(water_table_WI_re)
wt_WI_dep<-wt_WI[[1]]
wt<-merge(wt_MI_dep,wt_WI_dep)
plot(wt)
writeRaster(wt,"G:/GB_SDM/Soil_datasets/water_table.tif",overwrite=TRUE)
wt<-rast("G:/GB_SDM/Soil_datasets/water_table.tif")

##flood freq
ff_MI<-rast("G:/GB_SDM/Soil_datasets/flood_freq_MI_Apr_Sept.tif")
ff_WI<-rast("G:/GB_SDM/Soil_datasets/flood_freq_WI_Apr_Sept.tif")

ff_MI_84<-project(ff_MI,sand_MI)
ff_WI_84<-resample(ff_WI,sand_WI)

plot(c(ff_WI_84, ff_WI_c,ff_WI_NA,ff))
ff_MI_c<-catalyze(ff_MI_84)
ff_WI_c<-catalyze(ff_WI_84)
ff_MI_NA<-ff_MI_c	
ff_MI_NA[ff_MI_NA==2147483648]<-NA
plot(ff_MI_NA)
ff_WI_NA<-ff_WI_c	
ff_WI_NA[ff_WI_NA==2147483648]<-NA
plot(ff_WI_NA)
ff<-merge(ff_MI_NA,ff_WI_NA)

##now reclassify to be ordinal; checked with rasters in ArcMap
#1=Frequent
#2=None
#3=Occasional
#4=Rare
v_rec<-matrix(c(.9,1.1,4,
	1.9,2.1,1,
	2.9,3.1,3,
	3.9,4.1,2),ncol=3,byrow=TRUE)
ff_rec<-classify(ff,v_rec)
plot(c(ff_WI_c,ff_rec)))
plot(c(ff_MI_84,ff_WI_84,ff,ff_rec))
writeRaster(ff_rec,"G:/GB_SDM/Soil_datasets/ff_apr_sep.tif",overwrite=TRUE)
ff_rec<-rast("G:/GB_SDM/Soil_datasets/ff_apr_sep.tif")

sd_drain_m<-resample(sd_drain,ff_rec)
plot(c(sand,wt,ff_rec,sd_drain_m))
soil_params<-c(sand,wt,ff_rec,sd_drain_m)
writeRaster(soil_params,"G:/GB_SDM/Soil_datasets_final/soil_parameters.tif")
soil_params<-rast("G:/GB_SDM/Soil_datasets_final/soil_parameters.tif")
plot(soil_params)
soils<-crop(soil_params, GB_counties)
soils<-mask(soils,GB_counties)
plot(soils)
names(soils)<-c("SandPercent","DepthtoWaterTable","FloodFreq","DrainClass")
######################################################
######################################################
##landcover params
#https://www.sciencebase.gov/catalog/item/5d0ce918e4b0941bde51dc97
phrag<-rast("G:\\GB_SDM\\landcover\\GreenBay_mosaic_update.tif")
phrag_pr<-project(phrag,crs(GB_counties))
plot(phrag_pr)
phrag<-classify(phrag_pr,matrix(c(1,17.9,0,17.9,19.9,1,19.9,25,0),ncol=3,byrow=TRUE))
phrag<-classify(phrag,cbind(NA,0))
plot(phrag)
plot(GB_counties)
plot(phrag_pr,add=TRUE)
phrag_m<-crop(phrag,GB_counties,mask=TRUE)
plot(phrag_m)
phrag_re<-resample(phrag_m,predictors[[1]])
names(phrag_re)<-"Phragmites"
plot(phrag_re)


imp<-rast("G:\\GB_SDM\\landcover\\nlcd_2019_impervious_l48_20210604.img")
gb_alb<-project(GB_counties,imp)
imp_cr<-crop(imp,gb_alb,mask=TRUE)
plot(imp_cr)
imp_pr<-project(imp_cr,crs(phrag_m))
names(imp_pr)<-"PercentImpervious"
plot(imp_pr)

can<-rast("G:\\GB_SDM\\landcover\\nlcd_tcc_conus_2021_v2021-4.tif")
gb_nad<-project(GB_counties,can)
can_cr<-crop(can,gb_nad,mask=TRUE)
can_pr<-project(can_cr,crs(phrag_m))
names(can_pr)<-"PercentCanopy"
can_pr<-clamp(can_pr,upper=100,value=FALSE)
plot(can_pr)
##############################################
##############################################
#Climatic predictors
temp_wq<-rast("G:\\GB_SDM\\chelsea\\CHELSA_bio10_1981-2010_V.2.1.tif")
precip_wq<-rast("G:\\GB_SDM\\chelsea\\CHELSA_bio16_1981-2010_V.2.1.tif")

temp_cr<-crop(temp_wq,GB_counties,mask=TRUE)
precip_cr<-crop(precip_wq,GB_counties,mask=TRUE)

temp_re<-resample(temp_cr,can_pr)
precip_re<-resample(precip_cr,can_pr)

names(temp_re)<-"TempWarmQ"
names(precip_re)<-"PrecipWetQ"
#############################################
#############################################
#Water properties
#depth from https://www.ngdc.noaa.gov/mgg/greatlakes/michigan/
depth<-rast("G:\\GB_SDM\\GB_WaterParams\\michigan_lld.geotiff.tar\\michigan_lld\\michigan_lld.tif")
depth_pr<-project(depth,crs(predictors[[1]]))
depth_cr<-crop(depth_pr,predictors[[1]])
depth_re<-resample(depth_cr,predictors[[1]])
plot(depth_re)
depth_m<-mask(depth_re,predictors[[1]],inverse=TRUE)
depth_m<-clamp(depth_m,upper=0,values=FALSE)
plot(depth_m)


dist_coast<-distance(depth_m)
writeRaster(dist_coast,"G:\\GB_SDM\\GB_WaterParams\\dist_coast.tif")
dist_coast<-rast("G:/GB_SDM/GB_WaterParams/dist_coast.tif")
plot(dist_coast)
dist_coast_cr<-crop(dist_coast,predictors[[1]],mask=TRUE)
names(dist_coast_cr)<-"Dist2Coast"
plot(dist_coast_cr)


elev_re<-resample(cr.elev,can_pr)
slope_re<-resample(slope,can_pr)
soils_re<-resample(soils,can_pr)
predictors<-c(elev_re,slope_re,soils_re,imp_pr,can_pr,temp_re,precip_re)
pairs(predictors)
writeRaster(preds,"G:/GB_SDM/preds/predictors2.tif",overwrite=TRUE)
predictors<-rast("D:/GB_SDM/preds/predictors.tif")
names(predictors)
predictors2<-rast("D:/GB_SDM/preds/predictors2.tif")
names(predictors2)
predictors2<-c(predictors,dist_coast_cr,phrag_re)
names(predictors2)
plot(predictors2)
predictors<-c(predictors,dist_coast_cr)
predictors2<-c(predictors2[[1:6]],can_pr,predictors2[[8:11]])
writeRaster(pred_test,"G:/GB_SDM/preds/predictors2.tif",overwrite=TRUE)
names(pred_test)
names(predictors2)
(na.omit(predictors2[[7]]))
predictors<-c(predictors[[1:6]],imp_pr,can_pr,predictors[[9:11]],phrag_re)
predictors<-predictors[[2:11]]
pairs(predictors2)
require(usdm)
predictors2<-c(predictors2[[1:5]],as.numeric(predictors2[[6:7]]),predictors2[[8:11]])

param_vif<-usdm::vif(predictors2)
param_vif
pairs(predictors2)

hfi<-rast("D:\\GB_SDM\\Human_footprint\\hfp2022.tif")
plot(hfi)
hfi_pr<-project(hfi,crs(predictors2[[1]]))
hfi_cr<-crop(hfi_pr,predictors2[[1]],mask=TRUE)
hfi_re<-resample(hfi_cr,predictors2[[1]])
hfi_RE<-crop(hfi_re,predictors2[[1]],mask=TRUE)

plot(hfi_cr)
pred_test<-predictors2
pred_test<-c(predictors2,hfi_RE)
names(pred_test)
usdm::vif(pred_test)
pairs(pred_test)
#All VIF < 3, all corr < 0.7

writeRaster(pred_test,"D:\\GB_SDM\\updated_models\\predictors_hfi.tif")
pred_test<-rast("G:\\GB_SDM\\updated_models\\predictors_hfi.tif")
#########################################################################
#occurrence filtering
require(spThin)



##create a database of all occurrences
all_occ<-test
all_occ_1<-read.csv("G:\\GB_SDM\\updated_models\\waterbird_udpate_2025.csv")
all_occ<-read.csv("G:\\GB_SDM\\updated_models\\waterbird_additions_2026.csv")
all_occ<-test
colnames(all_occ_1)
all_occ_1<-all_occ_1[,-1]
colnames(all_occ)
all_occ<-rbind(all_occ_1,all_occ)
require(sf)
all_sf<-st_as_sf(all_occ,
	coords=c("decimalLongitude","decimalLatitude"))
all_sf<-st_set_crs(all_sf,crs(GB_counties))

rast_sp<-terra::as.polygons(pred_test[[1]])
rast_sp<-st_as_sf(rast_sp)
write_sf(rast_sp,"G:\\GB_SDM\\political\\",layer="study_area",driver="ESRI Shapefile")
rast_sp<-st_as_sf(vect("G:\\GB_SDM\\political\\study_area.shp"))
all_int<-st_filter(all_sf,rast_sp)

unique(all_int$species)
unique(all_int$coordinateUncertaintyInMeters)
all_int$coordinateUncertaintyInMeters[is.na(all_int$coordinateUncertaintyInMeters)]<-0
all_int_red<-all_int[which(all_int$coordinateUncertaintyInMeters<=500),]

require(dismo)
require(spThin)
require(terra)

pred_test<-pred_red_fine
pred_test<-rast("G:\\GB_SDM\\updated_models\\predictors_hfi.tif")
plot(pred_test)
unique(all_int_red$species)
test<-rast(choose.files())
plot(test)
output_dir<-"G:/GB_SDM/updated_models/MaxEnt_output"
dim(all_int_red)

#phragmites layer does not include all of the counties and thus was removed
#as it introduced too many NA locations

##March 7 2025-Phragmites layer has been added back, but with 
## the assumption that where not surveyed, there is no Phragmites.
##inference on phragmites will need to be couched in this process.

i<-1

unique(all_int_red$species)

plot(GB_Counties)

#rerun 1 and 12 
names(pred_test)
i<-1
Sys.setenv(JAVA_HOME="C:\\Users\\nba2\\OneDrive - University of Illinois - Urbana\\Desktop\\oryx\\Java\\jre1.8.0_451")
output_dir<-"G:/GB_SDM/updated_models/MaxEnt_output"

#snowy egret only had 8 occurrences and thus cannot be modeled
#piping plover only has 2 occurrences and thus cannot be modeled
#green-winged teal only has 2 occurrences and thus cannot be modeled
i<-2
#Anser albifrons crashed (i=20)

require(rmaxent)
neg.maxent<-import_maxent("G:\\GB_SDM\\updated_models\\MaxEnt_output\\rc_10repCygnus olor_maxent")
i<-22
for(i in 20:21){#22:length(unique(all_int_red$species))){
	sp_model<-all_int_red[which(all_int_red$species==unique(all_int_red$species)[i]),]
	#bg_points<-st_sample(all_int_red[which(all_int_red$species!=unique(all_int_red$species)[i]),],
	#		size=20000)



	thinned_pts<-thin.algorithm(st_coordinates(sp_model),thin.par=0.5,rep=1)
	neg.maxent <- dismo::maxent(raster::stack(pred_test), p= thinned_pts[[1]],
	#a=data.frame(st_coordinates(bg_points)[,1:2]),
	removeDuplicates=TRUE, 
	args=c("betamultiplier=1", "-J", "-P","replicates=10","threads=4","responsecurves", "writeplotdata"),
	path=paste0(output_dir,"/rc_10rep",unique(all_int_red$species)[i],"_maxent"))

	output<-paste0(output_dir,"/rc_10rep",unique(all_int_red$species)[i],"_maxent")
	full.results <- dismo::predict(neg.maxent, raster::stack(pred_test))
#plot(full.results)
	terra::writeRaster(full.results,
	paste0(output,"/",unique(all_int_red$species)[i],"_maxent.tif"),overwrite=TRUE)
	print(unique(all_int_red$species)[[i]])
}


plot(full.results)
rast_dirs<-list.dirs('G:\\GB_SDM\\updated_models\\MaxEnt_output', recursive=FALSE)
rast_dirs
plot(sdms)
#remove #20, "[34] "D:\\GB_SDM\\updated_models\\MaxEnt_output/rc_10repEgretta thula_maxent""
#remove #17 "[27] "D:\\GB_SDM\\updated_models\\MaxEnt_output/rc_10repCharadrius melodus_maxent"  
rast_dirs<-c(rast_dirs[1:26],rast_dirs[28:33],rast_dirs[21:71])
rast_dirs<-rast_dirs[-34]
rast_dirs<-rast_dirs[-27]
sdms<-rast(paste0(rast_dirs[1],"/",list.files(rast_dirs[1],pattern=".tif")[1]))
dim(sdms)
for(i in 2:length(rast_dirs)){
 sdm_rast<-rast(paste0(rast_dirs[i],"/",list.files(rast_dirs[i],pattern=".tif")[1]))
 sdms<-c(sdms,sdm_rast)
}

##########need to average the rasters together for each species

output_dir<-"G:\\GB_SDM\\updated_models\\averaged_rasters"

i<-1
for(i in seq(1,(dim(sdms))[3],by=10)){
	name<-strsplit(names(sdms[[i]]),"_")
	rast_mean<-app(sdms[[i:(i+9)]],fun="mean")
	names(rast_mean)<-paste0(name[[1]][1],"_mean")
	rast_sd<-app(sdms[[i:c(i+9)]],fun="sd")
	names(rast_sd)<-paste0(name[[1]][1],"_sd")
	writeRaster(rast_mean,paste0(output_dir,"/",names(rast_mean),".tif"),overwrite=TRUE)
	writeRaster(rast_sd,paste0(output_dir,"/",names(rast_sd),".tif"),overwrite=TRUE)
	
}


plot(c(rast_mean,rast_sd))

install.packages('spatialEco')
require(spatialEco)
require(terra)
rast_avg_dir<-"G:\\GB_SDM\\updated_models\\averaged_rasters"
sdms<-rast(paste0(rast_avg_dir,"/",list.files(rast_avg_dir,pattern="mean.tif")[1]))
names(sdms)
i<-2
for(i in 2:length(list.files(rast_avg_dir,pattern="mean.tif"))){
 sdm_rast<-rast(paste0(rast_avg_dir,"/",list.files(rast_avg_dir,pattern="mean.tif")[i]))
 sdms<-c(sdms,sdm_rast)
}

dim(sdms)[3]
warren_matrix_original<-matrix(NA,ncol=dim(sdms)[3],nrow=dim(sdms)[3])
warren_matrix_corrected<-matrix(NA,ncol=dim(sdms)[3],nrow=dim(sdms)[3])

i
for (j in 1:dim(sdms)[3]){
	for(i in 1:dim(sdms)[3]){
		if(i>=j){
			niche_ov<-spatialEco::overlap(sdms[[j]],sdms[[i]]) 
			#warren_matrix_original[j,i]<-niche_ov$original.I
			warren_matrix_corrected[j,i]<-niche_ov$corrected.I
		} else {
			#warren_matrix_original[j,i]<-warren_matrix_original[i,j]
			warren_matrix_corrected[j,i]<-warren_matrix_corrected[i,j]
			}
		}
	}



warren_matrix_corrected<-read.csv(choose.files())
write.csv(warren_matrix_corrected,"G:\\GB_SDM\\updated_models\\Warrens_I_corrected_2026.csv")
head(warren_matrix_corrected)
warren_matrix_corrected<-read.csv("G:\\GB_SDM\\updated_models\\Warrens_I_corrected_2026.csv")
dim(warren_matrix_corrected)
warren_matrix_corrected<-warren_matrix_corrected[,-1]
install.packages('pheatmap')
require(pheatmap)

dim(warren_matrix_corrected)

require(stringr)
colnames(warren_matrix_corrected)<-str_remove(names(sdms),pattern="_mean")
rownames(warren_matrix_corrected)<-str_remove(names(sdms),pattern="_mean")
head(warren_matrix_corrected)

#replace names with species code
ordered_names<-as.data.frame(str_remove(names(sdms),pattern="_mean"),ncol=1)


tr<-read.csv("G:\\GB_SDM\\updated_models\\trait_data_March2026.csv")


sp_names<-str_replace(tr[,1], "[.]", " ")
sp_names_2<-cbind(sp_names,tr[,3])
colnames(sp_names_2)<-c("Sci","Code")
colnames(ordered_names)<-"Sci"
rep_names<-merge(ordered_names,sp_names_2,by="Sci")
head(rep_names)

dev.new()
plot(1,10)
graphics.off()


colnames(warren_matrix_corrected)<-rep_names$Code
rownames(warren_matrix_corrected)<-rep_names$Code

graphics.off()
dev.new()
pheatmap(warren_matrix_corrected, display_numbers = T,
	cluster_rows=T,cluster_cols=T,
	treeheight_row=0,treeheight_col=0,
	cex=.75,
	color = colorRampPalette(c("goldenrod4","goldenrod","gold1","lightskyblue","deepskyblue","deepskyblue4"))(50))
,
	filename="G:\\GB_SDM\\updated_models\\updated_figs\\warrens_I_sp_code.png", height = 6, width = 6,units="in",res=600)

dev.off()
min(warren_matrix_corrected)
max(lower.tri(warren_matrix_corrected,diag=FALSE))
hist(Temperature, col="gold")
dev.off()
require(ggplot2)
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\warrens_I.png", units="in", width=6, height=6, dpi=900)#, compression = 'lzw')
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\warrens_I.tif", units="in", width=6, height=6, dpi=900, compression = 'lzw')


#rerun Aythya affinis and Pelecanus erythrorhynchos as they did not 
#create predictive maps. 3/8/2025

warnings()
install.packages('export')
require(export)

graph2ppt(file="G:\\GB_SDM\\updated_models\\SDMs_pheatmap_2026.pptx", width=6, height=5) 

sdm_mean<-mean(sdms)
dev.new()
plot(sdm_mean)
sdm_sd<-stdev(sdms)
plot(c(sdm_mean,sdm_sd))
graph2ppt(file="G:\\GB_SDM\\updated_models\\SDM_Mean_SD_2026.pptx", width=6, height=5) 

#####################
require(sf)
require(terra)
GB_counties<-vect("G:\\GB_SDM\\political\\GB_Counties.shp")
pred_test<-rast("G:\\GB_SDM\\updated_models\\predictors_hfi.tif")



test<-st_sample(st_as_sf(GB_counties),size=1000,method="uniform")
plot(test)
plot(GB_counties,add=TRUE)

require(spThin)

#initially thinned by 10km and buffered to 5km size. After review,
#we are reducing the "site" size to be 2km. Thus we will thin so points are
#2km apart and 500m apart
thinned_comm_points<-thin.algorithm(st_coordinates(test),thin.par=0.5,rep=1)
plot(thinned_comm_points[[1]])
class(thinned_comm_points[[1]])
write.csv(thinned_comm_points,"G:/GB_SDM/political/UniformPoints_500mthin_june_2026.csv")
write.csv(all_occ,"G:/GB_SDM/political/all_occs_jan2026.csv")

thinned_comm_pts<-read.csv("G:/GB_SDM/political/UniformPoints_500mthin_june_2026.csv")

pt_df<-data.frame(thinned_comm_pts)
pt_df
pts<-st_as_sf(pt_df, coords = c("Longitude","Latitude"))
plot(pts)

pts_buffer<-read_sf("G:\\GB_SDM\\political\\uniform_pts_500m.shp")


##create a database of all occurrences
all_occ_1<-read.csv("G:\\GB_SDM\\updated_models\\waterbird_udpate_2025.csv")
all_occ<-read.csv("G:\\GB_SDM\\updated_models\\waterbird_additions_2026.csv")
colnames(all_occ_1)
all_occ_1<-all_occ_1[,-1]
colnames(all_occ)
all_occ<-rbind(all_occ_1,all_occ)
require(sf)
all_sf<-st_as_sf(all_occ,
	coords=c("decimalLongitude","decimalLatitude"))
all_sf<-st_set_crs(all_sf,crs(GB_counties))

rast_sp<-terra::as.polygons(pred_test[[1]])
rast_sp<-st_as_sf(rast_sp)
write_sf(rast_sp,"G:\\GB_SDM\\political\\",layer="study_area",driver="ESRI Shapefile")
rast_sp<-st_as_sf(vect("G:\\GB_SDM\\political\\study_area.shp"))
all_int<-st_filter(all_sf,rast_sp)

unique(all_int$species)
unique(all_int$coordinateUncertaintyInMeters)
all_int$coordinateUncertaintyInMeters[is.na(all_int$coordinateUncertaintyInMeters)]<-0
all_int_red<-all_int[which(all_int$coordinateUncertaintyInMeters<=500),]
unique(all_int_red$species)

plot(pts_buffer[1])
plot(all_int_red,add=TRUE)
class(pts_buffer)
st_crs(all_sf)
st_crs(pts_buffer)<-st_crs(all_sf)
st_crs(pts)<-st_crs(all_sf)

install.packages('spatialEco')
require(spatialEco)
occ_int<-st_filter(all_sf,pts_buffer)

plot(occ_int[10])
unique(occ_int$species)
dim(occ_int)
unique(occ_int$coordinateUncertaintyInMeters)
occ_int$coordinateUncertaintyInMeters[is.na(occ_int$coordinateUncertaintyInMeters)]<-0
occ_int_red<-occ_int[which(occ_int$coordinateUncertaintyInMeters<=500),]
dim(occ_int_red)
colnames(occ_int_red)
plot(occ_int[10])
unique(occ_int_red$species)
plot(pts_buffer,add=TRUE)

joined<-st_join(pts_buffer,occ_int_red)
plot(joined)
names(joined)
joined_red<-st_join(pts_buffer[1],occ_int_red)
plot(joined_red[12])
names(joined_red)[1]<-"OBJECTID"

dim(joined_red)
names(joined_red)

library(dplyr)

#find sum of sales by region
pivot_table<-joined_red %>%
  group_by(OBJECTID,species) %>% 
	count(species)
boxplot(data.frame(pivot_table[3])[,1])
quantile(data.frame(pivot_table[3])[,1])

head(pivot_table[1])
pivottab<-sf::st_drop_geometry(pivot_table)
head(pivottab)
#write out the community data and assign site and species columns in excel
write.table(pivottab , file = "G:\\GB_SDM\\community_data_500m_2026.csv")
comm_data<-read.csv("G:\\GB_SDM\\community_data_2km_hmsc_2026.csv")
comm_data<-read.csv("G:\\GB_SDM\\community_data_500m_hmsc_2026.csv")

class(comm_data)
comm_data[is.na(comm_data)]<-0
names(comm_data)[1]<-"ID"
head(comm_data)
#########################################
##JSDM predictors###
cr_area<-crop(pred_test,joined_red,mask=TRUE)
plot(cr_area)
writeRaster(cr_area,"G:\\GB_SDM\\political\\jsdm_preds_2km_2026.tif")
writeRaster(cr_area,"G:\\GB_SDM\\political\\jsdm_preds_500m_2026.tif")


join_pred<-extract(cr_area,pts_buffer)
head(join_pred)
length(unique(join_pred$ID))
class(join_pred[,13])
##canopy and impervious were being read in as factors, so ensure they are numeric values
join_pred[,7]<-as.numeric(join_pred[,7])
join_pred[,8]<-as.numeric(join_pred[,8])
join_pred[,8]<-as.numeric(join_pred[,8])


str(join_pred)
backup<-join_pred
colnames(join_pred)
dim(join_pred)
class(join_pred[,12])
i<-1
jsdm_params<-data.frame()
str(col_val)
head(col_val)
for(i in 1: nrow(pts_buffer)){
	col_val<-join_pred[which(join_pred$ID==i),]
	col_val$Slope[is.na(col_val$Slope)] <- median(col_val$Slope, na.rm=TRUE) 
	col_val$SandPercent[is.na(col_val$SandPercent)] <- median(col_val$SandPercent, na.rm=TRUE) 
	col_val$DepthtoWaterTable[is.na(col_val$DepthtoWaterTable)] <- median(col_val$DepthtoWaterTable, na.rm=TRUE) 
	col_val$FloodFreq[is.na(col_val$FloodFreq)] <- median(col_val$FloodFreq, na.rm=TRUE) 
	col_val$DrainClass[is.na(col_val$DrainClass)] <- median(col_val$DrainClass, na.rm=TRUE) 
	col_val$PercentImpervious[is.na(col_val$PercentImpervious)] <- median(col_val$PercentImpervious, na.rm=TRUE) 
	col_val$PercentCanopy[is.na(col_val$PercentCanopy)] <- median(col_val$PercentCanopy, na.rm=TRUE) 
	col_val$TempWarmQ[is.na(col_val$TempWarmQ)] <- median(col_val$TempWarmQ, na.rm=TRUE) 
	col_val$PrecipWetQ[is.na(col_val$PrecipWetQ)] <- median(col_val$PrecipWetQ, na.rm=TRUE) 
	col_val$Dist2Coast[is.na(col_val$Dist2Coast)] <- median(col_val$Dist2Coast, na.rm=TRUE) 
	col_val$Phragmites[is.na(col_val$Phragmites)] <- median(as.numeric(col_val$Phragmites), na.rm=TRUE) 
	jsdm_params<-rbind(jsdm_params,col_val)
	
}

jsdm_site_params<-jsdm_params %>%
	group_by(ID) %>% 
	summarize_all(mean)

write.csv(jsdm_site_params , file = "G:\\GB_SDM\\env_data_2km_2026.csv")
env_data<-read.csv("G:\\GB_SDM\\env_data_2km_2026.csv")
env_data<-env_data[,-1]
head(env_data)
env_data[is.na(env_data)]
head(comm_data)
data_set<-merge(env_data,comm_data,by="ID")

head(data_set)

write.csv(data_set, file = "G:\\GB_SDM\\Hmsc_Data_2km_2026.csv")


#some sites have no detections. Given that we are using 
#presence only data, we should restrict sites to at least 1
#detection. Running HMSC models with both unverified 0s
#and just sites with at least 1 observation will assess sensitivity.


