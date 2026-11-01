Whispers<-read.csv("G:\\GB_SDM\\Whispers\\disease_3Mar2024.csv")
head(Whispers)

GB_WI<-Whispers[which(Whispers$state_or_equivalent=="Wisconsin"),]
unique(GB_WI$county_or_equivalent)
GB_MI<-Whispers[which(Whispers$state_or_equivalent=="Michigan"),]

w_do<-GB_WI[which(GB_WI$county_or_equivalent=="Door County"),]
w_ke<-GB_WI[which(GB_WI$county_or_equivalent=="Kewaunee County"),]
w_br<-GB_WI[which(GB_WI$county_or_equivalent=="Brown County"),]
w_oc<-GB_WI[which(GB_WI$county_or_equivalent=="Oconto County"),]
w_ma<-GB_WI[which(GB_WI$county_or_equivalent=="Marinette County"),]

m_mi<-GB_MI[which(GB_MI$county_or_equivalent=="Menominee County"),]
m_de<-GB_MI[which(GB_MI$county_or_equivalent=="Delta County"),]

study_co<-rbind(w_do,w_ke,w_br,w_oc,w_ma,m_mi,m_de)
study_co<-rbind(GB_MI,GB_WI)
study_co<-Whispers
unique(study_co$species_diagnosis)
diseases<-c("Highly Pathogenic Avian Influenza (AI virus H5N1)","Highly Pathogenic Avian Influenza (AI virus H5N2)",
	"Avian Cholera","Salmonellosis","Botulism Type E","Botulism Type C","Newcastle Disease")
	length(diseases)

study_co<-study_co[which(study_co$species_diagnosis==diseases[1]|
	study_co$species_diagnosis==diseases[2]|
	study_co$species_diagnosis==diseases[3]|
	study_co$species_diagnosis==diseases[4]|
	study_co$species_diagnosis==diseases[5]|
	study_co$species_diagnosis==diseases[6]|
	study_co$species_diagnosis==diseases[7]),]
dim(study_co)
require(dplyr)
install.packages('ggplot2')
require(ggplot2)

colnames(study_co)
class(study_co$known_sick)
study_co$known_sick<-as.numeric(study_co$known_sick)
study_co$known_sick
study_co$known_dead
study_co$known_dead<-as.numeric(study_co$known_dead)
class(study_co$known_dead)
study_co$known_dead[is.na(study_co$known_dead)]<-0
study_co$known_sick[is.na(study_co$known_sick)]<-0
study_co$estimated_dead<-as.numeric(study_co$estimated_dead)
study_co$estimated_sick<-as.numeric(study_co$estimated_sick)
study_co$estimated_dead[is.na(study_co$estimated_dead)]<-0
study_co$estimated_sick[is.na(study_co$estimated_sick)]<-0
#waterfowl<-list("Spotted Sandpiper|Scaup|Killdeer|Tern|Loon|Gull|Night Heron|Pelican|Cormorant|Piping Plover")

#waterfowl_no_co<-list("Spotted Sandpiper|Scaup|Killdeer|Tern|Loon|Gull|Night Heron|Pelican|Piping Plover")

ave_disease <- study_co %>%
  filter(species != "Fisher" & species != "River Otter" & species != "Painted Turtle")
#ave_disease<-study_co%>% filter(grepl(waterfowl,species))
ave_botulism<-ave_disease%>%filter(grepl("Botulism",species_diagnosis))
head(ave_disease)
dim(ave_disease)
unique(ave_disease$species)
names(study_co)
dev.new()
require(ggplot2)
  ggplot(data=ave_disease, aes(x = species, 
	fill=species_diagnosis)) +
	geom_col(aes(y=known_sick))+
	theme_minimal()+
	theme(axis.text.x=element_text(angle=90))

length(unique(ave_disease$event_id))
names(ave_disease)
min(ave_disease$event_start_date)
max(ave_disease$event_start_date)
require(ggplot2)
  ggplot(data=ave_disease, aes(x = species, 
	fill=species_diagnosis)) +
	geom_col(aes(y=known_dead))+
	theme_minimal()+
	#coord_cartesian(ylim = c(0, 100))+
	theme(axis.text.x=element_text(angle=90))

  ggplot(data=ave_disease, aes(x = species, 
	fill=species_diagnosis)) +
	geom_col(aes(y=estimated_sick))+
	theme_minimal()+
	theme(axis.text.x=element_text(angle=90))

install.packages('ggalluvial')
require(ggalluvial)
install.packages('ggrepel')
require(ggrepel)
install.packages('ggfittext
unique(ave_disease$species_diagnosis)
ggplot(ave_disease,
      aes(y = known_dead, axis1 =species, axis2 = species_diagnosis)) +
	geom_alluvium(aes(fill = species_diagnosis,group=species), width = 1/10)+
	geom_stratum(width = 0.1) +
	scale_x_discrete(limits = c("Species", "Diagnosis"), expand = c(.05, .05)) +
	ggrepel::geom_text_repel(aes(label=after_stat(stratum)),
		stat = "stratum", size = 4, direction = "y", nudge_x=-.5)+
	theme_classic()+
	theme(axis.text.y=element_blank(),
      axis.ticks.y=element_blank())+
	theme(legend.position="none")

install.packages('export')
require(export)
graph2ppt(file="G:\\GB_SDM\\MaxEnt_output\\WI_MI_knownsick_all_allsp.pptx", width=6, height=5) 
graph2ppt(file="G:\\GB_SDM\\MaxEnt_output\\WI_MI_knowndead_all_allsp.pptx", width=6, height=5) 
graph2ppt(file="D:\\GB_SDM\\MaxEnt_output\\WI_MI_known_dead_alluvial.pptx", width=8, height=10) 

