##Stacked SDM output
rm(list=ls())
SSDM<-read.csv("G:\\GB_SDM\\updated_models\\Stacked_SDM_summary.csv")

head(SSDM)

require(ggplot2)
require(reshape)
require(stringr)
require(cowplot)


SSDM_input <- melt(SSDM[,5:7]) 
head(SSDM_input)

SDM_pts<-
	ggplot(na.omit(SSDM_input), aes(x=variable,
		y=value,group=variable,fill=variable)) + 
		geom_boxplot(outlier.colour="red", outlier.shape=8,
      	     outlier.size=2) +
		scale_fill_manual(breaks = SSDM_input$variable,
                    values = c("deepskyblue4","goldenrod")) +
		theme_classic() +
		xlab("Occurrence Record") + ylab("frequency")+theme(legend.position="none")


colnames(SSDM)
dim(SSDM)
SSDM_AUC<-cbind(SSDM[,5],SSDM[,9:10])
colnames(SSDM_AUC)<-c("Code","AUC","SD")
dim(SSDM_AUC)
SDM_AUC<-
	ggplot(na.omit(SSDM_AUC), aes(x=Code,y=AUC,color=AUC)) + 
	geom_errorbar(aes(ymin=AUC-SD, ymax=AUC+SD), width=0.2) +
	 geom_point(size=2)+
	theme_classic()+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
	ylim(0.5,1.1)+
	xlab("Species")+
	geom_hline(aes(yintercept=1),linetype="dashed")+
	scale_color_gradient("AUC",low="goldenrod",high="deepskyblue4")

names(SSDM)
SDM_param<-cbind(SSDM[,5],SSDM[,11:22])
dim(SDM_param)
colnames(SDM_param)[1]<-"Code"
param_melt<-melt(SDM_param)
head(param_melt)
param_plot<-
	ggplot(param_melt) + geom_tile(aes(x = variable, y = Code, fill = value)) + 
	scale_fill_gradient("Percent Contribution", low = "lemonchiffon", high = "deepskyblue3") + theme_classic() + 
	theme(legend.position = "bottom") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
	theme(axis.text.y = element_text(vjus=0.5)) +
	xlab("Parameter") + ylab("Species") +
	theme(axis.text.y = element_text(size = 7))+
	theme(axis.text.x = element_text(size = 12))+
	theme(plot.margin = margin(1, 4, 1, 1, "lines"))
dev.new()


plot_grid(SDM_pts + labs(tag = "a."),SDM_AUC + labs(tag = "b."),align = "h", axis = "tb",
  nrow = 1, rel_widths = c(1, 4))
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\AUC_code.png", units="in", width=11, height=7, dpi=600)#, compression = 'lzw')
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\AUC_code.tif", units="in", width=11, height=7, dpi=600, compression = 'lzw')

param_plot
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\Param_code.png", units="in", width=8, height=9.5, dpi=600)#, compression = 'lzw')
ggsave("G:\\GB_SDM\\updated_models\\updated_figs\\Param_code.tif", units="in", width=8, height=9.5, dpi=600, compression = 'lzw')


