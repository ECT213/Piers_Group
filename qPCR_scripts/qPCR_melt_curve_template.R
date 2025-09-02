#Call packages
library(dplyr)
library(stringr)
library(ggplot2) 

#First create new excel file containing the raw qPCR melt curve data with the column headings "Well", "Well Position", "Reading", "Fluorescence", and "Derivative"
#Rename "Well Position" --> "Position 
#Add columns for target ("Target") and sample ("Sample") and add this information based on plate layout


##Plots melt curve of specific sample and target primer##
#Can be helpful to check for multiple peaks to troubleshoot for contamination or primer dimer in a sample or replicate
tidy_data_ABI3_Cao<- X20250813_Meltcurve_ET%>%
  filter(Target == "ABI3_Cao")%>%
  filter(Sample == "iMicro c1")
View(tidy_data_ABI3_Cao)

#Makes plot of melt curve
ggplot(tidy_data_ABI3_Cao, aes(x=Temperature, y=Derivative, group=Position))+
  geom_line()+ 
  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  ylim(0, 4)+ 
  ggtitle("ABI3_Cao")+
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))




##Melt curve plots for primer efficiency assays##
TREM2_ex1_1in5 <- X20250901_Melt_curve_ET%>%
  filter(primer == "ABI3_ex1")%>% #filter by primer
  filter(Sample == "iMicro c1 1:5") #filter by sample and dilution
  
#Creates plot of the melt curve for this primer and sample
ggplot(tidy_data_ABI3_ex1_1in5, aes(x=Temperature, y=Derivative, group=Position))+
  geom_line()+
  theme_classic()+ 
  scale_x_continuous(expand = c(0,0))+ 
  ylim(0, 10)+ 
  ggtitle("ABI3_ex1 1:5")+ 
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))




##Plot melt curves of same primer but different genotypes same plot for comparison by genotype##
tidy_data_ABI3_Cao <- X20250813_Meltcurve_ET%>%
  #Excluding samples with issues or outlier Ct values
  filter(Sample != "c1")%>%
  filter(Sample != "s2")%>%
  filter(Sample != "k2")%>%
  #Adds column for Genotype that contains first letter of sample name to allow for filtering by genotype
  #(In this case sample names start with C, K, or S which corresponds to genotype)
  mutate(Genotype = substr(Sample, 1, 1))%>%
  #produces column of mean_derivative and mean_temperature to combine technical replicates by genotype
  group_by(Genotype, Reading)%>%
  filter(Target == "ABI3_Cao")%>%
  summarise(mean_derivative = mean(Derivative),
            mean_temperature = mean(Temperature))

#Opens tab showing new data frame
View(tidy_data_ABI3_Cao)

#Creates plot of showing mean_derivative values by temperature for each genotype
ggplot(tidy_data_1in5, aes(x=mean_temperature, y=mean_derivative, group=Target)) +geom_line(aes(color=Target), size = 2) +
  theme_classic()+ 
  scale_x_continuous(expand = c(0,0))+
  ylim(0, 3)+
  ggtitle("ABI3 primers")+
  xlab("Temperature")+
  ylab("Derivative")+
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2), axis.text=element_text(size = 16), axis.title=element_text(size = 20), legend.text=element_text(size=14), axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1), legend.title=element_text(size=16),  plot.margin = margin(20, 10, 10, 20), legend.position="top")+
  scale_color_manual(values=c("#BD33A4", "#66b2b2", '#FFDB58'))









##Messing around with ways to annotate raw data
annotated_melt_curve_dateofqPCR<-annotated_melt_curve_dateofqPCR %>%  
  (primer,"13|14|15|25|26|27|37|38|39|49|50|51|61|62|63|73|74|75", "ABI3_ex1")
  
  
"22|23|24|34|35|36|46|47|48|58|59|60|70|71|72|82|83|84"
"TREM2_202"

View(annotated_melt_curve_dateofqPCR)

