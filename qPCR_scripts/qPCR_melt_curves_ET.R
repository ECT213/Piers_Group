library(dplyr)
library(stringr)
library(ggplot2) 

#fix file names to include dates, etc. 
#Better way to annotate excel sheet for layout and date? 
Layout_melt_curve_20250801 <- X20250801_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C1|C2|C3", "B2M 1in5"))%>%
  mutate(date = (20250801))


#13/03/2025 
#GAPDH
tidy_data_GAPDH2_neat <- X20250313_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"A10|A12", "GAPDH2 neat"))%>%
  filter(primer == "GAPDH2 neat")

#A11 is flat - very high Ct- excluded
ggplot(tidy_data_GAPDH2_neat, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("GAPDH neat sample 13.03.2025")

#GAPDH NC
tidy_data_Bactin_1in5 <- X20250313_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"A4|A5|A6", "B-actin 1in5"))%>%
  filter(primer == "B-actin 1in5")

ggplot(tidy_data_Bactin_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("B-actin 1:5 iPSC sample 13.03.2025")

#B-actin 1:5
tidy_data_Bactin_1in25 <- X20250313_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C4|C5|C6", "B-actin 1in25"))%>%
  filter(primer == "B-actin 1in25")

ggplot(tidy_data_Bactin_1in25, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("B-actin 1:25 iPSC sample 13.03.2025")

#B-actin 1:125
tidy_data_Bactin_1in125 <- X20250313_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"D4|D5|D6", "B-actin 1:125"))%>%
  filter(primer == "B-actin 1:125")

ggplot(tidy_data_Bactin_NC, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("B-actin 1:125 iPSC sample 13.03.2025")






#03/03/2025 dilution factor 5 B2M and OCT4
#B2M 1:625
tidy_data_B2M_625 <- X20250303_Meltcurve_ET%>%
  mutate(primer = str_replace(Position,"F1|F2|F3", "B2M 625"))%>%
  filter(primer == "B2M 625")

View(tidy_data_B2M_625)

ggplot(tidy_data_B2M_625, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 

#B2M neat
tidy_data_B2M_neat <- X20250303_Meltcurve_ET%>%
  mutate(primer = str_replace(Position,"A1|A2|A3", "B2M neat"))%>%
  filter(primer == "B2M neat")

ggplot(tidy_data_B2M_neat, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 

tidy_data_OCT4_neat <- X20250303_Meltcurve_ET%>%
  mutate(primer = str_replace(Position,"A5|A6", "OCT4 neat"))%>%
  filter(primer == "OCT4 neat")

View(tidy_data_OCT4_neat)

ggplot(tidy_data_OCT4_neat, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 

tidy_data_OCT4_625 <- X20250303_Meltcurve_ET%>%
  mutate(primer = str_replace(Position,"F4|F5|F6", "OCT4 625"))%>%
  filter(primer == "OCT4 625")

View(tidy_data_OCT4_625)

ggplot(tidy_data_OCT4_625, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 






#19/02/2025 dilution factor 5, B2M 0.16ul

tidy_data_B2M_5 <- X20250219_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"A7|A8", "B2M 1:5"))%>%
  filter(primer == "B2M 1:5")

View(tidy_data_B2M_5)


ggplot(tidy_data_B2M_5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 


#lowest concentration
tidy_data_B2M_15625 <- X20250219_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"F7|F8", "B2M 1:15625"))%>%
  filter(primer == "B2M 1:15625")

View(tidy_data_B2M_15625)
lower_primer <- tidy_data_B2M_15625 %>%
  bind_rows(tidy_data_B2M_5)

View(lower_primer)

ggplot(lower_primer, aes(x=Temperature, y=Derivative, group=Position, color=primer)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2)


#19/02/2025 dilution factor 5, B2M 0.33ul
tidy_data_B2M_5 <- X20250219_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"A4|A5|A6", "B2M 1:5"))%>%
  filter(primer == "B2M 1:5")

View(tidy_data_B2M_5)

ggplot(tidy_data_B2M_5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 

tidy_data_B2M_625_33 <- X20250219_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"E4|E5", "B2M 1:625"))%>%
  filter(primer == "B2M 1:625")

View(tidy_data_B2M_625_33)

ggplot(tidy_data_B2M_625_33, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 


medium_primer <- tidy_data_B2M_15625 %>%
  bind_rows(tidy_data_B2M_5)

ggplot(medium_primer, aes(x=Temperature, y=Derivative, group=Position, color=primer)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2)


tidy_data_GAPDH_NC <- X20250219_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"G7|G8|G9", "GAPDH_NC"))%>%
  filter(primer == "GAPDH_NC")

ggplot(tidy_data_GAPDH_NC, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 







#65C melt curves 
tidy_data_B2M_10 <- Melt_curve_tests%>%
  mutate(primer = str_replace(Position,"A4|A5|A6", "B2M 1:10"))%>%
  filter(primer == "B2M 1:10")

View(tidy_data_B2M_10)


ggplot(tidy_data_B2M_10, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2) 


#B2M 1:1000 melt curve 65C
tidy_data_B2M_1000 <- X20250217_Melt_Curve_ET%>%
  mutate(primer = str_replace(Position,"C1|C2|C3", "B2M 1:1000"))%>%
  filter(primer == "B2M 1:1000")

View(tidy_data_B2M_1000)

ggplot(tidy_data_B2M_1000, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 5)

tidy_data_B2M_10 <- X20250217_Melt_Curve_ET%>%
  mutate(primer = str_replace(Position,"A1|A2|A3", "B2M 1:10"))%>%
  filter(primer == "B2M 1:10")

View(tidy_data_B2M_10)

ggplot(tidy_data_B2M_10, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 2)




###01/08/2025###ABI3 and B2M####

#B2M 1:5
tidy_data_B2M_1in5 <- X20250801_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C1|C2|C3", "B2M 1:5"))%>%
  filter(primer == "B2M 1:5")

ggplot(tidy_data_B2M_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 4) + ggtitle("B2M 1:5") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))

#ABI3_TP 1:5 
tidy_data_ABI3_TP_1in5 <- X20250801_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C4|C5|C6", "ABI3_TP 1:5"))%>%
  filter(primer == "ABI3_TP 1:5")

ggplot(tidy_data_ABI3_TP_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 4) + ggtitle("ABI3_TP 1:5") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))


#ABI3_MH 1:5 
tidy_data_ABI3_MH_1in5 <- X20250801_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C7|C8|C9", "ABI3_MH 1:5"))%>%
  filter(primer == "ABI3_MH 1:5")

ggplot(tidy_data_ABI3_MH_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 4) + ggtitle("ABI3_MH 1:5") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))

#ABI3_Cao 1:5 
tidy_data_ABI3_Cao_1in5 <- X20250801_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C10|C11|C12", "ABI3_Cao 1:5"))%>%
  filter(primer == "ABI3_Cao 1:5")

ggplot(tidy_data_ABI3_Cao_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 4) + ggtitle("ABI3_Cao 1:5") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))

  
#plot melt curves on same plot for comparison 
tidy_data_1in5 <- X20250801_Melt_curve_ET %>%
  mutate(primer = str_replace(Position,"C4|C5|C6|C7|C8|C9|C10|C11|C12", "1:5"))%>%
  filter(primer == "1:5")%>%
  group_by(Target, Reading)%>%
  summarise(mean_derivative = mean(Derivative),
            mean_temperature = mean(Temperature))
View(tidy_data_1in5)

ggplot(tidy_data_1in5, aes(x=mean_temperature, y=mean_derivative, group=Target)) +geom_line(aes(color=Target), size = 2) +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("ABI3 primers") + xlab("Temperature") + ylab("Derivative") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2), axis.text=element_text(size = 16), axis.title=element_text(size = 20), legend.text=element_text(size=14), axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1), legend.title=element_text(size=16),  plot.margin = margin(20, 10, 10, 20), legend.position="top") + scale_color_manual(values=c("#BD33A4", "#66b2b2", '#FFDB58'))



### ABI3_Cao 1:5 13/08/2025 ###
tidy_data_ABI3_Cao<- X20250813_Meltcurve_ET%>%
  mutate(Genotype = substr(Sample, 1, 1))%>%
  filter(Target == "ABI3_Cao")%>%
  filter(Genotype == "c")
View(tidy_data_ABI3_Cao)

ggplot(tidy_data_ABI3_Cao, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 4) + ggtitle("ABI3_Cao 1:5") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))


#plot melt curves on same plot for comparison 
tidy_data_ABI3_Cao <- X20250813_Meltcurve_ET%>%
  filter(Sample != "c1")%>%
  filter(Sample != "s2")%>%
  filter(Sample != "k2")%>%
  mutate(Genotype = substr(Sample, 1, 1))%>%
  group_by(Genotype, Reading)%>%
  filter(Target == "ABI3_Cao")%>%
  summarise(mean_derivative = mean(Derivative),
            mean_temperature = mean(Temperature))
View(tidy_data_ABI3_Cao)

ggplot(tidy_data_ABI3_Cao, aes(x=mean_temperature, y=mean_derivative, group=Genotype)) +geom_line(aes(color=Genotype), size = 2) +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 3) + ggtitle("ABI3_Cao") + xlab("Temperature") + ylab("Derivative") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2), axis.text=element_text(size = 16), axis.title=element_text(size = 20), legend.text=element_text(size=14))




## 01/09/2025 ABI3_EX1, TREM2_202, TREM2_201, TREM2_EX1 primer efficiency

#ABI3_MH 1:5 
tidy_data_ABI3_ex1_1in25 <- X20250901_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C1|C2|C3", "ABI3_ex1 1:25"))%>%
  filter(primer == "ABI3_ex1 1:25")

  ggplot(tidy_data_ABI3_ex1_1in25, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 10) + ggtitle("ABI3_ex1 1:25") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))

##All TREM2 primers at 1:5 

tidy_data_TREM2_ex1_1in5 <- X20250901_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"B4|B5|B6", "ABI3_ex1 1:25"))%>%
  filter(primer == "ABI3_ex1 1:25")

ggplot(tidy_data_ABI3_ex1_1in5, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 10) + ggtitle("ABI3_ex1 1:25") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))

tidy_data_TREM2_201_1in25 <- X20250901_Melt_curve_ET%>%
  mutate(primer = str_replace(Position,"C7|C8|C9", "ABI3_ex1 1:25"))%>%
  filter(primer == "ABI3_ex1 1:25")

ggplot(tidy_data_TREM2_201_1in25, aes(x=Temperature, y=Derivative, group=Position)) +geom_line() +
  theme_classic() + scale_x_continuous(expand = c(0,0)) + ylim(0, 10) + ggtitle("ABI3_ex1 1:25") + theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5, vjust = 1.2))
