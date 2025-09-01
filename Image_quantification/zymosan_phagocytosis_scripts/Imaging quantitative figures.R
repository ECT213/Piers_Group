batch_means <-  flourescent_proportions_2 %>%
  group_by(genotype,batch) %>%
  summarise(batch_percentage_means = mean(normalized_percentage))
boxplot(batch_percentage_means ~ group, data = batch_means, xaxt = "n", boxwex=0.5, xlab = expression(bold("Genotype")), ylab = expression(bold("Percentage area red")))
axis(side = 1, at = c(1, 2, 3), labels = c("Control", "ABI3 KO", "ABI3 S209F"))

control_mean <- batch_means %>%
  filter(group == "C") %>%
  summarise(control_batch_means = mean (batch_percentage_means))

View(batch_means) 
View(control_mean) 

#First batch flourescent proportions normalized 
first_batch_normal <- flourescent_proportions_2 %>%
  filter(batch == 1)
View(first_batch_normal)

#descriptive stats for creating plot - first batch
library(dplyr)
std <- function(x) sd(x)/sqrt(length(x)) #standard error function
descriptive_stats_1 <- first_batch_normal%>%
  group_by(genotype)%>%
  summarise(avg_prop = mean(percentage_mean_minusNC), 
            stand_dev = sd(percentage_mean_minusNC), 
            stand_error = std(percentage_mean_minusNC))

View(descriptive_stats_1)

#Box plot - first batch
library(ggplot2)

p<- ggplot(descriptive_stats_1, aes(x=genotype, y=avg_prop)) + geom_col(width = 0.1) +
  geom_bar(stat="identity", 
           position=position_dodge()) + 
  geom_errorbar(aes(ymin=avg_prop-stand_error, ymax=avg_prop+stand_error), width=.2,
                position=position_dodge(.9))
print(p)


p + labs(x = "Genotype", y = "% of ")+
  theme_classic() + scale_x_discrete(labels=c('Control', 'ABI3 KO', 'ABI3 S209F')) +
  guides(fill = guide_legend(title = "Cell line")) + scale_fill_hue(labels = c("Control", "ABI3 KO", "ABI3 S209F")) + ylim(0, 20) + theme(axis.title.y = element_text(vjust = 1.7))


#ANOVA - first batch 
area.aov <- aov(percentage_mean_minusNC ~ genotype, data=first_batch_normal)
# Summary of the analysis
summary(area.aov)
TukeyHSD(area.aov)
View(area.aov)

#descriptive stats for creating plot
library(dplyr)
std <- function(x) sd(x)/sqrt(length(x)) #standard error function
descriptive_stats <- flourescent_proportions_2%>%
  group_by(genotype)%>%
  summarise(avg_prop = mean(normalized_percentage), 
            stand_dev = sd(normalized_percentage), 
            stand_error = std(normalized_percentage))

View(descriptive_stats)
library(ggplot2)

p<- ggplot(descriptive_stats, aes(x=genotype, y=avg_prop)) + geom_col(width = 0.1) +
  geom_bar(stat="identity", 
           position=position_dodge()) + 
  geom_errorbar(aes(ymin=avg_prop-stand_error, ymax=avg_prop+stand_error), width=.2,
                position=position_dodge(.9))
print(p)


p + labs(x = "Genotype", y = "% of ")+
  theme_classic() + scale_x_discrete(labels=c('Control', 'ABI3 KO', 'ABI3 S209F')) +
  guides(fill = guide_legend(title = "Cell line")) + scale_fill_hue(labels = c("Control", "ABI3 KO", "ABI3 S209F")) + ylim(0, 1.5) + theme(axis.title.y = element_text(vjust = 1.7))

ggsave(
  plot = p,
  filename = "tr_tst2.png",
  bg = "transparent"
)


area.aov <- aov(normalized_percentage ~ genotype, data=flourescent_proportions_2)
# Summary of the analysis
summary(area.aov)
TukeyHSD(area.aov)
View(area.aov)



shapiro.test(flourescent_proportions_2$normalized_percentage)

install.packages("DescTools")
library(DescTools)
DunnettTest(x=flourescent_proportions_2$normalized_percentage, g = flourescent_proportions_2$genotype, control = "C")