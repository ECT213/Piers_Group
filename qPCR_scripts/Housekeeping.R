
library(dplyr)
library(ggplot2)

View(X20250203_ET_CollectedqPCR)

#Organizing data

tidy_data <- X20250203_ET_CollectedqPCR %>%
  na.omit() %>%
  mutate(run = as.numeric(run)) %>% #changes run column into numeric if haven't loaded as numeric
  filter(sample.name !='NC') %>% #removes negative control
  filter(run == 20250123 | run == 20250128 ) %>% #selects runs from dates of interest
  filter(sample.name != "C1 IPSC") %>% #Removed C1 IPSC because of a lack of values - experiment error
  filter(run == 20250128 | target.name != "GAPDH") %>% #Removed GAPDH from 20250128 run because had issues with loading - lots of high and missing values and don't need 2 for comparing housekeepers
  #filter(CT < 36) %>% #Would filter Ct values over 36 out
  mutate(sample_l = substr(sample.name, 1, 1)) #Adds column with first letter of sample name - works to separate Control, KO, and S209F if keep naming consistent but should name in more detail on machine for less adjustment in excel

View(tidy_data)

#https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
#Gives colour options - the default or palette = R4 are supposed to be okay color blind friendly ones
palette.colors(6, recycle = FALSE)


#**Most recent boxplot format** boxplot of Ct grouped by target with boxes by cell line - outliers are white as putting all points on later
boxplot(
  Ct ~ sample_l * target.name, data = tidy_data, outcol = "white", xaxt = "n", boxwex=0.5, xlab = expression(bold("Target gene")), ylab = expression(bold("Ct")),
  col = c("#E69F00", "#56B4E9", "#009E73")
)

#Puts x axis labels evenly spaced on x axis
axis(side = 1, at = c(2, 5, 8, 11, 14), labels = c("B-Actin", "B2M", "GAPDH", "HPRT", "PGK1"))

#Adds legend to topright with cell line and colours - size is reduced so it fits above data points
legend(
  "topright", 
  legend = c("Control", "ABI3 KO", "ABI3 S209F"), fill = c("#E69F00", "#56B4E9", "#009E73"), cex = 0.6
)

#Adds lines separating target gene boxes
abline(v= c(3.5, 6.5, 9.5, 12.5),lty=1, col="black")

#Adds individual points to box plot
stripchart( Ct ~ sample_l * target.name,
            data = tidy_data,
            method = "jitter",
            pch = 20,
            col = "black",
            vertical = TRUE,
            add = TRUE)


#boxplot of Ct grouped by cell line and different boxes for targets
boxplot(
  Ct ~ target.name * sample_l, data = tidy_data, xaxt = "n", boxwex=0.5, xlab = expression(bold("Cell lines")), ylab = expression(bold("Ct")),
  col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
)


axis(side = 1, at = c(2, 8, 14), labels = c("Control", "ABI3 KO", "ABI3 S209F"))

legend(
  "topright", 
  legend = c("B-Actin", "B2M", "GAPDH", "HPRT", "PGK1"), fill = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
)

for(i in seq(0.5 , 20 , 5)){ 
  abline(v=i,lty=1, col="black")
}

abline(v=0.5, col="white")
abline(v=15.5, col = "white")

#Adds data points over box plot
stripchart( Ct ~ target.name * sample_l,
           data = tidy_data,
           method = "jitter",
           pch = 20,
           col = "black",
           vertical = TRUE,
           add = TRUE)



#Boxplot of target genes over all the samples rather than split by cell line
boxplot(
  Ct ~ target.name, data = tidy_data, xaxt = "n", boxwex=0.5, xlab = expression(bold("Target")), ylab = expression(bold("Ct")),
  col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
)

axis(side = 1, at = c(1, 2, 3, 4, 5), labels = c("B-Actin", "B2M", "GAPDH", "HPRT", "PGK1"))

stripchart( Ct ~ target.name,
            data = tidy_data,
            method = "jitter",
            pch = 20,
            col = "black",
            vertical = TRUE,
            add = TRUE)
legend(
  "topright", 
  legend = c("Control", "ABI3 KO", "ABI3 S209F"), fill = c("#E69F00", "#56B4E9", "#009E73"), cex = 0.6
)

for(i in seq(0.5 , 20 , 3)){ 
  abline(v=i,lty=1, col="black")
}

abline(v=0.5, col="white")
abline(v=15.5, col = "white")

stripchart( Ct ~ sample_l * target.name,
            data = tidy_data,
            method = "jitter",
            pch = 20,
            col = "black",
            vertical = TRUE,
            add = TRUE)

#Standard deviation and means table
target_dev <-  tidy_data %>%
  group_by(sample_l, target.name) %>%
  summarise(stability = sd(Ct))
View(target_dev)

target_dev_overall <-  tidy_data %>%
  group_by(target.name) %>%
  summarise(stability = sd(Ct)) 

target_mean_overall <-  tidy_data %>%
  group_by(target.name) %>%
  summarise(Ct_level = mean(Ct)) 
 
View(target_mean_overall)

