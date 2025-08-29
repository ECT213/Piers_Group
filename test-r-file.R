library(dplyr)
library(ggplot2)

#Load in data, load Ct column as numeric
#Puts loaded data into a df called tidy_data and omits NA entries
#Filter out NC and replicates / samples with errors
tidy_data <- qPCR_R_Practice %>%
  na.omit() %>%
  filter(sample.name !='NC') %>%
  filter(sample.name !='C1 IPSC') %>%
  filter(sample.name !="C2 IPSC" | target.name != "B-ACTIN")

#Check dataframe
View(tidy_data)


#Gives mean Ct for each primer and sample, e.g. average B2M Ct for Control 1 
summarized_data <- tidy_data %>%
  group_by(sample.name, target.name) %>%
  summarise(mean_Ct = mean(Ct))

#Plots mean Ct values
ggplot(summarized_data, aes(x = summarized_data$target.name, y = mean_Ct, colour = target.name)) +
  geom_point()

#Makes separate data frames for each primer and renames control mean_Ct and ref_Ct
GAPDH_data <- summarized_data %>%
  filter(target.name == "GAPDH") %>%
  rename("ref_Ct" = "mean_Ct")

B2M_data <- summarized_data %>%
  filter(target.name == "B2M")

B_actin_data <- summarized_data %>%
  filter(target.name == "B-ACTIN")

#Combines primer dataframes
combined_data <- left_join(B_actin_data, GAPDH_data, by = c("sample.name"))

#Adds column for delta_Ct values
combined_data <- mutate(combined_data, delta_Ct = ref_Ct - mean_Ct)

#plots delta_Ct values by sample (still separated out by biological replicate)
ggplot(combined_data, aes(x = sample.name, y = delta_Ct)) +
  geom_point()

#makes new dataframe with new column of first letter of samples
#Also adds column with the mean_delta_Ct of each cell line
treatment_summary <- combined_data %>%
  mutate(sample_l = substr(sample.name, 1, 1))%>%
  group_by(sample_l) %>%
  summarise(mean_delta_Ct = mean(delta_Ct)) 

head(treatment_summary)

#assigns mean delta Ct to mean_control
mean_control <- filter(treatment_summary, sample_l == "C") %>% 
  pull(mean_delta_Ct)

print(mean_control)

#Adds delta_delta_Ct column to combined_data 
combined_data <- combined_data %>% 
  mutate(delta_delta_Ct = mean_control - delta_Ct)

#Plots delta_delta_Ct value by sample name
ggplot(combined_data, aes(x = sample.name, y = delta_delta_Ct)) +
  geom_point()

#Adds columns to combined data for 2^-delta_delta_Ct and cell line letter
combined_data <- combined_data %>%
  mutate(rel_conc = 2^-delta_delta_Ct) %>%
  mutate(sample_l = substr(sample.name, 1, 1))

#descriptive stats for creating plot
std <- function(x) sd(x)/sqrt(length(x))
descriptive_stats <- combined_data %>%
  group_by(sample_l) %>%
  summarise(avg_delta = mean(rel_conc), stand_dev = sd(rel_conc), stand_error = std(rel_conc)) 


print(descriptive_stats)  

cells <- c("BIONi-C", "ABI3 KO", "ABI3 S209F")

# Default bar plot
p<- ggplot(descriptive_stats, aes(x=cells, y=avg_delta)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=avg_delta-stand_error, ymax=avg_delta+stand_error), width=.2,
                position=position_dodge(.9)) 
print(p)

p+labs(title="B-Actin Gene Expression", x="Cell Line", y = "Fold gene expression")+
  theme_classic() +
  scale_color_manual(values=c('cyan4','#E69F00'))

print()