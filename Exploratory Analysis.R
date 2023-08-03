# Library
library(ggplot2)
library(dplyr)
library(tidyr)

# Import Data
dataset <- read.csv("/Users/yiranzhang/Documents/McMaster/Grad/STATS 780/Assignment 1/international_undergraduate_tuition_data.csv")

# Data transformation
# Select desired columns: date, geo-location, field of study and the tuition fee values.
# Rename the columns to year, province, subject, and tuition_fee.
# Filter out the data whose province is Canada, and only look at the total field of study.
tuition <- dataset[, c("REF_DATE", "GEO", "Field.of.study", "VALUE")] %>%
  rename(year = REF_DATE, province = GEO, subject = Field.of.study, tuition_fee = VALUE) %>%
  filter(subject == "Total, field of study" & province != "Canada") %>%
  subset(select=-c(subject))

# change year format, make 2006/2007 to 2006, etc, and change format to factor.
tuition$year <- factor(substr(tuition$year, 1, 4))

# Change table format from long to wide to check for missing values.
tuition <- spread(tuition, province, tuition_fee)

# Count number of missing values for each column
colSums(is.na(tuition))

# Yukon too many missing values, and it is unfair to compare it with other provinces, therefore remove Yukon.
tuition <- subset(tuition, select = -c(Yukon))

# Change table format from wide to long for easier plotting.
tuition <- gather(tuition, province, tuition_fee, Alberta:Saskatchewan, factor_key=TRUE)

# Find outliers for each provinces.
ggplot(tuition, aes(x = province, y = tuition_fee)) +
  geom_boxplot() + 
  labs(title = "International Undergraduate Tuition Fee Boxplot by Province", x = "Province", y = "Tuition Fee") + 
  theme(plot.title = element_text(face="bold", size=20, hjust=0.5), 
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12)) + 
  scale_x_discrete(guide = guide_axis(angle = 30))

# Visualization, line graph, tuition trend for each province.
# Visualization
ggplot(tuition, aes(x = year, y = tuition_fee, colour = province, group = province)) +
  geom_line() + 
  geom_point() +
  labs(title = "Tuition Fee Trend Over Years", x = "Year", y = "Tuition Fee") + 
  theme(plot.title = element_text(face="bold", size=20, hjust=0.5), 
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(values=c("red", "chocolate1", "gold2", "green2", 
                              "cyan3", "blue", "darkviolet", "burlywood4", 
                              "lightpink3", "black"))

# aggregate over the provinces for Canada
# idea: add up all the values and take the mean value
tuition_Canada <- tuition %>%
  group_by(year) %>%
  summarise_at(vars(tuition_fee), mean, na.rm=TRUE)

ggplot(tuition_Canada, aes(x = year, y = tuition_fee, group=1, lty = 'Canada')) +
  geom_line() + 
  geom_point() + 
  scale_linetype('') +
  labs(title = "International Undergraduate Tuition Fee Trend Over Years", x = "Year", y = "Tuition Fee") + 
  theme(plot.title = element_text(face="bold", size=20, hjust=0.5), 
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))

