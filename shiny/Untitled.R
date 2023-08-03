dataset <- read.csv("international_undergraduate_tuition_data.csv")
tuition <- dataset[, c("REF_DATE", "GEO", "Field.of.study", "VALUE")] %>%
  rename(year = REF_DATE, province = GEO, 
         subject = Field.of.study, tuition_fee = VALUE) %>%
  filter(province != "Canada" & province != "Yukon" & subject == "Education")

tuition$year <- substr(tuition$year, 1, 4)

tuition1 <- tuition %>%
  group_by(province) %>%
  mutate(tuition_fee = ifelse(is.na(tuition_fee), median(tuition_fee, na.rm = T), tuition_fee))


tuition_active <- spread(tuition, province, tuition_fee) %>%
  filter(year > as.character(2015))

for (i in 1:ncol(tuition_active)){
  tuition_active[,i] <- replace_na(tuition_active[,i], median(tuition_active[,i], na.rm = TRUE))
}

tuition_active <- gather(tuition_active, province, tuition_fee, Alberta:Saskatchewan, 
                  factor_key=FALSE)



ggplot(tuition_active,
       aes(x = year, y = tuition_fee, colour = province, group = province)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Tuition Fee Trend Over Years", x = "Year", y = "Tuition Fee") + 
  theme(plot.title = element_text(face="bold", size=20, hjust=0.5), 
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 13), 
        legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15)) + 
  scale_color_manual(values=c("blue", "coral2", "gold2", "green3", 
                              "cyan3", "firebrick3", "darkviolet", 
                              "burlywood4", "lightpink3", "black"))
