library(tidyverse)
library(knitr)
library(stargazer)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(ggstance)
library(scales)
library(readr)
library(ggplot2)
library(gganimate)

install.packages("av")
install.packages("gifski")


###################### START FROM BELOW DATA ######################################
sociodemographic <- read.csv("sdi_data.csv", header = TRUE)
dentists <- read.csv("IHME_GBD_2019_HRH_1990_2019_DATA_Y2022M06D03.CSV", header = TRUE)
health_assessment <- read.csv("IHME_GBD_2019_HAQ_INDEX_1980_2022_VALUES_Y2023M06D27.csv", header = TRUE)
diet <- read.csv("IHME-GBD_2019_DATA-e9afc79b-1.csv", header = TRUE)
adult_prevalence <- read.csv("IHME-GBD_2019_DATA-03fe58dd-1.csv", header = TRUE)

# Clean data
# Density of Dentist and Dental Assistant
dental_density <- dentists %>%
  filter(cadre %in% c("Dentists", "Dental Assistants")) %>%
  select(location_name, location_id, year_id, cadre, mean) %>%
  pivot_wider(names_from = cadre, values_from = mean, names_prefix = "") %>%
  rename(name = location_name, year = year_id, dentist_density = Dentists, dental_assistant_density = `Dental Assistants`)
write.csv(dental_density, file = "dental_density.csv", row.names = FALSE)

# SDI
SDI <- sociodemographic %>%
  select(location_name, location_id, year_id, mean_value) %>%
  rename(SDI = mean_value, name = location_name, location_id = location_id, year = year_id)
write.csv(SDI, file = "SDI.csv", row.names = FALSE)

# HAQ
HAQ <- health_assessment %>%
  select(location_name, location_id, year_id, mean_value) %>%
  rename(name = location_name, location_id = location_id, year = year_id, HAQ = mean_value) %>%
  filter(year >= 1990 & year <= 2019)
write.csv(HAQ, file = "HAQ.csv", row.names = FALSE)

# Prevalence
adult_prevalence <- adult_prevalence %>%
  filter(cause_name %in% c("Periodontal diseases", "Caries of permanent teeth")) %>%
  select(location_name, location_id, year, cause_name, val, age_name) %>%
  rename(prevalence_percent = val) %>%
  rename(name = location_name) %>%
  rename(age = age_name)

adult_prevalence$cause_name[adult_prevalence$cause_name == "Caries of permanent teeth"] <- "caries_perm_prev"
adult_prevalence$prevalence_percent <- as.numeric(adult_prevalence$prevalence_percent)
adult_prevalence <- pivot_wider(adult_prevalence, names_from = cause_name, values_from = prevalence_percent)
write.csv(adult_prevalence, file = "adult_prevalence.csv", row.names = FALSE)

# cleaning diet data
diet <- diet %>%
  filter(rei_name %in% c("Diet low in milk", "Diet high in sugar-sweetened beverages", "Diet low in calcium")) %>%
  select(location_name, location_id, year, rei_name, val, age_name) %>%
  rename(name = location_name)
diet$rei_name[diet$rei_name == "Diet low in milk"] <- "low_milk"
diet$rei_name[diet$rei_name == "Diet high in sugar-sweetened beverages"] <- "high_sugar"
diet$rei_name[diet$rei_name == "Diet low in calcium"] <- "low_calcium"
diet$val <- as.numeric(diet$val)
diet <- pivot_wider(diet, names_from = rei_name, values_from = val)

# Filtering data for age group "25 and above"
diet_age1 <- diet %>%
  filter(age_name == "25+ years") %>%
  select(name, location_id, year, low_milk, high_sugar, low_calcium)
write.csv(diet_age1, file = "diet.csv", row.names = FALSE)

## ANNIE MAKE CHANGES SO THAT NO ERROR 
# Merge data
# data <- left_join(dental_density,HAQ , by = c("name", "year", "location_id")) %>%
#   left_join(adult_prevalence, by = c("location_id", "name", "year")) %>%
#   left_join(diet_age1, by = c("location_id", "name", "year")) %>%
#   left_join(SDI, by = c("name", "year", "location_id"))
# write.csv(data, file = "adult_data.csv", row.names = FALSE)

data <- left_join(dental_density, HAQ, by = c("name", "year", "location_id")) %>%
  left_join(adult_prevalence, by = c("location_id", "name", "year")) %>%
  left_join(diet_age1, by = c("location_id", "name", "year")) %>%
  left_join(SDI, by = c("name", "year", "location_id")) 
  
  clean_location_names <- function(name) {
    name %>%
    tolower() %>%
    gsub("\\.", "", .) %>%
    gsub("-", " ", .) %>%
    gsub("\\(.*\\)", "", .) %>%
    trimws() %>%
    gsub("\\busa\\b", "united states", .) %>%
    gsub("\\bus\\b", "united states", .) %>%
    gsub("\\bu\\.?s\\.?\\b", "united states", .) %>%
    gsub("\\buk\\b", "united kingdom", .) %>%
    gsub("\\bunited states of america\\b", "united states", .)
}

# Assuming 'data' dataframe contains the 'location' column you want to clean
my_data <- data %>%
  mutate(cleaned_name = clean_location_names(name))

# Sort data by the 'name' column in alphabetical order
my_data <- my_data[order(my_data$cleaned_name), ]

# Write the sorted data to a CSV file
write.csv(my_data, file = "my_data_alphabetical.csv", row.names = TRUE)

##################### WHEN RUNING,START FROM HERE #####################################
####################### MERGE CLEAN DATA WITH WORLD DATA ############################################################################################################

world <- ne_countries(scale = "medium", returnclass = "sf") 

clean_country_names <- function(name) {
  name %>%
    tolower() %>%                # Convert to lowercase
    iconv(to = "ASCII//TRANSLIT") %>%  # Convert accented characters to their closest ASCII representation
    gsub("\\.", "", .) %>%       # Remove periods
    gsub("-", " ", .) %>%        # Replace hyphens with spaces
    gsub("\\(.*\\)", "", .) %>%  # Remove text within parentheses
    gsub("[~'’]", "", .) %>%     # Remove tildes and apostrophes
    trimws() %>%                # Trim whitespace
    gsub("\\busa\\b", "united states", .) %>%  # Replace "usa" with "united states"
    gsub("\\bus\\b", "united states", .) %>%   # Replace "us" with "united states"
    gsub("\\bu\\.?s\\.?\\b", "united states", .) %>%  # Replace "u.s." or "u.s" with "united states"
    gsub("\\buk\\b", "united kingdom", .) %>%  # Replace "uk" with "united kingdom"
    gsub("\\bunited states of america\\b", "united states", .)  # Replace "united states of america" with "united states"
}
world_data <- world %>%
  mutate(cleaned_name = clean_country_names(name))
unique(world_data$cleaned_name)
View(world_data)

dat <- read.csv("cleaned_my_data.csv", header = TRUE)
unique(dat$cleaned_name)

data_with_map <- merge(world_data, dat, by = "cleaned_name", all.x = TRUE)
View(data_with_map_2019)

data_2019 <- subset(dat, year == 2019)

data_with_map_2019 <- merge(world_data, data_2019, by = "cleaned_name", all.x = TRUE)



#############################################################################################################################################################
############################# 
# View(merged_data)
# merged_data <- left_join(my_data, world_data, by = c("cleaned_name" = "cleaned_name"))
# unique(merged_data$cleaned_name)
# 
# write.csv(merged_data, file = "final_data.csv", row.names = FALSE)

############################

#################### WORLD MAP #################################################################################################
# World map for caries_perm_prev
map_outcome <- ggplot(data = data_with_map_2019) +
  geom_sf(aes(fill = caries_perm_prev)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "World Map - Caries of Permanent Teeth 2019",
       subtitle = "Prevalence of Caries of Permanent Teeth by Country")
ggsave("map_caries.png", plot = map_outcome, width = 10, height = 6, units = "in")
print(map_outcome)


# World map for Dental Assistant Density
map_dental_assistant_density <- ggplot(data = data_with_map_2019) +
  geom_sf(aes(fill = dental_assistant_density)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "World Map - Dental Assistant Density 2019",
       subtitle = "Density of Dental Assistants per Population by Country")
ggsave("map_dental_assistant_density.png", plot = map_dental_assistant_density, width = 10, height = 6, units = "in")
print(map_dental_assistant_density)

map_SDI <- ggplot(data = data_with_map_2019) +
  geom_sf(aes(fill = SDI)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "World Map - Social Development Index (SDI) 2019",
       subtitle = "Social Development Index by Country")
ggsave("map_SDI.png", plot = map_SDI, width = 10, height = 6, units = "in")
print(map_SDI)

# World map for Human Capital Index (HAQ)
map_HAQ <- ggplot(data = data_with_map_2019) +
  geom_sf(aes(fill = HAQ)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "World Map - Human Capital Index (HAQ) 2019",
       subtitle = "Human Capital Index by Country")
ggsave("map_HAQ.png", plot = map_HAQ, width = 10, height = 6, units = "in")
print(map_HAQ)

###############
library(ggplot2)
library(gganimate)

# Create the initial plot
library(ggplot2)
library(gganimate)

p <- ggplot(data = data_with_map_2019, aes(x = SDI, y = caries_perm_prev, size = dentist_density, color = cleaned_name)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ income_grp) +
  scale_x_log10() +
  scale_size(range = c(2, 12)) +
  labs(title = 'Year: {frame_time}',
       x = 'Social Development Index (SDI)',
       y = 'Caries Prevalence') +
  transition_time(year) +
  ease_aes('linear')

# Animate the plot
animation <- animate(p, nframes = 100)

# Display the animation
animation

##################### top 10 ########################

library(ggplot2)
library(dplyr)

# Top ten and least ten countries for each variable
top_least_caries <- data_with_map_2019 %>%
  arrange(caries_perm_prev) %>%
  filter(!is.na(caries_perm_prev)) %>%
  slice(c(1:10, (n() - 9):n()))

top_least_dentist_density <- data_with_map_2019 %>%
  arrange(dentist_density) %>%
  filter(!is.na(dentist_density)) %>%
  slice(c(1:10, (n() - 9):n()))

top_least_dental_assistant_density <- data_with_map_2019 %>%
  arrange(dental_assistant_density) %>%
  filter(!is.na(dental_assistant_density)) %>%
  slice(c(1:10, (n() - 9):n()))

top_least_SDI <- data_with_map_2019 %>%
  arrange(SDI) %>%
  filter(!is.na(SDI)) %>%
  slice(c(1:10, (n() - 9):n()))

top_least_HAQ <- data_with_map_2019 %>%
  arrange(HAQ) %>%
  filter(!is.na(HAQ)) %>%
  slice(c(1:10, (n() - 9):n()))

# Create bar charts with line in the middle
top_least_SDI_plot <- ggplot(top_least_SDI, aes(x = fct_reorder(cleaned_name, SDI), y = SDI)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "red") +  # Add line at middle (between top and least)
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Social Development Index (SDI) 2019",
       y = "Social Development Index",
       x = "Country")

top_least_HAQ_plot <- ggplot(top_least_HAQ, aes(x = fct_reorder(cleaned_name, HAQ), y = HAQ)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "red") +  # Add line at middle (between top and least)
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Human Capital Index (HAQ) 2019",
       y = "Human Capital Index",
       x = "Country")

# Create bar charts
# Create bar charts with line in the middle
top_least_caries_plot <- ggplot(top_least_caries, aes(x = fct_reorder(cleaned_name, -caries_perm_prev), y = caries_perm_prev)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "red") +  # Add line at middle (between top and least)
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Caries of Permanent Teeth 2019",
       y = "Prevalence of Caries of Permanent Teeth",
       x = "Country")

top_least_dentist_density_plot <- ggplot(top_least_dentist_density, aes(x = fct_reorder(cleaned_name, dentist_density), y = dentist_density)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "red") +  # Add line at middle (between top and least)
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Dentist Density 2019",
       y = "Dentist Density per Population",
       x = "Country")

top_least_dental_assistant_density_plot <- ggplot(top_least_dental_assistant_density, aes(x = fct_reorder(cleaned_name, dental_assistant_density), y = dental_assistant_density)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "red") +  # Add line at middle (between top and least)
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Dental Assistant Density 2019",
       y = "Dental Assistant Density per Population",
       x = "Country")

# Save the plots
ggsave("top_least_caries.png", plot = top_least_caries_plot, width = 10, height = 6, units = "in")
ggsave("top_least_dentist_density.png", plot = top_least_dentist_density_plot, width = 10, height = 6, units = "in")
ggsave("top_least_dental_assistant_density.png", plot = top_least_dental_assistant_density_plot, width = 10, height = 6, units = "in")

###################
library(ggplot2)
library(dplyr)
library(forcats) 

# ... (Code to create top_least_caries, top_least_dentist_density, and top_least_dental_assistant_density) ...

# Create a line to separate the top and least ten
middle_line <- geom_vline(xintercept = 10.5, linetype = "dashed", color = "red")

# Create customized themes
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    legend.position = "none"
  )

# Apply themes and create plots
top_least_caries_plot <- ggplot(top_least_caries, aes(x = fct_reorder(cleaned_name, -caries_perm_prev), y = caries_perm_prev)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  middle_line +
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Caries of Permanent Teeth 2019",
       subtitle = "Prevalence of Caries of Permanent Teeth by Country",
       y = "Prevalence of Caries",
       x = "Country") +
  custom_theme

top_least_dentist_density_plot <- ggplot(top_least_dentist_density, aes(x = fct_reorder(cleaned_name, dentist_density), y = dentist_density)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  middle_line +
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Dentist Density 2019",
       subtitle = "Dentist Density per Population by Country",
       y = "Dentist Density",
       x = "Country") +
  custom_theme

top_least_dental_assistant_density_plot <- ggplot(top_least_dental_assistant_density, aes(x = fct_reorder(cleaned_name, dental_assistant_density), y = dental_assistant_density)) +
  geom_bar(stat = "identity", fill = "orange") +
  middle_line +
  coord_flip() +
  labs(title = "Top and Least Ten Countries - Dental Assistant Density 2019",
       subtitle = "Dental Assistant Density per Population by Country",
       y = "Dental Assistant Density",
       x = "Country") +
  custom_theme

# Save the plots
ggsave("top_least_caries.png", plot = top_least_caries_plot, width = 10, height = 6, units = "in")
ggsave("top_least_dentist_density.png", plot = top_least_dentist_density_plot, width = 10, height = 6, units = "in")
ggsave("top_least_dental_assistant_density.png", plot = top_least_dental_assistant_density_plot, width = 10, height = 6, units = "in")



########################### REGRESSION MODELS ################################################################################
# model <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI  + high_sugar  + low_calcium + as.factor(year) +  as.factor(name), data = dat)
# model_summary <-summary(model)
# coefficients_table <- cbind(Coefficient = coef(model_summary)[, 1],
#                             `Std. Error` = coef(model_summary)[, 2],
#                             `p-value` = coef(model_summary)[, 4])
# rownames(coefficients_table) <- rownames(coef(model_summary))
# 
# kable(coefficients_table, digits = 4)


model1 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI +  as.factor(name), data = dat)

model2 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI + HAQ +  as.factor(name), data = dat)

model3 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI + HAQ + high_sugar  + low_calcium  +  as.factor(name), data = dat)

model4 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI + HAQ + high_sugar  + low_calcium +  as.factor(name) + as.factor(year), data = dat)

m1 <- summary(model1)
m2 <- summary(model2)
m3 <- summary(model3)
m4 <- summary(model4)
m5 <- summary(model5)


table <- capture.output(
  stargazer(
    model1, model2, model3, model4, 
    type = "text",
    title = "Comparative Analysis of Regression Models: Examining Variable Effects in Different Contexts",
    column.labels = c("SDI", "+ HAQ", "+ Risk factors", "Final"),
    digits = 6,
    omit = c("name", "year")
  )
)

# Save the captured output to a text file
file_path <- "output_table.txt"
writeLines(table, con = file_path)

# covariate.labels = c(
#   "Log(Dentist Density)", "Log(Dental Assistant Density)", "SDI", "HAQ",
#   "High Sugar", "Low Calcium", "Location Factors", "Year Factors"

print(table)


#####################################
coefficients_table <- cbind(Coefficient = coef(m5)[, 1],
                            `Std. Error` = coef(m5)[, 2],
                             `p-value` = coef(m5)[, 4])
# rownames(coefficients_table) <- rownames(coef(m4))
# 
# print(kable(coefficients_table, caption = "Regression Summary of SDI and HAQ with Risk Factors adding Locations" ))




coefficients_table_kable <- kable(
  coefficients_table,
  digits = 4,
  format = "html",
  align = c("l", "c", "c", "c"),
  caption = "Regression Summary of SDI and HAQ with Risk Factors adding Locations and years"
) %>%
  kable_styling(bootstrap_options = "striped")

# Print the kable table
print(coefficients_table_kable)



########################## PLOTS ############################################################################################
# scatter plot of Caries Perm Prev vs Dentist Density
scatter_plot_dentist <- ggplot(data_with_map, aes(x = dentist_density, y = caries_perm_prev)) +
  geom_point() +
  labs(title = "Scatter Plot: Caries Perm Prev vs Dentist Density",
       x = "Dentist Density",
       y = "Outcome (Caries Perm Prev)")

# scatter plot of Caries Perm Prev vs Dental Assistant Density
scatter_plot_dental_assistant <- ggplot(data_with_map, aes(x = dental_assistant_density, y = caries_perm_prev)) +
  geom_point() +
  labs(title = "Scatter Plot: Caries Perm Prev vs Dental Assistant Density",
       x = "Dental Assistant Density",
       y = "Outcome (Caries Perm Prev)")

# loess line plot of Caries Perm Prev vs Dentist Density
loess_plot_dentist <- ggplot(data_with_map, aes(x = dentist_density, y = caries_perm_prev)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Loess Line Plot: Caries Perm Prev vs Dentist Density",
       x = "Dentist Density",
       y = "Outcome (Caries Perm Prev)")+
  geom_smooth(method = "gam", color = "grey15", alpha = 0.5, size = 1.0)


#loess line plot of Caries Perm Prev vs Dental Assistant Density
loess_plot_dental_assistant <- ggplot(data_with_map, aes(x = dental_assistant_density, y = caries_perm_prev)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Loess Line Plot: Caries Perm Prev vs Dental Assistant Density",
       x = "Dental Assistant Density",
       y = "Outcome (Caries Perm Prev)")+
  geom_smooth(method = "gam", color = "grey15", alpha = 0.5, size = 1.0)


ggsave("scatter_plot_dentist.png", plot = scatter_plot_dentist, width = 10, height = 6, units = "in")
ggsave("scatter_plot_dental_assistant.png", plot = scatter_plot_dental_assistant, width = 10, height = 6, units = "in")
ggsave("loess_plot_dentist.png", plot = loess_plot_dentist, width = 10, height = 6, units = "in")
ggsave("loess_plot_dental_assistant.png", plot = loess_plot_dental_assistant, width = 10, height = 6, units = "in")

#######################################################################################################################################


#SDI quintiles for each country
data_with_map <- data_with_map %>%
  group_by(year) %>%
  mutate(SDI_quintile = cut(SDI, quantile(SDI, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE))


#fixing invalid geometries
invalid_geometries <- st_is_valid(data_with_map, reason = TRUE)
invalid_geometries <- data_with_map[invalid_geometries == FALSE, ]


#making geometries valid
data_with_map <- st_make_valid(data_with_map)

# Grouping SDI_quintile, and calculating the average of the variables
averaged_data <- data_with_map %>%
  group_by(year, SDI_quintile) %>%
  summarize(avg_caries_perm_prev = mean(caries_perm_prev, na.rm = TRUE),
            avg_dentist_density = mean(dentist_density, na.rm = TRUE),
            avg_dental_assistant_density = mean(dental_assistant_density, na.rm = TRUE))

# plots for each variable over time by SDI quintile
plot_caries <- ggplot(averaged_data, aes(x = year, y = avg_caries_perm_prev, color = factor(SDI_quintile))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Change in Caries Over Time by SDI Quintile",
       x = "Year",
       y = "Average Caries Prevalence",
       color = "SDI Quintile") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top")+
  geom_point(size = 2) +
  ylim (0.325, 0.375)


plot_dentist_density <- ggplot(averaged_data, aes(x = year, y = avg_dentist_density, color = factor(SDI_quintile))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Change in Dentist Density Over Time by SDI Quintile",
       x = "Year",
       y = "Average Dentist Density",
       color = "SDI Quintile") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_point(size = 2) 



plot_dental_assistant_density <- ggplot(averaged_data, aes(x = year, y = avg_dental_assistant_density, color = factor(SDI_quintile))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Change in Dental Assistant Density Over Time by SDI Quintile",
       x = "Year",
       y = "Average Dental Assistant Density",
       color = "SDI Quintile") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "top") +
  geom_point(size = 2)



ggsave("plot_caries.png", plot = plot_caries, width = 10, height = 6, units = "in")
ggsave("plot_dentist_density.png", plot = plot_dentist_density, width = 10, height = 6, units = "in")
ggsave("plot_dental_assistant_density.png", plot = plot_dental_assistant_density, width = 10, height = 6, units = "in")


data_2019 <- data[dat$year == 2019,]
model_2019 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI  + high_sugar  + low_calcium, data = dat)
summary(model_2019)

data_2019 <- data[dat$year == 2019,]
model_2019 <- lm(caries_perm_prev ~ log(dentist_density) + log(dental_assistant_density)  + SDI + HAQ + high_sugar  + low_calcium, data = dat)
summary(model_2019)
