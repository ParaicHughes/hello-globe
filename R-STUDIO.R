### Data Visualization in R ###
#my loading



install.packages("leaflet")
install.packages("plotly")
install.packages("scales")
# Loading all the Required Packages for the analysis:

library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)
library(plotly)
library(scales)

# Data Loading and Data Pre-Processing:

unicef_2 <- `unicef_indicator_2_3_`
unicef_2 <- as.data.frame(unicef_2[,c("time_period", "country","obs_value")])
head(unicef_2)

# time_period     country obs_value
# 1        2000 Afghanistan      2000
# 2        2001 Afghanistan      2300
# 3        2002 Afghanistan      2700
# 4        2003 Afghanistan      3100
# 5        2004 Afghanistan      3600
# 6        2005 Afghanistan      4100

unicef_3 <- `unicef_metadata_3_`

# Rename columns
colnames(unicef_3)[colnames(unicef_3) == "Population..total"] <- "Population_Total"
colnames(unicef_3)[colnames(unicef_3) == "GDP.per.capita..constant.2015.US.."] <- "GDP_perc_capita"
colnames(unicef_3)[colnames(unicef_3) == "Life.expectancy.at.birth..total..years."] <- "Life_expectancy"
unicef_3 <- as.data.frame(unicef_3[, c("year", "country", "Population_Total", "GDP_perc_capita", "Life_expectancy")])
head(unicef_3)

# year     country Population_Total GDP_per_capita Life_expectancy
# 1 1960 Afghanistan          8622466              NA          32.535
# 2 1961 Afghanistan          8790140              NA          33.068
# 3 1962 Afghanistan          8969047              NA          33.547
# 4 1963 Afghanistan          9157465              NA          34.016
# 5 1964 Afghanistan          9355514              NA          34.494
# 6 1965 Afghanistan          9565147              NA          34.953

# Remove NA values from dataframes
unicef_2 <- unicef_2[complete.cases(unicef_2), ]
unicef_3 <- unicef_3[complete.cases(unicef_3), ]


# EDA:

# EUROPEAN Countries

# Calculate average rise in the number of children affected by AIDS for each country
average_rise <- unicef_2 %>%
  group_by(country) %>%
  summarise(average_rise = mean(diff(obs_value), na.rm = TRUE))

average_rise <- unicef_2 %>%
  mutate(obs_value = as.numeric(obs_value)) %>% #
  group_by(country) %>%
  summarise(average_rise = if(n() > 1) mean(diff(obs_value), na.rm = TRUE) else NA_real_)


# Filter data for European countries
european_countries <- c("Germany", "France", "United Kingdom", "Italy", "Spain", "Netherlands", "Belgium", "Sweden", "Poland", "Austria", "Switzerland", "Norway", "Ireland", "Denmark", "Finland", "Portugal", "Greece", "Czech Republic", "Romania", "Hungary", "Slovakia", "Bulgaria", "Croatia", "Estonia", "Slovenia", "Lithuania", "Latvia", "Cyprus", "Luxembourg", "Malta")

european_data <- unicef_2 %>%
  filter(country %in% european_countries)

# Create scatter plot
scatterplot_europe <- ggplot(european_data, aes(x = time_period, y = obs_value, color = country)) +
  geom_point() +
  labs(title = "Number of Children Affected by AIDS in European Countries",
       x = "Year",
       y = "Estimated No. of Children Affected",
       color = "Country") +
  theme_minimal()

scatterplot_europe <- scatterplot_europe + theme(
  axis.text.y = element_blank()) 

plotly_scatterplot_europe <- ggplotly(scatterplot_europe)

plotly_scatterplot_europe


# Filter data for African countries
african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

# Calculate average rise in the number of children affected by AIDS for each country
average_rise <- unicef_2 %>%
  group_by(country) %>%
  summarise(average_rise = mean(diff(obs_value), na.rm = TRUE)) %>%
  filter(country %in% african_countries) %>%
  top_n(10, wt = average_rise) # Select top 10 African countries

average_rise <- unicef_2 %>%
  mutate(obs_value = as.numeric(obs_value)) %>% 
  group_by(country) %>%
  filter(country %in% african_countries) %>% 
  summarise(average_rise = if(n() > 1) mean(diff(obs_value), na.rm = TRUE) else NA_real_) %>% 
  top_n(10, wt = average_rise)

# Filter data for the top 10 African countries
top_10_african_countries <- average_rise$country

top_10_african_data <- unicef_2 %>%
  filter(country %in% top_10_african_countries)

# Create scatter plot
scatterplot_top_10_africa <- ggplot(top_10_african_data, aes(x = time_period, y = obs_value, color = country)) +
  geom_point() +
  labs(title = "Number of Children Affected by AIDS in Top 10 African Countries",
       x = "Year",
       y = "Estimated No. of Children Affected",
       color = "Country") +
  theme_minimal()

# Display the scatter plot
print(scatterplot_top_10_africa)

# Histogram for Top 15 in the world

# Aggregate data to calculate total number of children affected by AIDS for each country
country_totals <- unicef_2 %>%
  group_by(country) %>%
  summarise(total_affected = sum(obs_value, na.rm = TRUE))

country_totals <- unicef_2 %>%
  mutate(obs_value_numeric = as.numeric(as.character(obs_value))) %>%
  group_by(country) %>%
  summarise(total_affected = sum(obs_value_numeric, na.rm = TRUE))

# Select top 15 countries with highest total number of affected children
top_15_countries <- country_totals %>%
  top_n(15, total_affected) %>%
  filter(country != "Uzbekistan")

# Create bar plot

barplot_top_15_countries <- ggplot(top_15_countries, aes(x = reorder(country, total_affected), y = total_affected, fill = total_affected)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 15 Countries with Highest Number of Children Affected by AIDS 2000 - 2022",
       x = "Country",
       y = "Total No. of Children Affected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(labels = c("0", "10,000,000", "20,000,000", "30,000,000"))))

barplot_top_15_countries <- ggplot(top_15_countries, aes(x = reorder(country, total_affected), y = total_affected, fill = total_affected)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = c("lightblue", "darkblue"), 
                       values = rescale(c(min(top_15_countries$total_affected), max(top_15_countries$total_affected))),
                       breaks = c(0, 10000000, 20000000, 30000000),
                       labels = c("0", "10,000,000", "20,000,000", "30,000,000")) +
  labs(title = "Top 15 Countries with Highest Number of Children Affected by AIDS 2000 - 2022",
       x = "Country",
       y = "Total No. of Children Affected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma())

plotly_barchart <- ggplotly(barplot_top_15_countries)

plotly_barchart

# Time Series Plots:

# Aggregate data to calculate average population, GDP per capita, and life expectancy over time
average_data <- unicef_3 %>%
  group_by(year) %>%
  summarise(avg_population = mean(Population_Total, na.rm = TRUE),
            avg_gdp_per_capita = mean(GDP_perc_capita, na.rm = TRUE),
            avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

average_data <- unicef_3 %>%
  group_by(year) %>%
  summarise(
    avg_population = mean(`Population, total`, na.rm = TRUE),
    avg_gdp_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE),
    avg_life_expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE))

# Time Series Plot of Population Over Time
time_series_population <- ggplot(average_data, aes(x = year, y = avg_population)) +
  geom_line() +
  labs(title = "Time Series Plot of Population Over Time",
       x = "Year",
       y = "Average Population") +
  theme_minimal()

# Time Series Plot of GDP per Capita Over Time
time_series_gdp <- ggplot(average_data, aes(x = year, y = avg_gdp_per_capita)) +
  geom_line() +
  labs(title = "Time Series Plot of GDP per Capita Over Time",
       x = "Year",
       y = "Average GDP per Capita") +
  theme_minimal()

# Time Series Plot of Life Expectancy Over Time
time_series_life_expectancy <- ggplot(average_data, aes(x = year, y = avg_life_expectancy)) +
  geom_line() +
  labs(title = "Time Series Plot of Life Expectancy Over Time",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal()

# Display the plots
print(time_series_population)
print(time_series_gdp)
print(time_series_life_expectancy)


# Scatter Plot regression line 

# Filter the data for South Africa
south_africa_data<- unicef_3 %>%
  filter(country == "South Africa")

# Create a scatter plot of GDP per capita vs. life expectancy for South Africa with a regression line
scatterplot_gdp_life_expectancy_south_africa <- ggplot(south_africa_data, aes(x = `GDP per capita (constant 2015 US$)`, y = `Life expectancy at birth, total (years)`
)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +  # Add linear regression line
  labs(title = "Scatter Plot of GDP per Capita vs. Life Expectancy (South Africa)",
       x = "GDP per Capita",
       y = "Life Expectancy") +
  theme_minimal()

# Create a scatter plot of population vs. GDP per capita for Ireland with a regression line
scatterplot_population_gdp_south_africa<- ggplot(south_africa_data, aes(x = `Population, total`, y = `GDP per capita (constant 2015 US$)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +  # Add linear regression line
  labs(title = "Scatter Plot of Population vs. GDP per Capita (South Africa)",
       x = "Population",
       y = "GDP per Capita") +
  theme_minimal()

# Display the plots for Ireland with regression lines
print(scatterplot_gdp_life_expectancy_south_africa)
print(scatterplot_population_gdp_south_africa)

#plotly
plotly_South_Africa <- ggplotly(scatterplot_gdp_life_expectancy_south_africa)

plotly_South_Africa

#FOR IRELAND 

# Filter the data for Ireland
south_africa_data<- unicef_3 %>%
  filter(country == "Ireland")

# Create a scatter plot of GDP per capita vs. life expectancy for Ireland with a regression line
scatterplot_gdp_life_expectancy_ireland <- ggplot(ireland_data, aes(x = `GDP per capita (constant 2015 US$)`, y = `Life expectancy at birth, total (years)`
)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +  # Add linear regression line
  labs(title = "Scatter Plot of GDP per Capita vs. Life Expectancy (Ireland)",
       x = "GDP per Capita",
       y = "Life Expectancy") +
  theme_minimal()


# Display the plots for Ireland with regression lines
print(scatterplot_gdp_life_expectancy_ireland)


#plotly
plotly_Ireland <- ggplotly(scatterplot_gdp_life_expectancy_ireland)

plotly_Ireland


#joining
unicef_1 <- unicef_indicator_1_2_
unicef_2 <- unicef_2 %>%
  mutate(obs_value = as.numeric(obs_value))

Map_joining_1 <- full_join(unicef_1, unicef_2)
Map_joining_1 <- full_join(unicef_1, unicef_2, by = join_by("country", "time_period"))

Map_joining_1 <- full_join(unicef_1, unicef_3, by = c("country", "time_period" = "year"))

Map_joining_1 <- unicef_1 %>%
  full_join(unicef_2, by = c("country", "time_period")) %>%
  full_join(unicef_3, by = c("country", "time_period" = "year"))

#Map 
map_world <- map_("world")

to_map_data <- full_join(Map_joining_1, map_world, by = c("country" = "region"))

ggplot(to_map_data) +
  aes(x = long, y = lat, group = group, fill = obs_value.x) +
  geom_polygon(color = "white") + 
  labs(
    fill = "GDP per capita",
    title = "World Map of GDP per Capita") +
  theme_classic()


plotly_map_data <- ggplotly(to_map_data)


  




















