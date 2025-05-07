
install.packages("tidycensus")  
library(tidycensus)  
install.packages("tidyverse")
library(tidyverse)

census_api_key("520ce6a93bbbd7d5df4a88c208774e01e46fc409", install = TRUE, overwrite = TRUE)

test_data <- get_acs(
  geography = "state",
  variables = "B19013_001",  # Median household income
  year = 2023,
  survey = "acs5"
)
head(test_data)

#plot1: Median Household Income & Unemployment Rate by County in NY

ny_acs_data <- get_acs(
  geography = "county",
  state = "NY",
  variables = c(
    median_income = "B19013_001",       # Median Household Income
    rent_burden_30_35 = "B25070_007",   # Households paying 30-35% income on rent
    rent_burden_35_40 = "B25070_008",   # Households paying 35-40% income on rent
    rent_burden_40_50 = "B25070_009",   # Households paying 40-50% income on rent
    rent_burden_50_plus = "B25070_010", # Households paying 50%+ income on rent
    unemployment_rate = "B23025_005",   # Unemployed population
    poverty_rate = "B17001_002",        # People below poverty level
    homeowners = "B25003_002",          # Owner-occupied housing units
    renters = "B25003_003"              # Renter-occupied housing units
  ),
  year = 2023,
  survey = "acs5"
)

head(ny_acs_data)
tail(ny_acs_data)
str(ny_acs_data)

ny_county_list <- unique(ny_acs_data$NAME)
length(ny_county_list)  # Should return 62

#Bar Chart of Median Income by County
ny_income <- ny_acs_data %>% filter(variable == "median_income")
ny_unemployment <- ny_acs_data %>% filter(variable == "unemployment_rate")
ny_rent_burden <- ny_acs_data %>% filter(variable %in% c("rent_burden_30_35", "rent_burden_35_40", "rent_burden_40_50", "rent_burden_50_plus"))
ggplot(ny_income, aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Median Household Income Across NY Counties",
       x = "County", y = "Median Income ($)") +
  theme_minimal()

summary(ny_unemployment$estimate)
unique(ny_unemployment$variable)

unemployment_vars <- c(
  total_labor_force = "B23025_003",
  unemployed = "B23025_005"
)

ny_unemployment_data <- get_acs(
  geography = "county",
  state = "NY",
  variables = unemployment_vars,
  year = 2023,
  survey = "acs5"
)
unique(ny_unemployment_data$variable)
sum(is.na(ny_unemployment_data$estimate))
ny_unemployment_fixed <- ny_unemployment_data %>%
  pivot_wider(names_from = variable, values_from = estimate)
# Check if transformation worked
view(ny_unemployment_fixed)
ny_unemployment_fixed <- ny_unemployment_data %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)
view(ny_unemployment_fixed)
ny_unemployment_fixed <- ny_unemployment_fixed %>%
  filter(!is.na(total_labor_force) & !is.na(unemployed)) %>%
  mutate(unemployment_rate = (unemployed / total_labor_force) * 100)

summary(ny_unemployment_fixed$unemployment_rate)

#Top 10 Counties with Highest Unemployment
ny_unemployment_fixed %>%
  arrange(desc(unemployment_rate)) %>%
  select(NAME, unemployment_rate) %>%
  head(10)
#Top 10 Counties with lowest Unemployment
ny_unemployment_fixed %>%
  arrange(unemployment_rate) %>%
  select(NAME, unemployment_rate) %>%
  head(10)

ggplot(ny_unemployment_fixed, aes(x = reorder(NAME, unemployment_rate), y = unemployment_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip for better readability
  labs(title = "Unemployment Rate by County in NY",
       x = "County", y = "Unemployment Rate (%)") +
  theme_minimal()

ny_combined <- ny_unemployment_fixed %>%
  left_join(ny_income %>% select(GEOID, NAME, median_income = estimate), by = c("GEOID", "NAME"))

summary(ny_combined)
sum(is.na(ny_combined$median_income))  # Check for missing income data
sum(is.na(ny_combined$unemployment_rate))  # Check for missing unemployment rate

ggplot(ny_combined, aes(x = median_income, y = unemployment_rate)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Unemployment Rate vs. Median Household Income",
       x = "Median Household Income ($)",
       y = "Unemployment Rate (%)") +
  theme_minimal()

ggplot(ny_combined, aes(x = reorder(NAME, median_income), y = median_income)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_point(aes(y = unemployment_rate * 2000), color = "red", size = 2) +  # Scale unemployment for visibility
  coord_flip() +
  labs(title = "Median Household Income & Unemployment Rate by County in NY",
       x = "County",
       y = "Income ($) & Unemployment Rate (scaled)") +
  theme_minimal()

cor(ny_combined$median_income, ny_combined$unemployment_rate, use = "complete.obs", method = "pearson")
cor(ny_combined$median_income, ny_combined$unemployment_rate, use = "complete.obs", method = "spearman")

ny_combined %>%
  arrange(desc(unemployment_rate)) %>%
  select(NAME, unemployment_rate, median_income) %>%
  head(10)  # Highest Unemployment

ny_combined %>%
  arrange(unemployment_rate) %>%
  select(NAME, unemployment_rate, median_income) %>%
  head(10)  # Lowest Unemployment

# plot2:Scatter plot for Rent Burden vs Unemployment Rate

# Check the columns in combined dataset
names(ny_combined)

# Check the columns in the rent burden data
names(ny_rent_burden_wide)
head(ny_rent_burden_wide)

# Merge rent burden data with unemployment and median income data
ny_combined <- ny_combined %>%
  left_join(ny_rent_burden_wide %>%
              select(GEOID, NAME, rent_burden_30_35, rent_burden_35_40, rent_burden_40_50, rent_burden_50_plus), 
            by = c("GEOID", "NAME"))

# Check if the rent burden columns exist
names(ny_combined)  # To check that rent_burden_* columns are available

# Now, calculate rent burden percentages
ny_combined <- ny_combined %>%
  mutate(
    rent_burden_30_35_pct = rent_burden_30_35 / renters * 100,
    rent_burden_35_40_pct = rent_burden_35_40 / renters * 100,
    rent_burden_40_50_pct = rent_burden_40_50 / renters * 100,
    rent_burden_50_plus_pct = rent_burden_50_plus / renters * 100
  )

# Check for missing values
summary(ny_combined$rent_burden_30_35_pct)

# Filter out rows with NA values or zero values for better visualization
ny_combined_filtered <- ny_combined %>%
  filter(!is.na(rent_burden_30_35_pct) & rent_burden_30_35_pct > 0)

# Plot the bar chart with correct ordering
ggplot(ny_combined_filtered, aes(x = reorder(NAME, rent_burden_30_35_pct), y = rent_burden_30_35_pct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Rent Burden 30-35% by County in NY", x = "County", y = "Rent Burden 30-35%") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Scale the y-axis as percentages

# Check for missing values
summary(ny_combined$rent_burden_35_40_pct)

# Filter out rows with NA values or zero values for better visualization
ny_combined_filtered <- ny_combined %>%
  filter(!is.na(rent_burden_35_40_pct) & rent_burden_35_40_pct > 0)

# Plot the bar chart with correct ordering
ggplot(ny_combined_filtered, aes(x = reorder(NAME, rent_burden_35_40_pct), y = rent_burden_35_40_pct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Rent Burden 35-40% by County in NY", x = "County", y = "Rent Burden 35-40%") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Scale the y-axis as percentages

# Check for missing values
summary(ny_combined$rent_burden_35_40_pct)

# Filter out rows with NA values or zero values for better visualization
ny_combined_filtered <- ny_combined %>%
  filter(!is.na(rent_burden_40_50_pct) & rent_burden_40_50_pct > 0)

# Plot the bar chart with correct ordering
ggplot(ny_combined_filtered, aes(x = reorder(NAME, rent_burden_40_50_pct), y = rent_burden_40_50_pct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Rent Burden 40-50% by County in NY", x = "County", y = "Rent Burden 40-50%") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Scale the y-axis as percentages

# Scatter plot for Rent Burden 30-35% vs Unemployment Rate
ggplot(ny_combined, aes(x = unemployment_rate, y = rent_burden_30_35_pct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate vs Rent Burden (30-35%)",
       x = "Unemployment Rate (%)", y = "Rent Burden 30-35%") +
  theme_minimal()

# Scatter plot for Rent Burden 35-40% vs Unemployment Rate
ggplot(ny_combined, aes(x = unemployment_rate, y = rent_burden_35_40_pct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate vs Rent Burden (35-40%)",
       x = "Unemployment Rate (%)", y = "Rent Burden 35-40%") +
  theme_minimal()

# Scatter plot for Rent Burden 50%+ vs Unemployment Rate
ggplot(ny_combined, aes(x = unemployment_rate, y = rent_burden_50_plus_pct)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate vs Rent Burden 50%+",
       x = "Unemployment Rate (%)", y = "Rent Burden 50%+") +
  theme_minimal()

#plot3: Poverty Rate vs Unemployment Rate in New York Counties

ny_poverty_data <- get_acs(
  geography = "county",
  state = "NY",
  variables = "B17001_002",  # Poverty rate
  year = 2023,
  survey = "acs5"
)

names(ny_combined_with_poverty)

# Rename or select the correct 'total_population' column
ny_combined_with_poverty <- ny_combined_with_poverty %>%
  rename(total_population = total_population.x)  # Use total_population.x, assuming it's correct

# Now calculate the poverty rate percentage
  mutate(poverty_rate_pct = (poverty_rate / total_population) * 100)

ggplot(ny_combined_with_poverty, aes(x = poverty_rate_pct, y = unemployment_rate)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Poverty Rate vs Unemployment Rate in New York Counties",
       x = "Poverty Rate (%)", y = "Unemployment Rate (%)") +
  theme_minimal()

rent_burden_categories <- c("rent_burden_30_35_pct", "rent_burden_35_40_pct", 
                            "rent_burden_40_50_pct", "rent_burden_50_plus_pct")

for (cat in rent_burden_categories) {
  corr_val <- cor(ny_combined_with_poverty[[cat]], ny_combined_with_poverty$unemployment_rate, 
                  use = "complete.obs", method = "pearson")
  cat("Correlation between", cat, "and unemployment_rate:", corr_val, "\n")
}

ggplot(ny_combined_with_poverty, aes(x = unemployment_rate, y = rent_burden_30_35_pct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate vs Rent Burden 30-35%",
       x = "Unemployment Rate (%)", y = "Rent Burden 30-35 (%)") +
  theme_minimal()

ggplot(ny_combined_with_poverty, aes(x = unemployment_rate, y = rent_burden_50_plus_pct)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate vs Rent Burden 50%+",
       x = "Unemployment Rate (%)", y = "Rent Burden 50%+ (%)") +
  theme_minimal()

ny_combined_with_poverty <- ny_combined_with_poverty %>%
  mutate(overall_rent_burden_pct = (rent_burden_30_35_pct + rent_burden_35_40_pct +
                                      rent_burden_40_50_pct + rent_burden_50_plus_pct) / 4)

ny_combined_with_poverty <- ny_combined_with_poverty %>%
  mutate(poverty_rate_pct = (poverty_rate / total_population) * 100)

ggplot(ny_combined_with_poverty, aes(x = log(poverty_rate), y = overall_rent_burden_pct)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Log(Poverty Count) vs Overall Rent Burden in NY Counties",
       x = "Log(Poverty Count)", y = "Overall Rent Burden (%)") +
  theme_minimal()

install.packages("gridExtra")
library(gridExtra)

install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

ny_combined_long <- ny_combined_with_poverty %>%
  select(NAME, rent_burden_30_35_pct, rent_burden_35_40_pct, rent_burden_40_50_pct, rent_burden_50_plus_pct) %>%
  pivot_longer(cols = starts_with("rent_burden"),
               names_to = "rent_burden_category",
               values_to = "percentage")

poverty_plot <- ggplot(ny_combined_with_poverty, aes(x = reorder(NAME, poverty_rate), y = poverty_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Poverty Rate Distribution by County in New York",
       x = "County", y = "Poverty Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  

rent_plot_all_levels <- ggplot(ny_combined_long, aes(x = reorder(NAME, percentage), y = percentage, fill = rent_burden_category)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Rent Burden Distribution by County in New York",
       x = "County", y = "Rent Burden Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightyellow", "darkorange"))

print(poverty_plot)  
print(rent_plot_all_levels) 

poverty_rent_table <- ny_combined_with_poverty %>%
  select(NAME, poverty_rate_pct, rent_burden_30_35_pct, rent_burden_35_40_pct, rent_burden_40_50_pct, rent_burden_50_plus_pct)

view(poverty_rent_table)

