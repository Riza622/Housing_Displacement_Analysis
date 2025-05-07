
# =====================================================
# Step 1: Install and Load Required Packages
# =====================================================

install.packages("tidycensus")
library(tidycensus)  
install.packages("tidyverse")
library(tidyverse)

# =====================================================
# Step 2: Set Census API Key
# =====================================================
census_api_key("520ce6a93bbbd7d5df4a88c208774e01e46fc409", install = TRUE, overwrite = TRUE)

# =====================================================
# Step 3: Retrieve State-Level ACS Data
# =====================================================
test_data <- get_acs(
  geography = "state",
  variables = "B19013_001",  # Median household income
  year = 2023,
  survey = "acs5"
)
head(test_data)

install.packages("qs")          
library(qs)

# =====================================================
# Step 4: Retrieve Indiana County-Level ACS Data
# =====================================================
# Retrieve county-level data for the state of Indiana.
acs_county <- get_acs(
  geography = "county",
  state = "IN",
  variables = c(
    total_pop     = "B01003_001",
    median_income = "B19013_001",
    owner_occ     = "B25003_002",
    renter_occ    = "B25003_003"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide"    
)
head(acs_county)

# =====================================================
# Step 5: Retrieve Race Data from ACS
# =====================================================
# Retrieve county-level race data from Table B02001 for Indiana.

race_data <- get_acs(
  geography = "county",
  state = "IN",
  variables = c(
    white = "B02001_002",   # White alone
    black = "B02001_003",   # Black or African American alone
    asian = "B02001_005"    # Asian alone
  ),
  year = 2023,
  survey = "acs5",
  output = "wide"
)
head(race_data)

# =====================================================
# Step 6: Merge Race Data with County-Level ACS Data
# =====================================================

acs_merged <- acs_county %>%
  left_join(race_data, by = "GEOID")
glimpse(acs_merged)

# =====================================================
# Step 7: Retrieve and Merge Data on Children and Seniors
# =====================================================

# Step 7A: Retrieve Children Data (Ages 0-17)
child_vars <- c(
  male_under5   = "B01001_002",
  male_5_9      = "B01001_003",
  male_10_14    = "B01001_004",
  male_15_17    = "B01001_005",
  female_under5 = "B01001_027",
  female_5_9    = "B01001_028",
  female_10_14  = "B01001_029",
  female_15_17  = "B01001_030"
)

child_data <- get_acs(
  geography = "county",
  state = "IN",
  variables = child_vars,
  year = 2023,
  survey = "acs5",
  output = "wide"
)

child_data <- child_data %>%
  mutate(total_children = male_under5E + male_5_9E + male_10_14E + male_15_17E +
           female_under5E + female_5_9E + female_10_14E + female_15_17E) %>%
  select(GEOID, total_children)
head(child_data)

# Step 7B: Retrieve Seniors Data (Ages 65+)
senior_vars <- c(
  male_65_66    = "B01001_020",
  male_67_69    = "B01001_021",
  male_70_74    = "B01001_022",
  male_75_79    = "B01001_023",
  male_80_84    = "B01001_024",
  male_85_over  = "B01001_025",
  female_65_66  = "B01001_044",
  female_67_69  = "B01001_045",
  female_70_74  = "B01001_046",
  female_75_79  = "B01001_047",
  female_80_84  = "B01001_048",
  female_85_over= "B01001_049"
)
senior_data <- get_acs(
  geography = "county",
  state = "IN",
  variables = senior_vars,
  year = 2023,
  survey = "acs5",
  output = "wide"
)

senior_data <- senior_data %>%
  mutate(total_seniors = male_65_66E + male_67_69E + male_70_74E + male_75_79E + 
           male_80_84E + male_85_overE +
           female_65_66E + female_67_69E + female_70_74E + female_75_79E +
           female_80_84E + female_85_overE) %>%
  select(GEOID, total_seniors)
head(senior_data)

# Step 7C: Retrieve Basic County-Level Population Data
acs_county <- get_acs(
  geography = "county",
  state = "IN",
  variables = c(
    total_pop = "B01003_001"  # 总人口
  ),
  year = 2023,
  survey = "acs5",
  output = "wide"
)
head(acs_county)

# Step 7D: Merge Children and Seniors Data with the County-Level Population Data
acs_age_adjusted <- acs_county %>%
  left_join(child_data, by = "GEOID") %>%
  left_join(senior_data, by = "GEOID") %>%
  mutate(
    prop_children = total_children / total_popE,
    prop_seniors  = total_seniors  / total_popE
  )
head(acs_age_adjusted)

# Step 7E: Merge Race Data and Age-Adjusted Data into a Master Dataset

acs_master <- acs_merged %>%
  left_join(
    acs_age_adjusted %>% select(GEOID, total_children, total_seniors, prop_children, prop_seniors),
    by = "GEOID"
  )

glimpse(acs_master)

# ====================================================
# Step 8: Process External Eviction Data
# ====================================================

eviction_file <- "~/Desktop/d5_case_aggregated.qs"
file.exists(eviction_file)

eviction_data <- qs::qread(eviction_file)
dplyr::glimpse(eviction_data)

print(names(eviction_data))

# =====================================================
# Step 9: Construct County-Level GEOID in Eviction Data
# =====================================================

library(tidyverse)
if(all(c("state_code", "county_code") %in% names(eviction_data))) {
  eviction_data <- eviction_data %>%
    mutate(county_geoid = paste0(state_code, county_code))
  cat("County-level GEOID has been constructed, sample data:\n")
  print(head(eviction_data$county_geoid))
} else {
  stop("eviction_data is missing 'state_code' or 'county_code' fields. Please check the dataset!")
}

# =====================================================
# Step 10: Aggregate Eviction Data at the County Level
# =====================================================
eviction_county <- eviction_data %>%
  filter(year == 2022) %>%
  group_by(county_geoid) %>%
  summarize(
    total_evictions = sum(filings, na.rm = TRUE),
    total_renters_ev = first(co_totrent)
  ) %>%
  ungroup() %>%
  mutate(
    eviction_rate = total_evictions / total_renters_ev
  )

print(eviction_county)

# =====================================================
# Step 11: Merge County-Level Eviction Data with ACS Master Data
# =====================================================
acs_master <- acs_master %>%
  left_join(eviction_county, by = c("GEOID" = "county_geoid"))

dplyr::glimpse(acs_master)

# =====================================================
# Step 12: Construct ACS-Based Displacement Risk Indicator
# =====================================================
acs_master <- acs_master %>%
  mutate(
    eviction_rate_acs = total_evictions / renter_occE,
    risk_flag = ifelse(eviction_rate_acs > 0.05, 1, 0)
  )

table(acs_master$risk_flag, useNA = "ifany")


# =====================================================
# Step 13: Calculate Risk Proportions for Different Population Groups
# =====================================================

# Calculate the "at-risk" population for each group in each county.
acs_master <- acs_master %>%
  mutate(
    renters_at_risk  = renter_occE * risk_flag,
    owners_at_risk   = owner_occE  * risk_flag,
    white_at_risk    = whiteE      * risk_flag,
    black_at_risk    = blackE      * risk_flag,
    asian_at_risk    = asianE      * risk_flag,
    children_at_risk = total_children * risk_flag,
    seniors_at_risk  = total_seniors  * risk_flag
  )

# Sum the total population for each group across all counties (based on ACS data)
total_renters  <- sum(acs_master$renter_occE, na.rm = TRUE)
total_owners   <- sum(acs_master$owner_occE, na.rm = TRUE)
total_white    <- sum(acs_master$whiteE, na.rm = TRUE)
total_black    <- sum(acs_master$blackE, na.rm = TRUE)
total_asian    <- sum(acs_master$asianE, na.rm = TRUE)
total_children <- sum(acs_master$total_children, na.rm = TRUE)
total_seniors  <- sum(acs_master$total_seniors, na.rm = TRUE)

# Sum the at-risk population (i.e., population in high-risk counties) for each group
risk_renters  <- sum(acs_master$renters_at_risk, na.rm = TRUE)
risk_owners   <- sum(acs_master$owners_at_risk, na.rm = TRUE)
risk_white    <- sum(acs_master$white_at_risk, na.rm = TRUE)
risk_black    <- sum(acs_master$black_at_risk, na.rm = TRUE)
risk_asian    <- sum(acs_master$asian_at_risk, na.rm = TRUE)
risk_children <- sum(acs_master$children_at_risk, na.rm = TRUE)
risk_seniors  <- sum(acs_master$seniors_at_risk, na.rm = TRUE)

# Calculate the risk proportion for each group
prop_renters  <- risk_renters / total_renters
prop_owners   <- risk_owners / total_owners
prop_white    <- risk_white / total_white
prop_black    <- risk_black / total_black
prop_asian    <- risk_asian / total_asian
prop_children <- risk_children / total_children
prop_seniors  <- risk_seniors / total_seniors

# Create risk summary table with English labels
risk_summary <- tibble(
  group = c("Renters", "Homeowners", "White", "Black", "Asian", "Children", "Seniors"),
  total_population = c(total_renters, total_owners, total_white, total_black, total_asian, total_children, total_seniors),
  risk_population = c(risk_renters, risk_owners, risk_white, risk_black, risk_asian, risk_children, risk_seniors),
  risk_proportion = c(prop_renters, prop_owners, prop_white, prop_black, prop_asian, prop_children, prop_seniors)
)

print(risk_summary)

# =====================================================
# Step 14: Visualize Displacement Risk Proportions by Group:
# =====================================================

library(ggplot2)
ggplot(risk_summary, aes(x = group, y = risk_proportion)) +
  geom_segment(aes(x = group, xend = group, y = 0, yend = risk_proportion), color = "grey") +
  geom_point(size = 5, color = "steelblue") +
  labs(title = "Lollipop Chart of Displacement Risk Proportions",
       x = "Population Group",
       y = "Risk Proportion") +
  theme_minimal()


# =====================================================
# Step 15: Visualize County-Level Risk Distribution in Indiana
# =====================================================

# 1. Retrieve Indiana County-Level Spatial Data (sf Format)
library(tigris)
library(sf)
in_counties <- counties(state = "IN", cb = TRUE, class = "sf")

acs_master_sf <- in_counties %>%
  left_join(acs_master, by = "GEOID")

# Verify that the merged object is an sf object
class(acs_master_sf)  

# 4. Visualize the Data Using tmap
library(tmap)
tmap_mode("view")

tm_shape(acs_master_sf) +
  tm_fill(
    col = "risk_flag",               # Color polygons based on risk_flag (0: low risk, 1: high risk)
    title = "Risk Flag",             # Legend title for risk_flag
    palette = c("lightgreen", "red"), # Colors: light green for low risk, red for high risk
    style = "fixed",               
    breaks = c(-0.1, 0.1, 1.1)         # Fixed breaks: values <0.1 are 0, values ≥0.1 are 1
  ) +
  tm_borders() +
  tm_title("Risk Flag Distribution in Indiana Counties", size = 1.2) +
  tm_legend(outside = TRUE)


###############################################
# Step 16: Plot the Distribution of ACS Eviction Rates
###############################################

ggplot(acs_master, aes(x = eviction_rate_acs)) +
  geom_histogram(binwidth = 0.01, fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of ACS Eviction Rates in Indiana Counties",
       x = "Eviction Rate (ACS)",
       y = "Number of Counties") +
  theme_minimal()


library(tidycensus)

# =====================================================
# Step 17: Retrieve Marriage Status Data (Table B12001)
# =====================================================
# B12001_001 represents total households, 
# B12001_002 represents married households.

marriage_vars <- c(
  total_households = "B12001_001",
  married_households = "B12001_002"
)

marriage_data <- get_acs(
  geography = "county",
  state = "IN",
  variables = marriage_vars,
  year = 2023,
  survey = "acs5",
  output = "wide"
)
head(marriage_data)

# Calculate the proportion of non-married households 
# (assuming non-married = total_households - married_households)
marriage_data <- marriage_data %>%
  mutate(prop_nonmarried = (total_householdsE - married_householdsE) / total_householdsE)

head(marriage_data %>% select(GEOID, total_householdsE, married_householdsE, prop_nonmarried))

# Check the resulting columns: GEOID, total_householdsE, married_householdsE, prop_nonmarried
acs_master <- acs_master %>%
  left_join(marriage_data %>% select(GEOID, prop_nonmarried), by = "GEOID")

dplyr::glimpse(acs_master)

names(acs_master)


acs_master <- acs_master %>% rename(prop_nonmarried = prop_nonmarried.y)


# =====================================================
# Step 18: Explore the Relationship between Eviction Risk and Marriage-based Risk
# =====================================================

# Plot a scatterplot comparing the ACS-based eviction risk (eviction_rate_acs) 
# with the marriage-based risk indicator (prop_nonmarried). 
# The blue linear regression line shows the trend between these two risk factors.
ggplot(acs_master, aes(x = eviction_rate_acs, y = prop_nonmarried)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Eviction Risk vs. Marriage-based Risk",
       x = "ACS Eviction Risk (eviction_rate_acs)",
       y = "Proportion of Nonmarried Households (prop_nonmarried)") +
  theme_minimal()


# =====================================================
# Step 19: Categorize Marriage Risk and Plot by Category
# =====================================================

acs_master <- acs_master %>%
  mutate(risk_category = ifelse(prop_nonmarried > median(prop_nonmarried, na.rm = TRUE),
                                "High Marriage Risk", "Low Marriage Risk"))

# Create a scatterplot to compare ACS eviction risk (eviction_rate_acs)
ggplot(acs_master, aes(x = eviction_rate_acs, y = prop_nonmarried, color = risk_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Eviction Risk vs. Marriage-based Risk by Risk Category",
       x = "ACS Eviction Risk (eviction_rate_acs)",
       y = "Proportion of Nonmarried Households") +
  theme_minimal()

# =====================================================
# Step 20:  Compare Eviction Rates by Marriage-based Risk Category
# =====================================================

# Plot a boxplot to compare eviction_rate_acs distributions
# between High Marriage Risk and Low Marriage Risk categories.
ggplot(acs_master, aes(x = risk_category, y = eviction_rate_acs, fill = risk_category)) +
  geom_boxplot() +
  labs(title = "Eviction Rate by Marriage-based Risk Category",
       x = "Marriage-based Risk Category",
       y = "Eviction Rate (ACS)") +
  theme_minimal()


prop_renters_ev <- sum(acs_master$renters_at_risk_eviction, na.rm = TRUE) / sum(acs_master$renter_occE, na.rm = TRUE)
prop_owners_ev  <- sum(acs_master$owners_at_risk_eviction, na.rm = TRUE)  / sum(acs_master$owner_occE, na.rm = TRUE)
prop_white_ev   <- sum(acs_master$white_at_risk_eviction, na.rm = TRUE)   / sum(acs_master$whiteE, na.rm = TRUE)
prop_black_ev   <- sum(acs_master$black_at_risk_eviction, na.rm = TRUE)   / sum(acs_master$blackE, na.rm = TRUE)
prop_asian_ev   <- sum(acs_master$asian_at_risk_eviction, na.rm = TRUE)   / sum(acs_master$asianE, na.rm = TRUE)
prop_children_ev <- sum(acs_master$children_at_risk_eviction, na.rm = TRUE) / sum(acs_master$total_children, na.rm = TRUE)
prop_seniors_ev  <- sum(acs_master$seniors_at_risk_eviction, na.rm = TRUE)  / sum(acs_master$total_seniors, na.rm = TRUE)

prop_renters_mar <- sum(acs_master$renters_at_risk_marriage, na.rm = TRUE) / sum(acs_master$renter_occE, na.rm = TRUE)
prop_owners_mar  <- sum(acs_master$owners_at_risk_marriage, na.rm = TRUE)  / sum(acs_master$owner_occE, na.rm = TRUE)
prop_white_mar   <- sum(acs_master$white_at_risk_marriage, na.rm = TRUE)   / sum(acs_master$whiteE, na.rm = TRUE)
prop_black_mar   <- sum(acs_master$black_at_risk_marriage, na.rm = TRUE)   / sum(acs_master$blackE, na.rm = TRUE)
prop_asian_mar   <- sum(acs_master$asian_at_risk_marriage, na.rm = TRUE)   / sum(acs_master$asianE, na.rm = TRUE)
prop_children_mar <- sum(acs_master$children_at_risk_marriage, na.rm = TRUE) / sum(acs_master$total_children, na.rm = TRUE)
prop_seniors_mar  <- sum(acs_master$seniors_at_risk_marriage, na.rm = TRUE)  / sum(acs_master$total_seniors, na.rm = TRUE)


risk_summary_combined <- tibble(
  group = c("Renters", "Homeowners", "White", "Black", "Asian", "Children", "Seniors"),
  eviction_risk = c(prop_renters_ev, prop_owners_ev, prop_white_ev, prop_black_ev, prop_asian_ev, prop_children_ev, prop_seniors_ev),
  marriage_risk = c(prop_renters_mar, prop_owners_mar, prop_white_mar, prop_black_mar, prop_asian_mar, prop_children_mar, prop_seniors_mar)
)


risk_summary_long <- risk_summary_combined %>%
  pivot_longer(cols = c(eviction_risk, marriage_risk),
               names_to = "risk_factor",
               values_to = "risk_proportion")

ggplot(risk_summary_long, aes(x = group, y = risk_proportion, fill = risk_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Displacement Risk Proportions by Group",
       x = "Population Group",
       y = "Risk Proportion",
       fill = "Risk Factor") +
  scale_fill_manual(values = c("eviction_risk" = "steelblue", "marriage_risk" = "orange"),
                    labels = c("Eviction-based Risk", "Marriage-based Risk")) +
  theme_minimal()


acs_master <- acs_master %>%
  mutate(income_group = case_when(
    median_incomeE < quantile(median_incomeE, 0.33, na.rm = TRUE) ~ "Low Income",
    median_incomeE < quantile(median_incomeE, 0.67, na.rm = TRUE) ~ "Middle Income",
    TRUE ~ "High Income"
  ))

ggplot(acs_master, aes(x = income_group, y = prop_nonmarried.x, fill = income_group)) +
  geom_boxplot() +
  labs(title = "Distribution of Nonmarried Households by Income Group",
       x = "Income Group",
       y = "Proportion of Nonmarried Households") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

acs_master <- acs_master %>%
  mutate(
    renters_risk_marriage = renter_occE * prop_nonmarried.x,
    owners_risk_marriage  = owner_occE  * prop_nonmarried.x,
    white_risk_marriage   = whiteE      * prop_nonmarried.x,
    black_risk_marriage   = blackE      * prop_nonmarried.x,
    asian_risk_marriage   = asianE      * prop_nonmarried.x,
    children_risk_marriage = total_children * prop_nonmarried.x,
    seniors_risk_marriage  = total_seniors  * prop_nonmarried.x
  )

total_renters_mar <- sum(acs_master$renter_occE, na.rm = TRUE)
risk_renters_mar  <- sum(acs_master$renters_risk_marriage, na.rm = TRUE)
prop_renters_mar  <- risk_renters_mar / total_renters_mar

total_owners_mar <- sum(acs_master$owner_occE, na.rm = TRUE)
risk_owners_mar  <- sum(acs_master$owners_risk_marriage, na.rm = TRUE)
prop_owners_mar  <- risk_owners_mar / total_owners_mar

total_white_mar  <- sum(acs_master$whiteE, na.rm = TRUE)
risk_white_mar   <- sum(acs_master$white_risk_marriage, na.rm = TRUE)
prop_white_mar   <- risk_white_mar / total_white_mar

total_black_mar  <- sum(acs_master$blackE, na.rm = TRUE)
risk_black_mar   <- sum(acs_master$black_risk_marriage, na.rm = TRUE)
prop_black_mar   <- risk_black_mar / total_black_mar

total_asian_mar  <- sum(acs_master$asianE, na.rm = TRUE)
risk_asian_mar   <- sum(acs_master$asian_risk_marriage, na.rm = TRUE)
prop_asian_mar   <- risk_asian_mar / total_asian_mar

total_children_mar <- sum(acs_master$total_children, na.rm = TRUE)
risk_children_mar  <- sum(acs_master$children_risk_marriage, na.rm = TRUE)
prop_children_mar  <- risk_children_mar / total_children_mar

total_seniors_mar <- sum(acs_master$total_seniors, na.rm = TRUE)
risk_seniors_mar  <- sum(acs_master$seniors_risk_marriage, na.rm = TRUE)
prop_seniors_mar  <- risk_seniors_mar / total_seniors_mar

marriage_risk_summary <- tibble(
  group = c("Renters", "Homeowners", "White", "Black", "Asian", "Children", "Seniors"),
  total_population = c(total_renters_mar, total_owners_mar, total_white_mar,
                       total_black_mar, total_asian_mar, total_children_mar, total_seniors_mar),
  risk_population = c(risk_renters_mar, risk_owners_mar, risk_white_mar,
                      risk_black_mar, risk_asian_mar, risk_children_mar, risk_seniors_mar),
  risk_proportion = c(prop_renters_mar, prop_owners_mar, prop_white_mar,
                      prop_black_mar, prop_asian_mar, prop_children_mar, prop_seniors_mar)
)

print(marriage_risk_summary)

ggplot(marriage_risk_summary, aes(x = group, y = risk_proportion, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(risk_proportion, accuracy = 0.1)),
            vjust = -0.3, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Marriage-based Displacement Risk Proportions by Group",
       x = "Population Group",
       y = "Risk Proportion (Marriage-based)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")











