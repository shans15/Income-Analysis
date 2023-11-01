library(tidyr)
library(dplyr)
library(ggplot2)

#set this as a new variable
dataSet <- Income_Data

#(b)
dataSet$state <- as.factor(dataSet$state)
dataSet$county <- as.factor(dataSet$county)
dataSet$county_FIPS <- as.factor(dataSet$county_FIPS)

summary(select(dataSet, state, per_capita_personal_income_2020))

#(c)
df_summary <- dataSet %>% 
  select_if(is.numeric) %>% 
  summarise_all(list(mean = mean, sd = sd))

#(d)
dataSet$Population <- dataSet$bachelor_degree_numbers_2020 / 
  (dataSet$bachelor_degree_percentage_2020 / 100)

#Income_Data_With_Population <- Income_Data %>% 
#  mutate(Population = bachelor_degree_numbers_2020 / (bachelor_degree_percentage_2020 / 100))

#(e)
df_state <- dataSet %>%
  group_by(state) %>%
  summarize(Population = sum(Population),
            degree_holders = sum(bachelor_degree_numbers_2020))

# Calculate the average percentage of individuals with a bachelors degree
df_state$percentage <- (df_state$degree_holders / df_state$Population) * 100

df_state <- df_state %>% arrange(desc(percentage))

ggplot(df_state, aes(x = reorder(state,desc(percentage)), y = percentage)) +
  geom_bar(stat = "identity") +
  xlab("State") +
  ylab("Average Percentage of Individuals with a Bachelors Degree") +
  ggtitle("Average Percentage of Individuals with a Bachelors Degree by State in 2020")


#(f)
ggplot(dataSet,aes(x = bachelor_degree_percentage_2020, y = per_capita_personal_income_2020)) + 
  geom_point() + scale_y_log10() + 
  xlab("Bachelor Degree Percentage 2020") +
  ylab("Per Capita Personal Income 2020") +
  ggtitle("Scatterplot of Per Capita Personal Income 2020 vs Bachelor Degree Percentage 2020")

#(g)

# Calculate total population for each state
df_state_population <- dataSet %>% 
  group_by(state) %>% 
  summarize(Population = sum(Population))

# Get the top 10 states by population
top_10_states <- df_state_population %>% 
  arrange(desc(Population)) %>% 
  top_n(10, Population) %>% 
  pull(state)

# Filter data for only the top 10 states
df_top_10_states <- dataSet %>% 
  filter(state %in% top_10_states)

# Calculate per capita personal income for each county
df_top_10_states$per_capita_income <- df_top_10_states$per_capita_personal_income_2020 #/ df_top_10_states$Population

# Get the county with the highest per capita income for each state
df_highest_income_county <- df_top_10_states %>% 
  group_by(state) %>% 
  mutate(Rank = dense_rank(desc(per_capita_income))) %>% 
  filter(Rank == 1) %>% 
  select(state, county, per_capita_income)
