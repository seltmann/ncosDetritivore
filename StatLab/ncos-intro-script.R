# Zach Terner
# June 13, 2019
# CCBER code: Load, filter, run Poisson GLM
# Load packages
library(readr)
library(dplyr)

# Read in the data
# Change this path to wherever you put the ncos-csv file
ncos_csv <- read_csv("Desktop/ucsb-year4/ucsb4-spring/Statlab/ncos-csv.csv")

# Replace all "-" with NA
# This may be unnecessary but before the missing values were recorded as "-"
ncos_na <- na_if(ncos_csv, "-")

# Filter so that we only have Collembola
# Use mutate to make a new variable that combines habitat with trap
# This variable is called habitat_trap_all
ncos_col2 <- ncos_csv %>%
  filter(Order == "Collembola") %>%
  mutate(habitat_trap_all = paste0(Habitat, Trap)) 

# Next, we group by TrapID and Date so that we have one row for every TrapID and Date combination.
# This keeps separate traps separate since they vary in space.
# We make the following variables:
# sumcount is sum of the counts of Collembola at that TrapID/Date
# habitat_trap is the habitat/trap combination (for one of the models we wrote).
# This is because we want to know how habitat/trap combos affect the count.
# first_trap is to record the trap type. 
# (I use "first" to grab one of them since it doesn't change in the grouping.)
# Same with date, ht (habitat_trap), habitat, and trapping duration
# Last, create count_dur to be the count divided by the duration.
# count_dur will be our response variable.
ncos_col3 <- ncos_col2 %>%
  group_by(TrapID, Date) %>%
  summarise(sumcount = sum(Count), 
            habitat_trap = first(habitat_trap_all),
            first_habitat = first(Habitat),
            first_trap = first(Trap),
            first_date = first(Date),
            first_ht = first(habitat_trap_all),
            first_dur = first(Trapping_Duration),
            count_dur = sumcount/first_dur)

# Preview the count_dur variable
head(ncos_col3$count_dur)

# Fit a Poisson GLM using just trap and date
mod1 <- glm(count_dur ~ first_trap + first_date, data = ncos_col3, family = "poisson")
summary(mod1)

# Plot basic diagnostics
par(mfrow = c(2,2))
plot(mod1)

# Fit a Poisson GLM using habitat_trap and date
mod2 <- glm(count_dur ~ first_ht + first_date, data = ncos_col3, family = "poisson")
summary(mod2)

# Plot basic diagnostics
par(mfrow = c(2,2))
plot(mod2)

# Fit a poisson GLM using date, habitat, and trap, and the habitat/trap interaction
# This exactly the same as mod2 above, but now we see the main effect of trap is insignificant
# It is also insignificant in mod1 
mod3 <- glm(count_dur ~ first_trap*first_habitat + first_date, data = ncos_col3, family = "poisson")
summary(mod3)

# Plots are exactly the same too
par(mfrow = c(2,2))
plot(mod3)

# The second/third model has much lower residual deviance but diagnostics are questionable
# Both models cannot estimate a date effect for may 24-25