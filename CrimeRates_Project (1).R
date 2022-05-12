###############################
# Crime Rates Project

rm(list=ls())

###############################
# Load Libraries
pacman::p_load(dplyr, tidyr, caret, ggplot2, caTools, MLmetrics, mlbench, mlTools, corrplot, expss, PerformanceAnalytics, 
               AER, MASS, stargazer, pscl, jtools, Hmisc, ggcorrplot, rpart, rpart.plot, readxl, ROCR, lme4)

##############################
# Load crime data set
crime_df <- read.csv("C:/Users/Scott/Documents/Shyam_Personal/SDM Assignment/Project/combined_crime_data.csv")

# Load expenditure data set
exp_df <- read.csv('C:/Users/Scott/Documents/Shyam_Personal/SDM Assignment/Project/Expenditure_2010-2017_Updated_New.csv')

##############################
# Merge

# Ensure datatypes are matching for joining
str(crime_df)
str(exp_df)

# Convert the Year column to Integer for joining
crime_df$year <- as.integer(crime_df$year)

# Merge the two datasets
df <- merge(crime_df, exp_df, by=c("state", "city", "year"))

# Check for any NA values
colSums(is.na(df))

df <- df[complete.cases(df$forcible_rape), ]         # Drop incomplete rows

############################
# Clean datasets
str(df)

# Convert values to factors
cols_factor <- c("state", "city", "year")
df[cols_factor] <- (lapply(df[cols_factor], factor))

levels(df$year)

# convert variables to integer
cols_num <- c("population", "violent_crime", "non_violent_crime", "murder_and_nonnegligent_manslaughter", "forcible_rape", "robbery", "aggravated_assault", "property_crime", "burglary", "larceny_theft", "motor_vehicle_theft")
df[cols_num] <- (lapply(df[cols_num], as.integer))

# Create a calculated columns to see the total crimes in each city for that year
total_crimes <- df$violent_crime + df$non_violent_crime

# Create a new dataframe only with the columns that will be used
df_m <- subset(df, select = c(-population, -murder_and_nonnegligent_manslaughter, -larceny_theft, -motor_vehicle_theft, -forcible_rape, -robbery, -aggravated_assault, -property_crime, -burglary, -parks_and_recreation_exp, -general_public_buildings_exp))

str(df_m)

# Create a variable to get the total expenditure 
total_expenditure <- df$education_services_exp + df$public_welfare_exp + df$hospital_exp + df$health_exp +
  df$police_protection_exp + df$fire_protection_exp + df$corrections_exp + df$inspection_and_regulation + 
  df$housing_and_community_development_exp + df$financial_administration_exp + df$judicial_and_legal_exp + 
  df$other_governmental_admin_exp

#############################
# Visualization and Descriptive analysis

hist(df_m$violent_crime)
hist(log(df_m$violent_crime))

hist(df$non_violent_crime)
hist(log(df_m$non_violent_crime))

hist(total_crimes)
hist(log(total_crimes))

bwplot(population ~ year, data=df)
bwplot(violent_crime ~ year, data = df)
bwplot(non_violent_crime ~ year, data = df)

plot(population ~ total_expenditure, data = df)
plot(violent_crime ~ total_expenditure, data = df)
plot(non_violent_crime ~ total_expenditure, data = df)

plot(total_crimes~ total_expenditure, data = df)
bwplot(violent_crime + non_violent_crime ~ year, data = df)
hist(total_crimes)
hist(log(total_crimes))

chart.Correlation(df_m[as.integer(which(sapply(df_m,class)=="integer"))]) # Plot for numeric variables

### Descriptive Analysis
# Sum for Totals
sum(total_expenditure)

sum(df_m$violent_crime)

sum(df$non_violent_crime)

sum(total_crimes)

# Summary
summary(total_expenditure)

summary(df_m$violent_crime)

summary(total_crimes)

summary(df$non_violent_crime)

# Summary by Year
summary(df_m$violent_crime ~ df_m$year)

summary(df_m$non_violent_crime ~ df_m$year)

summary(total_crimes ~ df_m$year)

summary(total_expenditure ~ df_m$year)

############################
# Models

################
# Violent crimes

# Baseline 
vio_b <- lmer(log(violent_crime) ~ 1 + (1 | city) + (1 | state), data=df_m, REML=FALSE)
summary(vio_b)

# Model using mixed level
vio_m1 <- lmer(log(violent_crime) ~ education_services_exp + public_welfare_exp + hospital_exp + health_exp +
                 police_protection_exp + fire_protection_exp + corrections_exp + inspection_and_regulation + 
                 housing_and_community_development_exp + financial_administration_exp + judicial_and_legal_exp + 
                 other_governmental_admin_exp + year + (1 | city), data=df_m, REML=FALSE)
summary(vio_m1)
confint(vio_m1)
AIC(vio_m1)
fixef(vio_m1)                                       # Magnitude of fixed effect
ranef(vio_m1)                                       # Magnitude of random effect
coef(vio_m1)                                        # Magnitude of total effect

# Model using poisson method
vio_m2 <- glm(violent_crime ~ education_services_exp + public_welfare_exp + hospital_exp + health_exp +
                police_protection_exp + fire_protection_exp + corrections_exp + inspection_and_regulation + 
                housing_and_community_development_exp + financial_administration_exp + judicial_and_legal_exp + 
                other_governmental_admin_exp, data=df, family = poisson (link = log))
summary(vio_m2)

# Negative Binomial
vio_m3 <- glm.nb(violent_crime ~ education_services_exp + public_welfare_exp + hospital_exp + health_exp +
                   police_protection_exp + fire_protection_exp + corrections_exp + inspection_and_regulation + 
                   housing_and_community_development_exp + financial_administration_exp + judicial_and_legal_exp + 
                   other_governmental_admin_exp + year, data=df)
summary(vio_m3)

# Stargazer
stargazer(vio_m1, vio_m3, type="text", single.row=TRUE)

################
# Non-Violent crimes

# Baseline 
non_vio_b <- lmer(log(non_violent_crime) ~ 1 + (1 | city), data=df, REML=FALSE)
summary(non_vio_b)

# Model using mixed level
non_vio_m1 <- lmer(log(non_violent_crime) ~ education_services_exp + public_welfare_exp + hospital_exp + health_exp +
                   police_protection_exp + fire_protection_exp + corrections_exp + inspection_and_regulation + 
                   housing_and_community_development_exp + financial_administration_exp + judicial_and_legal_exp + 
                   other_governmental_admin_exp + year + (1 | city), data=df, REML=FALSE)
summary(non_vio_m1)
confint(non_vio_m1)
AIC(non_vio_m1)
fixef(non_vio_m1)                                       # Magnitude of fixed effect
ranef(non_vio_m1)                                       # Magnitude of random effect
coef(non_vio_m1)                                        # Magnitude of total effect

# Model using poisson method
m3 <- glm(non_violent_crime ~ capital_outlay + education_exp + higher_education_exp + elementary_and_secondary_exp + 
            hospitals_exp + highways_exp + correction_exp + 
            natural_resources_exp + parks_and_recreation_exp + sewerage_exp +
            solid_waste_management_exp + other_general_exp + utility_capital_outlay, data=df, family = poisson (link = log))
summary(m3)

# Negative Binomial
non_vio_m3 <- glm.nb(non_violent_crime ~education_services_exp + public_welfare_exp + hospital_exp + health_exp +
               police_protection_exp + fire_protection_exp + corrections_exp + inspection_and_regulation + 
               housing_and_community_development_exp + financial_administration_exp + judicial_and_legal_exp + 
               other_governmental_admin_exp + year, data=df)
summary(non_vio_m3)

# Stargazer
stargazer(non_vio_m2, non_vio_m3, type='text', single.row = TRUE)

###########################
# Stargazer between the two variable types
options(max.print=10000)

stargazer(vio_m1, non_vio_m1, type="text", title = "Crime Rate Analysis", dep.var.labels = c("Violent Crime", "Non-Violent Crime"),
          covariate.labels = c("Education Services", "Public Welfare", "Hospital", "Health", "Police Protection", "Fire Protection", 
                               "Correction", "Inspection and Regulation", "Housing and Community Dev", "Financial Administration",
                               "Judicial and Legal", "Other Governmental Admin", "Year 2011", "Year 2012", "Year 2013", "Year 2014", 
                               "Year 2015", "Year 2016", "Year 2017"),
          align=TRUE, single.row=TRUE, no.space = TRUE, out = "crime_rate_analysis.html")
