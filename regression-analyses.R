# Title -------------------------------------------------------------------

#  Regression analyses


# Packages ----------------------------------------------------------------

library(dplyr)
library(broom)

# Functions ---------------------------------------------------------------

standardize <- function(data){
  z <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  return(z)
}

# Collect and store logistic regression coefficients (10 coefficients inlcuding the intercept)
reg_coef <- function(log_reg){
  b0 <- log_reg[1] # intercept
  SAT.z <- log_reg[2]
  Asian <- log_reg[3]
  AA <- log_reg[4]
  Hispanic <- log_reg[5]
  International <- log_reg[6]
  SAT.z.Asian <- log_reg[7]
  SAT.z.AA <- log_reg[8]
  SAT.z.Hispanic <- log_reg[9]
  SAT.z.International <- log_reg[10]
  
  return(list(b0, SAT.z, Asian, AA, Hispanic, International, SAT.z.Asian, SAT.z.AA, SAT.z.Hispanic, SAT.z.International))
}

# Collect and store logistic regression coefficients (10 coefficients inlcuding the intercept)
# Set insignificant coefficients to 0
reg_coef_p <- function(log_reg, p_values){
  
  # Change p_values to 1 if significant and 0 if not significant
  p_values_binary <- replace(p_values, p_values <= 0.05, 1)
  p_values_binary <- replace(p_values_binary, p_values_binary != 1, 0)
  
  # Extract coefficients and multiply by binary p values
  b0 <- log_reg[1] * p_values_binary[1] # intercept
  SAT.z <- log_reg[2] * p_values_binary[2]
  Asian <- log_reg[3] * p_values_binary[3]
  AA <- log_reg[4] * p_values_binary[4]
  Hispanic <- log_reg[5] * p_values_binary[5]
  International <- log_reg[6] * p_values_binary[6]
  SAT.z.Asian <- log_reg[7] * p_values_binary[7]
  SAT.z.AA <- log_reg[8] * p_values_binary[8]
  SAT.z.Hispanic <- log_reg[9] * p_values_binary[9]
  SAT.z.International <- log_reg[10] * p_values_binary[10]
  
  return(list(b0, SAT.z, Asian, AA, Hispanic, International, SAT.z.Asian, SAT.z.AA, SAT.z.Hispanic, SAT.z.International))
  
}

# Create plots of the logistic regression model
log_plot <- function(cf, SAT_range, x_title, y_title, plot_title, SAT_avg){
  # Calculate the logits and convert to probabilities for each ethnicity
  white_logits <- cf[[1]] + cf[[2]]*SAT_range + cf[[3]]*0 + cf[[4]]*0 + cf[[5]]*0 + cf[[6]]*0 + cf[[7]]*0 + cf[[8]]*0 + cf[[9]]*0 + cf[[10]]*0
  white_probs <- exp(white_logits) / (1 + exp(white_logits))
  asian_logits <- cf[[1]] + cf[[2]]*SAT_range + cf[[3]]*1 + cf[[4]]*0 + cf[[5]]*0 + cf[[6]]*0 + cf[[7]]*1 + cf[[8]]*0 + cf[[9]]*0 + cf[[10]]*0
  asian_probs <- exp(asian_logits) / (1 + exp(asian_logits))
  aa_logits <- cf[[1]] + cf[[2]]*SAT_range + cf[[3]]*0 + cf[[4]]*1 + cf[[5]]*0 + cf[[6]]*0 + cf[[7]]*0 + cf[[8]]*1 + cf[[9]]*0 + cf[[10]]*0
  aa_probs <- exp(aa_logits) / (1 + exp(aa_logits))
  hispanic_logits <- cf[[1]] + cf[[2]]*SAT_range + cf[[3]]*0 + cf[[4]]*0 + cf[[5]]*1 + cf[[6]]*0 + cf[[7]]*0 + cf[[8]]*0 + cf[[9]]*1 + cf[[10]]*0
  hispanic_probs <- exp(hispanic_logits) / (1 + exp(hispanic_logits))
  international_logits <- cf[[1]] + cf[[2]]*SAT_range + cf[[3]]*0 + cf[[4]]*0 + cf[[5]]*0 + cf[[6]]*1 + cf[[7]]*0 + cf[[8]]*0 + cf[[9]]*0 + cf[[10]]*1
  international_probs <- exp(international_logits) / (1 + exp(international_logits))
  
  # Store data for plotting purposes
  plot.data <- data.frame(white = white_probs, asian = asian_probs, aa = aa_probs, hispanic = hispanic_probs, international = international_probs, SAT = SAT_range)
  plot.data <- gather(plot.data, key = group, value = prob, white:international)
  
  # Create plots
  ggplot(plot.data, aes(x = SAT, y = prob, color = group)) +
    geom_line(lwd = 1) +
    labs(x = x_title, y = y_title, title = plot_title) +
    geom_vline(xintercept = SAT_avg)
}
  

# Data selection ----------------------------------------------------------

# Filter student data for new students or transfer students only
# Create a column for ethnicity as a factor
students_ethnicity_scope <- students %>% 
  filter(profile_admissions_population == "B" | profile_admissions_population == "SB" | profile_admissions_population == "X"
         | profile_admissions_population == "T" | profile_admissions_population == "ST") %>% 
  filter(profile_reporting_ethnicity == "Asian" | profile_reporting_ethnicity == "Black or African American" |
           profile_reporting_ethnicity == "Hispanic/Latino" | profile_reporting_ethnicity == "International" |
           profile_reporting_ethnicity == "White") %>% 
  mutate(ethnicity.f = as_factor(profile_reporting_ethnicity))

# Define levels with White students as the reference cateogry (#1)
levels(students_ethnicity_scope$ethnicity.f) <- c("White", "Asian", "Black or African American", 
                                                  "Hispanic/Latino", "International")


# Standardize test scores -------------------------------------------------

# Standarize all new SAT scores and one year overall GPA
SAT_total_new.z <- standardize(students_ethnicity_scope$SAT_total_new)
SAT_ERW_new.z <- standardize(students_ethnicity_scope$SAT_ERW_new)
SAT_math_new.z <- standardize(students_ethnicity_scope$SAT_math_new)
one_year_overall_gpa.z <- standardize(students_ethnicity_scope$one_year_overall_gpa)

# Combine standardized scores with the data
students_ethnicity_scope <- cbind(students_ethnicity_scope, 
                                  SAT_ERW_new.z,
                                  SAT_math_new.z, 
                                  SAT_total_new.z,
                                  one_year_overall_gpa.z)


# Linear regression models for one year overall GPA -----------------------

# Create a data frame for 1-year retention analysis
students_ethnicity_scope_1 <- students_ethnicity_scope %>% 
  filter(profile_academic_period != 201920)

# Create a data frame for 6-year graduation analysis
students_ethnicity_scope_6 <- students_ethnicity_scope %>% 
  filter(profile_academic_period <= 201420)
  
# Linear regression model with ethnicity predicting one year overall GPA
reg.model.ethnicity.gpa1 <- lm(one_year_overall_gpa.z ~ ethnicity.f, data = students_ethnicity_scope_1)
summary(reg.model.ethnicity.gpa1)

# Linear regression model with SAT scores predicting one year overall GPA
reg.model.sat.total.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_total_new.z, 
                               data = students_ethnicity_scope_1)
summary(reg.model.sat.total.gpa1)

reg.model.sat.ERW.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_ERW_new.z, 
                             data = students_ethnicity_scope_1)
summary(reg.model.sat.ERW.gpa1)

reg.model.sat.math.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_math_new.z, 
                              data = students_ethnicity_scope_1)
summary(reg.model.sat.math.gpa1)

reg.model.sat.combined.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_ERW_new.z + SAT_math_new.z, 
                                  data = students_ethnicity_scope_1)
summary(reg.model.sat.combined.gpa1)

# Linear regression model with SAT scores and ethnicity predicting one year overall GPA
reg.model.sat.ethnicity.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_total_new.z + ethnicity.f, 
                                   data = students_ethnicity_scope_1)
summary(reg.model.sat.ethnicity.gpa1)

# Linear regression model with SAT scores and ethnicity predicting one year overall GPA including an interaction effect between ethnicity and SAT
reg.model.sat.ethnicity.i.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_total_new.z + ethnicity.f + SAT_total_new.z:ethnicity.f, 
                                data = students_ethnicity_scope_1)
summary(reg.model.sat.ethnicity.i.gpa1)

# Linear regression model with SAT scores, ethnicity, and gender predicting one year overall GPA
reg.model.sat.ethnicity.gender.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_total_new.z + ethnicity.f + profile_gender_desc, 
                                          data = students_ethnicity_scope_1)
summary(reg.model.sat.ethnicity.gender.gpa1)


# Logistic regression models for one year retention -----------------------

# Estimate the null model for model fit statistics
nullmodel.1 <- glm(one_year_retention_flag ~ 1, 
                   data = students_ethnicity_scope_1, 
                   family=binomial(link="logit"))
n.1 <- sum(!is.na(students_ethnicity_scope_1$SAT_total_new))

# Logistic regression model with ethnicity predicting one year retention
logreg.model.ethnicity.1 <- glm(one_year_retention_flag ~ ethnicity.f, 
                              data = students_ethnicity_scope_1, 
                              family=binomial(link="logit"))
summary(logreg.model.ethnicity.1)
# McFadden's R^2
print(1 - logLik(logreg.model.ethnicity.1) / logLik(nullmodel.1))
# Cox and Snell's R^2
print(1 - exp(-2 / nrow(students_ethnicity_scope_1) * (logLik(logreg.model.ethnicity.1) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores predicting one year retention
logreg.model.sat.1 <- glm(one_year_retention_flag ~ SAT_total_new.z, 
                        data = students_ethnicity_scope_1, 
                        family=binomial(link="logit"))
summary(logreg.model.sat.1)
print(1 - logLik(logreg.model.sat.1) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat.1) - logLik(nullmodel.1))))


# Logistic regression model with SAT scores and ethnicity predicting one year retention
logreg.model.sat.ethnicity.1 <- glm(one_year_retention_flag ~ SAT_total_new.z + ethnicity.f, 
                                    data=students_ethnicity_scope_1, 
                                    family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.1)
print(1 - logLik(logreg.model.sat.ethnicity.1) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat.ethnicity.1) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores, ethnicity, and gender predicting one year retention
logreg.model.sat.ethnicity.gender.1 <- glm(one_year_retention_flag ~ SAT_total_new.z + ethnicity.f + profile_gender_desc, 
                                         data=students_ethnicity_scope_1, 
                                         family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.gender.1)
print(1 - logLik(logreg.model.sat.ethnicity.gender.1) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat.ethnicity.gender.1) - logLik(nullmodel.1))))


# Logistic regression models for six year graduation ----------------------

# Estimate the null model for model fit statistics
nullmodel.6 <- glm(six_year_graduation_flag ~ 1, 
                   data = students_ethnicity_scope_6, 
                   family=binomial(link="logit"))
n.6 <- sum(!is.na(students_ethnicity_scope_6$SAT_total_new))

# Logistic regression model with ethnicity predicting six year graduation
logreg.model.ethnicity.6 <- glm(six_year_graduation_flag ~ ethnicity.f, 
                                data = students_ethnicity_scope_6, 
                                family=binomial(link="logit"))
summary(logreg.model.ethnicity.6)
# McFadden's R^2
print(1 - logLik(logreg.model.ethnicity.6) / logLik(nullmodel.6))
# Cox and Snell's R^2
print(1 - exp(-2 / nrow(students_ethnicity_scope_6) * (logLik(logreg.model.ethnicity.6) - logLik(nullmodel.6))))

# Logistic regression model with SAT scores predicting six year graduation
logreg.model.sat.6 <- glm(six_year_graduation_flag ~ SAT_total_new.z, 
                          data = students_ethnicity_scope_6, 
                          family=binomial(link="logit"))
summary(logreg.model.sat.6)
print(1 - logLik(logreg.model.sat.6) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat.6) - logLik(nullmodel.6))))

# Logistic regression model with SAT scores and ethnicity predicting six year graduation
logreg.model.sat.ethnicity.6 <- glm(six_year_graduation_flag ~ SAT_total_new.z + ethnicity.f, 
                                    data=students_ethnicity_scope_6, 
                                    family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.6)
print(1 - logLik(logreg.model.sat.ethnicity.6) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat.ethnicity.6) - logLik(nullmodel.6))))

# Logistic regression model with SAT scores, ethnicity, and gender predicting six year graduation
logreg.model.sat.ethnicity.gender.6 <- glm(six_year_graduation_flag ~ SAT_total_new.z + ethnicity.f + profile_gender_desc, 
                                           data=students_ethnicity_scope_6, 
                                           family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.gender.6)
print(1 - logLik(logreg.model.sat.ethnicity.gender.6) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat.ethnicity.gender.6) - logLik(nullmodel.6))))


# Regression models with SAT-ethnicity interaction effects ----------------

# Linear regression model with SAT scores and ethnicity predicting one year overall GPA including interaction effects
reg.model.sat.ethnicity.gpa1.int <- lm(one_year_overall_gpa.z ~ SAT_total_new.z + ethnicity.f + SAT_total_new.z:ethnicity.f, 
                                   data = students_ethnicity_scope_1)
summary(reg.model.sat.ethnicity.gpa1.int)

# Logistic regression model with SAT scores and ethnicity predicting one year retention including interaction effects
logreg.model.sat.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_total_new.z + ethnicity.f + SAT_total_new.z:ethnicity.f, 
                                    data=students_ethnicity_scope_1, 
                                    family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.1.int)
print(1 - logLik(logreg.model.sat.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores and ethnicity predicting six year graduation including interaction effects
logreg.model.sat.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_total_new.z + ethnicity.f + SAT_total_new.z:ethnicity.f, 
                                    data=students_ethnicity_scope_6, 
                                    family=binomial(link="logit"))
summary(logreg.model.sat.ethnicity.6.int)
print(1 - logLik(logreg.model.sat.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat.ethnicity.6.int) - logLik(nullmodel.6))))


# Regression models split by math SAT and ERW SAT -------------

# Linear regression model with SAT scores (math and ERW) and ethnicity predicting one year overall GPA
reg.model.sat_sep.ethnicity.gpa1 <- lm(one_year_overall_gpa.z ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f, 
                                           data = students_ethnicity_scope_1)
summary(reg.model.sat_sep.ethnicity.gpa1)

# Logistic regression model with SAT scores (math and ERW) and ethnicity predicting one year retention
logreg.model.sat_sep.ethnicity.1 <- glm(one_year_retention_flag ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f, 
                                            data=students_ethnicity_scope_1, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_sep.ethnicity.1)
print(1 - logLik(logreg.model.sat_sep.ethnicity.1) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_sep.ethnicity.1) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores (math and ERW) and ethnicity predicting six year graduation
logreg.model.sat_sep.ethnicity.6 <- glm(six_year_graduation_flag ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f, 
                                            data=students_ethnicity_scope_6, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_sep.ethnicity.6)
print(1 - logLik(logreg.model.sat_sep.ethnicity.6) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_sep.ethnicity.6) - logLik(nullmodel.6))))

# Linear regression model with SAT scores (math and ERW) and ethnicity predicting one year overall GPA including interaction effects
reg.model.sat_sep.ethnicity.gpa1.int <- lm(one_year_overall_gpa.z ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f + SAT_ERW_new.z:ethnicity.f + SAT_math_new.z:ethnicity.f, 
                                       data = students_ethnicity_scope_1)
summary(reg.model.sat_sep.ethnicity.gpa1.int)

# Logistic regression model with SAT scores (math and ERW) and ethnicity predicting one year retention including interaction effects
logreg.model.sat_sep.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f + SAT_ERW_new.z:ethnicity.f + SAT_math_new.z:ethnicity.f, 
                                        data=students_ethnicity_scope_1, 
                                        family=binomial(link="logit"))
summary(logreg.model.sat_sep.ethnicity.1.int)
print(1 - logLik(logreg.model.sat_sep.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_sep.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores (math and ERW) and ethnicity predicting six year graduation including interaction effects
logreg.model.sat_sep.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_ERW_new.z + SAT_math_new.z + ethnicity.f + SAT_ERW_new.z:ethnicity.f + SAT_math_new.z:ethnicity.f, 
                                        data=students_ethnicity_scope_6, 
                                        family=binomial(link="logit"))
summary(logreg.model.sat_sep.ethnicity.6.int)
print(1 - logLik(logreg.model.sat_sep.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_sep.ethnicity.6.int) - logLik(nullmodel.6))))


# Regression models separated by math SAT and ERW SAT ---------------------

# Logistic regression model with SAT math scores and ethnicity predicting one year retention including interaction effects
logreg.model.sat_math.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_math_new.z + ethnicity.f + SAT_math_new.z:ethnicity.f, 
                                             data=students_ethnicity_scope_1, 
                                             family=binomial(link="logit"))
summary(logreg.model.sat_math.ethnicity.1.int)
print(1 - logLik(logreg.model.sat_math.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_math.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT ERW scores and ethnicity predicting one year retention including interaction effects
logreg.model.sat_ERW.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_ERW_new.z + ethnicity.f + SAT_ERW_new.z:ethnicity.f, 
                                            data=students_ethnicity_scope_1, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_ERW.ethnicity.1.int)
print(1 - logLik(logreg.model.sat_ERW.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_ERW.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores math and ethnicity predicting six year graduation including interaction effects
logreg.model.sat_math.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_math_new.z + ethnicity.f + SAT_math_new.z:ethnicity.f, 
                                             data=students_ethnicity_scope_6, 
                                             family=binomial(link="logit"))
summary(logreg.model.sat_math.ethnicity.6.int)
print(1 - logLik(logreg.model.sat_math.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_math.ethnicity.6.int) - logLik(nullmodel.6))))

# Logistic regression model with SAT scores ERW and ethnicity predicting six year graduation including interaction effects
logreg.model.sat_ERW.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_ERW_new.z + ethnicity.f + SAT_ERW_new.z:ethnicity.f, 
                                            data=students_ethnicity_scope_6, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_ERW.ethnicity.6.int)
print(1 - logLik(logreg.model.sat_ERW.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_ERW.ethnicity.6.int) - logLik(nullmodel.6))))


# Plotting regression models ----------------------------------------------

# Check to see which group is the reference group
contrasts(students_ethnicity_scope_1$ethnicity.f)
contrasts(students_ethnicity_scope_6$ethnicity.f)

# Assign regression coefficients to variables by calling the reg_coef function
math_1 <- reg_coef(logreg.model.sat_math.ethnicity.1.int$coefficients)
math_6 <- reg_coef(logreg.model.sat_math.ethnicity.6.int$coefficients)
ERW_1 <- reg_coef(logreg.model.sat_ERW.ethnicity.1.int$coefficients)
ERW_6 <- reg_coef(logreg.model.sat_ERW.ethnicity.6.int$coefficients)

# Assign regression coefficients to variables by calling the reg_coef function
math_1_p <- reg_coef_p(logreg.model.sat_math.ethnicity.1.int$coefficients, tidy(logreg.model.sat_math.ethnicity.1.int)$p.value)
math_6_p <- reg_coef_p(logreg.model.sat_math.ethnicity.6.int$coefficients, tidy(logreg.model.sat_math.ethnicity.6.int)$p.value)
ERW_1_p <- reg_coef_p(logreg.model.sat_ERW.ethnicity.1.int$coefficients, tidy(logreg.model.sat_ERW.ethnicity.1.int)$p.value)
ERW_6_p <- reg_coef_p(logreg.model.sat_ERW.ethnicity.6.int$coefficients, tidy(logreg.model.sat_ERW.ethnicity.6.int)$p.value)

# Calculate probabilities for the plot
# Start by calculating the range on the x-axis
SAT_range_math <- seq(from = min(students_ethnicity_scope_1$SAT_math_new.z, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_math_new.z, na.rm = TRUE), by = 0.1)
SAT_range_ERW <- seq(from = min(students_ethnicity_scope_1$SAT_ERW_new.z, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_ERW_new.z, na.rm = TRUE), by = 0.1)

# SAT_range_math <- seq(from = min(students_ethnicity_scope_1$SAT_math_new.z - 15, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_math_new.z, na.rm = TRUE) + 8, by = 0.1)
# SAT_range_ERW <- seq(from = min(students_ethnicity_scope_1$SAT_ERW_new.z - 15, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_ERW_new.z, na.rm = TRUE) + 8, by = 0.1)

# Create logistic regression plots for each ethnicity
# Plots are for 1-year retention and 6-year graduation for SAT math and SAT ERW
log_plot(math_1, SAT_range_math, "SAT math", "P(1-year retention)", "Probability of 1-year retention \nfor math SAT scores by ethnicity")
log_plot(math_6, SAT_range_math, "SAT math", "P(6-year graduation)", "Probability of 6-year graduation \nfor math SAT scores by ethnicity")
log_plot(ERW_1, SAT_range_ERW, "SAT ERW", "P(1-year retention)", "Probability of 1-year retention \nfor ERW SAT scores by ethnicity")
log_plot(ERW_6, SAT_range_ERW, "SAT ERW", "P(6-year graduation)", "Probability of 6-year graduation \nfor ERW SAT scores by ethnicity")

# Create logistic regression plots for each ethnicity using only significant coefficients
# Plots are for 1-year retention and 6-year graduation for SAT math and SAT ERW
log_plot(math_1_p, SAT_range_math, "SAT math", "P(1-year retention)", "Probability of 1-year retention \nfor math SAT scores by ethnicity")
log_plot(math_6_p, SAT_range_math, "SAT math", "P(6-year graduation)", "Probability of 6-year graduation \nfor math SAT scores by ethnicity")
log_plot(ERW_1_p, SAT_range_ERW, "SAT ERW", "P(1-year retention)", "Probability of 1-year retention \nfor ERW SAT scores by ethnicity")
log_plot(ERW_6_p, SAT_range_ERW, "SAT ERW", "P(6-year graduation)", "Probability of 6-year graduation \nfor ERW SAT scores by ethnicity")


# Correlation analysis ----------------------------------------------------

test_1 <- students_ethnicity_scope_1 %>% 
  select(-hashed_puid:-department_short_title,
         -profile_academic_period_desc:-profile_college,
         -profile_gender_desc:-profile_sat_total,
         -two_year_retention_flag,-six_year_graduation_flag,
         -associate_1_grad:-associate_1_overall_credits_earned,
         -county_desc:-athlete_flag,
         -cco_internships:-hs_urban_centric_locale,
         -hs_location_zip:-hs_school_name,
         -lgbtq_flag,
         -reason_for_leaving,
         -Term:-Year,
         -ethnicity.f)
test_1$incoming_credits_flag <- as.logical(test_1$incoming_credits_flag)

test_6 <- students_ethnicity_scope_6 %>% 
  select(-hashed_puid:-department_short_title,
         -profile_academic_period_desc:-profile_college,
         -profile_gender_desc:-profile_sat_total,
         -associate_1_grad:-associate_1_overall_credits_earned,
         -county_desc:-athlete_flag,
         -cco_internships:-hs_urban_centric_locale,
         -hs_location_zip:-hs_school_name,
         -lgbtq_flag,
         -reason_for_leaving,
         -Term:-Year,
         -ethnicity.f)
test_6$incoming_credits_flag <- as.logical(test_6$incoming_credits_flag)

x <- lowerCor(test_1)
write.csv(x, "correlation_1.csv")

y <- lowerCor(test_6)
write.csv(y, "correlation_6.csv")

#test$academic_school_grouping_desc <- as.factor(test$academic_school_grouping_desc)
#test$department_short_title <- as.factor(test$department_short_title)
#test$profile_residence_desc <- as.factor(test$profile_residence_desc)
#test$cco_internships <- as.numeric(as.factor(test$cco_internships))
#test$citizenship_desc <- as.factor(test$citizenship_desc)
#test$hs_title_i_school_status <- as.factor(test$hs_title_i_school_status)
#test$hs_urban_centric_locale <- as.factor(test$hs_urban_centric_locale)

# Cramer's V is used to test the association between categorical variables
library(lsr)
cramersV(test)

# Regression models separated by math SAT and ERW SAT (not standardized)---------------------

# Logistic regression model with SAT math scores and ethnicity predicting one year retention including interaction effects
logreg.model.sat_math.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_math_new + ethnicity.f + SAT_math_new:ethnicity.f, 
                                             data=students_ethnicity_scope_1, 
                                             family=binomial(link="logit"))
summary(logreg.model.sat_math.ethnicity.1.int)
print(1 - logLik(logreg.model.sat_math.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_math.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT ERW scores and ethnicity predicting one year retention including interaction effects
logreg.model.sat_ERW.ethnicity.1.int <- glm(one_year_retention_flag ~ SAT_ERW_new + ethnicity.f + SAT_ERW_new:ethnicity.f, 
                                            data=students_ethnicity_scope_1, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_ERW.ethnicity.1.int)
print(1 - logLik(logreg.model.sat_ERW.ethnicity.1.int) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(logreg.model.sat_ERW.ethnicity.1.int) - logLik(nullmodel.1))))

# Logistic regression model with SAT scores math and ethnicity predicting six year graduation including interaction effects
logreg.model.sat_math.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_math_new + ethnicity.f + SAT_math_new:ethnicity.f, 
                                             data=students_ethnicity_scope_6, 
                                             family=binomial(link="logit"))
summary(logreg.model.sat_math.ethnicity.6.int)
print(1 - logLik(logreg.model.sat_math.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_math.ethnicity.6.int) - logLik(nullmodel.6))))

# Logistic regression model with SAT scores ERW and ethnicity predicting six year graduation including interaction effects
logreg.model.sat_ERW.ethnicity.6.int <- glm(six_year_graduation_flag ~ SAT_ERW_new + ethnicity.f + SAT_ERW_new:ethnicity.f, 
                                            data=students_ethnicity_scope_6, 
                                            family=binomial(link="logit"))
summary(logreg.model.sat_ERW.ethnicity.6.int)
print(1 - logLik(logreg.model.sat_ERW.ethnicity.6.int) / logLik(nullmodel.6))
print(1 - exp(-2 / n.6 * (logLik(logreg.model.sat_ERW.ethnicity.6.int) - logLik(nullmodel.6))))


# Plotting regression models (not standardized)----------------------------------------------

# Check to see which group is the reference group
contrasts(students_ethnicity_scope_1$ethnicity.f)
contrasts(students_ethnicity_scope_6$ethnicity.f)

# Assign regression coefficients to variables by calling the reg_coef function
math_1 <- reg_coef(logreg.model.sat_math.ethnicity.1.int$coefficients)
math_6 <- reg_coef(logreg.model.sat_math.ethnicity.6.int$coefficients)
ERW_1 <- reg_coef(logreg.model.sat_ERW.ethnicity.1.int$coefficients)
ERW_6 <- reg_coef(logreg.model.sat_ERW.ethnicity.6.int$coefficients)

# Assign regression coefficients to variables by calling the reg_coef function
math_1_p <- reg_coef_p(logreg.model.sat_math.ethnicity.1.int$coefficients, tidy(logreg.model.sat_math.ethnicity.1.int)$p.value)
math_6_p <- reg_coef_p(logreg.model.sat_math.ethnicity.6.int$coefficients, tidy(logreg.model.sat_math.ethnicity.6.int)$p.value)
ERW_1_p <- reg_coef_p(logreg.model.sat_ERW.ethnicity.1.int$coefficients, tidy(logreg.model.sat_ERW.ethnicity.1.int)$p.value)
ERW_6_p <- reg_coef_p(logreg.model.sat_ERW.ethnicity.6.int$coefficients, tidy(logreg.model.sat_ERW.ethnicity.6.int)$p.value)

# Calculate probabilities for the plot
# Start by calculating the range on the x-axis
SAT_range_math <- seq(from = min(students_ethnicity_scope_1$SAT_math_new, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_math_new, na.rm = TRUE), by = 0.1)
SAT_range_ERW <- seq(from = min(students_ethnicity_scope_1$SAT_ERW_new, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_ERW_new, na.rm = TRUE), by = 0.1)

# SAT_range_math <- seq(from = min(students_ethnicity_scope_1$SAT_math_new.z - 15, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_math_new.z, na.rm = TRUE) + 8, by = 0.1)
# SAT_range_ERW <- seq(from = min(students_ethnicity_scope_1$SAT_ERW_new.z - 15, na.rm = TRUE), to = max(students_ethnicity_scope_1$SAT_ERW_new.z, na.rm = TRUE) + 8, by = 0.1)

# Create logistic regression plots for each ethnicity
# Plots are for 1-year retention and 6-year graduation for SAT math and SAT ERW
log_plot(math_1, SAT_range_math, "SAT math", "P(1-year retention)", "Probability of 1-year retention \nfor math SAT scores by ethnicity", mean(students_ethnicity_scope_1$SAT_math_new, na.rm = TRUE))
log_plot(math_6, SAT_range_math, "SAT math", "P(6-year graduation)", "Probability of 6-year graduation \nfor math SAT scores by ethnicity", mean(students_ethnicity_scope_6$SAT_math_new, na.rm = TRUE))
log_plot(ERW_1, SAT_range_ERW, "SAT ERW", "P(1-year retention)", "Probability of 1-year retention \nfor ERW SAT scores by ethnicity", mean(students_ethnicity_scope_1$SAT_ERW_new, na.rm = TRUE))
log_plot(ERW_6, SAT_range_ERW, "SAT ERW", "P(6-year graduation)", "Probability of 6-year graduation \nfor ERW SAT scores by ethnicity", mean(students_ethnicity_scope_6$SAT_ERW_new, na.rm = TRUE))

# Create logistic regression plots for each ethnicity using only significant coefficients
# Plots are for 1-year retention and 6-year graduation for SAT math and SAT ERW
log_plot(math_1_p, SAT_range_math, "SAT math", "P(1-year retention)", "Probability of 1-year retention \nfor math SAT scores by ethnicity", mean(students_ethnicity_scope_1$SAT_math_new, na.rm = TRUE))
log_plot(math_6_p, SAT_range_math, "SAT math", "P(6-year graduation)", "Probability of 6-year graduation \nfor math SAT scores by ethnicity", mean(students_ethnicity_scope_6$SAT_math_new, na.rm = TRUE))
log_plot(ERW_1_p, SAT_range_ERW, "SAT ERW", "P(1-year retention)", "Probability of 1-year retention \nfor ERW SAT scores by ethnicity", mean(students_ethnicity_scope_1$SAT_ERW_new, na.rm = TRUE))
log_plot(ERW_6_p, SAT_range_ERW, "SAT ERW", "P(6-year graduation)", "Probability of 6-year graduation \nfor ERW SAT scores by ethnicity", mean(students_ethnicity_scope_6$SAT_ERW_new, na.rm = TRUE))

# Step-wise regression with additional variables ------------------------------------

# Logistic regression model with SAT math scores and ethnicity predicting one year retention including interaction effects
model1 <- glm(one_year_retention_flag ~ SAT_math_new + 
                                               SAT_ERW_new + 
                                               ethnicity.f +
                                               orientation + 
                                               community_affiliation + 
                                               work_related + 
                                               at_risk + 
                                               early_support + 
                                               low_income + 
                                               URM_programs + 
                                               scholarship, 
                                             data=students_ethnicity_scope_1, 
                                             family=binomial(link="logit"))
summary(model1)
print(1 - logLik(model1) / logLik(nullmodel.1))
print(1 - exp(-2 / n.1 * (logLik(model1) - logLik(nullmodel.1))))

step(model1, direction = "backward")

model1full <- glm(one_year_retention_flag ~ SAT_math_new + 
                SAT_ERW_new + 
                ethnicity.f +
                orientation + 
                community_affiliation + 
                work_related + 
                at_risk + 
                early_support + 
                low_income + 
                URM_programs + 
                scholarship, 
              data=na.omit(students_ethnicity_scope_1), 
              family=binomial(link="logit"))

model1part <- glm(one_year_retention_flag ~ 1, 
                  data=na.omit(students_ethnicity_scope_1), 
                  family=binomial(link="logit"))

step(model1part, direction = "forward", scope = formula(model1full))

# use na.omit(students_ethnicity_scope_1) to get through forward error