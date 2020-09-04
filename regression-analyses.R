# Title -------------------------------------------------------------------

#  Regression analyses


# Packages ----------------------------------------------------------------

library(dplyr)


# Functions ---------------------------------------------------------------

standardize <- function(data){
  z <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  return(z)
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

