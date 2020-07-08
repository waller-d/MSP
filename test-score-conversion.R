##############################
####################  waller-d
#
#
#   Test score conversion
#  
#
#
####################
##############################

library(readxl)
library(tidyverse)
library(psych)
library(PerformanceAnalytics)


# Import concordance tables for ACT to new SAT and old SAT to new SAT
ACT_to_SAT_new <- read_excel("SAT_ACT_concordance.xlsx", 
                                  sheet = "ACT-composite-to-SAT-total")
SAT_old_WCR_to_SAT_new_EBRW <- read_excel("SAT_ACT_concordance.xlsx", 
                                 sheet = "SAT-old-W+CR-to-SAT-new-EBR+W")
SAT_old_M_to_SAT_new_M <- read_excel("SAT_ACT_concordance.xlsx", 
                                 sheet = "SAT-old-M-to-SAT-new-M")

# Filter for first-time full-time students and select columns of interest
students_test_scores <- students %>%
  select(puid, citizenship_desc, profile_academic_period, grad_1_overall_gpa, 
         profile_highest_act_composite, profile_sat_total, profile_highest_sat_crit_read, 
         profile_highest_sat_math, profile_highest_sat_writing)

# Rename ACT composite and SAT total columns for merging purposes
colnames(students_test_scores) <- c("puid", "citizenship_desc", "profile_academic_period", 
                             "grad_1_overall_gpa", "ACT_composite", "SAT_old", 
                             "profile_highest_sat_crit_read", "SAT_old_Math", 
                             "profile_highest_sat_writing")

# Rename SAT total column for merging purposes
colnames(ACT_to_SAT_new) <- c("ACT_composite", "SAT_new", 
                              "SAT_total_min", "SAT_total_max")

# Calculate combined critical reading and writing score
# Convert old SAT critical reading and writing score to new SAT evidence-based reading and writing score
# Convert old SAT math score to new SAT math score
# Combine new SAT evidence-based reading and writing score with new SAT math score for new SAT total score
SAT_new <- students_test_scores %>% 
  mutate("SAT_old_W+CR"=profile_highest_sat_crit_read+profile_highest_sat_writing) %>% 
  filter(!is.na(SAT_old)) %>% # Filter for students who took the SAT
  left_join(SAT_old_WCR_to_SAT_new_EBRW, by="SAT_old_W+CR") %>% 
  left_join(SAT_old_M_to_SAT_new_M, by="SAT_old_Math") %>% 
  mutate("SAT_new"=`SAT_new_EBR+W`+`SAT_new_Math`) %>% 
  select(-SAT_new_Math_test,-`SAT_new_EBR+W`,-`SAT_new_Math`,-`SAT_old_W+CR`)

# Convert ACT scores to new SAT scores
ACT_all <- students_test_scores %>% 
  filter(is.na(SAT_old) & !is.na(ACT_composite)) %>% # Filter for students who only took the ACT
  left_join(ACT_to_SAT_new, by="ACT_composite") %>% 
  select(-SAT_total_min, -SAT_total_max)

# Filter for students who did not take either the ACT or SAT
no_test_all <- students_test_scores %>% 
  filter(is.na(SAT_old) & is.na(ACT_composite)) %>% 
  mutate(SAT_new=SAT_old)

# Check to see if all students are accounted for in the test score conversion (looking for TRUE)
# Combine data frames with concordance scores
(nrow(SAT_new)+nrow(ACT_all)+nrow(no_test_all))==nrow(students_test_scores)
students_test_scores_equated <- rbind(SAT_new, ACT_all, no_test_all)

# Add coloumn to student data with equated concordance scores
concordance_scores <- students_test_scores_equated %>% 
  select(puid, SAT_new)
students <- students %>% 
  left_join(concordance_scores, by="puid")

# Calculate the Pearson correlation between graduating GPA and new SAT score
# Generate a scatter plot matrix for graduating GPA and new SAT score
cor.test(students_test_scores_equated$grad_1_overall_gpa, students_test_scores_equated$SAT_new)
data.gpa <- students_test_scores_equated %>% 
  select(grad_1_overall_gpa, SAT_new) %>% 
  chart.Correlation(method = "pearson", histogram = TRUE, pch = 16)
