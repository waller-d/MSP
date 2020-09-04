# Title -------------------------------------------------------------------

#  Test score conversion


# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)


# Import concordance table data -------------------------------------------

# Import concordance tables for ACT to new SAT and old SAT to new SAT
SAT_old_M_to_SAT_new_M <- read_excel("SAT_ACT_concordance.xlsx", 
                                     sheet = "SAT-old-M-to-SAT-new-M")
SAT_old_WCR_to_SAT_new_EBRW <- read_excel("SAT_ACT_concordance.xlsx", 
                                          sheet = "SAT-old-W+CR-to-SAT-new-EBR+W")
ACT_math_to_SAT_math <- read_excel("SAT_ACT_concordance.xlsx", 
                                   sheet = "ACT-M-to-SAT-M")
ACT_ER_to_SAT_new_ERW <- read_excel("SAT_ACT_concordance.xlsx", 
                                   sheet = "ACT-E+R-to-SAT-ERW")
ACT_to_SAT_new <- read_excel("SAT_ACT_concordance.xlsx", 
                             sheet = "ACT-composite-to-SAT-total")


# Create data frames for analysis ------------------------------------------

# Filter for test score columns
students_test_scores <- students %>%
  select(hashed_puid, profile_highest_sat_math, profile_highest_sat_writing,
         profile_highest_sat_crit_read, profile_highest_act_english, profile_highest_act_math,
         profile_highest_act_reading, profile_highest_act_composite, profile_sat_total)

# Rename ACT composite and SAT total columns for merging purposes
colnames(students_test_scores) <- c("hashed_puid", "SAT_math_old", "SAT_writing_old", "SAT_crit_read_old",
                                    "ACT_english", "ACT_math", "ACT_read", "ACT_total", "SAT_total_old")

# Rename SAT total column for merging purposes
colnames(ACT_to_SAT_new) <- c("ACT_total", "SAT_total_new", 
                              "SAT_total_min", "SAT_total_max")
colnames(ACT_math_to_SAT_math) <- c("ACT_math", "SAT_math_new")
colnames(ACT_ER_to_SAT_new_ERW) <- c("ACT_E+R", "SAT_ERW_new", "single_score_ACT_E+R")
colnames(SAT_old_M_to_SAT_new_M) <- c("SAT_math_old", "SAT_math_new", "SAT_math_test")
colnames(SAT_old_WCR_to_SAT_new_EBRW) <- c("SAT_W+CR_old", "SAT_ERW_new")


# Convert old SAT scores to new SAT scores --------------------------------

# Calculate combined critical reading and writing score
# Convert old SAT critical reading and writing score to new SAT evidence-based reading and writing score
# Convert old SAT math score to new SAT math score
# Combine new SAT evidence-based reading and writing score with new SAT math score for new SAT total score
SAT_new <- students_test_scores %>% 
  filter(!is.na(SAT_total_old)) %>% # Filter for students who took the SAT
  mutate("SAT_W+CR_old" = SAT_writing_old + SAT_crit_read_old) %>% 
  left_join(SAT_old_WCR_to_SAT_new_EBRW, by = "SAT_W+CR_old") %>% 
  left_join(SAT_old_M_to_SAT_new_M, by = "SAT_math_old") %>%
  mutate("SAT_total_new" = SAT_ERW_new + SAT_math_new) %>% 
  select(hashed_puid, SAT_ERW_new, SAT_math_new, SAT_total_new)


# Convert ACT scores to new SAT scores ------------------------------------

# Calculate combined English and reading score
# Convert combined English and reading ACT score to evidence-based reading and writing SAT score
# Convert math ACT score to math SAT score
ACT_all <- students_test_scores %>% 
  filter(is.na(SAT_total_old) & !is.na(ACT_total)) %>% # Filter for students who only took the ACT
  mutate("ACT_E+R" = ACT_english + ACT_read) %>% 
  left_join(ACT_ER_to_SAT_new_ERW, by = "ACT_E+R") %>% 
  left_join(ACT_math_to_SAT_math, by = "ACT_math") %>% 
  mutate("SAT_total_new" = SAT_ERW_new + SAT_math_new) %>% 
  select(hashed_puid, SAT_ERW_new, SAT_math_new, SAT_total_new)


# Combine test score data frames ------------------------------------------

# Filter for students who did not take either the ACT or SAT
no_test_all <- students_test_scores %>% 
  filter(is.na(SAT_total_old) & is.na(ACT_total)) %>% 
  mutate(SAT_ERW_new = SAT_writing_old, SAT_math_new = SAT_math_old, SAT_total_new = SAT_total_old) %>% 
  select(hashed_puid, SAT_ERW_new, SAT_math_new, SAT_total_new)

# Check to see if all students are accounted for in the test score conversion (looking for TRUE)
# Combine data frames with concordance scores
(nrow(SAT_new)+nrow(ACT_all)+nrow(no_test_all))==nrow(students_test_scores)
students_test_scores_equated <- rbind(SAT_new, ACT_all, no_test_all)

# Add test score data to student data
students <- students %>% 
  left_join(students_test_scores_equated, by="hashed_puid")

