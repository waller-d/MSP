# Title -------------------------------------------------------------------

#  Representation metrics


# Packages ----------------------------------------------------------------

library(tidyverse)


# College representation --------------------------------------------------

# Calculate the total number enrolled in each college across all years
college_enrollment <- students %>% 
  group_by(academic_school_grouping_desc) %>% 
  summarise(N = n())

# Calculate the total number enrolled in each college across all years by gender
# Calculate the percentage of male/female in each college
college_enrollment_gender <- students %>% 
  group_by(academic_school_grouping_desc, profile_gender_desc) %>% 
  summarise(N = n()) %>% 
  left_join(., college_enrollment, by = "academic_school_grouping_desc") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each college across all years by URM status
# Calculate the percentage of URM in each college
college_enrollment_URM <- students %>% 
  group_by(academic_school_grouping_desc, URM) %>% 
  summarise(N = n()) %>% 
  left_join(., college_enrollment, by = "academic_school_grouping_desc") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each college for each cohort
college_enrollment_cohort <- students %>% 
  group_by(academic_school_grouping_desc, profile_academic_period_desc) %>% 
  summarise(N = n()) %>% 
  mutate(college_year = paste(academic_school_grouping_desc, profile_academic_period_desc))

# Calculate the total number enrolled in each college for each cohort by gender
# Calculate the percentage of male/female in each college for each cohort
college_enrollment_cohort_gender <- students %>% 
  group_by(academic_school_grouping_desc, profile_academic_period_desc, profile_gender_desc) %>% 
  summarise(N = n()) %>% 
  mutate(college_year = paste(academic_school_grouping_desc, profile_academic_period_desc)) %>% 
  left_join(., college_enrollment_cohort, by = "college_year") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each college for each cohort by URM status
# Calculate the percentage of URM in each college for each cohort
college_enrollment_cohort_URM <- students %>% 
  group_by(academic_school_grouping_desc, profile_academic_period_desc, URM) %>% 
  summarise(N = n()) %>% 
  mutate(college_year = paste(academic_school_grouping_desc, profile_academic_period_desc)) %>% 
  left_join(., college_enrollment_cohort, by = "college_year") %>% 
  mutate(percent = N.x/N.y)

# Major representation --------------------------------------------------

# Calculate the total number enrolled in each major across all years
major_enrollment <- students %>% 
  group_by(department_short_title) %>% 
  summarise(N = n())

# Calculate the total number enrolled in each major across all years by gender
# Calculate the percentage of male/female in each major
major_enrollment_gender <- students %>% 
  group_by(department_short_title, profile_gender_desc) %>% 
  summarise(N = n()) %>% 
  left_join(., major_enrollment, by = "department_short_title") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each major across all years by URM status
# Calculate the percentage of URM in each major
major_enrollment_URM <- students %>% 
  group_by(department_short_title, URM) %>% 
  summarise(N = n()) %>% 
  left_join(., major_enrollment, by = "department_short_title") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each major for each cohort
major_enrollment_cohort <- students %>% 
  group_by(department_short_title, profile_academic_period_desc) %>% 
  summarise(N = n()) %>% 
  mutate(major_year = paste(department_short_title, profile_academic_period_desc))

# Calculate the total number enrolled in each major for each cohort by gender
# Calculate the percentage of male/female in each major for each cohort
major_enrollment_cohort_gender <- students %>% 
  group_by(department_short_title, profile_academic_period_desc, profile_gender_desc) %>% 
  summarise(N = n()) %>% 
  mutate(major_year = paste(department_short_title, profile_academic_period_desc)) %>% 
  left_join(., major_enrollment_cohort, by = "major_year") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each major for each cohort by URM status
# Calculate the percentage of URM in each major for each cohort
major_enrollment_cohort_URM <- students %>% 
  group_by(department_short_title, profile_academic_period_desc, URM) %>% 
  summarise(N = n()) %>% 
  mutate(major_year = paste(department_short_title, profile_academic_period_desc)) %>% 
  left_join(., major_enrollment_cohort, by = "major_year") %>% 
  mutate(percent = N.x/N.y)

