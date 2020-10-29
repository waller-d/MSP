# Title -------------------------------------------------------------------

#  Representation metrics


# Packages ----------------------------------------------------------------

library(tidyverse)


# Engineering population breakdown ----------------------------------------

eng_studens <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(profile_reporting_ethnicity) %>% 
  summarise(N = n()) %>% 
  mutate(percent = N/21993)

eng_students_ethnicity <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(profile_reporting_ethnicity, department_short_title) %>% 
  summarise(N = n())

eng_students_department <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(department_short_title, profile_reporting_ethnicity) %>% 
  summarise(N = n()) %>% 
  mutate(percent = N/sum(N))

eng_students_department %>% 
  filter(department_short_title == "Aeronautics & Astronautics") %>% 
  summarise(sum(percent))

eng_students_cohort <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(profile_academic_period, department_short_title, profile_reporting_ethnicity) %>% 
  summarise(N = n()) %>% 
  mutate(percent = N/sum(N))

ggplot(eng_students_ethnicity, aes(x = N, y = profile_reporting_ethnicity)) +
  geom_segment(aes(x = 0, y = profile_reporting_ethnicity, xend = N, yend = profile_reporting_ethnicity)) +
  geom_point() + facet_grid(department_short_title ~ .)

eng_students_cohort_hispanic_AA <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  filter(profile_reporting_ethnicity == "Hispanic/Latino" | profile_reporting_ethnicity == "Black or African American" | profile_reporting_ethnicity == "White" | profile_reporting_ethnicity == "Asian" ) %>% 
  filter(Term == "Fall") %>% 
  group_by(profile_academic_period, profile_reporting_ethnicity) %>% 
  summarise(N = n()) #%>% 
mutate(percent = N/sum(N))

ggplot(eng_students_cohort_hispanic_AA, aes(x = profile_academic_period, y = N)) +
  geom_point(aes(color = profile_reporting_ethnicity)) +
  geom_line(aes(color = profile_reporting_ethnicity))

ggplot(eng_students_cohort_hispanic_AA, aes(fill = profile_reporting_ethnicity, y = N, x = profile_academic_period)) +
         geom_bar(position = "fill", stat = "identity")



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

# Calculate the total number enrolled in each major across all years by URM status
# Calculate the percentage of URM in each engineering major
major_enrollment_URM_engineering <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(department_short_title, URM) %>% 
  summarise(N = n()) %>% 
  left_join(., major_enrollment, by = "department_short_title") %>% 
  mutate(percent = N.x/N.y)

# Calculate the total number enrolled in each major across all years by gender status
# Calculate the percentage of URM in each engineering major
major_enrollment_gender_engineering <- students %>% 
  filter(academic_school_grouping_desc == "College of Engineering") %>% 
  group_by(department_short_title, profile_gender_desc) %>% 
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



# Gender and URM representation in engineering ----------------------------

x <- students %>% 
  filter(academic_school_grouping_desc =="College of Engineering") %>% 
  group_by(department_short_title, Term, Year, URM) %>% 
  summarise(N = n()) %>% 
  mutate(major_year = paste(department_short_title, Term, Year)) %>% 
  left_join(., major_enrollment_cohort, by = "major_year") %>% 
  mutate(percent = N.x/N.y) %>% 
  filter(Term == "Fall", URM == TRUE)

x

ggplot(data = x, aes(x = Year, y = percent, group = department_short_title.x)) +
  geom_line(aes(color = department_short_title.x)) +
  geom_point(aes(color = department_short_title.x)) +
  facet_grid(department_short_title.x ~ .)

y <- students %>% 
  filter(academic_school_grouping_desc =="College of Engineering") %>% 
  group_by(department_short_title, Term, Year, profile_gender_desc) %>% 
  summarise(N = n()) %>% 
  mutate(major_year = paste(department_short_title, Term, Year)) %>% 
  left_join(., major_enrollment_cohort, by = "major_year") %>% 
  mutate(percent = N.x/N.y) %>% 
  filter(Term == "Fall", profile_gender_desc == "Female")

y

ggplot(data = y, aes(x = Year, y = percent, group = department_short_title.x)) +
  geom_line(aes(color = department_short_title.x)) +
  geom_point(aes(color = department_short_title.x)) +
  facet_grid(department_short_title.x ~ .)
