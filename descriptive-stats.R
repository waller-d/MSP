##############################
####################  waller-d
#
#
#   Descriptive statistics    
#  
#
#
####################
##############################

library(tidyverse)

# Function to summarize retention and graduation data for a specific demographic
ret_grad_rates <- function(dat, dem){
  dat %>%
    group_by_at(dem) %>% 
    summarise("N"=n(),
              "% first-time\nfull-time"=mean(profile_firstime_fulltime_ind),
              "% first-year\nfirst-generation"=mean(first_year_first_gen_flag),
              "% first-\ngeneration"=mean(first_gen_flag),
              "Mean SAT"=mean(SAT_new, na.rm = TRUE),
              "SD SAT"=sd(SAT_new, na.rm = TRUE),
              "% 1 year\nretention"=mean(one_year_retention_flag),
              "% 2 year\nretention"=mean(two_year_retention_flag),
              "% 3 year\nretention"=mean(three_year_retention_flag),
              "% 3 year\ngraduation"=mean(three_year_graduation_flag),
              "% 4 year\ngraduation"=mean(four_year_graduation_flag),
              "% 5 year\ngraduation"=mean(five_year_graduation_flag),
              "% 6 year\ngraduation"=mean(six_year_graduation_flag))
}

# Function to create bar charts for a given demographic variable (dem) faceted by semester
# Two bar charts are generated, one for 1-year rentention and one for 6-year graduation
ret_grad_plots <- function(dat, dem, time){
  
  # Create data frame with values for plotting
  data <- dat %>% 
    group_by_at(vars(dem, time)) %>% # allows for grouping by input string variables
    summarise("N"=n(),
              `Mean SAT`=mean(SAT_new, na.rm = TRUE),
              `SD SAT`=sd(SAT_new, na.rm = TRUE),
              `% 1 year\nretention`=mean(one_year_retention_flag),
              `% 6 year\ngraduation`=mean(six_year_graduation_flag)) %>% 
    separate(profile_academic_period_desc, c("Term","Year"))
  
  ycol1 <- "`% 1 year\nretention`" # for work around for parsing errors from aes_string
  ycol2 <- "`% 6 year\ngraduation`" # for work around for parsing errors from aes_string
  
  # Create a bar chart for the demographic variable faceted by semester
  # Bar chart is for 1-year retention
  plot1 <- ggplot(data) +
    geom_bar(aes_string(x = dem, y = ycol1, fill = dem), stat = "identity") +
    facet_grid(Term ~ Year) +
    scale_fill_hue(l=40)
  
  # Create a bar chart for the demographic variable faceted by semester
  # Bar chart is for 6-year graduation
  plot2 <- ggplot(data) +
    geom_bar(aes_string(x = dem, y = ycol2, fill = dem), stat = "identity") +
    facet_wrap(~ `profile_academic_period_desc`, nrow = 2) +
    scale_fill_hue(l=40)
  
  plots <- list(plot1, plot2)
  # Return the plot to the function call
  return(plots)
}

# Create data set for students with complete six year graduation data (latest semester is Fall 2013)
students_complete <- students %>% 
  filter(profile_academic_period<=201410)

# Summarize overall retention and graduation information
res.overall <- students_complete %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information for demographic groups
# Print results to a text file
res.ftft <- students_complete %>% 
  group_by(profile_firstime_fulltime_ind) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.ftft
ret_grad_plots(students_complete, dem = "profile_firstime_fulltime_ind", time = "profile_academic_period_desc")
res.gender <- students_complete %>% 
  group_by(profile_gender_desc) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.gender
res.ethnicity <- students_complete %>% 
  group_by(profile_reporting_ethnicity) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.ethnicity
res.residency <- students_complete %>% 
  group_by(profile_residence_desc) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.residency
res.URM <- students_complete %>% 
  group_by(profile_underrep_minority_ind) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.URM
res.athlete <- students_complete %>% 
  group_by(athlete_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.athlete
res.fgen <- students_complete %>% 
  group_by(first_gen_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.fgen
res.fyfgen <- students_complete %>% 
  group_by(first_year_first_gen_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.fyfgen
res.lgbtq <- students_complete %>% 
  group_by(lgbtq_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.lgbtq
res.military <- students_complete %>% 
  group_by(military_active_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.military
res.veteran <- students_complete %>% 
  group_by(military_veteran_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.veteran
res.milfam <- students_complete %>% 
  group_by(military_family_flag) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag), mean(SAT_new, na.rm = TRUE))
res.milfam

# Summarize retention and graduation information grouped by Fall or Spring starting semester
b.1 <- students_complete %>% 
  group_by(str_detect(profile_academic_period, "20$")) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by Fall or Spring starting semester
# Results are for first-time full-time student only
b.2 <- students %>% 
  group_by(str_detect(profile_academic_period, "20$")) %>% 
  filter(profile_firstime_fulltime_ind==TRUE) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by starting semester
c.1 <- students %>% 
  group_by(profile_academic_period) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by Fall or Spring starting semester and ethnicity
d.1 <- students_complete %>% 
  group_by(str_detect(profile_academic_period, "20$"), profile_reporting_ethnicity) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by ethnicity
e.1 <- students_complete %>% 
  group_by(profile_reporting_ethnicity) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by academic school grouping
f.1 <- students_complete %>% 
  group_by(academic_school_grouping_desc) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

# Summarize retention and graduation information grouped by academic school grouping and ethnicity
f.2 <- students_complete %>% 
  group_by(academic_school_grouping_desc, profile_reporting_ethnicity) %>% 
  summarise(n(), mean(profile_firstime_fulltime_ind) , mean(first_year_first_gen_flag), mean(first_gen_flag), 
            mean(one_year_retention_flag), mean(two_year_retention_flag), mean(three_year_retention_flag), 
            mean(three_year_graduation_flag), mean(four_year_graduation_flag), mean(five_year_graduation_flag), 
            mean(six_year_graduation_flag))

