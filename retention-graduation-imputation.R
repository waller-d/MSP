# Title -------------------------------------------------------------------

#   Impute retention and graduation


# Packages ----------------------------------------------------------------

library(dplyr)


# Data selection ----------------------------------------------------------

student_outcomes <- students %>% 
  select(one_year_retention_flag:six_year_graduation_flag)


# Test for inconsistencies in retention and graduation --------------------

# Impute indeces for inconsistent retention and graudation flags
# Impute retention and graduation flags to resolve inconsistencies
# If a student is retained after two years but not after one year, this codes changes the one year retention flag to TRUE (etc.)
# If a student graduated after three years but not after four yeras, this code changes the four year graduation flag to TRUE (etc.)
ind_1 <- student_outcomes$two_year_retention_flag > student_outcomes$one_year_retention_flag
students[ind_1,"one_year_retention_flag"] <- TRUE

ind_2 <- student_outcomes$three_year_retention_flag > student_outcomes$two_year_retention_flag
students[ind_2,"two_year_retention_flag"] <- TRUE

ind_3 <- student_outcomes$three_year_graduation_flag > student_outcomes$four_year_graduation_flag
students[ind_3,"four_year_graduation_flag"] <- TRUE

ind_4 <- student_outcomes$four_year_graduation_flag > student_outcomes$five_year_graduation_flag
students[ind_4,"five_year_graduation_flag"] <- TRUE

ind_5 <- student_outcomes$five_year_graduation_flag > student_outcomes$six_year_graduation_flag
students[ind_5,"six_year_graduation_flag"] <- TRUE

# Exploration of retention inconsistencies --------------------------------

# Incosistency in retention and graduation exploration
# no_one_year_ret <- students[ind_1,"hashed_puid"]
# no_one_year_ret_semesters <- semesters %>% 
#   filter(hashed_puid == no_one_year_ret[[1]][1])
# no_one_year_ret_students <- students %>% 
#   filter(hashed_puid == no_one_year_ret[[1]][1])
