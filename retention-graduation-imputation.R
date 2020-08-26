##############################
####################  waller-d
#
#
#   Impute retention and 
#   graduation
#
#
####################
##############################

# Consistency of retention and graduation outcomes ------------------------
student_outcomes <- students %>% 
  select(one_year_retention_flag:six_year_graduation_flag)

# Impute retention and graudation information
ind_1 <- student_outcomes$two_year_retention_flag > student_outcomes$one_year_retention_flag
ind_2 <- student_outcomes$three_year_retention_flag > student_outcomes$two_year_retention_flag
ind_3 <- student_outcomes$three_year_graduation_flag > student_outcomes$four_year_graduation_flag
ind_4 <- student_outcomes$four_year_graduation_flag > student_outcomes$five_year_graduation_flag
ind_5 <- student_outcomes$five_year_graduation_flag > student_outcomes$six_year_graduation_flag

# Incosistency in retention and graduation exploration
no_one_year_ret <- students[ind_1,"hashed_puid"]
no_one_year_ret_semesters <- semesters %>% 
  filter(hashed_puid == no_one_year_ret[[1]][1])
no_one_year_ret_students <- students %>% 
  filter(hashed_puid == no_one_year_ret[[1]][1])

# Impute retention and graudation variables
students[ind_1,"one_year_retention_flag"] <- TRUE
students[ind_2,"one_year_retention_flag"] <- TRUE
students[ind_2,"two_year_retention_flag"] <- TRUE
students[ind_3,"four_year_graduation_flag"] <- TRUE
students[ind_3,"five_year_graduation_flag"] <- TRUE
students[ind_3,"six_year_graduation_flag"] <- TRUE
students[ind_4,"five_year_graduation_flag"] <- TRUE
students[ind_4,"six_year_graduation_flag"] <- TRUE
students[ind_5,"six_year_graduation_flag"] <- TRUE