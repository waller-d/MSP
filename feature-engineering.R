# Title -------------------------------------------------------------------

#  Feature engineering


# Packages ----------------------------------------------------------------

library(tidyverse)


# Functions ---------------------------------------------------------------




# Create variables for further analysis ---------------------------------------------

# Create a variable that combines "bgr", "bgri" for orientation programs (community affiliation)
orientation <- ifelse((students$bgr == TRUE | students$bgri == TRUE), TRUE, FALSE)

# Create a variable that combines "learning_communities_flag", "mep_academic_boot_camp", "bop_flag", "wiepgroup_mentoring_flag", "wieppair_mentoring_flag" for community affiliation
community_affiliation <- ifelse((students$learning_communities_flag == TRUE | 
                                   students$mep_academic_boot_camp == TRUE | 
                                   students$bop_flag == TRUE | 
                                   students$study_abroad_flag == TRUE | # move to community affiliation
                                   students$wiepgroup_mentoring_flag == TRUE | 
                                   students$wieppair_mentoring_flag == TRUE), TRUE, FALSE)

# Create a variable that combines "cco_internship_flag", "coop_flag", "geare_flag", "study_abroad_flag", "summer_stay_flag", "summer_finish_flag" for research/internship/enrichment activities
work_related <- ifelse((students$cco_internship_flag == TRUE | 
                                   students$coop_flag == TRUE | 
                                   students$geare_flag == TRUE | 
                                   
                                   students$summer_stay_flag == TRUE |
                                   students$summer_finish_flag == TRUE), TRUE, FALSE) # remove summer finish (no work)

# Create a variable that combines "probation_flag", "behavioral_intervention_flag", "expulsion_suspension_probation_warning_flag" for at-risk students
at_risk <- ifelse((students$probation_flag == TRUE | 
                                   students$behavioral_intervention_flag == TRUE | 
                                   students$expulsion_suspension_probation_warning_flag == TRUE), TRUE, FALSE)

# Create a variable that combines "early_start_flag", "summer_start_flag", "bop_flag", "academic_bootcamp_flag", "mep_academic_boot_camp" for early summer programs (student support)
early_support <- ifelse((students$early_start_flag == TRUE | 
                          students$summer_start_flag == TRUE | # at_risk related
                          students$bop_flag == TRUE | # underrepresented student focus from Krannert (URM_programs)
                          students$academic_bootcamp_flag == TRUE | 
                          students$mep_academic_boot_camp == TRUE), TRUE, FALSE)

# Create a variable that combines "horizons_flag", "purpromise_flag", "twenty_first_century_scholar_flag" for low-income students (student support)
low_income <- ifelse((students$horizons_flag == TRUE | 
                     students$purpromise_flag == TRUE | 
                     students$twenty_first_century_scholar_flag == TRUE), TRUE, FALSE)

# Create a variable that combines "mep_academic_boot_camp", "purdue_bound_flag", "span_plan_nontraditional_student_flag", "emerging_ldr_flag", "wiepgroup_mentoring_flag", "wieppair_mentoring_flag" for underrepresented students
URM_programs <- ifelse((students$mep_academic_boot_camp == TRUE | 
                           students$purdue_bound_flag == TRUE | 
                           students$span_plan_nontraditional_student_flag == TRUE | 
                           students$bop_flag == TRUE | # underrepresented student focus from Krannert (URM_programs)
                           students$emerging_ldr_flag == TRUE | 
                           students$wiepgroup_mentoring_flag == TRUE | 
                           students$wieppair_mentoring_flag == TRUE), TRUE, FALSE)

# Create a variable that combines "si_attended_flag" for student support

# Create a variable that combines "si_student_leader_flag" for leadership activities

# Create a variable that combines "scholarship_flag", "pres_scholar_flag", "beering_scholar_flag", "trustee_scholar_flag", "stamps_ldrs_scholar_flag", "emerging_ldr_flag" for scholarships (student support)
scholarship <- ifelse((students$scholarship_flag == TRUE | 
                          students$pres_scholar_flag == TRUE | 
                          students$beering_scholar_flag == TRUE | 
                          students$trustee_scholar_flag == TRUE | 
                          students$stamps_ldrs_scholar_flag == TRUE | 
                          students$emerging_ldr_flag == TRUE), TRUE, FALSE)

# Append columns together

students <- cbind(students, orientation, community_affiliation, work_related, at_risk, early_support, low_income, URM_programs, scholarship)
# NOT considered: incoming credit variables, "drc_flag", "student_of_concern_flag"
# Relevant variables - student support, community affiliation, leadership activities

students %>% select_if(~ is.numeric(.) | is.logical(.))

lowerCor(students %>% select_if(~ is.numeric(.) | is.logical(.)))


