# ADHD_Questionnaire_Project

## data analysis for final project: Biostats Fall 2020 
# at St. Lawrence University
# by Anna Foster
# Topic: ADHD questionnaire over two separate periods in a study about working memory and reward in children, with and without ADHD
  # source: Booth J.R., Cooke G.E., Gayda J., Hammer R., Lytle M.N., Stein M.A., Tennekoon M. 
    # Working memory and reward in children with and without attention deficit hyperactivity disorder (ADHD) 2020
    # https://openneuro.org/datasets/ds002424/versions/1.1.1

# research question: what questions from the questionnaire best predict the total raw ADHDRS-IV score?
  # dependent variable is ADHD Total Raw Score
  # independent variables are the questions asked of participants (see QuestionnaireDescription.txt file)
    # after looking at correlations, we only will examine questions 1,2,4,5,6,8,14
    # as these variables can only have values 0 -> 3, there are no outliers
# all variables follow normality fairly well, 
  # but questions 1,2, and 6 taper off at the ends more than the others

# hypothesis: it is predicted that Questions 4 and 5 will be the best as they have the best normal distributions
  # we can reject or support this hypothesis through multiple regression and AIC
    # AIC tests how well each questions' linear model predicts total raw score
