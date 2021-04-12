#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)

# .................load the libraries..............................................
# args <- commandArgs(trailingOnly = TRUE)
# srcFile <- args[1]

srcFile="input/ques_input_file.csv"
data<- read.csv(file = srcFile, sep = ',', header = TRUE , stringsAsFactors = FALSE)


library(ltm)
library(tidyr)
library(lubridate)
library(ltm)
library(gtools)
library('dplyr')

# .................set working directory and read the raw data ..............................................

# setwd()
# data <- newdata

### read question_parameters from centerlised CSV file
ques_param<- read.csv(file = "output/ques_param.csv", sep = ',', header = TRUE , stringsAsFactors = FALSE)

### check if all required columns present in inout

reg_cols<- c("unique_userid","question_id","test_id","subject","correctness")
for (c in reg_cols)
{
  if (!(c %in% colnames(data))){
    print(paste(c,"column not present in input file"))
  } 
}

# .................data pre-processing..............................................
### dichotomy

data$correctness <- replace(data$correctness, data$correctness == "-1" , "0")


# keep required columns only
a_df <- data[c("unique_userid","question_id","test_id","subject","correctness")]

sections <- unique(a_df$subject)
print(sections)

### 2PL model

final_df <- data.frame()
questions_df_final <- ques_param
sections <- unique(a_df$subject)


for (c in sections){
  print(paste("question parameterisation for subject",c))
  a_df_section <- a_df[which(a_df$subject == c),]
  test_id <- unique(a_df_section$test_id)
  users_resp_count = aggregate(question_id ~ unique_userid, a_df_section, function(x) length(unique(x)))
  cnt <- max(users_resp_count$question_id)
  print(paste("Number of Questions:",cnt))
  selected_user <- users_resp_count[which(users_resp_count$question_id == cnt),] 
  print(paste("Total number of user_id in input file",length(unique(a_df_section$unique_userid))))
  unique_userid <- unique(selected_user$unique_userid)
  
  selected_users <- as.data.frame(unique_userid)
  print(paste("Number of user_id selected for model",length(selected_users$unique_userid)))
  
  selected_user_resp <- merge(a_df_section, selected_users, by.x="unique_userid", by.y="unique_userid")
  selected_user_resp2 <- selected_user_resp[c("unique_userid","question_id","correctness")]
  resp_wide_df <- selected_user_resp2 %>% spread(question_id, correctness)  ## user response matrix 
  resp_wide_df2 <- resp_wide_df[,2:ncol(resp_wide_df)]
  
  ## Learn 2pl parameters using tpm function
  model <- paste(test_id,c,"2pl",sep="_")
  model_name <- paste("output/",model,".rds",sep="")
  print(model_name)
  model = ltm(resp_wide_df2 ~ z1, IRT.param = TRUE)
 
  # save the model to disk
  saveRDS(model, model_name)
  
  ### save questions parameters
  ques_parameters <- as.data.frame(coef(model))
  ques_parameters$test_id <- test_id
  ques_parameters$chapter <- c
  ques_parameters$question_code <- rownames(ques_parameters)
  ques_parameters$updated_date <- format(Sys.Date(),"%m/%d/%Y")
  
  ### check for not descriminative Questions
  non_discr_ques <- ques_parameters[which(ques_parameters$Dffclt >= 5),] 
  print(paste("These questions are not discrimnative and resulting into unintutive question difficulty",as.list(unique(non_discr_ques$question_code))))
  ### actual resposne count
  ques_cor_resp <- aggregate(as.numeric(selected_user_resp$correctness), by=list(Category=selected_user_resp$question_id), FUN=sum)
  colnames(ques_cor_resp)[2] <- "correctness_per"
  questions_df <- merge(ques_parameters, ques_cor_resp, by.x="question_code", by.y="Category")
  questions_df_final <- rbind(questions_df_final,questions_df)
  write.csv(questions_df_final, file = "output/ques_param.csv", row.names = FALSE)
  print("Question parameterisation done successfully")
}


