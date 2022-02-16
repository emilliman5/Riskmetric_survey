library(tidyverse)
setwd("~/OneDrive - Biogen/packages/r_package_survey/R_package_risk_survey/")

ap_df_filtered <- readRDS("packages_selection_current.rds")
#ap_df_filtered %>% select(1:3) %>% View()

qs <- dplyr::bind_rows(data.frame(question = c(rep("How many years of experience with R do you have?", 4),
                                        rep("What sector are you currently in?", 6),
                                        rep("What best describes your current role?", 6),
                                        rep("Have you ever published an R package (either public or private)?", 2)),
                           option = c("< 1","1-3","4-7","> 7", 
                                      "Academia","Government","Biotech/Pharma", "Healthcare", "Non-profit", "Other",
                                      "Student", "Statistician","Statistical Programmer", "Data Scientist","Quality Lead","Manager/Director/VP",
                                      "Yes","No"),
                           input_type = c(rep("mc", 16), "y/n", "y/n"),
                           input_id = c(rep("yearsExp", 4), rep("industry", 6), rep("role",6), rep("package",2)),
                           dependence = NA,
                           dependence_value = NA,
                           required = T,
                           stringsAsFactors = F),
                data.frame(question = rep(ap_df_filtered$package, each = 6),
                           option = rep(c("NA","1 - (Lowest Risk/Highest Quality)",2:4, 
                                          "5 - (Highest Risk/Lowest Quality)"), 
                                        length(ap_df_filtered$package)),
                           input_type = c("mc"),
                           input_id = rep(ap_df_filtered$package, each = 6),
                           dependence = NA,
                           dependence_value = NA,
                           required = F,
                           stringsAsFactors = F) 
)
saveRDS(qs, paste0("qs_",gsub("-","", Sys.Date()),".rds"))
saveRDS(qs, paste0("qs_","current",".rds"))
