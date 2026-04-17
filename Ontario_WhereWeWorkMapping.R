#Where we work map for Ontario Programs. 

library(naturecounts)
library(tidyverse)

data<-nc_data_dl(username = "dethier", years=c(2024,2025), region = list(statprov = "ON"), info="Ontario Sample Map")

data<-nc_data_dl(username = "dethier", 
                 years=c(2024,2025), 
                 region = list(statprov = "ON"), 
                 collection = c("ONBANS", "NESTWATCH", "ONATLAS3BE_DO", "ONOWLS", "PFW", "SWIFTWATCH", "NIGHTJAR", "MMPBIRDS", "CMMN-DET-IPBO", "CMMN-DET-LPBO", "CMMN-DET-PIBO", "CMMN-DET-PEPBO", "CMMN-DET-TCBO", "CMMN-DET-TTPBRS"),
                 info="Ontario Sample Map")
