library(readr)
setwd("C:/Users/BT/Documents/IMSM_SS/data")
drug <- read.table("SAMSI_DrugTable_v2.txt", sep = "\t", header = TRUE)
member <- read.table("SAMSI_Member.txt", sep = "\t", header = TRUE)
names(member)[1] <- "MemberID"
member.wk <- read.table("SAMSI_MemberSummaryWk_v2.txt", sep = "\t", header = TRUE,
                        colClasses = c(rep("numeric", 26), "factor"))
names(member.wk)[1] <- "MemberID"
member.wk.pos <- member.wk[member.wk$Treatmend_Ind>0,]

### Make descriptive statistics table of charlson comorbidities, gender, age
## Use age at first diagnosis
library(dplyr)
library(qwraps2)
my_summary1 <- list("Age at First Diagnosis" = 
                      list("min" = ~ min(Age_FirstRADx),
                           "max" = ~ max(Age_FirstRADx),
                           "mean (sd)" = ~ qwraps2::mean_sd(Age_FirstRADx),
                           "median (iqr)" = ~ qwraps2::median_iqr(Age_FirstRADx)),
                    "Gender" =
                      list("Male" = ~ qwraps2::n_perc0(Gender == "M"),
                           "Female" = ~ qwraps2::n_perc0(Gender == "F")),
                    "AIDS/HIV" =
                      list("Prior" = ~ qwraps2::n_perc0(AIDS_HIV_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(AIDS_HIV_After == 1)),
                    "Acute Myocardial Infarction" =
                      list("Prior" = ~ qwraps2::n_perc0(AMI_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(AMI_After == 1)),
                    "Angina" =
                      list("Prior" = ~ qwraps2::n_perc0(Angina_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Angina_After == 1)),
                    "Cancer" =
                      list("Prior" = ~ qwraps2::n_perc0(Cancer_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Cancer_After == 1)),
                    "Cerebrovascular Disease" =
                      list("Prior" = ~ qwraps2::n_perc0(CEVD_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(CEVD_After == 1)),
                    "Congestive Heart Failure" =
                      list("Prior" = ~ qwraps2::n_perc0(CHF_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(CHF_After == 1)),
                    "COPD" =
                      list("Prior" = ~ qwraps2::n_perc0(COPD_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(COPD_After== 1)),
                    "Dementia" =
                      list("Prior" = ~ qwraps2::n_perc0(Dementia_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Dementia_After == 1)),
                    "Diabetes" =
                      list("Prior" = ~ qwraps2::n_perc0(Diabetes_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Diabetes_After == 1)),
                    "Hypertension" =
                      list("Prior" = ~ qwraps2::n_perc0(Hypertension_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Hypertension_After == 1)),
                    "Liver Disease" =
                      list("Prior" = ~ qwraps2::n_perc0(Liver_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Liver_After == 1)),
                    "Paralysis" =
                      list("Prior" = ~ qwraps2::n_perc0(Paralysis_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Paralysis_After == 1)),
                    "Peripheral Vascular Disease" =
                      list("Prior" = ~ qwraps2::n_perc0(PVD_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(PVD_After == 1)),
                    "Renal Failure" =
                      list("Prior" = ~ qwraps2::n_perc0(Renal_Failure_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Renal_Failure_After == 1)),
                    "Ulcers" =
                      list("Prior" = ~ qwraps2::n_perc0(Ulcers_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Ulcers_After == 1)),
                    "Depression" =
                      list("Prior" = ~ qwraps2::n_perc0(Depression_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Depression_After == 1)),
                    "Skin Ulcers" =
                      list("Prior" = ~ qwraps2::n_perc0(Skin_Prior == 1),
                           "After" = ~ qwraps2::n_perc0(Skin_After == 1)))
summary_table(member, my_summary1) # gives LaTeX code for a table


## 2-Way Frequency Tables of Charlson comorbidities
attach(member)
aids.tab <- table(AIDS_HIV_Prior, AIDS_HIV_After)
angina.tab <- table(Angina_Prior, Angina_After)
ami.tab <- table(AMI_Prior, AMI_After)
cancer.tab <- table(Cancer_Prior, Cancer_After)
cevd.tab <- table(CEVD_Prior, CEVD_After)
chf.tab <- table(CHF_Prior, CHF_After)
copd.tab <- table(COPD_Prior, COPD_After)
dementia.tab <- table(Dementia_Prior, Dementia_After)
diabetes.tab <- table(Diabetes_Prior, Diabetes_After)
hypertension.tab <- table(Hypertension_Prior, Hypertension_After)
liver.tab <- table(Liver_Prior, Liver_After)
paralysis.tab <- table(Paralysis_Prior, Paralysis_After)
pvd.tab <- table(PVD_Prior, PVD_After)
renal.tab <- table(Renal_Failure_Prior, Renal_Failure_After)
ulcers.tab <- table(Ulcers_Prior, Ulcers_After)
depress.tab <- table(Depression_Prior, Depression_After)
skin.tab <- table(Skin_Prior, Skin_After)

###################################
## Use Data from data_analysis.R ##
###################################
my_summary2 <- list("Age at First Diagnosis" = 
                      list("min" = ~ min(Age_FirstRADx),
                           "max" = ~ max(Age_FirstRADx),
                           "mean (sd)" = ~ qwraps2::mean_sd(Age_FirstRADx),
                           "median (iqr)" = ~ qwraps2::median_iqr(Age_FirstRADx)),
                    "Gender" =
                      list("Male" = ~ qwraps2::n_perc0(Gender == "M"),
                           "Female" = ~ qwraps2::n_perc0(Gender == "F")),
                    "AIDS/HIV" =
                      list("Yes" = ~ qwraps2::n_perc0(AIDS_Ind == 1)),
                    "Acute Myocardial Infarction" =
                      list("Yes" = ~ qwraps2::n_perc0(AMI_Ind == 1)),
                    "Angina" =
                      list("Yes" = ~ qwraps2::n_perc0(Angina_Ind == 1)),
                    "Cancer" =
                      list("Yes" = ~ qwraps2::n_perc0(Cancer_Ind == 1)),
                    "Cerebrovascular Disease" =
                      list("Yes" = ~ qwraps2::n_perc0(CEVD_Ind == 1)),
                    "Congestive Heart Failure" =
                      list("Yes" = ~ qwraps2::n_perc0(CHF_Ind == 1)),
                    "COPD" =
                      list("Yes" = ~ qwraps2::n_perc0(COPD_Ind == 1)),
                    "Dementia" =
                      list("Yes" = ~ qwraps2::n_perc0(Dementia_Ind == 1)),
                    "Diabetes" =
                      list("Yes" = ~ qwraps2::n_perc0(Diabetes_Ind == 1)),
                    "Hypertension" =
                      list("Yes" = ~ qwraps2::n_perc0(Hyptn_Ind == 1)),
                    "Liver Disease" =
                      list("Yes" = ~ qwraps2::n_perc0(Liver_Ind == 1)),
                    "Paralysis" =
                      list("Yes" = ~ qwraps2::n_perc0(Paralysis_Ind == 1)),
                    "Peripheral Vascular Disease" =
                      list("Yes" = ~ qwraps2::n_perc0(PVD_Ind == 1)),
                    "Renal Failure" =
                      list("Yes" = ~ qwraps2::n_perc0(Renal_Ind == 1)),
                    "Ulcers" =
                      list("Yes" = ~ qwraps2::n_perc0(Ulcers_Ind == 1)),
                    "Depression" =
                      list("Yes" = ~ qwraps2::n_perc0(Depression_Ind == 1)),
                    "Skin Ulcers" =
                      list("Yes" = ~ qwraps2::n_perc0(Skin_Ind == 1)))
summary_table(stage1.analysis, my_summary2) # stats for patients included in month 1
summary_table(stage2.analysis, my_summary2) # stats for patients in Stage 2 Analysis


### Treatment Frequencies
# Method 1
plyr::count(member.wk, "Treatment_Group")
by_trt <- group_by(member.wk, Treatment_Group)
summarise(by_trt, count=n())
# Method 2
trt.freqs <- plyr::count(member.wk.pos, "Treatment_Group")
## Determine cutoff for uncommon treatment combinations
(sum(trt.freqs$freq)-trt.freqs$freq[1])*.001
sum(trt.freqs$freq < 500)

### Automated search for importance of uncommon treatment combinations
trts.drop.inds <- which(trt.freqs$freq < 500)
trts.drop <- as.vector(trt.freqs$Treatment_Group[trts.drop.inds])
    ids.wkinds <- vector("list", length(trts.drop))
list.drop <- vector("list", length(trts.drop))
drop.ids <- vector("list", length(trts.drop))
poss.valid <- vector("list", length(trts.drop))
list.drop.nrow <- matrix(0L, nrow=length(trts.drop), ncol=2)
for(i in 1:length(trts.drop)){
  ids.wkinds[[i]] <- member.wk.pos[member.wk.pos$Treatment_Group == trts.drop[i], 1:2]
  list.drop[[i]] <- count(ids.wkinds[[i]], ids.wkinds[[i]][,1]) # count weeks on trt i for each unique subject
  drop.ids[[i]] <- list.drop[[i]][,1]
  poss.valid[[i]] <- list.drop[[i]][list.drop[[i]][,2]>=4,] # subjects who used trt i for >= 4 wks
  list.drop.nrow[i,1] <- nrow(list.drop[[i]]) # number of unique subjects on trt i
  list.drop.nrow[i,2] <- nrow(poss.valid[[i]]) # number of subjects who used trt i for >= 4 wks
}
sum(unique(list.drop.nrow[,1])) # 243
sum(list.drop.nrow[,2]) #  53

ids.drop <- unique(member.wk.pos$MemberID[which(member.wk.pos$Treatment_Group %in% trts.drop)])

## Sort member.wk.pos by memberID
member.wk.pos <- member.wk.pos[order(member.wk.pos$MemberID, member.wk.pos$WeekInd),]

####################################################################33
# Add Charlson Index for Prior and After
attach(member.red)
index_prior <- vector("numeric",nrow(member.red))
for(i in 1:nrow(member.red)){
  if(Age_FirstRADx[i] %in% 50:59){index_prior[i] <- index_prior[i] + 1}
  else if(Age_FirstRADx[i] %in% 60:69){index_prior[i] <- index_prior[i] + 2}
  else if(Age_FirstRADx[i] %in% 70:79){index_prior[i] <- index_prior[i] + 3}
  else if(80 <= Age_FirstRADx[i]){index_prior[i] <- index_prior[i] + 4}
  
  if(Diabetes_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(Liver_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1} # assume mild liver disease
  if(Cancer_Prior[i] == 1){index_prior[i] <- index_prior[i] + 6} # assume metastatic solid tumor
  if(AIDS_HIV_Prior[i] == 1){index_prior[i] <- index_prior[i] + 6}
  if(Renal_Failure_Prior[i] == 1){index_prior[i] <- index_prior[i] + 2}
  if(CHF_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(AMI_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(COPD_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(PVD_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(CEVD_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(Dementia_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
  if(Ulcers_Prior[i] == 1){index_prior[i] <- index_prior[i] + 1}
}

index_after <- vector("numeric",nrow(member.red))
for(i in 1:nrow(member.red)){
  if(Age_FirstRADx[i] %in% 50:59){index_after[i] <- index_after[i] + 1}
  else if(Age_FirstRADx[i] %in% 60:69){index_after[i] <- index_after[i] + 2}
  else if(Age_FirstRADx[i] %in% 70:79){index_after[i] <- index_after[i] + 3}
  else if(80 <= Age_FirstRADx[i]){index_after[i] <- index_after[i] + 4}
  
  if(Diabetes_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(Liver_After[i] == 1){index_after[i] <- index_after[i] + 1} # assume mild liver disease
  if(Cancer_After[i] == 1){index_after[i] <- index_after[i] + 6} # assume metastatic solid tumor
  if(AIDS_HIV_After[i] == 1){index_after[i] <- index_after[i] + 6}
  if(Renal_Failure_After[i] == 1){index_after[i] <- index_after[i] + 2}
  if(CHF_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(AMI_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(COPD_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(PVD_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(CEVD_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(Dementia_After[i] == 1){index_after[i] <- index_after[i] + 1}
  if(Ulcers_After[i] == 1){index_after[i] <- index_after[i] + 1}
}
#####################################################################################