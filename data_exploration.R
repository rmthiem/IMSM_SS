library(readr)
setwd("C:/Users/BT/Documents/IMSM_SS/data")
drug <- read.table("SAMSI_DrugTable_v2.txt", sep = "\t", header = TRUE)
member <- read.table("SAMSI_Member.txt", sep = "\t", header = TRUE)
member.wk <- read.table("SAMSI_MemberSummaryWk_v2.txt", sep = "\t", header = TRUE,
                        colClasses = c(rep("numeric", 26), "factor"))
names(member.wk)[1] <- "MemberID"
member.wk.pos <- member.wk[member.wk$WeekInd>=0,]
member.wk.neg <- member.wk[member.wk$WeekInd<0,]

dim(drug)
dim(member)
dim(member.wk)
str(member.wk)

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
sum(list.drop.nrow[,1]) # 243
sum(list.drop.nrow[,2]) #  53


## Sort member.wk.pos by memberID
member.wk.pos <- member.wk.pos[order(member.wk.pos$MemberID, member.wk.pos$WeekInd),]