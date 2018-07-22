#######################
## IMSM 2018         ##
## Data Analysis     ##
#######################

# Load data
member.wk <- read.table("SAMSI_MemberSummaryWk_v2.txt", sep = "\t", header = TRUE,
                        colClasses = c(rep("numeric", 26), "factor"))
names(member.wk)[1] <- "MemberID"
all.months <- read.csv("dataframe_postrt_month_updated.csv", header = TRUE)
member <- read.table("SAMSI_Member.txt", sep = "\t", header = TRUE)
names(member)[1] <- "MemberID"
# Merge data
all.mo.covs <- merge(all.months, member, by="MemberID")


## Create Gluco_or_Painkiller_Flag
alt <- vector("numeric", nrow(all.mo.covs))
alt[which(all.mo.covs$Painkiller_flag == 1 | all.mo.covs$Gluco_Flag == 1)] <- 1
alt2 <- vector("numeric", nrow(all.mo.covs))
alt2[which(all.mo.covs$Painkiller_flag == 1 & all.mo.covs$Gluco_Flag == 1)] <- 1
all.mo.covs <- cbind(all.mo.covs, "Gluco_or_Painkiller_Flag" = alt, 
                       "Gluco_and_Painkiller_Flag" = alt2)

###########################################
#### Consolidate Comorbidity Variables ####
###########################################
member.wk <- read.table("SAMSI_MemberSummaryWk_v2.txt", sep = "\t", header = TRUE,
                        colClasses = c(rep("numeric", 26), "factor"))
names(member.wk)[1] <- "MemberID"
my.peeps <- unique(all.mo.covs$MemberID)

AIDS_Month_After_Trt <- vector("numeric", length(my.peeps))
AMI_Month_After_Trt <- vector("numeric", length(my.peeps))
Angina_Month_After_Trt <- vector("numeric", length(my.peeps))
Cancer_Month_After_Trt <- vector("numeric", length(my.peeps))
CEVD_Month_After_Trt <- vector("numeric", length(my.peeps))
CHF_Month_After_Trt <- vector("numeric", length(my.peeps))
COPD_Month_After_Trt <- vector("numeric", length(my.peeps))
Dementia_Month_After_Trt <- vector("numeric", length(my.peeps))
Diabetes_Month_After_Trt <- vector("numeric", length(my.peeps))
Hyptn_Month_After_Trt <- vector("numeric", length(my.peeps))
Liver_Month_After_Trt <- vector("numeric", length(my.peeps))
Paralysis_Month_After_Trt <- vector("numeric", length(my.peeps))
PVD_Month_After_Trt <- vector("numeric", length(my.peeps))
Renal_Month_After_Trt <- vector("numeric", length(my.peeps))
Ulcers_Month_After_Trt <- vector("numeric", length(my.peeps))
Depression_Month_After_Trt <- vector("numeric", length(my.peeps))
Skin_Month_After_Trt <- vector("numeric", length(my.peeps))

for(i in 1:length(my.peeps)){
  if(member$AIDS_HIV_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    AIDS_Month_After_Trt[i] <- ceiling((member$AIDS_HIV_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$AMI_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    AMI_Month_After_Trt[i] <- ceiling((member$AMI_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Angina_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Angina_Month_After_Trt[i] <- ceiling((member$Angina_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Cancer_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Cancer_Month_After_Trt[i] <- ceiling((member$Cancer_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$CEVD_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    CEVD_Month_After_Trt[i] <- ceiling((member$CEVD_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$CHF_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    CHF_Month_After_Trt[i] <- ceiling((member$CHF_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$COPD_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    COPD_Month_After_Trt[i] <- ceiling((member$COPD_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Dementia_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Dementia_Month_After_Trt[i] <- ceiling((member$Dementia_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Diabetes_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Diabetes_Month_After_Trt[i] <- ceiling((member$Diabetes_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Hypertension_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Hyptn_Month_After_Trt[i] <- ceiling((member$Hypertension_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Liver_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Liver_Month_After_Trt[i] <- ceiling((member$Liver_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Paralysis_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Paralysis_Month_After_Trt[i] <- ceiling((member$Paralysis_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$PVD_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    PVD_Month_After_Trt[i] <- ceiling((member$PVD_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Renal_Failure_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Renal_Month_After_Trt[i] <- ceiling((member$Renal_Failure_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Ulcers_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Ulcers_Month_After_Trt[i] <- ceiling((member$Ulcers_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Depression_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Depression_Month_After_Trt[i] <- ceiling((member$Depression_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
  if(member$Skin_After_Week[which(member$MemberID == my.peeps[i])] > 0){
    Skin_Month_After_Trt[i] <- ceiling((member$Skin_After_Week[which(member$MemberID == my.peeps[i])] - 
                                         member.wk$WeekInd[which(member.wk$MemberID == my.peeps[i] & member.wk$TreatmentInd == 1)] + 1)/4)
  }
}

AIDS_Ind <- vector("numeric", nrow(all.mo.covs))
AMI_Ind <- vector("numeric", nrow(all.mo.covs))
Angina_Ind <- vector("numeric", nrow(all.mo.covs))
Cancer_Ind <- vector("numeric", nrow(all.mo.covs))
CEVD_Ind <- vector("numeric", nrow(all.mo.covs))
CHF_Ind <- vector("numeric", nrow(all.mo.covs))
COPD_Ind <- vector("numeric", nrow(all.mo.covs))
Dementia_Ind <- vector("numeric", nrow(all.mo.covs))
Diabetes_Ind <- vector("numeric", nrow(all.mo.covs))
Hyptn_Ind <- vector("numeric", nrow(all.mo.covs))
Liver_Ind <- vector("numeric", nrow(all.mo.covs))
Paralysis_Ind <- vector("numeric", nrow(all.mo.covs))
PVD_Ind <- vector("numeric", nrow(all.mo.covs))
Renal_Ind <- vector("numeric", nrow(all.mo.covs))
Ulcers_Ind <- vector("numeric", nrow(all.mo.covs))
Depression_Ind <- vector("numeric", nrow(all.mo.covs))
Skin_Ind <- vector("numeric", nrow(all.mo.covs))

for(i in 1:length(my.peeps)){
  # AIDS/HIV
  if(member$AIDS_HIV_Prior[which(member$MemberID == my.peeps[i])] == 1){
    AIDS_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(AIDS_Month_After_Trt[i] < 0){
    AIDS_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Acute Myocardial Infarction
  if(member$AMI_Prior[which(member$MemberID == my.peeps[i])] == 1){
    AMI_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(AMI_Month_After_Trt[i] < 0){
    AMI_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Angina
  if(member$Angina_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Angina_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Angina_Month_After_Trt[i] < 0){
    Angina_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Cancer
  if(member$Cancer_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Cancer_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Cancer_Month_After_Trt[i] < 0){
    Cancer_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Cerebrovascular disease
  if(member$CEVD_Prior[which(member$MemberID == my.peeps[i])] == 1){
    CEVD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(CEVD_Month_After_Trt[i] < 0){
    CEVD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Congestive Heart Failure
  if(member$CHF_Prior[which(member$MemberID == my.peeps[i])] == 1){
    CHF_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(CHF_Month_After_Trt[i] < 0){
    CHF_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # COPD
  if(member$COPD_Prior[which(member$MemberID == my.peeps[i])] == 1){
    COPD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(COPD_Month_After_Trt[i] < 0){
    COPD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Dementia
  if(member$Dementia_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Dementia_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Dementia_Month_After_Trt[i] < 0){
    Dementia_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Diabetes
  if(member$Diabetes_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Diabetes_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Diabetes_Month_After_Trt[i] < 0){
    Diabetes_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Hypertension
  if(member$Hypertension_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Hyptn_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Hyptn_Month_After_Trt[i] < 0){
    Hyptn_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Liver Disease
  if(member$Liver_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Liver_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Liver_Month_After_Trt[i] < 0){
    Liver_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Paralysis
  if(member$Paralysis_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Paralysis_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Paralysis_Month_After_Trt[i] < 0){
    Paralysis_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # PVD
  if(member$PVD_Prior[which(member$MemberID == my.peeps[i])] == 1){
    PVD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(PVD_Month_After_Trt[i] < 0){
    PVD_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # Renal Failure
  if(member$Renal_Failure_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Renal_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Renal_Month_After_Trt[i] < 0){
    Renal_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(member$Ulcers_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Ulcers_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Ulcers_Month_After_Trt[i] < 0){
    Ulcers_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(member$Depression_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Depression_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Depression_Month_After_Trt[i] < 0){
    Depression_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(member$Skin_Prior[which(member$MemberID == my.peeps[i])] == 1){
    Skin_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  if(Skin_Month_After_Trt[i] < 0){
    Skin_Ind[which(all.mo.covs$MemberID == my.peeps[i])] <- 1
  }
  # else if(AIDS_Month_After_Trt[i] > 0){
  #   AIDS_Ind[which(all.mo.covs$MemberID == my.peeps[i] & 
  #       all.mo.covs$month_count >= AIDS_Month_After_Trt[i])] <- 1
  # }
}
cov.inds <- cbind(AIDS_Ind, AMI_Ind, Angina_Ind, Cancer_Ind, CEVD_Ind, CHF_Ind, 
                  COPD_Ind, Dementia_Ind, Diabetes_Ind, Hyptn_Ind, Liver_Ind, 
                  Paralysis_Ind, PVD_Ind, Renal_Ind, Ulcers_Ind, Depression_Ind,
                  Skin_Ind)
## Need to adjust columns
all.mo.covs.adj <- cbind(all.mo.covs[,c(1, 4:9, 11:13, 15, 17:18, 20:24, 79:80)], cov.inds)

#write.csv(all.mo.covs.adj, "allmonths_adjcomorbidities.csv", row.names = FALSE)

#########################
#### Model Selection ####
#########################
all.mo.covs.adj <- read.csv("allmonths_adjcomorbidities.csv", header = TRUE)
all.mo.covs.adj$Treatment_Group <- as.factor(all.mo.covs.adj$Treatment_Group)
month1.data <- all.mo.covs.adj[which(all.mo.covs.adj$month_count == 1),]
month2.data <- all.mo.covs.adj[which(all.mo.covs.adj$month_count == 2),]
stage1.data <- merge(month1.data, month2.data[,c(1,19)], by="MemberID")
mo1.freqs <- plyr::count(stage1.data, "Treatment_Group")
mo1.trts.drop <- mo1.freqs$Treatment_Group[which(mo1.freqs$freq < 100)]
mo1.ID.drop <- unique(stage1.data$MemberID[which(stage1.data$Treatment_Group %in% mo1.trts.drop)])
stage1.analysis <- stage1.data[-which(stage1.data$MemberID %in% mo1.ID.drop),]
my.IDs <- unique(stage1.analysis$MemberID)
## Fit model with all comorbidites, response = Gluco or Painkiller
fullmod1 <- glm(Gluco_or_Painkiller_Flag.y ~ Treatment_Group*(Age_FirstRADx + Gender + 
                  Sbscr_Ind + AIDS_Ind + AMI_Ind + Angina_Ind + Cancer_Ind + CEVD_Ind +
                  CHF_Ind + COPD_Ind + Dementia_Ind + Diabetes_Ind + Hyptn_Ind + Liver_Ind +
                  Paralysis_Ind + PVD_Ind + Renal_Ind + Ulcers_Ind + Depression_Ind +
                  Skin_Ind), family=binomial(link = "logit"), data=stage1.analysis)
backwards1 <- step(fullmod1)
formula(backwards1)
## Fit model with comorbidities >= 5%, response = Gluco or Painkiller
fullmod12 <- glm(Gluco_or_Painkiller_Flag.y ~ Treatment_Group*(Age_FirstRADx + Gender + 
                    Sbscr_Ind + Angina_Ind + COPD_Ind + Diabetes_Ind + Hyptn_Ind + 
                    Liver_Ind + Depression_Ind + Skin_Ind), 
                family=binomial(link = "logit"), data=stage1.analysis)


data.anal.new$Treatment_Group <- as.numeric(data.anal.new$Treatment_Group)
data.anal.new$Age_FirstRADx <- as.numeric(data.anal.new$Age_FirstRADx)
Gender_bin <- vector("numeric", nrow(stage1.analysis))
Gender_bin[which(stage1.analysis$Gender == "M")] <- 1
Gender_bin[which(stage1.analysis$Gender == "F")] <- 0

cov.mat <- matrix(unlist(c(data.anal.new[,c(3,7,15:32)],Gender_bin)), nrow = nrow(data.anal.new))
corr.mat <- cor(cov.mat, method="pearson") # corr(Age, Hypertension) = .31
cor(Gender_bin, stage1.analysis$Sbscr_Ind, method="pearson")

########################################
### Subset Data for 2 Month Analysis ###
########################################
freq.ID <- plyr::count(all.mo.covs.adj, "MemberID")
keep.ID <- freq.ID$MemberID[which(freq.ID$freq >= 3)]
data.IDlength <- all.mo.covs.adj[which(all.mo.covs.adj$MemberID %in% keep.ID),]
data.IDlength$Treatment_Group <- as.factor(data.IDlength$Treatment_Group)

month1.new <- data.IDlength[which(data.IDlength$month_count == 1),]
month2.new <- data.IDlength[which(data.IDlength$month_count == 2),]

# Delete patients whose treatments occur fewer than 100 times in each month
month1.freqs <- plyr::count(month1.new, "Treatment_Group")
month1.trts.drop <- month1.freqs$Treatment_Group[which(month1.freqs$freq < 100)]
month1.ID.drop <- unique(month1.new$MemberID[which(month1.new$Treatment_Group %in% month1.trts.drop)])
month2.freqs <- plyr::count(month2.new, "Treatment_Group")
month2.trts.drop <- month2.freqs$Treatment_Group[which(month2.freqs$freq < 100)]
month2.ID.drop <- unique(month2.new$MemberID[which(month2.new$Treatment_Group %in% month2.trts.drop)])

final.ID <- union(month1.ID.drop, month2.ID.drop)
data.IDlength.red <- data.IDlength[-which(data.IDlength$MemberID %in% final.ID),]
month1.red <- data.IDlength.red[which(data.IDlength.red$month_count == 1),]
month2.red <- data.IDlength.red[which(data.IDlength.red$month_count == 2),]
month2.freqs2 <- plyr::count(month2.red, "Treatment_Group")
month2.trts.drop2 <- month2.freqs2$Treatment_Group[which(month2.freqs2$freq < 100)]
month2.ID.drop2 <- unique(month2.red$MemberID[which(month2.red$Treatment_Group %in% month2.trts.drop2)])

data.IDlength.red2 <- data.IDlength.red[-which(data.IDlength.red$MemberID %in% month2.ID.drop2),]
#write.csv(data.IDlength.red2, "stage2data_commonTrts.csv", row.names = FALSE)
#read.my.file <- read.csv("stage2data_commonTrts.csv")
month1.red2 <- data.IDlength.red2[which(data.IDlength.red2$month_count == 1),]
month2.red2 <- data.IDlength.red2[which(data.IDlength.red2$month_count == 2),]

##################################
### Stage 2 Variable Selection ###
##################################
# Define analysis matrix
stage2.data <- month1.red2[,c(1:7, 10:14, 17, 19, 21:37)]
stage2.analysis <- merge(stage2.data, month2.red2[,c(1,10,19)], by="MemberID")

# Stage 2 analysis
stage2.mod <- glm(Gluco_or_Painkiller_Flag.y ~ Treatment_Group.x + Treatment_Group.y*(Age_FirstRADx + Gender + Sbscr_Ind +
                      AIDS_Ind + AMI_Ind + Angina_Ind + Cancer_Ind + CEVD_Ind + CHF_Ind +
                      COPD_Ind + Dementia_Ind + Diabetes_Ind + Hyptn_Ind + Liver_Ind +
                      Paralysis_Ind + PVD_Ind + Renal_Ind + Ulcers_Ind + Depression_Ind +
                      Skin_Ind + IP_Visits + IP_Days + OP_Visits + ER_Visits + 
                      DR_Visits + RX_Scripts + RA_Dx_SpecialistFlag +
                      Gluco_or_Painkiller_Flag.x), 
                  family=binomial(link="logit"), data=stage2.analysis)
stage2.mod2 <- glm(Gluco_or_Painkiller_Flag.y ~ Treatment_Group.x + 
                    Treatment_Group.y*(Age_FirstRADx + Gender + Sbscr_Ind +
                    Angina_Ind + Cancer_Ind + CEVD_Ind + CHF_Ind +
                    COPD_Ind + Diabetes_Ind + Hyptn_Ind + Liver_Ind +
                    PVD_Ind + Renal_Ind + Ulcers_Ind + Depression_Ind +
                    Skin_Ind + OP_Visits + ER_Visits + 
                    DR_Visits + RX_Scripts + RA_Dx_SpecialistFlag +
                    Gluco_or_Painkiller_Flag.x), 
                  family=binomial(link="logit"), data=stage2.analysis)
back.stage2 <- step(stage2.mod2)
formula(back.stage2)

stage2.mod3 <- glm(Gluco_or_Painkiller_Flag.y ~ Treatment_Group.x + 
                     Treatment_Group.y*(Age_FirstRADx + Gender + Sbscr_Ind +
                      Angina_Ind + Cancer_Ind + CEVD_Ind + CHF_Ind + COPD_Ind +
                      Diabetes_Ind + Hyptn_Ind + Liver_Ind + PVD_Ind + Renal_Ind + 
                      Ulcers_Ind + Depression_Ind + Skin_Ind  +
                      Gluco_or_Painkiller_Flag.x), 
                   family=binomial(link="logit"), data=stage2.analysis)
back.stage3 <- step(stage2.mod3)
formula(back.stage2)

month2.new.red <- month2.new[which(month2.new$MemberID %in% month12.analysis$MemberID),]