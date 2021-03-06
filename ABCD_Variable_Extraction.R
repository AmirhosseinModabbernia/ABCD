######--------------------------ABCD Preparation Script---------------------------######
#setwd()     #uncomment and set working directory to the directory you have the files in. 
setwd("/Volumes/Toshiba/ABCD_release3/ABCDStudyNDA")  #comment this when you set above

#All files but one are in the ABCD release package (as of Release 3.0.1). The
#the file is  neurocogpcs.csv that includes the scores on three genetic
#prinicpal components. It can be found in the latest ABCD release folder on our dropbox

# for some reason dhx01.txt (perinatal risk factors) file in the third release has empty values for the birthweight,
#so this script uses release 2.1.0 instead
#subjectkey will be  the main subject identifier 
#for the follow up data "eventname" also needs to be added because it identifies the number of follow up visit


###how to create git version control
#see here: https://hansenjohnson.org/post/sync-github-repository-with-existing-r-project/
#need to add ssh key this way:
#for git errors : https://stackoverflow.com/questions/22532943/how-to-resolve-git-error-updates-were-rejected-because-the-tip-of-your-current

library(car)
library(stringr)
library(rmatio)



#######------------------------------demographics (site sex age)----------------------------------------------#####
abcd_lt01 <- read.delim("abcd_lt01.txt")
abcd_lt01<-abcd_lt01[-1,]
abcd_lt01<-abcd_lt01[,c(4,7:10)]
names(abcd_lt01)
abcd_lt01[,2]<-sapply(abcd_lt01[,2], as.character)
abcd_lt01[,2]<-sapply(abcd_lt01[,2], as.numeric)
subjectkey_all<-subset(abcd_lt01,!duplicated(abcd_lt01$subjectkey,fromLast=FALSE)) 
write.csv(subjectkey_all[,1],"subjectkey_all.csv",row.names = F )

######----------------------------------KSADS background items-functioning-service use-Parent------------------------------------#######
dibf01<-read.delim("dibf01.txt")
dibf01<-dibf01[-1,]
names(dibf01)
dibf01<-dibf01[,c(4,89,36:43,46,49,60,62,65,70)]
dibf01[,-c(1,2)]<-sapply(dibf01[,-c(1,2)], as.character)
dibf01[,-c(1,2)]<-sapply(dibf01[,-c(1,2)], as.numeric)
summary(dibf01)
dibf01[,3:16]<-lapply(dibf01[,3:16],function(x) {ifelse(x==777, NA,x)})
dibf01[,3:16]<-lapply(dibf01[,3:16],function(x) {ifelse(x==999, NA,x)})
dibf01[,3:16]<-lapply(dibf01[,3:16],function(x) {ifelse(x==-1, NA,x)})
dibf01$kbi_p_grades_in_school<-ifelse(dibf01$kbi_p_grades_in_school==6, NA,dibf01$kbi_p_grades_in_school)

dibf01[,c("kbi_p_c_det_susp","kbi_p_c_best_friend","kbi_p_c_reg_friend_group" ,"kbi_p_c_mh_sa")]<-
  lapply(dibf01[,c("kbi_p_c_det_susp","kbi_p_c_best_friend","kbi_p_c_reg_friend_group"  ,"kbi_p_c_mh_sa")],
         function(x) {ifelse(x==3, NA,x)})




######-----------------------------Multiple subscale score pertaining to family, social, and school functioning----------------------------------######

abcd_sscey01 <- read.delim("abcd_sscey01.txt")
abcd_sscey01<-abcd_sscey01[-1,]
names(abcd_sscey01)
abcd_sscey01<-abcd_sscey01[,c(4,9,10,13,18,22,25,28,31,34,37,40,45,48,51,54,57,58)]
abcd_sscey01[,-c(1:2)]<-sapply(abcd_sscey01[,-c(1:2)], as.character)
abcd_sscey01[,-c(1:2)]<-sapply(abcd_sscey01[,-c(1:2)], as.numeric)
summary(abcd_sscey01)


######---------------------------------CBCL T SCORE-------------------------------#####
abcd_cbcls01 <-  read.delim("abcd_cbcls01.txt")
abcd_cbcls01<-abcd_cbcls01[-1,]
names(abcd_cbcls01)
abcd_cbcls01<-abcd_cbcls01[,c(4,9,10:89)]
abcd_cbcls01[,-c(1:2)]<-sapply(abcd_cbcls01[,-c(1:2)], as.character)
abcd_cbcls01[,-c(1:2)]<-sapply(abcd_cbcls01[,-c(1:2)], as.numeric)
summary(abcd_cbcls01)
abcd_cbcls01<-abcd_cbcls01[lapply(abcd_cbcls01,function(x)sum(is.na(x))/length(x))<0.5]
abcd_cbcls01<-abcd_cbcls01[,c(1,2,seq(4,61,3))]


######--------------------------------------subsyndromal mania-------------------------######
abcd_pgbi01 <- read.delim("abcd_pgbi01.txt")
abcd_pgbi01<-abcd_pgbi01[-1,]
names(abcd_pgbi01)
abcd_pgbi01<-abcd_pgbi01[,c(4,9,11:20)]
abcd_pgbi01[,-c(1:2)]<-sapply(abcd_pgbi01[,-c(1:2)], as.character)
abcd_pgbi01[,-c(1:2)]<-sapply(abcd_pgbi01[,-c(1:2)], as.numeric)
summary(abcd_pgbi01)
abcd_pgbi01$abcd_pgbi01<-rowSums(abcd_pgbi01[,3:12])



######--------------------------------------prodromal -------------------------######
pps01 <- read.delim("pps01.txt")
pps01<-pps01[-1,]
names(pps01)
pps01<-pps01[,-c(1:3,5:8,73)]
pps01[,-c(1:2)]<-sapply(pps01[,-c(1:2)], as.character)
pps01[,-c(1:2)]<-sapply(pps01[,-c(1:2)], as.numeric)
pps01<-pps01[lapply(pps01,function(x)sum(is.na(x))/length(x))<0.50]
summary(pps01)
pps01$pps01_total<-rowSums(pps01[,3:23])
pps01<-pps01[,c(1,2,24)]
names(pps01)[2]<-"eventname"


                                  
                                  
######-----------------------------Parental Demographics----------------------------------######

pdem02 <-read.delim("pdem02.txt")
pdem02<-pdem02[-1,]
names(pdem02)
pdem02<-pdem02[,c(4,128,86:88,98:106,94,95)]
pdem02[,-c(1:2)]<-sapply(pdem02[,-c(1:2)], as.character)
pdem02[,-c(1:2)]<-sapply(pdem02[,-c(1:2)], as.numeric)
pdem02[,-c(1:2)]<-sapply(pdem02[,-c(1:2)], function(x) ifelse(x>100,NA,x))
pdem02$marital<-pdem02$demo_prnt_marital_v2
pdem02$marital<-ifelse(pdem02$marital==1 | pdem02$marital==6,1,ifelse(pdem02$marital==777,NA,0))
pdem02$severe_financial_difficulty<-rowSums(pdem02[,7:13])
pdem02$demo_prnt_empl_v2<-ifelse(pdem02$demo_prnt_empl_v2==1,1,0)
pdem02$demo_prtnr_empl_v2<-ifelse(pdem02$demo_prtnr_empl_v2==1,1,0)
summary(pdem02)
pdem02<-pdem02[,c(1:2,4:6,15:18)]




######-----------------------------Family History----------------------------------######

fhxp201 <-read.delim("fhxp201.txt")
fhxp201<-fhxp201[-1,]
names(fhxp201)
fhxp201<-fhxp201[,c("subjectkey","fam_history_11_yes_no","fam_history_q11a_professional","fam_history_q11d_professional")]
fhxp201[,-1]<-sapply(fhxp201[,-1], as.character)
fhxp201[,-1]<-sapply(fhxp201[,-1], as.numeric)
fhxp201[,-1]<-sapply(fhxp201[,-1], function(x) ifelse(x>3,NA,x))
fhxp201$parental_psychopathology<-fhxp201$fam_history_q11a_professional+fhxp201$fam_history_q11d_professional
fhxp201$parental_psychopathology<-ifelse(is.na(fhxp201$parental_psychopathology),
                                     fhxp201$fam_history_11_yes_no,fhxp201$parental_psychopathology)
summary(fhxp201)
fhxp201<-fhxp201[,c(1,5)]



#################--------------------------------Adult Self Report Scores*-----------------------########
abcd_asrs01 <- read.delim("abcd_asrs01.txt")
abcd_asrs01<-abcd_asrs01[-1,]
names(abcd_asrs01)
abcd_asrs01 = abcd_asrs01[,!c(grepl("*_r",names(abcd_asrs01)) | grepl("*_total",names(abcd_asrs01)) | grepl("*_nm",names(abcd_asrs01)))]
abcd_asrs01<-abcd_asrs01[,c(4,9,10:28)]
abcd_asrs01[,-(1:2)]<-sapply(abcd_asrs01[,-(1:2)], as.character)
abcd_asrs01[,-(1:2)]<-sapply(abcd_asrs01[,-(1:2)], as.numeric)
summary(abcd_asrs01)



#################--------------------------------Adult Self Report Scores other parent*-----------------------########
abcd_abcls01 <- read.delim("abcd_abcls01.txt")
abcd_abcls01<-abcd_abcls01[-1,]
names(abcd_abcls01)
abcd_abcls01 = abcd_abcls01[,!c(grepl("*_r",names(abcd_abcls01)) | grepl("*_nt",names(abcd_abcls01)) | grepl("*_nm",names(abcd_abcls01)))]
abcd_abcls01<-abcd_abcls01[,c(4,9,10:26)]
abcd_abcls01[,-(1:2)]<-sapply(abcd_abcls01[,-(1:2)], as.character)
abcd_abcls01[,-(1:2)]<-sapply(abcd_abcls01[,-(1:2)], as.numeric)
summary(abcd_abcls01)


########--------------------------FH scores---------------------------------------#######
abcd_fhxssp01<-read.delim("abcd_fhxssp01.txt")
abcd_fhxssp01<-abcd_fhxssp01[-1,]
abcd_fhxssp01<-abcd_fhxssp01[,c(4,9,57,105,153,201,249,297,345,393,441,489)]
names(abcd_fhxssp01)
abcd_fhxssp01[,-(1:2)]<-sapply(abcd_fhxssp01[,-(1:2)], as.character)
abcd_fhxssp01[,-(1:2)]<-sapply(abcd_fhxssp01[,-(1:2)], as.numeric)

abcd_fhxssp01[,-c(1:2)] <- lapply(abcd_fhxssp01[,-c(1:2)], function(x) 
  recode(x,"3=2;2=1;1=1;-1=1;-2=1;0=0"))
summary(abcd_fhxssp01)


########--------------------------Child Victimization---------------------------------------#######
abcd_peq01<-read.delim("abcd_peq01.txt")
abcd_peq01<-abcd_peq01[-1,]
names(abcd_peq01)
abcd_peq01<-abcd_peq01[,c(4,9:27)]
abcd_peq01[,-(1:2)]<-sapply(abcd_peq01[,-(1:2)], as.character)
abcd_peq01[,-(1:2)]<-sapply(abcd_peq01[,-(1:2)], as.numeric)
abcd_peq01$abcd_peq01_perpetrator = rowSums(abcd_peq01[,grepl("*perp",names(abcd_peq01))])
abcd_peq01$abcd_peq01_victim = rowSums(abcd_peq01[,grepl("*vic",names(abcd_peq01))])
summary(abcd_peq01)
abcd_peq01<-abcd_peq01[,c(1,2,21:22)]



#########---------------------Bullying------------------------------------######
bully <- read.delim("dibf01.txt")
bully<-bully[-1,]
names(bully)
bully<-bully[,c(4,89,65)]
bully[,-c(1:2)]<-sapply(bully[,-c(1:2)], as.character)
bully[,-c(1:2)]<-sapply(bully[,-c(1:2)], as.numeric)
summary(bully)
bully<-bully[bully$eventname=="baseline_year_1_arm_1",]
bully<-bully[,c(1,3)]

######-----------------------------Traumatic Events----------------------------------######

abcd_ptsd01 <-read.delim("abcd_ptsd01.txt")
abcd_ptsd01<-abcd_ptsd01[-1,]
names(abcd_ptsd01)
abcd_ptsd01<-abcd_ptsd01[,c(4,9,10:26)]
abcd_ptsd01[,-c(1:2)]<-sapply(abcd_ptsd01[,-c(1:2)], as.character)
abcd_ptsd01[,-c(1:2)]<-sapply(abcd_ptsd01[,-c(1:2)], as.numeric)
summary(abcd_ptsd01)
abcd_ptsd01$trauma<-rowSums(abcd_ptsd01[,c(3:19)])
abcd_ptsd01<-abcd_ptsd01[,c(1,2,20)]
abcd_ptsd01$trauma<-ifelse(abcd_ptsd01$trauma>0,1,abcd_ptsd01$trauma)
                             
######-----------------------------Medical----------------------------------######

abcd_mx01 <- read.delim("abcd_mx01.txt")
abcd_mx01<-abcd_mx01[-1,]
names(abcd_mx01)
abcd_mx01<-abcd_mx01[,c(4,9,13:29)]
abcd_mx01[,-c(1:2)]<-sapply(abcd_mx01[,-c(1:2)], as.character)
abcd_mx01[,-c(1:2)]<-sapply(abcd_mx01[,-c(1:2)], as.numeric)
abcd_mx01[,-c(1:2)]<-sapply(abcd_mx01[,-c(1:2)], function(x) ifelse(x>3,NA,x))
abcd_mx01$common_medical<-abcd_mx01$medhx_2a+abcd_mx01$medhx_2b+abcd_mx01$medhx_2d
abcd_mx01$neurological<-abcd_mx01$medhx_2c+abcd_mx01$medhx_2f+abcd_mx01$medhx_2h+abcd_mx01$medhx_2l+abcd_mx01$medhx_2m
abcd_mx01$serious_medical<-abcd_mx01$medhx_2e+abcd_mx01$medhx_2g+abcd_mx01$medhx_2j+abcd_mx01$medhx_2k+abcd_mx01$medhx_2o+abcd_mx01$medhx_2p
abcd_mx01<-abcd_mx01[,c(1,20:22)]
abcd_mx01[,-1]<-sapply(abcd_mx01[,-1], function(x) ifelse(x>=1,1,0))
summary(abcd_mx01)                                  
                                  
 ######--------------------------------------Screen Media-------------------------######
#release 2.0.1 as the third release has missing values
stq01 <-read.delim("/Volumes/Toshiba/ABCD_release_2point0point1/stq01.txt") 
stq01<-stq01[-1,]
names(stq01)
stq01<-stq01[,c(4,13,9:12)]
stq01[,-c(1:2)]<-sapply(stq01[,-c(1:2)], as.character)
stq01[,-c(1:2)]<-sapply(stq01[,-c(1:2)], as.numeric)
stq01$weekdayscreen<-stq01$screentime1_p_hours+(stq01$screentime1_p_minutes/60)
stq01$weekendscreen<-stq01$screentime2_p_hours+(stq01$screentime2_p_minutes/60)
stq01<-stq01[stq01$eventname=="baseline_year_1_arm_1",]
stq01<-stq01[,-c(2:6)]
summary(stq01)
stq01$weekdayscreen<-gtools::quantcut(stq01$weekdayscreen)
stq01$weekdayscreen<-as.numeric(stq01$weekdayscreen)
stq01$weekendscreen<-gtools::quantcut(stq01$weekendscreen)
stq01$weekendscreen<-as.numeric(stq01$weekendscreen)

                                  
#####-------------------------------------Cognitive PCs------------------------------#####
neurocogpcs<-read.csv("neurocogpcs.csv")
neurocogpcs<-neurocogpcs[,-1]
names(neurocogpcs)[4]<-"subjectkey"
neurocogpcs<-subset(neurocogpcs,!duplicated(neurocogpcs$subjectkey,fromLast=FALSE)) 



######-----------------------------------RESIDENTIAL-----------------------------######
abcd_rhds01 <- read.delim("abcd_rhds01.txt")
abcd_rhds01<-abcd_rhds01[-1,]
abcd_rhds01<-abcd_rhds01[,c(4,9,16,42,45,250,258:262)]
names(abcd_rhds01)
abcd_rhds01[,-c(1:2)]<-sapply(abcd_rhds01[,-c(1:2)], as.character)
abcd_rhds01[,-c(1:2)]<-sapply(abcd_rhds01[,-c(1:2)], as.numeric)
summary(abcd_rhds01)


######-----------------------------------RESIDENTIAL-----------------------------######
abcd_rhds01 <- read.delim("abcd_rhds01.txt")
abcd_rhds01<-abcd_rhds01[-1,]
abcd_rhds01<-abcd_rhds01[,c(4,9,16,42,45,250,258:262)]
names(abcd_rhds01)
abcd_rhds01[,-c(1:2)]<-sapply(abcd_rhds01[,-c(1:2)], as.character)
abcd_rhds01[,-c(1:2)]<-sapply(abcd_rhds01[,-c(1:2)], as.numeric)
summary(abcd_rhds01)
abcd_rhds01<-abcd_rhds01[abcd_rhds01$eventname=="baseline_year_1_arm_1",]
abcd_rhds01<-abcd_rhds01[,c(1,4,5,6)]

######-----------------------------------Neighborhood Safety youth-----------------------------######
abcd_nsc01<-read.delim("abcd_nsc01.txt")
abcd_nsc01<-abcd_nsc01[-1,]
abcd_nsc01<-abcd_nsc01[,c(4,9,10)]
names(abcd_nsc01)
abcd_nsc01[,-c(1:2)]<-sapply(abcd_nsc01[,-c(1:2)], as.character)
abcd_nsc01[,-c(1:2)]<-sapply(abcd_nsc01[,-c(1:2)], as.numeric)
summary(abcd_nsc01)

######-----------------------------------Neighborhood Safety parents-----------------------------######
abcd_pnsc01<-read.delim("abcd_pnsc01.txt")
abcd_pnsc01<-abcd_pnsc01[-1,]
abcd_pnsc01<-abcd_pnsc01[,c(4,9,11:13)]
names(abcd_pnsc01)
abcd_pnsc01[,-c(1:2)]<-sapply(abcd_pnsc01[,-c(1:2)], as.character)
abcd_pnsc01[,-c(1:2)]<-sapply(abcd_pnsc01[,-c(1:2)], as.numeric)
abcd_pnsc01$abcd_pnsc01<-rowSums(abcd_pnsc01[,3:5])
abcd_pnsc01<-abcd_pnsc01[,-c(3:5)]
summary(abcd_pnsc01)

                                  
                                  
######-----------------------------Pregnancy----------------------------------######
#data for birthweigh is not available on release 3 so using release 2.0.1
dhx01 <- read.delim("/Volumes/Toshiba/ABCD_release_2point0point1/dhx01.txt")
dhx01<-dhx01[-1,]
names(dhx01)
dhx01<-dhx01[,c(4,9,12,14,16,18,20,21,107,147,150,157,160,163,166,169,213:225,228,230,231:238,245,255,256)]
dhx01[,-c(1,2)]<-sapply(dhx01[,-c(1,2)], as.character)
dhx01[,-c(1,2)]<-sapply(dhx01[,-c(1,2)], as.numeric)
dhx01[,-c(1,2)]<-sapply(dhx01[,-c(1,2)], function(x) ifelse(x>366,NA,x))
dhx01<-dhx01[,-2]
dhx01$dhx01_drugs<-rowSums(dhx01[,9:15])
dhx01$medical_dhx01<-rowSums(dhx01[,16:28])
dhx01$birth_complications<-rowSums(dhx01[,31:38])
dhx01$devhx_4_p<-ifelse(dhx01$devhx_4_p>70,NA,dhx01$devhx_4_p)
names(dhx01)
summary(dhx01)
dhx01$birth_weight_lbs<-ifelse(is.na(dhx01$birth_weight_oz),dhx01$birth_weight_lbs,dhx01$birth_weight_lbs+(dhx01$birth_weight_oz*0.0625)) 
dhx01<-dhx01[,c(1:2,4:8,29:30,39:44)]
dhx01[,c(13:15)]<-sapply(dhx01[,c(13:15)], function(x) ifelse(x>0,1,0))
dhx01$devhx_18_p<-as.numeric(gtools::quantcut(dhx01$devhx_18_p))



#######------------------------------genetic_race----------------------------------------------#####
acspsw03 <- read.delim("acspsw03.txt")
acspsw03<-acspsw03[-1,]
acspsw03<-acspsw03[,c(4,10,11,14,17,22:32)]
names(acspsw03)
acspsw03[,-c(1,3,5,14:16)]<-sapply(acspsw03[,-c(1,3,5,14:16)], as.character)
acspsw03[,-c(1,3,5,14:16)]<-sapply(acspsw03[,-c(1,3,5,14:16)], as.numeric)
summary(acspsw03)


#########-----------------------------------HANDEDNESS----------------------------------------------######
abcd_ehis01 <- read.delim("abcd_ehis01.txt")
abcd_ehis01<-abcd_ehis01[-1,]
names(abcd_ehis01)
abcd_ehis01<-abcd_ehis01[,c(4,9,15)]
abcd_ehis01[,-c(1:2)]<-sapply(abcd_ehis01[,-c(1:2)], as.character)
abcd_ehis01[,-c(1:2)]<-sapply(abcd_ehis01[,-c(1:2)], as.numeric)
summary(abcd_ehis01)

######---------------------------------ABCD Youth Anthropometrics Modified From PhenX--------------------######
abcd_ant01 <- read.delim("abcd_ant01.txt")
abcd_ant01<-abcd_ant01[-1,]
names(abcd_ant01)
abcd_ant01<-abcd_ant01[,c(4,9,13,19,20)]
names(abcd_ant01)
abcd_ant01[,-(1:2)]<-sapply(abcd_ant01[,-(1:2)], as.character)
abcd_ant01[,-(1:2)]<-sapply(abcd_ant01[,-(1:2)], as.numeric)
summary(abcd_ant01[,-1])

outlier_iqr <- function(variable) {
  iqrup<-quantile(variable,na.rm=T,0.999)
  iqrdown<-quantile(variable,na.rm=T,0.001)
  variable<-ifelse(variable>iqrup,NA,variable)
  variable<-ifelse(variable<iqrdown,NA,variable)
}
abcd_ant01$anthroheightcalc<-ifelse(abcd_ant01$anthroheightcalc==999,NA,abcd_ant01$anthroheightcalc)
abcd_ant01$anthroweightcalc<-ifelse(abcd_ant01$anthroweightcalc==999,NA,abcd_ant01$anthroweightcalc)
abcd_ant01$anthroheightcalc<-outlier_iqr(abcd_ant01$anthroheightcalc)
abcd_ant01$anthroweightcalc<-outlier_iqr(abcd_ant01$anthroweightcalc)
abcd_ant01$anthro_waist_cm<-outlier_iqr(abcd_ant01$anthro_waist_cm)


######--------------------------------------Pubertal Youth----------------------------------------------######
abcd_ssphy01 <- read.delim("abcd_ssphy01.txt")
abcd_ssphy01<-abcd_ssphy01[-1,]
names(abcd_ssphy01)
abcd_ssphy01<-abcd_ssphy01[,c(4,8,9,10,13,18,19)]
abcd_ssphy01[,-c(1:3)]<-sapply(abcd_ssphy01[,-c(1:3)], as.character)
abcd_ssphy01[,-c(1:3)]<-sapply(abcd_ssphy01[,-c(1:3)], as.numeric)
abcd_ssphy01$puberty_y<-NA
abcd_ssphy01$puberty_y<-ifelse(!is.na(abcd_ssphy01$pds_y_ss_female_category_2),abcd_ssphy01$pds_y_ss_female_category_2,abcd_ssphy01$pds_y_ss_male_cat_2)
abcd_ssphy01[is.na(abcd_ssphy01$puberty_y) & abcd_ssphy01$sex=="M",]$puberty_y<-abcd_ssphy01[is.na(abcd_ssphy01$puberty_y) & abcd_ssphy01$sex=="M",]$pds_y_ss_male_category
abcd_ssphy01[is.na(abcd_ssphy01$puberty_y) & abcd_ssphy01$sex=="F",]$puberty_y<-abcd_ssphy01[is.na(abcd_ssphy01$puberty_y) & abcd_ssphy01$sex=="F",]$pds_y_ss_female_category
abcd_ssphy01<-abcd_ssphy01[,c(1,3,8)]

######--------------------------------------Pubertal parent and sleep and diet----------------------------------------------######
abcd_ssphp01 <- read.delim("abcd_ssphp01.txt")
abcd_ssphp01<-abcd_ssphp01[-1,]
names(abcd_ssphp01)
abcd_ssphp01<-abcd_ssphp01[,c(4,8,9,10,13,16,19,22,25,28,31,34,37,42,43)]
abcd_ssphp01[,-c(1:3)]<-sapply(abcd_ssphp01[,-c(1:3)], as.character)
abcd_ssphp01[,-c(1:3)]<-sapply(abcd_ssphp01[,-c(1:3)], as.numeric)
abcd_ssphp01$puberty_p<-NA
abcd_ssphp01$puberty_p<-ifelse(!is.na(abcd_ssphp01$pds_p_ss_female_category_2),abcd_ssphp01$pds_p_ss_female_category_2,abcd_ssphp01$pds_p_ss_male_category_2)
abcd_ssphp01[is.na(abcd_ssphp01$puberty_p) & abcd_ssphp01$sex=="M",]$puberty_p<-abcd_ssphp01[is.na(abcd_ssphp01$puberty_p) & abcd_ssphp01$sex=="M",]$pds_p_ss_male_category
abcd_ssphp01[is.na(abcd_ssphp01$puberty_p) & abcd_ssphp01$sex=="F",]$puberty_p<-abcd_ssphp01[is.na(abcd_ssphp01$puberty_p) & abcd_ssphp01$sex=="F",]$pds_p_ss_female_category
abcd_ssphp01<-abcd_ssphp01[,c(1,3:10,13,16)]


#########---------------------Parent Reported-Family Conflict Subscale (ongoing)------------------------------------######
fes02 <- read.delim("fes02.txt")
fes02<-fes02[-1,]
names(fes02)
fes02<-fes02[,c(4,9,10:18)]
fes02[,-c(1:2)]<-sapply(fes02[,-c(1:2)], as.character)
fes02[,-c(1:2)]<-sapply(fes02[,-c(1:2)], as.numeric)
summary(fes02)
fes02<-fes02[fes02$eventname=="baseline_year_1_arm_1",]
fes02<-fes02[,-2]
fes02$family_conflict_parents<-rowSums(fes02[,2:10])
fes02<-fes02[,c(1,11)]
fes02<-fes02[!duplicated(fes02[c('subjectkey')]),]

#########---------------------Youth Reported-Family Conflict Subscale (ongoing)------------------------------------######
abcd_fes01 <- read.delim("abcd_fes01.txt")
abcd_fes01<-abcd_fes01[-1,]
names(abcd_fes01)
abcd_fes01<-abcd_fes01[,c(4,18,9:17)]
abcd_fes01[,-c(1:2)]<-sapply(abcd_fes01[,-c(1:2)], as.character)
abcd_fes01[,-c(1:2)]<-sapply(abcd_fes01[,-c(1:2)], as.numeric)
summary(abcd_fes01)
abcd_fes01$family_conflict_youth<-rowSums(abcd_fes01[,3:11])
abcd_fes01<-abcd_fes01[,c(1,2,12)]
abcd_fes01<-abcd_fes01[abcd_fes01$eventname=="baseline_year_1_arm_1",]

######--------------------------------------physical activity-------------------------######
abcd_yrb01 <-read.delim("abcd_yrb01.txt")
abcd_yrb01<-abcd_yrb01[-1,]
names(abcd_yrb01)
abcd_yrb01<-abcd_yrb01[,c(4,9,10)]
abcd_yrb01[,-c(1:2)]<-sapply(abcd_yrb01[,-c(1:2)], as.character)
abcd_yrb01[,-c(1:2)]<-sapply(abcd_yrb01[,-c(1:2)], as.numeric)

######-----------------------------------------------ksads-------------------------------------------#####
####based on the matlab script for extracting KSADS diagnosis
#healthy controls
hc<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_HC.mat'))))
bipolar<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_bipolar.mat'))))
depression<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_depression.mat'))))
anxiety<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_anxiety.mat'))))
adhd<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_ADHD.mat'))))
conduct<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_conductDisorder.mat'))))
ocd<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_OCD.mat'))))
ptsd<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_traumaStressor.mat'))))
psychosis<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_schizophrenia.mat'))))
suicide<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_Suicidal.mat'))))
odd<-unique(as.vector(unlist(rmatio::read.mat('/Users/amirhosseinmodabbernia/Documents/Phd/Thesis/ABCD/ksad_grouping_RG/subjectkey_oppositionalDefiantDisorder.mat'))))
ksads_positive<-unique(c(bipolar, depression, anxiety, adhd, conduct, ocd, ptsd, psychosis, suicide, odd))



########--------------------------Imaging-----------------------------------#######
#####-------------------------------------SCANNER ID---------------------------#####
abcd_mri01 <- read.delim("abcd_mri01.txt")
abcd_mri01<-abcd_mri01[-1,]
names(abcd_mri01)
abcd_mri01<-abcd_mri01[,c(4,9,13)]

# #################--------------------------------freesurfer quality overlaps with the image inclusion-----------------------########
# freesqc01 <- read.delim("freesqc01.txt")
# freesqc01<-freesqc01[-1,]
# names(freesqc01)
# freesqc01<-freesqc01[,c(4,9,15)]
# freesqc01[,-c(1,2)]<-sapply(freesqc01[,-c(1,2)], as.character)
# freesqc01[,-c(1,2)]<-sapply(freesqc01[,-c(1,2)], as.numeric)

###-----------------------------image inclusion--1=include (use this and freesurfer)---------------------------#####
abcd_imgincl01<-read.delim("abcd_imgincl01.txt") 
abcd_imgincl01<-abcd_imgincl01[-1,]
names(abcd_imgincl01)
abcd_imgincl01<-abcd_imgincl01[,c(4,9,11:17)]
abcd_imgincl01[,-c(1,2)]<-sapply(abcd_imgincl01[,-c(1,2)], as.character)
abcd_imgincl01[,-c(1,2)]<-sapply(abcd_imgincl01[,-c(1,2)], as.numeric)


#########-----------------------------------Structural----------------------------------------------######
abcd_smrip101<-read.delim("abcd_smrip101.txt")
names(abcd_smrip101)
abcd_smrip101<-abcd_smrip101[,c(4,9,11:78,115:185,222:292,329:331,7)]
abcd_smrip101<-abcd_smrip101[-1,]
abcd_smrip101[,-c(1,2)]<-sapply(abcd_smrip101[,-c(1,2)], as.character)
abcd_smrip101[,-c(1,2)]<-sapply(abcd_smrip101[,-c(1,2)], as.numeric)
summary(abcd_smrip101)

abcd_smrip201<-read.delim("abcd_smrip201.txt")
names(abcd_smrip201)
abcd_smrip201<-abcd_smrip201[,c(4,331,334:338,342:343,346:347,349,352:358,360:361,373)]
abcd_smrip201<-abcd_smrip201[-1,]
abcd_smrip201[,-c(1)]<-sapply(abcd_smrip201[,-c(1)], as.character)
abcd_smrip201[,-c(1)]<-sapply(abcd_smrip201[,-c(1)], as.numeric)
summary(abcd_smrip201)
smri<-cbind(abcd_smrip101,abcd_smrip201[,-1])
smri<-merge(smri,abcd_imgincl01)
smri<-smri[smri$imgincl_t1w_include==1,]
names(smri)


#########-----------------------------------Restriction----------------------------------------------######
mri_rsi_p102 <- read.delim("mri_rsi_p102.txt")
dim(mri_rsi_p102)
mri_rsi_p102<-mri_rsi_p102[-1,]
names(mri_rsi_p102)
mri_rsi_p102<-mri_rsi_p102[,c(4,9,11:913)]
mri_rsi_p102[,-c(1,2)]<-lapply(mri_rsi_p102[,-c(1,2)],as.character)
mri_rsi_p102[,-c(1,2)]<-lapply(mri_rsi_p102[,-c(1,2)],as.numeric)

mri_rsi_p202 <- read.delim("mri_rsi_p202.txt")
dim(mri_rsi_p202)
mri_rsi_p202<-mri_rsi_p202[-1,]
names(mri_rsi_p202)
mri_rsi_p202<-mri_rsi_p202[,c(9:860)]
mri_rsi_p202<-lapply(mri_rsi_p202,as.character)
mri_rsi_p202<-lapply(mri_rsi_p202,as.numeric)

restrict_mri<-cbind(mri_rsi_p202,mri_rsi_p102)
restrict_mri<-merge(restrict_mri,abcd_imgincl01)
restrict_mri<-restrict_mri[restrict_mri$imgincl_dmri_include==1,]

#########-----------------------------------GWC----------------------------------------------######
abcd_smrip101<-read.delim("abcd_smrip101.txt")
names(abcd_smrip101)
abcd_smrip101<-abcd_smrip101[,c(4, 9, which(grepl("*t1wcnt_cdk",names(abcd_smrip101))==TRUE))]
abcd_smrip101<-abcd_smrip101[-1,]
abcd_smrip101[,-c(1,2)]<-sapply(abcd_smrip101[,-c(1,2)], as.character)
abcd_smrip101[,-c(1,2)]<-sapply(abcd_smrip101[,-c(1,2)], as.numeric)
abcd_smrip101<-merge(abcd_smrip101,abcd_imgincl01)
abcd_smrip101<-abcd_smrip101[abcd_smrip101$imgincl_t1w_include==1,]


#########-----------------------------------DTI----------------------------------------------######
abcd_dmdtifp101 <- read.delim("abcd_dmdtifp101.txt")
dti_names<-abcd_dmdtifp101[1,]
abcd_dmdtifp101<-abcd_dmdtifp101[-1,]
names(abcd_dmdtifp101)
abcd_dmdtifp101<-abcd_dmdtifp101[,-c(1:3,5:8,178:978)]
names(abcd_dmdtifp101)
abcd_dmdtifp101[,-c(1,2)]<-sapply(abcd_dmdtifp101[,-c(1,2)], as.character)
abcd_dmdtifp101[,-c(1,2)]<-sapply(abcd_dmdtifp101[,-c(1,2)], as.numeric)
abcd_dmdtifp101<-merge(abcd_dmdtifp101,abcd_imgincl01)
abcd_dmdtifp101<-abcd_dmdtifp101[abcd_dmdtifp101$imgincl_dmri_include==1,]


x<-sapply(dti_names[names(abcd_dmdtifp101[,2:170])], as.character)
names(x)<-c()
colnames(abcd_dmdtifp101)[2:170]<-x
colnames(abcd_dmdtifp101)[2:170]<-str_remove(colnames(abcd_dmdtifp101)[2:170], c("within DTI atlas tract"))
names(abcd_dmdtifp101)[2]<-"eventname"
colnames(abcd_dmdtifp101)[3:44]<-gsub("Average fractional anisotropy", "fa", colnames(abcd_dmdtifp101)[3:44])
colnames(abcd_dmdtifp101)[45:86]<-gsub("Mean diffusivity", "md", colnames(abcd_dmdtifp101)[45:86])
colnames(abcd_dmdtifp101)[87:128]<-gsub("Average longitudinal diffusion coefficient", "ldc", colnames(abcd_dmdtifp101)[87:128])
colnames(abcd_dmdtifp101)[129:170]<-gsub("Average transverse diffusion coefficient", "tdc", colnames(abcd_dmdtifp101)[129:170])
colnames(abcd_dmdtifp101)<-gsub(" ", "_", colnames(abcd_dmdtifp101))
colnames(abcd_dmdtifp101)<-gsub("/", "_", colnames(abcd_dmdtifp101))
colnames(abcd_dmdtifp101)<-gsub("[-]", "_", colnames(abcd_dmdtifp101))
colnames(abcd_dmdtifp101)<-gsub(",", "_", colnames(abcd_dmdtifp101))



########----------------------------------functional-------------------------#####

fun_conn <- read.delim("abcd_betnet02.txt")
library(stringr)
for (i in 23:191) {
  colnames(fun_conn)[i]<-as.character(fun_conn[1,i])
  colnames(fun_conn)[i]<-word(string = colnames(fun_conn)[i], start = 4,end=8, sep = fixed(" "))
  colnames(fun_conn)[i]<-str_remove(colnames(fun_conn)[i], "network")
}
fun_conn<-fun_conn[-1,]
names(fun_conn)
fun_conn = fun_conn[,!grepl("*none",names(fun_conn))]
fun_conn<-fun_conn[,c(4,9,11:166)]
names(fun_conn)
fun_conn[,-c(1,2)]<-sapply(fun_conn[,-c(1,2)], as.character)
fun_conn[,-c(1,2)]<-sapply(fun_conn[,-c(1,2)], as.numeric)
summary(fun_conn)
fun_conn<-fun_conn[,-c(26,38:39,50:52,62:65,74:78,86:91,98:104,110:117,122:130,134:143,146:156)] #remove repeat values
fun_conn<-merge(fun_conn,abcd_imgincl01)
fun_conn<-fun_conn[fun_conn$imgincl_rsfmri_include==1,]
colnames(fun_conn)<-gsub(" ", "_", colnames(fun_conn))
colnames(fun_conn)<-gsub("-", "_", colnames(fun_conn))
colnames(fun_conn)<-gsub("[.]", "_", colnames(fun_conn))




# ANCILLARY MEASURES (Developing List)------------------------------------------------------



######---------------------------------------Fitbit Physical Activity ------------------------######
abcd_fbwpas01<-read.delim("abcd_fbwpas01.txt")
abcd_fbwpas01<-abcd_fbwpas01[-1,]
names(abcd_fbwpas01)
abcd_fbwpas01<-abcd_fbwpas01[,c(4,9,11:34)]
abcd_fbwpas01[,-c(1:2)]<-sapply(abcd_fbwpas01[,-c(1:2)], as.character)
abcd_fbwpas01[,-c(1:2)]<-sapply(abcd_fbwpas01[,-c(1:2)], as.numeric)
summary(abcd_fbwpas01)

######-----------------------------Game of dice---------------------------######
abcd_gdss01 <- read.delim("abcd_gdss01.txt")
abcd_gdss01<-abcd_gdss01[-1,]
names(abcd_gdss01)
abcd_gdss01<-abcd_gdss01[,c(4,9,17:26)]
abcd_gdss01[,-c(1:2)]<-sapply(abcd_gdss01[,-c(1:2)], as.character)
abcd_gdss01[,-c(1:2)]<-sapply(abcd_gdss01[,-c(1:2)], as.numeric)
summary(abcd_gdss01)


