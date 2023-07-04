rm(list=ls()) #clears workspace
library(data.table)
library(dplyr)
library(sjmisc)
library(haven)
library(openxlsx)

# ---- Read in the datasets ---- ###
set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }
# ---- Malawi  ----
mw.bio <- fread("~/mphia2015adultbio.csv")
mw.ind <- fread("~/mphia2015adultind.csv")
mw.h <- fread("~/mphia2015hh.csv")

mw1 <- mw.ind[mw.bio, on = "personid"]
mw <- mw1[mw.h, on = "householdid"]
rm(mw.bio,mw.ind, mw.h, mw1 )

# We chose all appropriate variables for selection in the complete dataset
mw <- mw[, .(gender, country, varunit, centroidid, householdid, personid, husid, intwt0, btwt0, age, varstrat, vm_status, vmflag, vmpstw0,vmpstw_varstrat, vmpstw_varunit, householdheadgender,
             urban,  wealthquintile, religion,  surveystmonth, surveystyear, education = education,  work12mo, evermar, curmar, numwif, 
             anyPhysAbuse= vlnc12motimes, partPhysAbuse = vlnc12moptnr, anyPhysSex = frcsx12mo,  anyPresSex = prssx12mo,  sexualviolencepart12mo, physicalviolencepart12mo, 
             hivtstever, lasTTm = hivtslstm, lasTTy = hivtslsty, hivself = hivtstrslt, known_hiv_status, testedreceiveddetail, hivstatusfinal,
             sexPart12 = part12monum, condomlastsex12months,  alcfreq, buysxever, buysx12mo, sexever, firstsxage, stddiag,
             healthc, money, okhitout, okhitkids, okhitargue, okhitsex, cndm12vagsx1,cndm12vagsx2, cndm12vagsx3,
             arvstatus, arvstakenev, arvscurrent, arvsmissdays, arvsmissdaysdk, vls, resultvlc, recentlagvlarv, 
             partlastsxtimed1, partfirstsxtimed1,  partlastsxtimed2, partfirstsxtimed2,  partlastsxtimed3, partfirstsxtimed3, mcstatus
             )]

mw$surveyid <- "MW2015PHIA"; mw$country <- "Malawi"; mw$survyear <- 2015; mw$region <- "Eastern Africa"


# recode education categories
mw$education <- ifelse(mw$education == "1 ", 0, mw$education)
mw$education <- ifelse(mw$education == "2 ", 1, mw$education)
mw$education <- ifelse(mw$education == "3 ", 2, mw$education)
mw$education <- ifelse(mw$education == "4 ", 3, mw$education)

                            
# ---- Zambia ----
zm.bio <- fread("~/zamphia2016adultbio.csv")
zm.ind <- fread("~/zamphia2016adultind.csv")
zm.h <- fread("~/zamphia2016hh.csv")


zm1<- zm.ind[zm.bio, on = "personid"]
zm <- zm1[zm.h, on = "householdid"]
rm(zm.bio,zm.ind, zm.h, zm1)


zm <-zm[, .(gender, country, varunit, centroidid, householdid, personid, husid,  intwt0, btwt0, age, varstrat, vm_status, vmflag, vmpstw0,vmpstw_varstrat, vmpstw_varunit, householdheadgender,
            urban,  wealthquintile, religion, surveystmonth, surveystyear,  education = education,  work12mo, evermar,  curmar,   numwif, 
            hivtstever, lasTTm = hivtslstm,    lasTTy =  hivtslsty, hivself = hivtstrslt, known_hiv_status, testedreceiveddetail,   hivstatusfinal,
            anyPhysAbuse= vlnc12motimes, anyPhysSex = frcsx12mo, anyPresSex = prssx12mo,  sexualviolencepart12mo, physicalviolencepart12mo,
            sexPart12 = part12mo, sexPart = partlifetm, condomlastsex12months,  alcfreq, buysxever,  buysx12mo, sexever, firstsxage, stddiag,
            healthc, money,
            arvstatus, arvstakenev, arvscurrent, arvsmissdays, arvsmissdaysdk, vls, resultvlc,recentlagvlarv, 
            partlastsxtimed1, partfirstsxtimed1,  partlastsxtimed2, partfirstsxtimed2,  partlastsxtimed3, partfirstsxtimed3, mcstatus)]


zm$surveyid <- "ZM2016PHIA"; zm$country <- "Zambia"; zm$survyear <- 2016; zm$region <- "Eastern Africa"

# recode education
zm$education <- ifelse(zm$education == "1 ", 0, zm$education)
zm$education <- ifelse(zm$education == "2 ", 1, zm$education) #primary
zm$education <- ifelse(zm$education == "3 ", 2, zm$education) #Secondary
zm$education <- ifelse(zm$education == "4 ", 3, zm$education) #Higher

zm$curmar <- as.integer(zm$curmar)
zm$currMarried <- as.integer(set_na(zm$curmar, c(-8,-9)) < 3) #


# ---- Uganda ----
ug.bio <- fread("~/uphia2016adultbio.csv")
ug.ind <- fread("~/uphia2016adultind.csv")
ug.h <- fread("~/uphia2016hh.csv")

ug1<- ug.ind[ug.bio, on = "personid"]
ug<- ug1[ug.h, on = "householdid"]
rm(ug.bio,ug.ind, ug.h, ug1)

ug<-ug[,.(gender, country, varunit, centroidid, householdid, personid, husid, intwt0, btwt0, age, varstrat, vm_status, vmflag, vmpstw0, vmpstw_varstrat, vmpstw_varunit,householdheadgender,
          urban,  wealthquintile, religion, surveystmonth, surveystyear,  education = educationuganda, work12mo, evermar,curmar, numwif, 
          anyPhysAbuse= vlnc12motimes, partPhysAbuse = vlnc12moptnr, anyPhysSex = frcsx12mo,  anyPresSex = prssx12mo, sexualviolencepart12mo,physicalviolencepart12mo,
          hivtstever, lasTTm = hivtestm,    lasTTy =  hivtesty, hivself = hivtstrslt, known_hiv_status, testedreceiveddetail,  hivstatusfinal, 
          sexPart = lifetimesex, sexPart12 = part12monum, condomlastsex12months,  sexever, firstsxage, 
          healthc, money,
          arvstatus, arvstakenev, arvsmissdays, arvsmissdaysdk, vls, resultvlc, recentlagvlarv, 
          partlastsxtimed1, partfirstsxtimed1,  partlastsxtimed2, partfirstsxtimed2,  partlastsxtimed3, partfirstsxtimed3, mcstatus)]

ug$education <- ifelse(ug$education == "1 ", 0, ug$education) ## None
ug$education <- ifelse(ug$education %in% c("2 ","3 "),  1, ug$education) #primary
ug$education <- ifelse(ug$education %in% c("4 ") , 2, ug$education) # secondary
ug$education <- ifelse(ug$education %in% c("5 ") , 3, ug$education) # Higher 


ug$surveyid <- "UG2016PHIA"; ug$country <- "Uganda"; ug$survyear <- 2016; ug$region <- "Eastern Africa"

# ---- Swaziland  ----

sw.bio <- fread("~/shims22016adultbio.csv")
sw.ind <- fread("~/shims22016adultind.csv")
sw.h <- fread("~/shims22016hh.csv")

sw1<- sw.ind [sw.bio, on = "personid"]
sw<- sw1[sw.h, on = "householdid"]

rm(sw.bio  ,sw.ind, sw.h)


sw <-sw[,.(gender, country, varunit, centroidid, householdid, personid, husid, intwt0, btwt0, age, varstrat, vm_status, vmflag, vmpstw0,vmpstw_varstrat, vmpstw_varunit, householdheadgender,
           urban,  wealthquintile,  surveystmonth, surveystyear, education = educationeswatini, work12mo, evermar,   curmar, numwif, 
           anyPhysAbuse= vlnc12motimes, partPhysAbuse = vlnc12mowho,  sexualviolencepart12mo,physicalviolencepart12mo,  
           hivtstever, lasTTm = hivtestm,     lasTTy = hivtesty, hivself = hivtstrslt, known_hiv_status, testedreceiveddetail,  hivstatusfinal, 
           sexPart = lifetimesex,  sexPart12 = part12monum,condomlastsex12months, alcfreq, buysxever, buysx12mo, sexever, firstsxage, 
           healthc, money,
           arvstatus, arvstakenev, arvscurrent, arvsmissdays, arvsmissdaysdk, vls, resultvlc, recentlagvlarv, 
           partlastsxtimed1, partfirstsxtimed1,  partlastsxtimed2, partfirstsxtimed2,  partlastsxtimed3, partfirstsxtimed3, mcstatus)]
sw$surveyid <- "SZ2016PHIA"; sw$country <- "Eswatini"; sw$survyear <- 2016; sw$region <- "Southern Africa"

# Recode education 

sw$education <- ifelse(sw$education == "1 ", 0, sw$education)
sw$education <- ifelse(sw$education == "2 ", 1, sw$education)
sw$education <- ifelse(sw$education %in% c("3 ", "4 "),  2, sw$education) 
sw$education <- ifelse(sw$education == "5 ", 3, sw$education)

## recode physical partner physical violence and physical force of sex 

sw$partPhysAbuse <- ifelse (sw$partPhysAbuse %in% "A ", "1 ", 
                            ifelse  (sw$partPhysAbuse %in% c( "B ", "C ", "X "), "2 ", 
                                     ifelse (sw$partPhysAbuse %in% c( "Y ", "Z "), "-9 " ,
                                             ifelse (sw$partPhysAbuse %in% c("  "), ". ",  sw$partPhysAbuse ))))


sw$partPhysSex <- ifelse (sw$partPhysSex %in% "A ", "1 ", 
                          ifelse  (sw$partPhysSex %in% c( "B ", "C ", "X "), "2 ", #
                                   ifelse (sw$partPhysSex %in% c( "Y ", "Z "), "-9 ",
                                           ifelse (sw$partPhysSex %in% c("  "), ". ",  sw$partPhysSex ))))


# ---- Zimbabwe  ----r

zim.bio <- fread("~/zimphia2015adultbio.csv")
zim.ind <- fread("~/zimphia2015adultind.csv")
zim.h <- fread("~/zimphia2015hh.csv")

zim1<- zim.ind[zim.bio, on = "personid"]
zim<- zim1[zim.h, on = "householdid"]
rm(zim.bio,zim.ind, zim.h, zim1)

zim <-zim[,.(gender, country, varunit, centroidid, householdid, personid, husid, intwt0, btwt0, age, varstrat, vm_status, vmflag, vmpstw0, vmpstw_varstrat, vmpstw_varunit,householdheadgender,
             urban,  wealthquintile, religion, surveystmonth, surveystyear,  work12mo, education = education, evermar, curmar,  numwif, 
             anyPhysAbuse= vlnc12motimes,  partPhysAbuse = vlnc12moptnr, anyPhysSex = frcsx12mo,  anyPresSex = prssx12mo,   sexualviolencepart12mo, physicalviolencepart12mo,
             hivtstever,lasTTm = hivtslstm,    lasTTy =  hivtslsty, hivself = hivtstrslt,  known_hiv_status, testedreceiveddetail, hivstatusfinal,
             sexPart12 = part12monum,   condomlastsex12months,  alcfreq, buysx12mo, sexever, firstsxage, stddiag,
             healthc, money,
             arvstatus, arvstakenev, arvscurrent, arvsmissdays, arvsmissdaysdk, vls, resultvlc,recentlagvlarv, 
             partlastsxtimed1, partfirstsxtimed1,  partlastsxtimed2, partfirstsxtimed2,  partlastsxtimed3, partfirstsxtimed3, mcstatus)]


zim$education <- ifelse(zim$education == "1 ", 0, zim$education)
zim$education <- ifelse(zim$education == "2 ", 1, zim$education) #Primary
zim$education <- ifelse(zim$education %in% c("3 "), 2, zim$education) # secondary
zim$education <- ifelse(zim$education == "4 ", 3, zim$education) ## Higher

zim$surveyid <- "ZW2015PHIA"; zim$country <- "Zimbabwe"; zim$survyear <- 2015; zim$region <- "Eastern Africa"

dat1<- rbindlist(list(mw, zm, ug, sw, zim ), fill=TRUE)
dat1 <- as.data.frame(dat1)



# ---- Create a couple's dataset  ---- ###########

# Split dataset into  M and F
dfMale <- dat1 [which(dat1$gender == "1 "),]
dfFemale <- dat1 [which(dat1$gender == "2 "),]

dfFemale$husid_1 <- dfFemale$husid ## mock husband ID variable to check 
dfFemale$personid_f <- dfFemale$personid
dat1<-left_join( dfMale, dfFemale,by = c("personid" = "husid"))


## Identify female variables  as a separate df

dw<-dat1[,c("hivstatusfinal.y", "intwt0.y", "btwt0.y", "age.y", "wealthquintile.y", "education.y", "work12mo.y", "vls.y", "firstsxage.y", "sexPart.y", "sexPart12.y", "condomlastsex12months.y", 'stddiag.y', "healthc.y", "money.y", 
            "partPhysAbuse.y", "physicalviolencepart12mo.y", "anyPhysAbuse.y", "sexualviolencepart12mo.y", "vmpstw0.y", "vmpstw_varstrat.y", "vmpstw_varunit.y",
            "okhitout.y", "okhitkids.y", "okhitargue.y", "okhitsex.y", "arvstatus.y", "arvstakenev.y", "arvscurrent.y", "vls.y", "resultvlc.y", "recentlagvlarv.y", "vmflag.y", "husid_1", 
            "partlastsxtimed1.y", "partfirstsxtimed1.y",  "partlastsxtimed2.y","partfirstsxtimed2.y",  "partlastsxtimed3.y", "partfirstsxtimed3.y")]

dat1<-dat1 %>% dplyr::select(-contains(c(".y")))
dat1<-cbind(dat1,dw) ## bind 

## Rename the female variables that I will use 
names(dat1)[names(dat1) == c('hivstatusfinal.y')] <- c('hivstatus_w')
names(dat1)[names(dat1) == c('btwt0.y')] <- c('hivweight_w') 
names(dat1)[names(dat1) == c('intwt0.y')] <- c('indweight_w')
names(dat1)[names(dat1) == c('vmpstw0.y')] <- c('dvweight_w')
names(dat1)[names(dat1) == c('vmpstw_varstrat.y')] <- c('dv_strat')
names(dat1)[names(dat1) == c('vmpstw_varunit.y')] <- c('dv_varunit_w')
names(dat1)[names(dat1) == c('age.y')] <- c('agew')
names(dat1)[names(dat1) == c("wealthquintile.y")] <- c('wealthquintile_w')
names(dat1)[names(dat1) == c("education.y")] <- c('schoolw')
names(dat1)[names(dat1) == c("work12mo.y")] <- c('employmentw')
names(dat1)[names(dat1) == c('vls.y')] <- c('vlsup_w')
names(dat1)[names(dat1) == c('firstsxage.y')] <- c('age1s_w')
names(dat1)[names(dat1) == c('sexPart.y')] <- c('life_w')
names(dat1)[names(dat1) == c('sexPart12.y')] <- c('partners_rec_w')
names(dat1)[names(dat1) == c('condomlastsex12months.y')] <- c('conLast_w')
names(dat1)[names(dat1) == c('stddiag.y')] <- c('stddiag_w')
names(dat1)[names(dat1) == c('healthc.y')] <- c('health')
names(dat1)[names(dat1) == c('money.y')] <- c('spend')
names(dat1)[names(dat1) == c('okhitout.y')] <- c('go_outw')
names(dat1)[names(dat1) == c('okhitkids.y')] <- c('neglectw')
names(dat1)[names(dat1) == c('okhitargue.y')] <- c('arguew')
names(dat1)[names(dat1) == c('okhitsex.y')] <- c('ref_sexw')
names(dat1)[names(dat1) == c('arvstatus.y')] <- c('arvstatus_w')
names(dat1)[names(dat1) == c('arvstakenev.y')] <- c('arvstakenev_w')
names(dat1)[names(dat1) == c('arvscurrent.y')] <- c('arvscurrent_w')
names(dat1)[names(dat1) == c('resultvlc.y')] <- c('resultvlc_w')
names(dat1)[names(dat1) == c('recentlagvlarv.y')] <- c('recentlagvlarv_w')

names(dat1)[names(dat1) == c('partPhysAbuse.y')] <- c('partPhysAbuse_w')
names(dat1)[names(dat1) == c('physicalviolencepart12mo.y')] <- c('physicalviolencepart12mo_w')
names(dat1)[names(dat1) == c('anyPhysAbuse.y')] <- c('anyPhysAbuse_w')
names(dat1)[names(dat1) == c('sexualviolencepart12mo.y')] <- c('sexualviolencepart12mo_w')


names(dat1)[names(dat1) == c("partlastsxtimed1.y")] <- c('last_recw')
names(dat1)[names(dat1) == c("partfirstsxtimed1.y")] <- c('first_recw')
names(dat1)[names(dat1) == c("partlastsxtimed2.y")] <- c('last_2recw')
names(dat1)[names(dat1) == c("partfirstsxtimed2.y")] <- c('first_2recw')
names(dat1)[names(dat1) == c("partlastsxtimed3.y")] <- c('last_3recw')
names(dat1)[names(dat1) == c("partfirstsxtimed3.y")] <- c('first_3recw')
names(dat1)[names(dat1) == c('vmflag.y')] <- c('vmflag_w')

colnames(dat1) <- gsub(pattern = "\\.x$", replacement = "", x = names(dat1))

## Remove those that were not matched 
dat1<-subset(dat1,!is.na(husid_1))


# ---- RECODE ---- ###########
dat1$psu <- as.character(dat1$centroidid)

dat1$psu_u <- as.character(paste0(dat1$psu,sep= "_",dat1$surveyid))
dat1$stratum <- as.integer(dat1$varstrat)
dat1$stratum_dv <- as.integer(dat1$dv_strat)
dat1$indweight_m <- dat1$intwt0
dat1$indweight_w <- dat1$indweight_w
dat1$hivweight_w <- dat1$hivweight_w
dat1$hivweight_m <- dat1$btwt0
dat1$dvweight<- dat1$dvweight_w
dat1$period <- '2015-2019'
dat1$household <- dat1$householdid

dat1$linem <- dat1$personid
dat1$linew <- dat1$personid_f

# Age
dat1$agew <- as.numeric(dat1$agew)
dat1$agem <- as.numeric(dat1$age)

# Age disparity
dat1$agedisp <- (dat1$agem - dat1$agew)

# Age group 
agegr = c('15-24', '25-34', '35-44', '45-64', "65+")
dat1$agegrw <- cut(dat1$agew, breaks = c(15, 25, 35, 45, 65,  Inf), labels = agegr, TRUE, FALSE)
dat1$agegrm <- cut(dat1$agem, breaks = c(15, 25, 35, 45, 65,  Inf), labels = agegr, TRUE, FALSE)

# Residence type 
dat1$urban <- as.integer(dat1$urban)
dat1$restype <- factor(set_na(dat1$urban, 99) > 1, c(TRUE, FALSE), c("Rural", "Urban"))


# Wealth index - Male 
set(dat1, i=which(dat1[["wealthquintile"]] %in% c("1 ")), j = "wealthquintile", value = 1)
set(dat1, i=which(dat1[["wealthquintile"]] %in% c("2 ")), j = "wealthquintile", value = 2)
set(dat1, i=which(dat1[["wealthquintile"]] %in% c("3 ")), j = "wealthquintile", value = 3)
set(dat1, i=which(dat1[["wealthquintile"]] %in% c("4 ")), j = "wealthquintile", value = 4)
set(dat1, i=which(dat1[["wealthquintile"]] %in% c("5 ")), j = "wealthquintile", value = 5)
set(dat1, i=which(dat1[["wealthquintile"]] %in% c(". ")), j = "wealthquintile", value = NA)
dat1$wealthm <- as.integer(dat1$wealthquintile)


# Wealth index - Female 
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c("1 ")), j = "wealthquintile_w", value = 1)
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c("2 ")), j = "wealthquintile_w", value = 2)
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c("3 ")), j = "wealthquintile_w", value = 3)
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c("4 ")), j = "wealthquintile_w", value = 4)
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c("5 ")), j = "wealthquintile_w", value = 5)
set(dat1, i=which(dat1[["wealthquintile_w"]] %in% c(". ")), j = "wealthquintile_w", value = NA)
dat1$wealthw <- as.integer(dat1$wealthquintile_w)


# age @ first sex  - Men 
dat1$firstsxage <- as.integer(dat1$firstsxage)
dat1$age1s_m <- as.integer(set_na(dat1$firstsxage, c(-8,-9,96)))


# age @ first sex  - Women 
dat1$age1s_w <- as.integer(dat1$age1s_w)
dat1$age1s_w <- as.integer(set_na(dat1$age1s_w, c(-8,-9,96)))

# ever-married - men 
dat1$evermar<- as.integer(dat1$evermar)
dat1$evermarried<- as.integer(set_na(dat1$evermar, c(-8,-9)) < 2) # if = 1, Yes, married, if 2, not married 


# currently married - men
dat1$curmar <- as.integer(dat1$curmar)
dat1$currMarried <- as.integer(set_na(dat1$curmar, c(-8,-9)) < 3) #

# Polygamy 
dat1$poly <- as.integer(set_na(dat1$curmar, c(-8,-9,-7)) > 1) # 


## condom use at last sex - Men 
dat1$condomlastsex12months<-as.integer(dat1$condomlastsex12months)
dat1$conLast_m <- as.integer(set_na(dat1$condomlastsex12months, c(3, 99)) < 2) # 1 0 used ; 2 did not use 

## condom use at last sex - Women 
dat1$conLast_w<-as.integer(dat1$conLast_w)
dat1$conLast_w <- as.integer(set_na(dat1$conLast_w, c(3, 99)) < 2) # 1 0 used ; 2 did not use 


## Education - Men 
dat1$edu <- factor(set_na(dat1$education, 99))
dat1$schoolm<- factor(dat1$edu, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))


## Education - Women
dat1$eduw <- factor(set_na(dat1$schoolw, 99))
dat1$schoolw<- factor(dat1$eduw, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))


## Employment  - Women 
dat1$employmentw<- as.integer(dat1$employmentw)
dat1$employmentw <- as.integer(set_na(dat1$employmentw, c(-8,-9, 99)) < 2) 


## Employment  - Men 
dat1$work12mo <- as.integer(dat1$work12mo)
dat1$employmentm <- as.integer(set_na(dat1$work12mo, c(-8,-9, 99)) < 2) #


# Buy sex  - ever 
dat1$buysxever <- as.integer(dat1$buysxever)
dat1$pay <- as.integer(set_na(dat1$buysxever , c(-8,-9, 99)) < 2) # 


# Buy sex  - past 12 months 
dat1$buysx12mo <- as.integer(dat1$buysx12mo)
dat1$pay_rec <- as.integer(set_na(dat1$buysx12mo, c(-8,-9, 99)) < 2) # 
dat1$pay_rec [dat1$pay  %in%  0] <- 0

# Headship 
dat1$head <- factor(set_na(dat1$householdheadgender, 99))
dat1$head <- factor(dat1$head, labels=c("Male", "Female"))


# Alcohol 
set(dat1, i=which(dat1[["alcfreq"]] %in% c("0 ")), j = "alcfreq", value = 0) # never 
set(dat1, i=which(dat1[["alcfreq"]] %in% c("1 ")), j = "alcfreq", value = 1) # sometimes
set(dat1, i=which(dat1[["alcfreq"]] %in% c("2 ")), j = "alcfreq", value = 1) # sometimes
set(dat1, i=which(dat1[["alcfreq"]] %in% c("3 ")), j = "alcfreq", value = 2) # often 
set(dat1, i=which(dat1[["alcfreq"]] %in% c("4 ")), j = "alcfreq", value = 2) # often 
set(dat1, i=which(dat1[["alcfreq"]] %in% c("-8 ", "-9 ", ". ")), j = "alcfreq", value = NA) #

dat1$part_alcoholFRQ<-as.factor(dat1$alcfreq)
dat1$part_alcoholFRQ<- factor(dat1$part_alcoholFRQ, levels=c(0,1,2),labels=c("Never", "Sometimes", "Often"))

# Number of sex partners in the past 12 months - Men 
dat1$partners_rec_m <- as.integer(dat1$sexPart12)
dat1$partners_rec_m <- as.integer(set_na(dat1$partners_rec_m, c(-8, -9, - 7)))

# Number of sex partners in the past 12 months - Women 
dat1$partners_rec_w <- as.integer(dat1$partners_rec_w)
dat1$partners_rec_w <- as.integer(set_na(dat1$partners_rec_w, c(-8, -9, - 7)))

# Concurrency 
#  Clean: days since the first or last sexual encounter with each partner 
dat1$first_recm_n <- as.integer(dat1$partfirstsxtimed1, c(-9,-8))
dat1$last_recm_n <- as.integer(dat1$partlastsxtimed1,  c(-9,-8))

dat1$first_2recm_n <- as.integer(dat1$partfirstsxtimed2, c(-9,-8))
dat1$last_2recm_n <- as.integer(dat1$partlastsxtimed2,  c(-9,-8))

dat1$first_3recm_n <- as.integer(dat1$partfirstsxtimed3, c(-9,-8))
dat1$last_3recm_n <- as.integer(dat1$partlastsxtimed1,  c(-9,-8))

dat1$first_recw_n<- as.integer(dat1$first_recw, c(-9,-8))
dat1$last_recw_n <- as.integer(dat1$last_recw,  c(-9,-8))

dat1$first_2recw_n <- as.integer(dat1$first_2recw, c(-9,-8))
dat1$last_2recw_n <- as.integer(dat1$last_2recw,  c(-9,-8))

dat1$first_3recw_n <- as.integer(dat1$first_3recw, c(-9,-8))
dat1$last_3recw_n <- as.integer(dat1$last_3recw,  c(-9,-8))

# First sex was  before the six-month cutoff and last sex was  the six-month cutoff 
dat1$had_mostrecm <- ifelse( dat1$first_recm_n > 182.5 &  dat1$last_recm_n < 182.5, 1, 0)
dat1$had_2ndrecm <- ifelse ( dat1$first_2recm_n > 182.5 & dat1$last_2recm_n < 182.5, 1, 0)
dat1$had_3rdrecm <- ifelse(dat1$first_3recm_n > 182.5 & dat1$last_3recm_n < 182.5, 1, 0)

dat1$had_mostrecw <- ifelse( dat1$first_recw_n > 182.5 &  dat1$last_recw_n < 182.5, 1, 0)
dat1$had_2ndrecw <- ifelse ( dat1$first_2recw_n > 182.5 & dat1$last_2recw_n < 182.5, 1, 0)
dat1$had_3rdrecw <- ifelse(dat1$first_3recw_n > 182.5 & dat1$last_3recw_n < 182.5, 1, 0)


fun1 <- function(x) { 
  x$conc_m= ifelse(rowSums(x[,1:3], na.rm=TRUE) >= 2, 1, 
                   ifelse (rowSums(x[,1:3], na.rm=TRUE) <= 1 & x[,4] > 1, 0,
                           ifelse (x[,4] <=1, NA, NA))) 
  return (x) 
}

fun2 <- function(x) { 
  x$conc_w= ifelse(rowSums(x[,1:3], na.rm=TRUE) >= 2, 1,
                   ifelse (rowSums(x[,1:3], na.rm=TRUE) <= 1 & x[,4] > 1, 0, 
                           ifelse (x[,4] <=1, NA, NA)))  
  return (x) 
}


dat3 <- fun1(x = dat1[ , c("had_mostrecm","had_2ndrecm","had_3rdrecm", 'partners_rec_m')])
dat4 <- fun2(x = dat1[ , c("had_mostrecw","had_2ndrecw","had_3rdrecw", 'partners_rec_w')])

dat1 <- cbind (dat1,dat3)
dat1 <- cbind (dat1,dat4)


# Number of sex partners in the past 12 months - Men 
dat1$life_m <- as.integer(dat1$sexPart)
dat1$life_m <- as.integer(set_na(dat1$life_m, c(-8, -9, - 7)))

# Number of sex partners in the past 12 months - Women
dat1$life_w <- as.integer(dat1$life_w)
dat1$life_w <- as.integer(set_na(dat1$life_w, c(-8, -9, - 7)))


# Justification of IPV - men
dat1$okhitargue <- as.integer(dat1$okhitargue)
dat1$arguem <- as.integer(set_na(dat1$okhitargue, c(-8, -9)) < 2 )
dat1$okhitkids<- as.integer(dat1$okhitkids)
dat1$neglectm <- as.integer(set_na(dat1$okhitkids, c(-8, -9)) < 2 )
dat1$okhitout <- as.integer(dat1$okhitout)
dat1$go_outm <- as.integer(set_na(dat1$okhitout , c(-8, -9)) < 2)
dat1$okhitsex <- as.integer(dat1$okhitsex)
dat1$ref_sexm<- as.integer(set_na(dat1$okhitsex, c(-8, -9)) < 2)
dat1$justify_m =apply(dat1[ , c("go_outm", "neglectm", "arguem", "ref_sexm")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), 1,
                                                                                                         ifelse (all (k==0), 0, NA))})


# Justification of IPV - women 
dat1$arguew <- as.integer(dat1$arguew)
dat1$arguew <- as.integer(set_na(dat1$arguew, c(-8, -9)) < 2) 
dat1$neglectw <- as.integer(dat1$neglectw)
dat1$neglectw <- as.integer(set_na(dat1$neglectw, c(-8, -9)) < 2 )
dat1$go_outw <- as.integer(dat1$go_outw)
dat1$go_outw <- as.integer(set_na(dat1$go_outw , c(-8, -9)) < 2)
dat1$ref_sexw <- as.integer(dat1$ref_sexw)
dat1$ref_sexw<- as.integer(set_na(dat1$ref_sexw , c(-8, -9)) < 2)

dat1$justify_w =apply(dat1[ , c("go_outw", "neglectw", "arguew", "ref_sexw")], 1, function(k) { ifelse(any(k== 1, na.rm = TRUE), 1,
                                                                                                             ifelse (all (k==0), 0, NA))})


# Female decision-making 
dat1$health <- as.integer(dat1$health)
dat1$health <- as.integer(set_na(dat1$health, c(-8, -9)))
dat1$spend<- as.integer(dat1$spend)
dat1$spend <- as.integer(set_na(dat1$spend, c(-8, -9)))

dat1$health <- ifelse(dat1$health %in% c(1,3), 1, 0) 
dat1$spend <- ifelse(dat1$spend %in% c(1,3), 1, 0) 

# The indicator in DHS on household decision making contains: healthcare decision making, visit to family and large household purchases NOT spending. Therefore for PHIA I will use only healthcare 
# decision making indicator to create an overall decision making indicator. 
dat1$decision  <- dat1$health

# HIV biomarker outcomes: Women 
dat1$hivstatus_w <- as.integer(dat1$hivstatus_w)
dat1$hivstatus_w  <- factor(set_na(dat1$hivstatus_w , 99) > 1, c(TRUE, FALSE), c("Negative", "Positive"))


# HIV biomarker outcomes: Men
dat1$hivstatusfinal_m <- as.integer(dat1$hivstatusfinal)
dat1$hivstatus_m <- factor(set_na(dat1$hivstatusfinal_m, 99) > 1, c(TRUE, FALSE), c("Negative", "Positive"))


## Violence 

# Eligible and responded to domestic violence questions 
dat1$vmflag_w<- as.integer (dat1$vmflag_w)
dat1$DVSel <- ifelse(dat1$vmflag_w== 1, 1, 0) 


# partner physical violence
for (i in 1:nrow(dat1)) {
  if(dat1$surveyid[i] %in% c("CM2017PHIA","CI2017PHIA", "LS2016PHIA")) {
    dat1$rec_phys[i] <- ifelse( dat1$partPhysAbuse_w[i] %in% "1 ", 1, 
                                ifelse (dat1$partPhysAbuse_w[i]  %in% c("2 ","3 "), 0, 
                                        ifelse ( dat1$partPhysAbuse_w[i] %in% c ("-8 ","-9 ", ". "), NA, dat1$partPhysAbuse[i] )))
  } else {
    dat1$physicalviolencepart12mo_w[i]<-as.integer(dat1$physicalviolencepart12mo_w[i])
    dat1$rec_phys[i] <- as.integer(set_na(dat1$physicalviolencepart12mo_w[i], 99) < 2) }
}
dat1$rec_phys<-as.integer(dat1$rec_phys)


# partner any sexual violence 
dat1$sexualviolencepart12mo_w<-as.integer(dat1$sexualviolencepart12mo_w)
dat1$rec_sex <- as.integer(set_na(dat1$sexualviolencepart12mo_w, 99) < 2)


# Any recent violence 
dat1$recent_viol<- apply(dat1[, c("rec_sex","rec_phys")],1, function(x) { ifelse(any(x > 0, na.rm = TRUE), "Yes",
                                                                                 ifelse (all(x==0), "No", NA))})

# Self-reported ARV - women 
dat1$arv_self_w <- ifelse(dat1$arvscurrent_w == "1 ", 1,
                        ifelse((dat1$arvscurrent_w == "2 " | dat1$arvstakenev_w == "2 "), 0,
                               ifelse(dat1$arvstakenev_w == ". ", NA, NA)))
dat1$arv_self_w <- as.integer(dat1$arv_self_w)


# Biomaerker ARV - women 
dat1$arv_bio_w <- ifelse(dat1$arvstatus_w == "1 ", 1,
                       ifelse(dat1$arvstatus_w == "2 ", 0, NA))

## ARV created final - women 
length <- nrow(dat1)
for(i in 1:length) {
  if ((dat1$arv_self_w[i] | dat1$arv_bio_w[i])  %in% c(1) ) {
    dat1$arv_w[i] <- "Yes" }
  else if ((dat1$arv_self_w[i] & dat1$arv_bio_w[i])  %in% c(NA)){
    dat1$arv_w[i] <- NA }
  else {  dat1$arv_w[i] <- "No" 
  }
}


# Self-reported ARV - men 
dat1$arv_self_m <- ifelse(dat1$arvscurrent == "1 ", 1,
                          ifelse((dat1$arvscurrent == "2 " | dat1$arvstakenev == "2 "), 0,
                                 ifelse(dat1$arvstakenev == ". ", NA, NA)))
dat1$arv_self_m <- as.integer(dat1$arv_self_m)


# Biomarker ARV - men
dat1$arv_bio_m <- ifelse(dat1$arvstatus == "1 ", 1,
                         ifelse(dat1$arvstatus == "2 ", 0, NA))


## ARV created final - men 
length <- nrow(dat1)
for(i in 1:length) {
  if ((dat1$arv_self_m[i] | dat1$arv_bio_m[i])  %in% c(1) ) {
    dat1$arv_m[i] <- "Yes" }
  else if ((dat1$arv_self_m[i] & dat1$arv_bio_m[i])  %in% c(NA)){ 
    dat1$arv_m[i] <- NA }
  else {  dat1$arv_m[i] <- "No" 
  }
}

# Viral load suppression- men 
dat1$vlsup_m <- ifelse(dat1$vls == "1 ", 1,
                     ifelse(dat1$vls == "2 ", 0, NA))
dat1$vlsup_m <- as.integer(dat1$vlsup_m)

# Viral load suppression- women 
dat1$vlsup_w <- ifelse(dat1$vlsup_w == "1 ", 1,
                       ifelse(dat1$vlsup_w == "2 ", 0, NA))
dat1$vlsup_w <- as.integer(dat1$vlsup_w)



earn_disp <- NA; purchase <- NA ; visit <- NA;
dat1$ever_phys <- NA; dat1$ever_sex_vio <- NA; any_viol <- NA ;  head <- NA; burnw <- NA; burnm <- NA; 


dat1<- subset(dat1, DVSel==1 & currMarried %in% 1 & (agem > 14 & agew > 14))


dat1 <- dat1 %>% dplyr:: mutate(unique_ID = group_indices(., psu_u, household, linew)) ## Add a unique ID to each variable 

dat1 <- setDT(dat1)[,.(country,  household, linem, linew, surveyid, survyear, psu,  psu_u, period, stratum,stratum_dv, region, indweight_w, 
                hivweight_w, hivweight_m, dvweight,  agem, agew, agegrw, agegrm, agedisp,  schoolw, schoolm,   poly,
                employmentm, employmentw, wealthw,  wealthm,  restype, age1s_w, age1s_m, go_outw, neglectw,  arguew, ref_sexw,  burnw,  justify_w, 
                go_outm, neglectm, arguem,  ref_sexm, burnm, justify_m, earn_disp,  spend, health, purchase, visit, decision,  hivstatus_w, hivstatus_m, 
                conLast_w, conLast_m,   ever_phys, ever_sex_vio, any_viol, recent_viol, 
                pay, pay_rec,  part_alcoholFRQ,  evermarried, life_w, life_m, partners_rec_w, partners_rec_m,  DVSel,  head,  unique_ID,  vlsup_m,  vlsup_w, arv_m, arv_w, 
                conc_m, conc_w)]

dat1 <- as.data.frame(dat1)

saveRDS(dat1 , "PHIA_perp_recode_jul3_clean.rds")

  