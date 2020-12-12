## ----Setup, include=FALSE, results="hide", warning=FALSE-----------------
require(knitr)
require(pander)
panderOptions('knitr.auto.asis', FALSE)

opts_chunk$set(dev=c("png","pdf"),
               fig.lp="fig:",
               cache=FALSE,
               par=TRUE,
               echo=FALSE,
               results="hide", ## not 'markup' or 'asis'
               message=FALSE,
               warning=TRUE,
               dpi=300)


## ----loadlibs------------------------------------------------------------
library(foreign)
library(readxl)
library(tidyverse)
library(rstanarm)
library(parallel)

## ----getpsw--------------------------------------------------------------
psw <- read.csv("hlv_psw.csv", stringsAsFactors = FALSE)
psw <- psw %>%
    dplyr::select(GSSCode, sex, age0, housing,
                  hrsocgrd, education, weight)


## ----getbesdata----------------------------------------------------------
## Read in the BES individual-level data
url <- "http://www.britishelectionstudy.com/wp-content/uploads/2017/11/BES2017_W13_v1.2.sav"
destfile <- "BES2017_W13_v1.2.sav"

if (file.exists(destfile)) {
    ## Do nothing
} else {
    download.file(url, destfile)
}

## Read in and select the variables we want
bes <- read.spss(destfile, to.data.frame = TRUE)
bes <- bes %>%
    dplyr::select(onscode,
                  gender,
                  housing,
                  education,
                  profile_socialgrade_cie,
                  age,
                  leftRight)



## ----getbesaux-----------------------------------------------------------
url <- "http://www.britishelectionstudy.com/wp-content/uploads/2017/07/BES-2017-General-Election-results-file-v1.0.xlsx"
destfile <- "BES-2017-General-Election-results-file-v1.0.xlsx"

if (file.exists(destfile)) {
    ## Do nothing
} else {
    download.file(url, destfile)
}

aux <- read_excel(destfile)
aux <- aux %>%
    dplyr::select(ONSConstID,
                  Region,
                  Lab17,
                  Con17,
                  leaveHanretty,
                  Green17,
                  PC17,
                  c11Age18to19,
                  c11Age20to24,
                  c11Age65to74,                   
                  c11Age75to84, 
                  c11Age85to89,                    
                  c11Age90plus,
                  c11HealthBad,
                  c11HealthVeryBad,
                  c11QualLevel4,
                  c11PopulationDensity,
                  c11EthnicityWhite,
                  c11EconomicInactive,
                  c11Unemployed,
                  c11SelfEmployed,
                  c11HouseOwned)

## Combine some of these
aux <- aux %>%
    dplyr::mutate(GSSCode = ONSConstID,
                  c11Age18to24 = c11Age18to19 +
                      c11Age20to24,
                  c11Age65plus = c11Age65to74 +
                      c11Age75to84 +
                      c11Age85to89 +
                      c11Age90plus,
                  c11HealthBad = c11HealthBad +
                      c11HealthVeryBad)

## Zero-impute some missing values
aux <- aux %>%
    dplyr::mutate_at(vars(contains("17")),
                     dplyr::coalesce, 0)
## Mean-impute the rest


## ----besmunge------------------------------------------------------------
### Constituency identifier
bes$GSSCode <- as.character(bes$onscode)

### Gender variable
bes$sex <- bes$gender

### Housing variable
bes$housing <- dplyr::recode(bes$housing,
                             "Own the leasehold/freehold outright" = "Owns",
                             "Buying leasehold/freehold on a mortgage" = "Owns",
                             .default = "Rents")

## Education variable
bes$education <- dplyr::recode(as.character(bes$education),
                               "City & Guilds certificate" = "Level 1",
                               "City & Guilds certificate - advanced" = "Level 2",
                               "Clerical and commercial" = "Level 1",
                               "CSE grade 1, GCE O level, GCSE, School Certificate" = "Level 2",
                               "CSE grades 2-5" = "Level 1",
                               "Don't know" = "No qualifications",
                               "GCE A level or Higher Certificate" = "Level 3",
                               "No formal qualifications" = "No qualifications",
                               "Nursing qualification (e.g. SEN, SRN, SCM, RGN)" = "Level 4/5",
                               "ONC" = "Level 2",
                               "Other technical, professional or higher qualification" = "Other",
                               "Prefer not to say" = "No qualifications",
                               "Recognised trade apprenticeship completed" = "Level 2",
                               "Scottish Higher Certificate" = "Level 3",
                               "Scottish Ordinary/ Lower Certificate" = "Level 2",
                               "Teaching qualification (not degree)" = "Level 4/5",
                               "University diploma" = "Level 4/5",
                               "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)" = "Level 4/5",
                               "University or CNAA higher degree (e.g. M.Sc, Ph.D)" = "Level 4/5",
                               "Youth training certificate/skillseekers" = "Level 2")

## Social grade
bes$hrsocgrd <- dplyr::recode(bes$profile_socialgrade_cie,
                              "A"= "AB",
                              "B" = "AB",
                              "C1" = "C1",
                              "C2" = "C2",
                              "D" = "DE",
                              "E" = "DE",
                              .default = NA_character_)

## Age
age_breaks <- c(-Inf, 19, 24,
                29, 44, 59, 64, 74, Inf)
age_labels <- c("16-19",
                "20-24",
                "25-29",
                "30-44",
                "45-59",
                "60-64",
                "65-74",
                "75+")

bes$age <- as.numeric(as.character(bes$age))
bes$age0 <- cut(bes$age,
               breaks = age_breaks,
               labels = age_labels)

## Left-Right (the dependent variable)
bes$leftRight <- dplyr::recode(bes$leftRight,
                               "Left" = 0,
                               "1" = 1,
                               "2" = 2,
                               "3" = 3,
                               "4" = 4,
                               "5" = 5,
                               "6" = 6,
                               "7" = 7,
                               "8" = 8,
                               "9" = 9,
                               "Right" = 10,
                               .default = NA_real_)

## Chop things down again
bes <- bes %>%
    dplyr::select(GSSCode, sex, age0, housing,
                  hrsocgrd, education, leftRight)

## Check that there are no funny constituency identifiers
bes <- bes %>%
    filter(GSSCode != "         ")

## Complete cases only
## This drops a lot of cases with missing values on housing

bes <- bes[complete.cases(bes),]


## ----domrp, cache = TRUE-------------------------------------------------
source("mrp.R", echo = FALSE)
my_formula <- leftRight ~ sex + age0 + housing +
    hrsocgrd + education |
    Con17 + Lab17 + Region + leaveHanretty +
    Green17 + PC17 + c11Age18to24 +
    c11Age65plus + c11HouseOwned +
    c11SelfEmployed + c11Unemployed +
    c11EconomicInactive + c11EthnicityWhite +
    c11PopulationDensity + c11QualLevel4 +
    c11HealthBad

options(mc.cores = parallel::detectCores() - 1)
my_chains <- ifelse(parallell::detectCores() == 1,
                    2,
                    parallel::detectCores() - 1)

res <- mrp(my_formula,
           surv = bes,
           ps = psw,
           aux = aux,
           const = "GSSCode",
           type = "continuous",
           chains = my_chains,
           iter = 1000,
           weight.var = "weight",
           seed = 181518)


## ----codeout-------------------------------------------------------------
purl("article.Rmd")
code <- readLines("article.R")
cat(code, sep = "\n")


