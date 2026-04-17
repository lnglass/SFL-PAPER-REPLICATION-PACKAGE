library(dplyr)
library(haven)
library(foreign)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(panelView)
library(haven)
library(readr)
library(purrr)


#join student level data for each year (example 2022)

d22 <- read_dta("import 2022 student data")
g622<-read_dta("import 2022 grade 6 data")
g922<-read_dta("import 2022 grade 9 data")
ntest6<-read_dta("import 2022 national test 6 data")
ntest9<-read_dta("import 2022 national test 9 data")


d22<-d22%>%
  mutate(year=2022)


#merge all years together 

students<-bind_rows(d14,d15,d16,d17,d18,d19,d20,d21,d22)
grades6<-bind_rows(g614,g615,g616,g617,g618,g619,g620,g621,g622)
grades9<-bind_rows(g914,g915,g916,g917,g918,g919,g920,g921,g922)
ntest6<-bind_rows(ntest614,ntest615,ntest616,ntest617,ntest618,ntest621,ntest622)
ntest9<-bind_rows(ntest914,ntest915,ntest916,ntest917,ntest918,ntest921,ntest922)
teachers<-bind_rows(teachers14,teachers15,teachers16,teachers17,teachers18,teachers19,teachers20,teachers22,teachers22)

ntest6<-ntest6%>%
  mutate(
    swe26=swe2,
    swetestgrade6=swed_testgrade,
    mathtestgrade6=math_testgrade)
ntest9<-ntest9%>%
  mutate(
    swe29=swe2,
    swetestgrade9=swed_testgrade,
    mathtestgrade9=math_testgrade)

grades6<-grades6%>%
  mutate(
    math6=mathematics,
    swe6=swedish_both,
    sci6=naturalscience_both)
grades9<-grades9%>%
  mutate(
 math9=mathematics,
 swe9=swedish_both,
 sci9=naturalscience_both)


studentsgradesntest<-studentsgrades%>%
  left_join(ntest3, c("idstud","year"))%>%
  left_join(ntest6, c("idstud","year"))%>%
  left_join(ntest9, c("idstud","year"))%>%
  left_join(grades6, c("idstud","year"))%>%
  left_join(grades9, c("idstud","year"))
  

ds<-subset(studentsgradesntest, schooltype==11)


resourceschools<-c(138, 507, 675, 700, 729, 1024, 1294, 1304, 1323,
                   1484, 1530, 1619, 1630, 1994, 2278, 2295, 2398, 2451,
                   2470, 2506, 2543, 2734, 2749, 2942, 3388, 3504, 3568, 3626,
                   3751, 3826, 3830, 3885, 4049, 4556, 4728, 5401, 5403, 5446,
                   5476, 5796, 5799, 5852, 5901, 5948, 6097, 6146, 6221, 6242,
                   6680, 6745, 6843, 7072, 8166, 8370, 8550, 8975, 9181, 9290,
                   9393, 9453, 9462, 9531, 9691, 9797, 9900, 9988, 10021, 10138,
                   10153, 10196 , 10202, 10229, 10242, 10259, 10267, 10302, 10308,
                   10329, 10341, 10348, 10371, 10382, 10400, 10403 ,10512 ,10514,
                   10529, 10544, 10546)
ds<-ds%>%
  mutate(resourceschool=if_else(idschool.x%in%resourceschools,1,0))


ds<-ds%>%
  mutate(across(c(grade1,grade2,grade3,grade4,
                  grade5,grade6,grade7,grade8,
                  grade9,grades19), ~replace_na(.,0)))


grade_labels<-c(
  "1-3","1-4","3-6","3-9","4-6",
  "1-5","1-6","1-7","1-9","4-9",
  "5-9","6-9","7-9"
)

collapsed$grades<-factor(collapsed$grades,
                         levels=1:13,
                         labels=grade_labels)

windows()
plot<-ggplot(collapsed,aes(x=grades))+ geom_bar(aes(y=after_stat(count)),fill="skyblue")+
labs(x="Grades taught",y="Number of schools",)+theme_minimal()
plot

d1<-all%>%
    group_by(year)%>%
  summarise(
    participants1=mean(participants,na.rm=T),
    supervisors1=mean(supervisors,na.rm=T)
    )



studentsgradesntestC<-studentsgradesntest%>%
  select(idstud, year, idschool.x, sch_municipality.x, schoolowner.x, schooltype, idclass.x, female.x, parentedu.x, spec_ind, spec_group, iep, reduced_program, migrant.x,
         migrationyear, interpreter, gradevar, grade1, grade2, grade3, grade4, grade5, grade6, grade7, grade8, grade9, grades19,
         L2_swedish, swedish.x,  swedish_both.x, mathematics.x, eligible_secondary.x, gpa16.x, gpa17.x, npass.x, nfail.x, 
         swe26, swetestgrade6, mathtestgrade6, swe2, swe29, swetestgrade9, mathtestgrade9, math6, swe6, gpa16.y, gpa17.y, npass.y, nfail.y, 
         eligible_secondary.y, math9, swe9, swedish, swedish_L2, swedish_both, mathematics)


studentsgradesntestC<- studentsgradesntestC %>%
  mutate(across(everything(), ~{
    x<-haven::zap_labels(.x)
    x<-haven::zap_formats(x)
    x<-haven::zap_label(x)
    x<-as.character(x)
    x[x %in% c(".", "NA", "")]<- NA
    as.numeric(x)
  }))

ns<-subset(studentsgradesntestC, resourceschool==0)

### standardize
ns<- ns%>%
  group_by(year)%>%
  mutate(
    math6_z=scale(math6),
    swe6_z=scale(swe6),
    math9_z=scale(math9),
    swe9_z=scale(swe9),
    ntswe6_z=scale(ntswe6),
    ntswe9_z=scale(ntswe9),
    ntmath3_z=scale(ntmath3),
    ntmath6_z=scale(ntmath6),
    ntmath9_z=scale(ntmath9))



ns<-ns%>%
  mutate(private=
           case_when(
             schoolowner==5~1,
             schoolowner==2~0,
             schoolowner==1~0))

ns <- ns %>%
  mutate(
    zmathgrade = if_else(grade %in% c(6, 9), coalesce(math6, math9), NA_real_),
    zswegrade  = if_else(grade %in% c(6, 9), coalesce(swe6, swe9), NA_real_),
    zntmath    = if_else(grade %in% c(6, 9), coalesce(ntmath6, ntmath9), NA_real_),
    zntswe     = if_else(grade %in% c(6, 9), coalesce(ntswe6, ntswe9), NA_real_)
  )

write_sav(ns, "data.sav")

#####merge with sfl administrative

sfl<- read_sav("//micro.intra/Projekt/P1368$/P1368_Gem/Leah/s1422_sfl_treat.sav")

sflc<-sfl%>%
  select(idschool,year,numteach,experience,firstteacher,lecturer,
         fulltime,tfemale,idorg,supervisors,participants,groupsize,
         treatment,groups,treatment1,treatment_ever,nevertreated,
         firsttreatmentyear,firsttreatment,firsttreatmentyear2)
  sflc<-sflclean%>%
    mutate(idschool.x=idschool)
  
  
  
  new<-left_join(studentsgradesntestC,
            sflc,
            by=c("year","idschool"))


  #merge with teacher data
  ####################TEACHER FILES 
  ###########teacherstuff
  path<-"//micro.intra/Projekt/P1368$/P1368_Gem/Leah"
  list.files(path)
  
  files<-list.files(
    path, 
    pattern = "^teachers[0-9]{4}\\.dta$",
    full.names = T)
  
  teachers<-files%>%
    map_df(~{
      year<-str_extract(basename(.x),"[0-9]{4}")%>%as.integer()
      df<-read_dta(.x)
      df$year<-year
      df})
  
  
  teachers<-teachers%>%
    mutate(cert0=case_when(
      certified==0~1,
      certified==1~0,
      certified==2~0
    ))
  
  teachers<-teachers%>%
    mutate(cert1=case_when(
      certified==0~0,
      certified==1~1,
      certified==2~1
    ))
  
  teachagg<-teachers%>%
    group_by(idschool,year)%>%
    summarise(cert=mean(cert1))
  
  
  teachagg<-teachagg%>%
    rename(idschool1=idschool)
  
 new<-new%>%
    left_join(teachagg, by=c("idschool1","year"))
  
#variables 

new<-new%>%
  mutate(
    leniencyswe=zswegrade-zntswe,
    leniencymath=zmathgrade-zntmath,
  )

new<-new%>%
  mutate(
    new5=if_else(
      years_since_migration<=5,1,0)
    )

new<-new%>%
  mutate(
    treatment=coalesce(supervisors,0)
  )

new<-new%>%
  mutate(treatment1=case_when(
    treatment>0~1,
    treatment==0~0)
  )

new<-new%>%
  group_by(idschool)%>%
  mutate(
    treatment_ever=ifelse(any(treatment1==1),1,0))

new<-new%>%
  mutate(nevertreated=case_when(
    treatment_ever==1~0,
    treatment_ever==0~1)
  )

ns<-ns%>%
  group_by(idschool)%>%
  mutate(
    firsttreatmentyear=min(year2[treatment1==1],na.rm = T),
    firsttreatment=ifelse(year2==firsttreatmentyear&treatment1==1,1,0)
  )

new<-new%>%
  mutate(
    firsttreatmentyear2=ifelse(
      is.infinite(firsttreatmentyear),0, firsttreatmentyear
    ))


####balancing tests
library(ggplot2)
library(dplyr)
library(fixest)
library(broom)

pre2016<-subset(all,year<2016)

balschool16<-pre2016%>%
  group_by(idschool1)%>%
  summarise(
    treat=first(treatment_ever),
    childimm=mean(childimm),
    mig=mean(migrant1),
    newlyarrived5=mean(newlyarrived5),
    certified=mean(certified),
    experience=mean(experience),
    priv=mean(private),
    zntmath=mean(zntmath),
    zntswe=mean(zntswe),
    zswegrade=mean(zswegrade),
    zmathgrade=mean(zmathgrade),
    iep=mean(iep),
    support=mean(support),
    pared=mean(pared)
  )
names(pre2016)

library(tableone)

vars<-c("treat", "childimm", "mig", "newlyarrived5", "certified", "experience",
        "priv","zntmath","zntswe","zswegrade","zmathgrade", "iep","support", "pared")
tab<-CreateTableOne(vars=vars, strata="treat",data=balschool16,test=T)
print(tab, smd=T,digits.mean=4,noSpaces=T)


###generate top and bottom quartile distinctions 

bal2016<-bal2016e%>%
  mutate(ach_quartile=ntile(ach,4),
         cert_quart=ntile(certified,4),
         exp_quart=ntile(experience,4))

pre2016<-bal2016 %>%
  left_join(ach_quartile, cert_quart, exp_quart, by=c("idschool1"))

mergesfl<-pre2016 %>%
  left_join(ach_quartile, cert_quart, exp_quart, by=c("idschool1"))





####merge data for analysis in stata 

write_sav(mergessfl,"new.sav")

