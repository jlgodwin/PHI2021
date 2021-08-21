compute_survey_proportions <- function(dataframe){

  print("# Average household size #")
  
  formula<-~hh_size
  
  print("# ALL #")
  All<-svymean(formula,
               my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type <-svyby(formula,
              ~hh_type,my.svydesign,svymean,na.rm=T)

  print("# by tenure#")
  hh_tenure <-svyby(formula,
                 ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both <-svyby(formula,
               ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  
  hh_type<-hh_type%>%right_join(hh_type_merge)
  hh_tenure<-hh_tenure%>%right_join(hh_tenure_merge)
  Both<-Both%>%right_join(both_merge)
  names(Both)[which(names(Both)=="se")]<-"se.average_hh_size"
  names(hh_type)[which(names(hh_type)=="se")]<-"se.average_hh_size"
  names(hh_tenure)[which(names(hh_tenure)=="se")]<-"se.average_hh_size"
  
  
  print("1-person household: ")
  formula2<-~as.numeric(hh_size == 1)
  
  print("# ALL #")
  All2<-svymean(formula2,
               my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type2 <-svyby(formula2,
                  ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure2 <-svyby(formula2,
                    ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both2 <-svyby(formula2,
               ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both2)[which(names(Both2)=="se")]<-"se.prevalence.1_person_hh"
  names(hh_type2)[which(names(hh_type2)=="se")]<-"se.prevalence.1_person_hh"
  names(hh_tenure2)[which(names(hh_tenure2)=="se")]<-"se.prevalence.1_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type2%>%right_join(hh_type)
  hh_tenure<-hh_tenure2%>%right_join(hh_tenure)
  Both<-Both2%>%right_join(Both)
  
  print("2-person household: ")
  formula3<-~as.numeric(hh_size == 2)
  
  print("# ALL #")
  All3<-svymean(formula3,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type3 <-svyby(formula3,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure3 <-svyby(formula3,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both3 <-svyby(formula3,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both3)[which(names(Both3)=="se")]<-"se.prevalence.2_person_hh"
  names(hh_type3)[which(names(hh_type3)=="se")]<-"se.prevalence.2_person_hh"
  names(hh_tenure3)[which(names(hh_tenure3)=="se")]<-"se.prevalence.2_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type3%>%right_join(hh_type)
  hh_tenure<-hh_tenure3%>%right_join(hh_tenure)
  Both<-Both3%>%right_join(Both)
  
  print("3-person household: ")
  formula4<-~as.numeric(hh_size == 3)
  
  print("# ALL #")
  All4<-svymean(formula4,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type4 <-svyby(formula4,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure4 <-svyby(formula4,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both4 <-svyby(formula4,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both4)[which(names(Both4)=="se")]<-"se.prevalence.3_person_hh"
  names(hh_type4)[which(names(hh_type4)=="se")]<-"se.prevalence.3_person_hh"
  names(hh_tenure4)[which(names(hh_tenure4)=="se")]<-"se.prevalence.3_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type4%>%right_join(hh_type)
  hh_tenure<-hh_tenure4%>%right_join(hh_tenure)
  Both<-Both4%>%right_join(Both)
  
  print("4-person household: ")
  formula5<-~as.numeric(hh_size == 4)
  
  print("# ALL #")
  All5<-svymean(formula5,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type5 <-svyby(formula5,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure5 <-svyby(formula5,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both5 <-svyby(formula5,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both5)[which(names(Both5)=="se")]<-"se.prevalence.4_person_hh"
  names(hh_type5)[which(names(hh_type5)=="se")]<-"se.prevalence.4_person_hh"
  names(hh_tenure5)[which(names(hh_tenure5)=="se")]<-"se.prevalence.4_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type5%>%right_join(hh_type)
  hh_tenure<-hh_tenure5%>%right_join(hh_tenure)
  Both<-Both5%>%right_join(Both)
  
  print("5-person household: ")
  formula6<-~as.numeric(hh_size == 5)
  
  print("# ALL #")
  All6<-svymean(formula6,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type6 <-svyby(formula6,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure6 <-svyby(formula6,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both6 <-svyby(formula6,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both6)[which(names(Both6)=="se")]<-"se.prevalence.5_person_hh"
  names(hh_type6)[which(names(hh_type6)=="se")]<-"se.prevalence.5_person_hh"
  names(hh_tenure6)[which(names(hh_tenure6)=="se")]<-"se.prevalence.5_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type6%>%right_join(hh_type)
  hh_tenure<-hh_tenure6%>%right_join(hh_tenure)
  Both<-Both6%>%right_join(Both)
  
  print("5-person household: ")
  formula6<-~as.numeric(hh_size == 5)
  
  print("# ALL #")
  All6<-svymean(formula6,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type6 <-svyby(formula6,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure6 <-svyby(formula6,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both6 <-svyby(formula6,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both6)[which(names(Both6)=="se")]<-"se.prevalence.5_person_hh"
  names(hh_type6)[which(names(hh_type6)=="se")]<-"se.prevalence.5_person_hh"
  names(hh_tenure6)[which(names(hh_tenure6)=="se")]<-"se.prevalence.5_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type6%>%right_join(hh_type)
  hh_tenure<-hh_tenure6%>%right_join(hh_tenure)
  Both<-Both6%>%right_join(Both)
  
  print("6-person household: ")
  formula7<-~as.numeric(hh_size == 6)
  
  print("# ALL #")
  All7<-svymean(formula7,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type7 <-svyby(formula7,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure7 <-svyby(formula7,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both7 <-svyby(formula7,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both7)[which(names(Both7)=="se")]<-"se.prevalence.6_person_hh"
  names(hh_type7)[which(names(hh_type7)=="se")]<-"se.prevalence.6_person_hh"
  names(hh_tenure7)[which(names(hh_tenure7)=="se")]<-"se.prevalence.6_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type7%>%right_join(hh_type)
  hh_tenure<-hh_tenure7%>%right_join(hh_tenure)
  Both<-Both7%>%right_join(Both)
  
  print("7-person household: ")
  formula8<-~as.numeric(hh_size == 7)
  
  print("# ALL #")
  All8<-svymean(formula8,
                my.svydesign,na.rm=T)
  
  print("# by type #")
  hh_type8 <-svyby(formula8,
                   ~hh_type,my.svydesign,svymean,na.rm=T)
  
  print("# by tenure#")
  hh_tenure8 <-svyby(formula8,
                     ~tenure,my.svydesign,svymean,na.rm=T)
  
  print("# both hh_type and hh_tenure#")
  Both8 <-svyby(formula8,
                ~hh_type+tenure,my.svydesign,svymean,na.rm=T)
  
  
  # rename the standard error to match other conventions #
  names(Both8)[which(names(Both8)=="se")]<-"se.prevalence.7_person_hh"
  names(hh_type8)[which(names(hh_type8)=="se")]<-"se.prevalence.7_person_hh"
  names(hh_tenure8)[which(names(hh_tenure8)=="se")]<-"se.prevalence.7_person_hh"
  
  
  # merge results with empty objects #
  print("Merging results.")
  hh_type<-hh_type8%>%right_join(hh_type)
  hh_tenure<-hh_tenure8%>%right_join(hh_tenure)
  Both<-Both8%>%right_join(Both)
  
  # All is returned as list, create matrix using the shape of factype
  # dat<-factype[1,]
  dat<-hh_type[1,]
  dat$hh_type<-"ALL"
  dat$tenure<-"ALL"
  dat[,names(All)]<-as.data.frame(All)[,1]
  dat[,paste0("se.",names(All))]<-as.data.frame(All)[,2]
  dat[,names(All2)]<-as.data.frame(All2)[,1]
  dat[,paste0("se.",names(All2))]<-as.data.frame(All2)[,2]
  dat[,names(All3)]<-as.data.frame(All3)[,1]
  dat[,paste0("se.",names(All3))]<-as.data.frame(All3)[,2]
  dat[,names(All4)]<-as.data.frame(All4)[,1]
  dat[,paste0("se.",names(All4))]<-as.data.frame(All4)[,2]
  dat[,names(All5)]<-as.data.frame(All5)[,1]
  dat[,paste0("se.",names(All5))]<-as.data.frame(All5)[,2]
  dat[,names(All6)]<-as.data.frame(All6)[,1]
  dat[,paste0("se.",names(All6))]<-as.data.frame(All6)[,2]
  dat[,names(All7)]<-as.data.frame(All7)[,1]
  dat[,paste0("se.",names(All7))]<-as.data.frame(All7)[,2]
  dat[,names(All8)]<-as.data.frame(All8)[,1]
  dat[,paste0("se.",names(All8))]<-as.data.frame(All8)[,2]
  
  final<-rbind(dat[,names(Both)],hh_type[,names(Both)],
               hh_tenure[,names(Both)],Both[,names(Both)])
  
  final
}
