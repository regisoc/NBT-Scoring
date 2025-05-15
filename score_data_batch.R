###############################################################################
#### NIH Baby Toolbox scoring script ####
# this script scores the following tests: Mullen Receptive (En and Sp), 
# Mullen Expressive (En and Sp), Executive Function (Familiarization) (En and Sp), 
# and Looking While Listening (En and Sp).
#
# Note: those that complete MR and ME get a Language composite.
# No scores are provided for NCD and SCD
# Contact ych@northwestern.edu regarding and issues or troubleshooting
###############################################################################

# required libraries, uncomment below and install if unavailable
# install.packages('psych',dep=T)
# install.packages(janitor,dep=T)
# install.packages(tidyverse,dep=T)
# install.packages(mirt,dep=T)
# install.packages(mirtCAT,dep=T)
# install.packages(mnormt,dep=T)
# install.packages(nlme,dep=T)
# install.packages(jsonlite,dep=T)
# install.packages(purrr,dep=T)
library(psych)
library(janitor)
library(tidyverse)
library(mirt)
library(mirtCAT)
library(mnormt)
library(nlme)
library(jsonlite)
library(purrr)

# set directory - if this doesn't work, click Session >> Set Working Directory >> To Source File Location
setwd(getwd()) 

# load scoring files
source('scoring/score_lwl.R')
source('scoring/score_lwl_sp.R')
source('scoring/score_mr.R')
source('scoring/score_me.R')
source('scoring/score_mvr.R')
source('scoring/score_ef.R')
source('scoring/score_norming.R')
source('scoring/check_calibration.R')

###############################################################################
########### change the file path ########
# note: item_export_path and registration_export_path require CSV files 

# default export
result_folder <- "/data/nbt_scoring/"

item_export_path         <-result_folder # change this to the folder for item exports
registration_export_path <-result_folder # change this to the folder for registration exports
json_export_path         <-result_folder # change this to the folder for json (gaze) data

item_export_files         <-list.files(item_export_path,pattern='ItemExportNarrow') # do not change
registration_export_files <-list.files(registration_export_path,pattern='RegistrationExportNarrow') # do not change
json_export_files         <-list.files(json_export_path,pattern='AssessmentGazeData') # do not change

######### do not change below: this will run through all item exports and score them ###########

# final output to be dump into a file
all_output<-data.frame()

# iterate over all export files
for(i in 1:length(item_export_files)) {
  
  # scoring by file
  file_name=item_export_files[i]
  print(paste0("------ Scoring: ",file_name))
  
  # match_id format "PSCID_CANDID_VISIT" e.g. "DCC090_123456_V01"
  t <- str_split(file_name,pattern='_')[[1]]
  instrument_name <- paste0(t[1], '_', t[2], '_', t[3]) # ncl_ch_nbtb
  match_id        <- paste0(t[4], '_', t[5], '_', t[6]) # "PSCID_CANDID_VISIT"

  # match_id<-str_split(file_name,pattern='_')[[1]][2]
  item_export         <- read.csv(paste0(item_export_path,instrument_name,'_',match_id,'_ItemExportNarrow.csv'))
  registration_export <- read.csv(paste0(registration_export_path,instrument_name,'_',match_id,'_RegistrationExportNarrow.csv'))

  # pull age information
  age<-registration_export%>% 
    subset(Key=='TotalAgeInMonths')%>%
    mutate(Value=as.numeric(Value))%>%
    pull(Value)
  
  # pull pid
  pid<-registration_export%>% 
    slice(1)%>%
    pull(PID)
  
  ###############################################################################
  
  #### score LWL ####
  if('Looking While Listening' %in% item_export$InstrumentTitle){
    lwl_data<-item_export%>%
      filter(InstrumentTitle=='Looking While Listening')%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    items_completed=lwl_data%>%
      dplyr::select(contains(c('G1','G2','G3','G4','G5')))%>%
      dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
    
    if(items_completed==0){
      lwl_scored=NULL
    }else{
      # note: you may get warnings from running this code, , ignore unless error produced
      lwl_scored<-score_lwl(lwl_data,age=age)%>%
        as_tibble()%>%
        mutate(CSS=mirtTheta_1*9.1024+430,
               CSS_SE=analyticSE1*9.1024)%>%
        dplyr::select(-c('mirtTheta_1','analyticSE1'))%>%
        mutate(score='LWL En',
               pid=pid,
               age=age)
    }
    
  }else if('Looking While Listening (Spanish)' %in% item_export$InstrumentTitle){
    
    lwl_data<-item_export%>%
      filter(InstrumentTitle=='Looking While Listening (Spanish)')%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    lwl_scored<-score_lwl_sp(lwl_data)
    lwl_scored<-as.data.frame(t(lwl_scored))%>%
      mutate(score='LWL Sp',
             pid=pid,
             age=age)
    
  }else{
    print('no LWL data')
    lwl_scored<-NULL
  }
  
  #### score Mullen Receptive ####
  if('Mullen Receptive' %in% item_export$InstrumentTitle || 'Mullen Receptive (Spanish)' %in% item_export$InstrumentTitle){
    mr_data<-item_export%>%
      filter(InstrumentTitle=='Mullen Receptive'|InstrumentTitle=='Mullen Receptive (Spanish)')%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    items_completed=mr_data%>%
      dplyr::select(contains('RL'))%>%
      dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
    
    if(items_completed==0){
      mr_scored=NULL
    }else{
      # note: you may get warnings from running this code, ignore unless error produced
      mr_scored<-score_mr(mr_data)%>%
        as_tibble()%>%
        mutate(CSS=mirtTheta_1*9.1024+430,
               CSS_SE=analyticSE1*9.1024,
               pid=pid,
               age=age)
      
      if(is.na(mr_scored$mirtTheta_1)){
        mr_scored<-NULL
      }
    }
    
  }else{
    print('no MR data')
    mr_scored<-NULL
  }
  
  #### score Mullen Expressive ####
  if('Mullen Expressive Observational' %in% item_export$InstrumentTitle||'Mullen Expressive Prompted' %in% item_export$InstrumentTitle||
     'Mullen Expressive Observational (Spanish)' %in% item_export$InstrumentTitle||
     'Mullen Expressive Prompted (Spanish)' %in% item_export$InstrumentTitle){
    me_data<-item_export%>%
      filter(InstrumentTitle=='Mullen Expressive Observational'|InstrumentTitle=='Mullen Expressive Prompted'|
               InstrumentTitle=='Mullen Expressive Observational (Spanish)'|InstrumentTitle=='Mullen Expressive Prompted (Spanish)')%>%
      dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    items_completed=me_data%>%
      dplyr::select(contains('EL'))%>%
      dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
    
    if(items_completed==0){
      print('completed 0 items')
      me_scored=NULL
    }else{
      # note: you may get warnings from running this code, ignore unless error produced
      me_scored<-score_me(me_data)%>%
        as_tibble()%>%
        mutate(CSS=mirtTheta_1*9.1024+440,
               CSS_SE=analyticSE1*9.1024,
               pid=pid,
               age=age)
      
      if(is.na(me_scored$mirtTheta_1)){
        me_scored<-NULL
      }
    }
    
  }else{
    print('no ME data')
    me_scored<-NULL
  }
  
  #### score Mullen Visual Reception ####
  if('Mullen Visual Reception' %in% item_export$InstrumentTitle || 
     'Mullen Visual Reception (Spanish)' %in% item_export$InstrumentTitle){
    mvr_data<-item_export%>%
      filter(InstrumentTitle=='Mullen Visual Reception'|InstrumentTitle=='Mullen Visual Reception (Spanish)')%>%
      dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    items_completed=mvr_data%>%
      dplyr::select(contains('MVR'))%>%
      dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
    
    if(items_completed==0){
      mvr_scored=NULL
    }else{
      # note: you may get warnings from running this code, ignore unless error produced
      mvr_scored<-score_mvr(mvr_data)%>%
        as_tibble()%>%
        mutate(CSS=mirtTheta_1*9.1024+445,
               CSS_SE=analyticSE1*9.1024)%>%
        dplyr::select(-c('mirtTheta_1','analyticSE1'))%>%
        mutate(score='MVR',
               pid=pid,
               age=age)
    }
    
  }else{
    print('no MVR data')
    mvr_scored<-NULL
  }
  
  #### score Familiarization ####
  if('Executive Function' %in% item_export$InstrumentTitle ||
     'Executive Function (Spanish)' %in% item_export$InstrumentTitle){
    
    ef_warning=NA
    
    # check calibration first - if calibration unsuccessful, no familiarization score
    ef_data<-item_export%>%
      filter(InstrumentTitle=='Executive Function'|InstrumentTitle=='Executive Function (Spanish)')%>%
      dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
      subset(Key=='Score')%>%
      pivot_wider(names_from=ItemID,values_from=Value)
    
    calibrated<-check_calibration(ef_data)
    
    if(calibrated==1&&length(calibrated)>0){
      
      # find approximate match (json file name is some numbers off)
      json_match<-str_split(match_id,'T')
      json_match[[1]][2]<-str_remove(json_match[[1]][2],'.csv')
      json_match[[1]][2]<-as.numeric(json_match[[1]][2])-1
      json_export=NULL
      for(i in 1:length(json_export_files)){
        match_date<-str_detect(json_export_files[i],json_match[[1]][1])
        if(match_date==T){
          # print(paste('match',json_export_files[i],json_match[[1]][1]))
          temp_file<-str_split(json_export_files[i],'T')
          temp_file<-str_remove(temp_file[[1]][2],'.json')%>%as.numeric()
          if(temp_file<=as.numeric(json_match[[1]][2])+5&&temp_file>=as.numeric(json_match[[1]][2])-5){
            json_export<-jsonlite::read_json(paste0(json_export_path,json_export_files[i]),simplifyVector=T)
          }
        }
      }
      
      # double check pid is the same
      if(!is.null(json_export)){
        if(!is.null(json_export[[1]]$dataPairs)){
          if(json_export[[1]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN)!=pid){
            ef_warning='id mismatch: recheck if this is the correct json file'
            print(ef_warning)
          }
        }else{
          print('missing id and/or data')
        }
      }
      
      # pull ef data
      correct_data=NA
      if(!is.null(json_export)){
        for(i in 1:length(json_export)){
          item_list=json_export[[i]]%>%distinct(itemID)
          for(k in 1:length(item_list$itemID)){
            if(str_detect(item_list$itemID[k],'Hab')==T){
              correct_data=i
              break
            }
            else if(is.na(correct_data)){
              correct_data=NA
            }
          }
        }
      }
      
      # pull fps 
      # real_fps<-json_export[[correct_data]]%>%subset(dataKey=='arFramesPerSecond_test_actual')%>%pull(dataValue)%>%as.numeric()
      
      # pull raw json data
      ef_data<-json_export[[correct_data]]
      
      # externally score json data
      if(is.null(ef_data)){
        ef_scored<-NULL
      }else{
        ef_scored<-score_ef(ef_data)%>%
          mutate(score='familiarization',
                 pid=json_export[[1]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN),
                 age=age,
                 message=ef_warning)%>%
          as_tibble()
      }
    } else if(calibrated==0||is.na(calibrated)||length(calibrated)==0){
      ef_warning='did not successfully calibrate'
      print(ef_warning)
      ef_scored<-data.frame(score='familiarization',
                            pid=pid,
                            age=age,
                            message=ef_warning)
    }
    
  }else{
    print('no EF data')
    ef_scored<-NULL
  }
  
  #### score Language composite + get norms ####
  if(!is.null(mr_scored)&&!is.null(me_scored)){
    lang_norms=score_norming(mr_css=mr_scored$CSS,
                             me_css=me_scored$CSS,
                             mr_css_se=mr_scored$CSS_SE,
                             me_css_se=me_scored$CSS_SE,
                             age=age)%>%
      mutate(pid=pid,
             age=age)
  }else if(!is.null(mr_scored)&&is.null(me_scored)){
    lang_norms=score_norming(mr_css=mr_scored$CSS,
                             me_css=NA,
                             mr_css_se=mr_scored$CSS_SE,
                             me_css_se=NA,
                             age=age)%>%
      mutate(pid=pid,
             age=age)
  }else if(is.null(mr_scored)&&!is.null(me_scored)){
    lang_norms=score_norming(mr_css=NA,
                             me_css=me_scored$CSS,
                             mr_css_se=NA,
                             me_css_se=me_scored$CSS_SE,
                             age=age)%>%
      mutate(pid=pid,
             age=age)
  }else{
    lang_norms=NULL
  }
  
  #### output scored data into a table #####
  output<-data.frame(lang_norms)%>%
    dplyr::bind_rows(mvr_scored)%>%
    dplyr::bind_rows(lwl_scored)%>%
    dplyr::bind_rows(ef_scored)
  
  print(output) # view output
  
  if(nrow(output)==0){
    'no data to add'
  }else{
    all_output<-all_output%>%
      dplyr::bind_rows(output)
  }
  
}

###############################################################################

# write output into csv - creates one file containing all participants' scores
write.csv(
  all_output,
  file=paste0(result_folder,'ncl_ch_nbtb_scores.csv'),
  na='',
  row.names = F
)
