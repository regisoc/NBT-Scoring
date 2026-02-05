###############################################################################
#### NIH Baby Toolbox scoring script ####
# this script scores the following tests: Mullen Receptive (En and Sp),
# Mullen Expressive (En and Sp), Executive Function (Familiarization) (En and Sp),
# Looking While Listening (En and Sp), and Mullen Visual Reception (En and Sp).
# Newly added measures include:
# Memory Task Learning and Test, Visual Delayed Response, Who Has More, Subitizing,
# Verbal Arithmetic, Counting
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
source('scoring/score_mtl.R')
source('scoring/score_mtt.R')
source('scoring/score_vdr.R')
source('scoring/score_whm.R')
source('scoring/score_subitizing.R')
source('scoring/score_va.R')
source('scoring/score_counting.R')
source('scoring/score_lang_norms.R')
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

# match ids to skip
excluded_match_ids <- c(
  # not sure yet, Catherine to look into the JSON file for missing gazeLocationOnScreen
  "CHCCH0177_587280_V04",
  # missing MTBfx lib?
  "CHCHL0056_575298_V06",
  "CHEUY0018_116400_V06"
)

# final output to be dump into a file
all_output<-data.frame()
for(i in 1:length(item_export_files)){
  # scoring by file
  # ex of filename
  # ncl_ch_nbtb_QIUMN0013_523319_P06_ScoresExport_2024-03-22T153050.csv
  # ncl_ch_nbtb_QINWU0022_523520_P06_RegistrationExport_2024-04-30T193955.csv
  file_name=item_export_files[i]
  print(paste0("------ Scoring [",i,"]: ",file_name))

  # filename without extension
  barename <- str_split(file_name, pattern='.csv')[[1]][1]

  # match_id format "PSCID_CANDID_VISIT" e.g. "DCC090_123456_V01"
  t <- str_split(barename,pattern='_')[[1]]
  instrument_name <- paste0(t[1], '_', t[2], '_', t[3]) # ncl_ch_nbtb
  match_id        <- paste0(t[4], '_', t[5], '_', t[6]) # "PSCID_CANDID_VISIT"
  timestamp       <- t[8]                               # 2024-04-30T193955 => not ISO-8601 compliant.\

  # skip item
  if (match_id %in% excluded_match_ids) {
    next
  }

  # match_id<-str_split(file_name,pattern='_')[[1]][2]
  item_export <- readr::read_csv(paste0(item_export_path,instrument_name,'_',match_id,'_ItemExportNarrowStructure_',timestamp,'.csv'))
  registration_export <- readr::read_csv(paste0(registration_export_path,instrument_name,'_',match_id,'_RegistrationExportNarrowStructure_',timestamp,'.csv'))

  # check for multiple ids
  item_export_ids<-item_export%>%distinct(PID)
  multiple_ids=F
  if(nrow(item_export_ids)>1){
    multiple_ids=T
    print(paste('multiple PIDs found for ',file_name))
  }

  # # find registration export
  # registration_export<-NULL
  # if(paste0('RegistrationExportNarrowStructure_',match_id) %in% registration_export_files){
  #   print(paste0('exact match: registration export found for RegistrationExportNarrowStructure_',match_id))
  #   registration_export<-read.csv(paste0(registration_export_path,'RegistrationExportNarrowStructure_',match_id))
  # }else{
  #   print(paste0('no registration export not found for RegistrationExportNarrowStructure_',match_id))
  #   print('looking for approximate match')

  #   # find approximate match (filename is some numbers off)
  #   registration_match<-str_split(match_id,'T')
  #   registration_match[[1]][2]<-str_remove(registration_match[[1]][2],'.csv')
  #   registration_match[[1]][2]<-as.numeric(registration_match[[1]][2])-1
  #   registration_export=NULL
  #   for(k in 1:length(registration_export_files)){
  #     match_date<-str_detect(registration_export_files[k],registration_match[[1]][1])
  #     if(match_date==T){
  #       # print(paste(i,'date match',registration_export_files[k],registration_match[[1]][1]))
  #       temp_file<-str_split(registration_export_files[k],'T')
  #       temp_file<-str_remove(temp_file[[1]][2],'.csv')%>%as.numeric()
  #       if(temp_file<=as.numeric(registration_match[[1]][2])+5&&temp_file>=as.numeric(registration_match[[1]][2])-5){
  #         registration_export<-read.csv(paste0(registration_export_path,registration_export_files[k]))
  #         print('found approximate registration export match')
  #       }
  #     }
  #   }
  # }

  # double check pid is the same between registration and item export
  if(!is.null(registration_export)){

    registration_export_ids=registration_export%>%distinct(PID)
    print(paste('registration export ids:',registration_export_ids))
    print(paste('item export ids:',item_export_ids))

    # pull out matching PIDs
    matching_reg_ids<-registration_export_ids$PID[registration_export_ids$PID %in% item_export_ids$PID]
    print(paste('registration export ids:',registration_export_ids))
    print(paste('item export ids:',item_export_ids))

    if (!("TotalAgeInMonths" %in% registration_export$Key)) {
      reg_id_df <- NULL
    } else {
      # pull out age info
      reg_id_df<-registration_export%>%
        subset(Key=='TotalAgeInMonths')%>%
        mutate(Value=as.numeric(Value),
              PID=as.character(PID))%>%
        pivot_wider(names_from=Key,values_from=Value)%>%
        rename(age=TotalAgeInMonths)%>%
        select(c(PID,age))%>%
        group_by(PID)%>%
        slice(1)%>%
        ungroup()
    }


  }else{
    print('no registration export')
    reg_id_df=NULL
  }

  # if multiple ids, runs through for loop to score all of them
  if(is.null(reg_id_df)){
    print('no registration file for this item export, cannot score')
  }else{
    for(id in 1:nrow(reg_id_df)){
      age=reg_id_df$age[id]
      pid=reg_id_df$PID[id]

      ###############################################################################

      #### score LWL ####
      if('Looking While Listening' %in% item_export$InstrumentTitle){
        lwl_data<-item_export%>%
          filter(InstrumentTitle=='Looking While Listening')%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=lwl_data%>%
          dplyr::select(contains(c('G1','G2','G3','G4','G5')))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          lwl_scored=NULL
          print('no LWL items')
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
          print('Successfully scored LWL English')
        }

      }else if('Looking While Listening (Spanish)' %in% item_export$InstrumentTitle){

        lwl_data<-item_export%>%
          filter(InstrumentTitle=='Looking While Listening (Spanish)')%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        lwl_scored<-score_lwl_sp(lwl_data)
        lwl_scored<-as.data.frame(t(lwl_scored))%>%
          mutate(score='LWL Sp',
                 pid=pid,
                 age=age)
        print('Successfully scored LWL Spanish')

      }else{
        print('no LWL data')
        lwl_scored<-NULL
      }

      #### score Mullen Receptive ####
      if('Mullen Receptive' %in% item_export$InstrumentTitle || 'Mullen Receptive (Spanish)' %in% item_export$InstrumentTitle){
        mr_data<-item_export%>%
          filter(InstrumentTitle=='Mullen Receptive'|InstrumentTitle=='Mullen Receptive (Spanish)')%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=mr_data%>%
          dplyr::select(contains('RL'))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          mr_scored=NULL
          print('No MR items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          mr_scored<-score_mr(mr_data)%>%
            as_tibble()%>%
            mutate(CSS=mirtTheta_1*9.1024+430,
                   CSS_SE=analyticSE1*9.1024,
                   pid=pid,
                   age=age)
          print('Succesfully scored MR')

          if(is.na(mr_scored$mirtTheta_1)){
            mr_scored<-NULL
            print ('No MR theta score')
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
          subset(Key=='Score'&PID==pid)%>%
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
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=mvr_data%>%
          dplyr::select(contains(c('MVR','RL')))%>%
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
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        calibrated<-check_calibration(ef_data)

        if(calibrated==1&&length(calibrated)>0){

          # # find approximate match (json file name is some numbers off)
          # json_match<-str_split(match_id,'T')
          # json_match[[1]][2]<-str_remove(json_match[[1]][2],'.csv')
          # json_match[[1]][2]<-as.numeric(json_match[[1]][2])-1
          # json_export=NULL
          # for(i in 1:length(json_export_files)){
          #   match_date<-str_detect(json_export_files[i],json_match[[1]][1])
          #   if(match_date==T){
          #     # print(paste(i,'date match',json_export_files[i],json_match[[1]][1]))
          #     temp_file<-str_split(json_export_files[i],'T')
          #     temp_file<-str_remove(temp_file[[1]][2],'.json')%>%as.numeric()
          #     if(temp_file<=as.numeric(json_match[[1]][2])+5&&temp_file>=as.numeric(json_match[[1]][2])-5){
          #       json_export<-jsonlite::read_json(paste0(json_export_path,json_export_files[i]),simplifyVector=T)
          #     }
          #   }
          # }

          # exact file name
          json_file_name <- paste0(json_export_path,instrument_name,'_',match_id,'_AssessmentGazeData_',timestamp,'.json')
          if (file.exists(json_file_name)) {
            json_export <- jsonlite::read_json(json_file_name, simplifyVector=T)
          } else {
            print(paste0("No exact Assessment Gaze JSON file name match for ", match_id))
            json_export <- NULL
          }

          # double check pid is the same
          if(!is.null(json_export)){
            if(!is.null(json_export[[1]]$dataPairs)){
              if(json_export[[1]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN)!=pid){
                print(paste('id in json: ',json_export[[1]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN)))
                print(paste('current id: ',pid))
                ef_warning=paste('id mismatch: recheck if this is the correct json file. ID in json is ',
                                 json_export[[1]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN),
                                 'but current id is',pid)
                print(ef_warning)
              }
            }else{
              ef_warning='missing id and/or data'
              print(ef_warning)
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
          if (!is.null(json_export) && !is.na(correct_data)) {
            ef_data<-json_export[[correct_data]]
          } else {
            ef_data <- NULL
          }

          # externally score json data
          if(is.null(ef_data)){
            ef_scored<-NULL
          }else{
            ef_scored<-score_ef(ef_data)%>%
              mutate(score='familiarization',
                     pid=json_export[[correct_data]]$dataPairs%>%distinct(userPIN)%>%drop_na()%>%pull(userPIN),
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

      #### score Memory Task Learning ####
      if('Memory Task Learning' %in% item_export$InstrumentTitle ||
         'Memory Task Learning (Spanish)' %in% item_export$InstrumentTitle){
        mtl_data<-item_export%>%
          filter(InstrumentTitle=='Memory Task Learning'|InstrumentTitle=='Memory Task Learning (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=mtl_data%>%
          dplyr::select(contains('Encoding'))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          mtl_scored=NULL
          print('no MTL items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          mtl_scored<-score_mtl(mtl_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+430,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='MTL',
                   pid=pid,
                   age=age)
          print('Successfully scored MTL')
        }

      }else{
        print('no MTL data')
        mtl_scored<-NULL
      }

      #### score Memory Task Test ####
      if('Memory Task Test' %in% item_export$InstrumentTitle ||
         'Memory Task Test (Spanish)' %in% item_export$InstrumentTitle){
        mtt_data<-item_export%>%
          filter(InstrumentTitle=='Memory Task Test'|InstrumentTitle=='Memory Task Test (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=mtt_data%>%
          dplyr::select(contains('MemTest'))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          mtt_scored=NULL
          print('no MTT items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          mtt_scored<-score_mtt(mtt_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+445,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='MTT',
                   pid=pid,
                   age=age)
          print('Successfully scored MTT')
        }

      }else{
        print('no MTT data')
        mtt_scored<-NULL
      }

      #### score Visual Delayed Response ####
      if('Visual Delayed Response' %in% item_export$InstrumentTitle ||
         'Visual Delayed Response (Spanish)' %in% item_export$InstrumentTitle){
        vdr_data<-item_export%>%
          filter(InstrumentTitle=='Visual Delayed Response'|InstrumentTitle=='Visual Delayed Response (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=vdr_data%>%
          dplyr::select(contains(paste0('VDRTouch_',c(4:11))))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          vdr_scored=NULL
          print('no VDR items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          vdr_scored<-score_vdr(vdr_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+445,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='VDR',
                   pid=pid,
                   age=age)
          print('Successfully scored VDR')
        }

      }else{
        print('no VDR data')
        vdr_scored<-NULL
      }

      #### score Who Has More ####
      if('Who Has More' %in% item_export$InstrumentTitle ||
         'Who Has More (Spanish)' %in% item_export$InstrumentTitle){
        whm_data<-item_export%>%
          filter(InstrumentTitle=='Who Has More'|InstrumentTitle=='Who Has More (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=whm_data%>%
          dplyr::select(contains(paste0('WHM',c(1:22))))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          whm_scored=NULL
          print('no WHM items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          whm_scored<-score_whm(whm_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+440,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='WHM',
                   pid=pid,
                   age=age)
          print('Successfully scored WHM')
        }

      }else{
        print('no WHM data')
        whm_scored<-NULL
      }

      #### score Subitizing ####
      if('Subitizing' %in% item_export$InstrumentTitle ||
         'Subitizing (Spanish)' %in% item_export$InstrumentTitle){
        sub_data<-item_export%>%
          filter(InstrumentTitle=='Subitizing'|InstrumentTitle=='Subitizing (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=sub_data%>%
          dplyr::select(contains(paste0('NRS')))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          sub_scored=NULL
          print('no SUB items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          sub_scored<-score_subitizing(sub_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+440,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='SUB',
                   pid=pid,
                   age=age)
          print('Successfully scored SUB')
        }

      }else{
        print('no SUB data')
        sub_scored<-NULL
      }

      #### score Verbal Arithmetic ####
      if('Verbal Arithmetic' %in% item_export$InstrumentTitle ||
         'Verbal Arithmetic (Spanish)' %in% item_export$InstrumentTitle){
        va_data<-item_export%>%
          filter(InstrumentTitle=='Verbal Arithmetic'|InstrumentTitle=='Verbal Arithmetic (Spanish)')%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=va_data%>%
          dplyr::select(contains(paste0('REMA')))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          va_scored=NULL
          print('no VA items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          va_scored<-score_va(va_data)%>%
            as_tibble()%>%
            mutate(CSS=theta*9.1024+450,
                   CSS_SE=SE*9.1024)%>%
            dplyr::select(-c('theta','SE'))%>%
            mutate(score='VA',
                   pid=pid,
                   age=age)
          print('Successfully scored VA')
        }

      }else{
        print('no VA data')
        va_scored<-NULL
      }

      #### score Counting ####
      # Note: uses both Verbal Counting and Object Counting data
      # these are combined under "Counting" post-norming
      if('Object Counting' %in% item_export$InstrumentTitle ||
         'Verbal Counting' %in% item_export$InstrumentTitle ||
         'Object Counting (Spanish)' %in% item_export$InstrumentTitle||
         'Verbal Counting (Spanish)' %in% item_export$InstrumentTitle){
        counting_data<-item_export%>%
          filter(str_detect(InstrumentTitle,'Counting'))%>%
          dplyr::select(-c('InstrumentID','InstrumentTitle'))%>%
          subset(Key=='Score'&PID==pid)%>%
          pivot_wider(names_from=ItemID,values_from=Value)

        items_completed=counting_data%>%
          dplyr::select(contains(c('VC','OC')))%>%
          dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))

        if(items_completed==0){
          counting_scored=NULL
          print('no Counting items')
        }else{
          # note: you may get warnings from running this code, ignore unless error produced
          counting_scored<-score_counting(counting_data)%>%
            as_tibble()%>%
            mutate(CSS=mirtTheta_1*9.1024+440,
                   CSS_SE=analyticSE1*9.1024)%>%
            dplyr::select(-c('mirtTheta_1','analyticSE1'))%>%
            mutate(score='Counting',
                   pid=pid,
                   age=age)
          print('Successfully scored Counting')
        }

      }else{
        print('no Counting data')
        counting_scored<-NULL
      }

      #### score Language composite + get norms ####
      if(!is.null(mr_scored)&&!is.null(me_scored)){
        lang_norms=score_lang_norms(mr_css=mr_scored$CSS,
                                    me_css=me_scored$CSS,
                                    mr_css_se=mr_scored$CSS_SE,
                                    me_css_se=me_scored$CSS_SE,
                                    age=age)%>%
          mutate(pid=pid,
                 age=age)
      }else if(!is.null(mr_scored)&&is.null(me_scored)){
        lang_norms=score_lang_norms(mr_css=mr_scored$CSS,
                                    me_css=NA,
                                    mr_css_se=mr_scored$CSS_SE,
                                    me_css_se=NA,
                                    age=age)%>%
          mutate(pid=pid,
                 age=age)
      }else if(is.null(mr_scored)&&!is.null(me_scored)){
        lang_norms=score_lang_norms(mr_css=NA,
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
        dplyr::bind_rows(ef_scored)%>%
        dplyr::bind_rows(mtl_scored)%>%
        dplyr::bind_rows(mtt_scored)%>%
        dplyr::bind_rows(vdr_scored)%>%
        dplyr::bind_rows(whm_scored)%>%
        dplyr::bind_rows(sub_scored)%>%
        dplyr::bind_rows(va_scored)%>%
        dplyr::bind_rows(counting_scored)

      print(output) # view output

      if(nrow(output)==0){
        'no data to add'
      }else{
        all_output<-all_output%>%
          dplyr::bind_rows(output)
      }
    }
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
