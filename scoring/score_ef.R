score_ef<-function(data){
  require(tidyverse)
  require(janitor)

  # clean up json data to only select look detection portion.
  hab_item_by_id_temp<-data%>%
    clean_names()%>%
    dplyr::select(-contains('data_pairs'))%>%
    arrange(item_id,elapsed_time)%>%
    # grab look detection info
    mutate(temp_start=case_when(event_name=='beganLookDetection'~row_number()),
           temp_end=case_when(event_name=='endedLookDetection'~row_number()),
           temp=row_number())%>%
    group_by(item_id)%>%
    # re-calculate time elapsed
    mutate(time_itersum=elapsed_time-lag(elapsed_time, default = first(elapsed_time)))%>%
    mutate(true_time_elapsed=cumsum(time_itersum))
  
  # now combine start/end data by item and person
  hab_end<-hab_item_by_id_temp%>%
    subset(!is.na(temp_end))%>%
    dplyr::select(c('item_id','temp_end'))
  
  hab_starts<-hab_item_by_id_temp%>%
    subset(!is.na(temp_start))%>%
    dplyr::select(c('item_id','temp_start'))%>%
    merge(hab_end,by=c('item_id'),all.x=T)%>%
    arrange(temp_start)
  
  # now clean up previous data frame to select rows that fall between start and end 
  # for that item + select data where gaze was recorded
  hab_item_by_id_temp_cleaned<-hab_item_by_id_temp%>%
    dplyr::select(-c('temp_start','temp_end'))%>%
    merge(hab_starts,by=c('item_id'),all.x=T)%>%
    arrange(item_id,elapsed_time)%>%
    subset(temp>temp_start&temp<temp_end)%>%
    subset(event_name=='faceVerticesChanged')
  
  # rescore using correct event logs for habituation/familiarization only
  hab_item_by_id <- hab_item_by_id_temp_cleaned%>%
    filter(str_detect(item_id,'Hab'))%>%
    # re-calculate time elapsed
    group_by(item_id)%>%
    arrange(elapsed_time)%>%
    mutate(time_itersum=elapsed_time-lag(elapsed_time, default = first(elapsed_time)))%>%
    mutate(true_time_elapsed=cumsum(time_itersum))%>%
    # remove any duplicate data where event name and elapsed time and look at x are the same
    group_by(item_id,elapsed_time,event_name,look_at_point_x)%>%
    slice(1)%>%
    ungroup()%>%
    group_by(item_id)%>%
    arrange(elapsed_time)
  
  hab_item_by_id<-hab_item_by_id%>%
    ungroup()%>%
    # score based on gaze locations name
    group_by(item_id)%>%
    reframe(center=length(which(gaze_location_name == "center")),
            on_away = length(which(gaze_location_name == "away" & 
                                     gaze_location_on_screen == "true")),
            off_away = length(which(gaze_location_name == "away" & 
                                      gaze_location_on_screen == "false")),
            center_time=sum(subset(time_itersum,gaze_location_name == 'center'),na.rm=T),
            away_time=sum(subset(time_itersum,gaze_location_name=='away'),na.rm=T))%>%
    ungroup()%>%
    mutate(item_key='center',
           item_id=tolower(item_id))
  
  hab_cols<-c(hab_1_1=NA_real_,hab_1_2=NA_real_,hab_1_3=NA_real_,hab_1_4=NA_real_,
               hab_1_5=NA_real_,hab_1_6=NA_real_,hab_1_7=NA_real_,hab_1_8=NA_real_,
               hab_2_1=NA_real_,hab_2_2=NA_real_,hab_2_3=NA_real_,hab_2_4=NA_real_,
               hab_2_5=NA_real_,hab_2_6=NA_real_,hab_2_7=NA_real_,hab_2_8=NA_real_)
  
  clean_data<-hab_item_by_id%>%
    dplyr::select(c('item_id','center_time'))%>%
    pivot_wider(names_from=item_id,values_from=center_time)%>%
    add_column(!!!hab_cols[!names(hab_cols) %in% names(.)])
  
  # pull # items completed
  items_completed=length(clean_data)
  
  # scoring routine starts here - items where they were looking away entirely are set to 0
  # mean look both rounds is 0 if either round 1 or 2 scores are 0
  out<-clean_data%>%
    mutate_all(~replace(., is.na(.), 0))%>%
    rowwise()%>%
    mutate(peak_look_round1=max(hab_1_1,hab_1_2,hab_1_3,hab_1_4,hab_1_5,hab_1_6,hab_1_7,hab_1_8,na.rm=T),
           peak_look_round2=max(hab_2_1,hab_2_2,hab_2_3,hab_2_4,hab_2_5,hab_2_6,hab_2_7,hab_2_8,na.rm=T),
           peak_look_both_rounds=case_when(peak_look_round1!=0&peak_look_round2!=0~max(peak_look_round1,peak_look_round2)),
           sum_look_round1=sum(hab_1_1,hab_1_2,hab_1_3,hab_1_4,hab_1_5,hab_1_6,hab_1_7,hab_1_8,na.rm=T),
           sum_look_round2=sum(hab_2_1,hab_2_2,hab_2_3,hab_2_4,hab_2_5,hab_2_6,hab_2_7,hab_2_8,na.rm=T),
           sum_look_both_rounds=case_when(sum_look_round1!=0&sum_look_round2!=0~sum(sum_look_round1,sum_look_round2)))%>%
    ungroup()%>%
    mutate(mean_look_round1=rowMeans(dplyr::select(.,cs(hab_1_1,hab_1_2,hab_1_3,hab_1_4,hab_1_5,hab_1_6,hab_1_7,hab_1_8)),na.rm=T),
           mean_look_round2=rowMeans(dplyr::select(.,cs(hab_2_1,hab_2_2,hab_2_3,hab_2_4,hab_2_5,hab_2_6,hab_2_7,hab_2_8)),na.rm=T))%>%
    mutate(mean_look_both_rounds=case_when(mean_look_round1==0|mean_look_round2==0~NA,
                                           mean_look_round1!=0&mean_look_round2!=0~rowMeans(dplyr::select(.,cs(mean_look_round1,mean_look_round2)))),
           sum_look_both_rounds=case_when(sum_look_round1==0|sum_look_round2==0~NA,
                                          sum_look_round1!=0&sum_look_round2!=0~rowMeans(dplyr::select(.,cs(sum_look_round1,sum_look_round2)))),
           peak_look_both_rounds=case_when(peak_look_round1==0|peak_look_round2==0~NA,
                                           peak_look_round1!=0&peak_look_round2!=0~rowMeans(dplyr::select(.,cs(peak_look_round1,mean_look_round2)))))%>%
    dplyr::select(contains('round'))
  
  return(out)
}
