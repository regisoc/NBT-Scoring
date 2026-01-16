score_vdr<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/vdr_ipar.RData')
  
  # clean up ipar for scoring
  vdr_ipar<-vdr_ipar[rowSums(is.na(vdr_ipar)) != ncol(vdr_ipar), ]%>%
    filter(row_number()!=17)%>%
    rename(ItemID=Item)%>%
    mutate(ItemID=str_remove(ItemID,'_Score'))%>%
    mutate_at(c(2:4),as.numeric)%>%
    rename(a1 = Slope,
           d = Intercept,
           g = PseudoGuess)
  
  # get list of items
  vdr_items<-vdr_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  vdr_cols<-c(VDRTouch_4_9s=NA_real_, VDRTouch_5_9s=NA_real_, VDRTouch_6_9s=NA_real_, 
              VDRTouch_7_9s=NA_real_, VDRTouch_4_11s=NA_real_, VDRTouch_5_11s=NA_real_, 
              VDRTouch_6_11s=NA_real_, VDRTouch_7_11s=NA_real_, VDRTouch_8_11s=NA_real_, 
              VDRTouch_9_11s=NA_real_, VDRTouch_10_11s=NA_real_, VDRTouch_11_11s=NA_real_, 
              VDRTouch_8_13s=NA_real_, VDRTouch_9_13s=NA_real_, VDRTouch_10_13s=NA_real_, 
              VDRTouch_11_13s=NA_real_)
  
  clean_data=data%>%
    add_column(!!!vdr_cols[!names(vdr_cols) %in% names(.)])%>%
    mutate(across(contains(c('VDR')),as.numeric))%>%
    rename_all(~str_remove(.,'_Score'))%>%
    dplyr::select(any_of(vdr_items))
  
  # check how many items completed
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # identify vdr group
  group=NA
  if(!is.na(clean_data$VDRTouch_4_9s)||!is.na(clean_data$VDRTouch_5_9s)||
     !is.na(clean_data$VDRTouch_6_9s)||!is.na(clean_data$VDRTouch_7_9s)||
     !is.na(clean_data$VDRTouch_8_11s)||!is.na(clean_data$VDRTouch_9_11s)||
     !is.na(clean_data$VDRTouch_10_11s)||!is.na(clean_data$VDRTouch_11_11s)){
    group=1
  }else if(!is.na(clean_data$VDRTouch_4_11s)||!is.na(clean_data$VDRTouch_5_11s)||
           !is.na(clean_data$VDRTouch_6_11s)||!is.na(clean_data$VDRTouch_7_11s)||
           !is.na(clean_data$VDRTouch_8_13s)||!is.na(clean_data$VDRTouch_9_13s)||
           !is.na(clean_data$VDRTouch_10_13s)||!is.na(clean_data$VDRTouch_11_13s)){
    group=2
  }
  
  # remove nas - mbtfx doesn't tolerate na
  clean_data=clean_data%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=vdr_ipar%>%
                                 subset(ItemID %in% names(clean_data))%>%
                                 dplyr::select(-ItemID),
                        u=clean_data,
                        minTheta = -8, maxTheta = 8)
    out=calc_theta%>%
      as_tibble()%>%
      dplyr::select(-iter)
    
    colnames(out)=c('theta','SE')
    
    out=out%>%
      mutate(theta=as.character(theta),
             SE=as.character(SE))%>%
      # set values for extreme cases when responses are all correct or incorrect 
      mutate(SE=case_when(theta=='Inf'&group==1~'1.38',
                          theta=='-Inf'&group==1~'1.17',
                          theta=='Inf'&group==2~'1.32',
                          theta=='-Inf'&group==2~'1.23',
                          .default=SE),
             theta=case_when(theta=='Inf'&group==1~'3.04',
                             theta=='-Inf'&group==1~'-3.68',
                             theta=='Inf'&group==2~'3.2',
                             theta=='-Inf'&group==2~'-3.5',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(theta=NA_real_,SE=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
