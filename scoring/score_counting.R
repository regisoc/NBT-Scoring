score_counting<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/counting_ipar.RData')
  
  # clean up ipar for scoring
  counting_ipar<-counting_ipar[rowSums(is.na(counting_ipar)) != ncol(counting_ipar), ]%>%
    mutate(ItemID=tolower(ItemID))
  
  # get list of items
  counting_items<-counting_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  counting_cols<-c(vc_2=NA_real_, oc_1r=NA_real_, oc_2r=NA_real_, oc_3r=NA_real_, oc_4r=NA_real_, oc_5r=NA_real_, oc_6r=NA_real_)
  
  clean_data=data%>%
    clean_names()%>%
    add_column(!!!counting_cols[!names(counting_cols) %in% names(.)])%>%
    mutate(across(contains(c('vc','oc')),as.numeric))%>%
    rowwise()%>%
    mutate(vc_2=case_when(vc_2==0~0,
                          vc_2==1|vc_2==2~1,
                          vc_2>=3&vc_2<=5~2,
                          vc_2>5~3))%>%
    rename_all(~str_remove(.,'_score'))%>%
    dplyr::select(any_of(counting_items))
  
  # check how many items from verbal counting
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  vc_completed=clean_data%>%dplyr::select(c('oc_1r','oc_2r','oc_3r','oc_4r','oc_5r','oc_6r'))%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # check how many items from object counting
  oc_completed=clean_data%>%dplyr::select(c('vc_2'))%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # refined for our revised MAP calculator
  # at least one vc and oc item needed for score
  if(oc_completed>=1&&vc_completed>=1){
    out<-myMAP(dat=clean_data,par=counting_ipar)
  }else{
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
