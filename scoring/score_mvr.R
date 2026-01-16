score_mvr<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/mvr_ipar.RData')
  
  # clean up ipar for scoring
  mvr_ipar<-mvr_ipar%>%
    mutate(across(everything(), ~replace(., . ==  '' , NA)))%>%
    mutate(ItemID=tolower(ItemID))
  mvr_ipar<-mvr_ipar[rowSums(is.na(mvr_ipar)) != ncol(mvr_ipar), ]
  
  # clean input data for scoring + imputation
  mvr_cols<-c(mvr3=NA_real_, mvr5=NA_real_, mvr6=NA_real_, mvr7=NA_real_, mvr9=NA_real_, mvr10=NA_real_, 
              mvr11=NA_real_, mvr12=NA_real_, mvr13=NA_real_, mvr14=NA_real_, rl27_sum=NA_real_, 
              mvr23=NA_real_, mvr24=NA_real_, mvr27_sum=NA_real_, mvr28=NA_real_,mvr28_graded=NA_real_)
  
  clean_data=data%>%
    clean_names()%>%
    dplyr::select(any_of(names(mvr_cols)))%>%
    mutate(across(everything(),as.numeric))%>%
    add_column(!!!mvr_cols[!names(mvr_cols) %in% names(.)])%>%
    mutate(mvr28_graded=case_when(mvr28==0|mvr28==1|mvr28==2|mvr28==3~0,
                                  mvr28==4|mvr28==5~1,
                                  mvr28==6~2,
                                  .default=mvr28_graded))%>%
    dplyr::select(-mvr28)
  
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # scoring routine - no score if no items completed
  if(items_completed==0){
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
  }else{
    out<-myMAP(dat=clean_data,par=mvr_ipar)
  }

  return(out)
}
