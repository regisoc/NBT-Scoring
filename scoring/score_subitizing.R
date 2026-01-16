score_subitizing<-function(data){
  
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/subitizing_ipar.RData')
  
  # clean up ipar for scoring
  subitizing_ipar<-subitizing_ipar[rowSums(is.na(subitizing_ipar)) != ncol(subitizing_ipar), ]%>%
    rename(ItemID=Item)%>%
    mutate(ItemID=str_remove(ItemID,'_Score'))%>%
    mutate_at(c(2:4),as.numeric)%>%
    rename(a1 = Slope,
           d = Intercept,
           g = PseudoGuess)
  
  # get list of items
  subitizing_items<-subitizing_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  subitizing_cols<-c(NRS_T_1InR=NA_real_, NRS_T_2InR=NA_real_, NRS_T_3TriR=NA_real_, NRS_T_4InR=NA_real_)
  
  clean_data=data%>%
    add_column(!!!subitizing_cols[!names(subitizing_cols) %in% names(.)])%>%
    mutate(across(contains(c('NRS')),as.numeric))%>%
    rename_all(~str_remove(.,'_Score'))%>%
    dplyr::select(any_of(subitizing_items))%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # check how many items from verbal counting
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=subitizing_ipar%>%
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
      mutate(SE=case_when(theta=='Inf'~'1.202975',
                          theta=='-Inf'~'1.653384',
                          .default=SE),
             theta=case_when(theta=='Inf'~'3.814678',
                             theta=='-Inf'~'-2.324564',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
