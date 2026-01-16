score_va<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/va_ipar.RData')
  
  # clean up ipar for scoring
  va_ipar<-va_ipar[rowSums(is.na(va_ipar)) != ncol(va_ipar), ]%>%
    mutate(ItemID=str_remove(ItemID,'_Score'))%>%
    mutate_at(c(2:4),as.numeric)#%>%
    # rename(a1 = Slope,
    #        d = Intercept,
    #        g = PseudoGuess)
  
  # get list of items
  va_items<-va_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  va_cols<-c(REMA_3=NA_real_, REMA_4=NA_real_, REMA_5=NA_real_, REMA_7=NA_real_,REMA_8=NA_real_)
  
  clean_data=data%>%
    add_column(!!!va_cols[!names(va_cols) %in% names(.)])%>%
    mutate(across(contains(c('REMA')),as.numeric))%>%
    rename_all(~str_remove(.,'_Score'))%>%
    dplyr::select(any_of(va_items))%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # check how many items from verbal counting
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=va_ipar%>%
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
      mutate(SE=case_when(theta=='Inf'~'1.206637',
                          theta=='-Inf'~'1.567506',
                          .default=SE),
             theta=case_when(theta=='Inf'~'3.704593',
                             theta=='-Inf'~'-2.54722',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(theta=NA_real_,SE=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
