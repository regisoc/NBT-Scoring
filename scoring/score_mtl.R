score_mtl<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/mtl_ipar.RData')
  
  # clean up ipar for scoring
  mtl_ipar<-mtl_ipar[rowSums(is.na(mtl_ipar)) != ncol(mtl_ipar), ]%>%
    rename(ItemID=Item)%>%
    mutate(ItemID=str_remove(ItemID,'_score'))%>%
    mutate_at(c(2:4),as.numeric)%>%
    rename(a1 = Slope,
           d = Intercept,
           g = PseudoGuess)

  # get list of items
  mtl_items<-mtl_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  mtl_cols<-c(encoding1=NA_real_, encoding2=NA_real_, 
              encoding3=NA_real_, encoding4=NA_real_, 
              encoding5=NA_real_, encoding6=NA_real_, 
              encoding7=NA_real_, encoding8=NA_real_, 
              encoding9=NA_real_, encoding10=NA_real_, 
              encoding11=NA_real_)
  
  clean_data=data%>%
    clean_names()%>%
    rename_all(~str_remove(.,'_score'))%>%
    add_column(!!!mtl_cols[!names(mtl_cols) %in% names(.)])%>%
    mutate(across(contains(c('encoding')),as.numeric))%>%
    dplyr::select(any_of(mtl_items))%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # check how many items completed
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=mtl_ipar%>%
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
      mutate(SE=case_when(theta=='Inf'~'0.857',
                          theta=='-Inf'~'1.52',
                          .default=SE),
             theta=case_when(theta=='Inf'~'4.62',
                             theta=='-Inf'~'-2.64',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(theta=NA_real_,SE=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
