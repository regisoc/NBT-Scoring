score_whm<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/whm_ipar.RData')
  
  # clean up ipar for scoring
  whm_ipar<-whm_ipar[rowSums(is.na(whm_ipar)) != ncol(whm_ipar), ]%>%
    rename(ItemID=`Item ID`,
           PseudoGuess=Pseudoguess)%>%
    mutate(ItemID=str_remove(ItemID,'_Score'))%>%
    mutate_at(c(2:4),as.numeric)%>%
    rename(a1 = Slope,
           d = Intercept,
           g = PseudoGuess)
  
  # get list of items
  whm_items<-whm_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  whm_cols<-c(WHM1=NA_real_, WHM2=NA_real_, WHM3=NA_real_, WHM4=NA_real_, WHM5=NA_real_, 
              WHM6=NA_real_, WHM7=NA_real_, WHM8=NA_real_, WHM9=NA_real_, WHM10=NA_real_, 
              WHM11=NA_real_, WHM12=NA_real_, WHM13=NA_real_, WHM14=NA_real_, WHM15=NA_real_, 
              WHM16=NA_real_, WHM17=NA_real_, WHM18=NA_real_, WHM19=NA_real_, WHM20=NA_real_, 
              WHM21=NA_real_, WHM22=NA_real_)
  
  clean_data=data%>%
    add_column(!!!whm_cols[!names(whm_cols) %in% names(.)])%>%
    mutate(across(contains(c('WHM')),as.numeric))%>%
    rename_all(~str_remove(.,'_Score'))%>%
    dplyr::select(any_of(whm_items))%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # check how many items from verbal counting
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=whm_ipar%>%
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
      mutate(SE=case_when(theta=='Inf'~'1.194189',
                          theta=='-Inf'~'0.8665015',
                          .default=SE),
             theta=case_when(theta=='Inf'~'3.644495',
                             theta=='-Inf'~'-4.575415',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(theta=NA_real_,SE=NA_real_)
    print('not enough items towards scoring for counting')
  }
    
  return(out)
}
