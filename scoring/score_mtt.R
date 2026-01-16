score_mtt<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  require(MTBfx)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/mtt_ipar.RData')
  
  # clean up ipar for scoring
  mtt_ipar<-mtt_ipar[rowSums(is.na(mtt_ipar)) != ncol(mtt_ipar), ]%>%
    rename(ItemID=`Item ID`)%>%
    mutate(ItemID=str_remove(ItemID,'_Score'))%>%
    mutate_at(c(2:4),as.numeric)%>%
    rename(a1 = Slope,
           d = Intercept,
           g = PseudoGuess)
  
  # get list of items
  mtt_items<-mtt_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  mtt_cols<-c(dyad1=NA_real_, dyad2=NA_real_, 
              dyad3=NA_real_, dyad4=NA_real_, 
              dyad5=NA_real_, dyad6=NA_real_, 
              dyad7=NA_real_, dyad8=NA_real_, 
              dyad9=NA_real_, dyad10=NA_real_,
              mem_test_1_1=NA_real_, mem_test_1_2=NA_real_,
              mem_test_1_3=NA_real_, mem_test_1_4=NA_real_,
              mem_test_1_5=NA_real_, mem_test_1_6=NA_real_,
              mem_test_1_7=NA_real_, mem_test_1_8=NA_real_,
              mem_test_1_9=NA_real_, mem_test_1_10=NA_real_,
              mem_test_1_11=NA_real_, mem_test_1_12=NA_real_,
              mem_test_1_13=NA_real_, mem_test_1_14=NA_real_,
              mem_test_1_15=NA_real_, mem_test_1_16=NA_real_,
              mem_test_1_17=NA_real_, mem_test_1_18=NA_real_,
              mem_test_1_19=NA_real_, mem_test_1_20=NA_real_)
  
  clean_data=data%>%
    clean_names()%>%
    rename_all(~str_remove(.,'_score'))%>%
    add_column(!!!mtt_cols[!names(mtt_cols) %in% names(.)])%>%
    mutate(dyad1=case_when(mem_test_1_1==1&mem_test_1_11==1~1,
                           mem_test_1_1==1&mem_test_1_11==0~0,
                           mem_test_1_1==0&mem_test_1_11==1~0,
                           mem_test_1_1==0&mem_test_1_11==0~0),
           dyad2=case_when(mem_test_1_2==1&mem_test_1_13==1~1,
                           mem_test_1_2==1&mem_test_1_13==0~0,
                           mem_test_1_2==0&mem_test_1_13==1~0,
                           mem_test_1_2==0&mem_test_1_13==0~0),
           dyad3=case_when(mem_test_1_3==1&mem_test_1_14==1~1,
                           mem_test_1_3==1&mem_test_1_14==0~0,
                           mem_test_1_3==0&mem_test_1_14==1~0,
                           mem_test_1_3==0&mem_test_1_14==0~0),
           dyad4=case_when(mem_test_1_4==1&mem_test_1_12==1~1,
                           mem_test_1_4==1&mem_test_1_12==0~0,
                           mem_test_1_4==0&mem_test_1_12==1~0,
                           mem_test_1_4==0&mem_test_1_12==0~0),
           dyad5=case_when(mem_test_1_5==1&mem_test_1_15==1~1,
                           mem_test_1_5==1&mem_test_1_15==0~0,
                           mem_test_1_5==0&mem_test_1_15==1~0,
                           mem_test_1_5==0&mem_test_1_15==0~0),
           dyad6=case_when(mem_test_1_6==1&mem_test_1_16==1~1,
                           mem_test_1_6==1&mem_test_1_16==0~0,
                           mem_test_1_6==0&mem_test_1_16==1~0,
                           mem_test_1_6==0&mem_test_1_16==0~0),
           dyad7=case_when(mem_test_1_7==1&mem_test_1_17==1~1,
                           mem_test_1_7==1&mem_test_1_17==0~0,
                           mem_test_1_7==0&mem_test_1_17==1~0,
                           mem_test_1_7==0&mem_test_1_17==0~0),
           dyad8=case_when(mem_test_1_8==1&mem_test_1_18==1~1,
                           mem_test_1_8==1&mem_test_1_18==0~0,
                           mem_test_1_8==0&mem_test_1_18==1~0,
                           mem_test_1_8==0&mem_test_1_18==0~0),
           dyad9=case_when(mem_test_1_9==1&mem_test_1_20==1~1,
                           mem_test_1_9==1&mem_test_1_20==0~0,
                           mem_test_1_9==0&mem_test_1_20==1~0,
                           mem_test_1_9==0&mem_test_1_20==0~0),
           dyad10=case_when(mem_test_1_10==1&mem_test_1_19==1~1,
                            mem_test_1_10==1&mem_test_1_19==0~0,
                            mem_test_1_10==0&mem_test_1_19==1~0,
                            mem_test_1_10==0&mem_test_1_19==0~0))%>%
    mutate(across(contains(c('dyad')),as.numeric))%>%
    dplyr::select(any_of(mtt_items))%>%
    dplyr::select_if(~ !any(is.na(.)))
  
  # check how many items completed
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # at least one item needs to be completed
  if(items_completed>=1){
    calc_theta<-MTBfx::MLE_xPL(ipar=mtt_ipar%>%
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
      mutate(SE=case_when(theta=='Inf'~'1.135352',
                          theta=='-Inf'~'1.356845',
                          .default=SE),
             theta=case_when(theta=='Inf'~'3.709178',
                             theta=='-Inf'~'-3.003868',
                             .default=theta))%>%
      mutate(theta=as.numeric(theta),
             SE=as.numeric(SE))
    
  }else{
    out=tibble(theta=NA_real_,SE=NA_real_)
    print('not enough items towards scoring for memory task test')
  }
    
  return(out)
}
