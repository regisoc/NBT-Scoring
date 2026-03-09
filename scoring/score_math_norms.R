score_math_norms<-function(whm_css=NA,sub_css=NA,counting_css=NA,whm_css_se=NA,sub_css_se=NA,counting_css_se=NA,age=NA){
  require(tidyverse)
  
  load('norming/composite_sem.RData')
  load('norming/math_wgts.RData')
  load('norming/refs.RData')
  
  if(length(whm_css)==0){whm_css=NA}
  if(length(sub_css)==0){sub_css=NA}
  if(length(counting_css)==0){counting_css=NA}
  
  math_wgts<-math_wgts%>%
    mutate(instrument=case_when(instrument=='Who Has More'~'WHM',
                                instrument=='Subitizing'~'SUB',
                                instrument=='Counting'~'Counting'))%>%
    pivot_wider(names_from='instrument',values_from=c('meanVector','sdVector','weights'))
  math_composite_sem<-composite_sem%>%subset(Composite=='Math Composite')
  math_refs<-refs%>%
    dplyr::select(c('CAMOS','math_ref','math_sd'))
  
  # scoring routine for mathuage composite starts here
  if(!is.na(whm_css)&!is.na(sub_css)&!is.na(counting_css)&
     !is.na(whm_css_se)&!is.na(sub_css_se)&!is.na(counting_css_se)){
    math_out<-data.frame(age)%>%
      rename(CAMOS=age)%>%
      left_join(math_wgts,by=join_by(CAMOS>=minCAMOS,CAMOS<=maxCAMOS))%>%
      rowwise()%>%
      # interim composite
      mutate(interim_composite_css=sum(((whm_css-`meanVector_WHM`)/`sdVector_WHM`)*`weights_WHM`,
                                       ((sub_css-`meanVector_SUB`)/`sdVector_SUB`)*`weights_SUB`,
                                       ((counting_css-`meanVector_Counting`)/`sdVector_Counting`)*`weights_Counting`,
                                       na.rm=T))%>%
      # css composite
      mutate(composite_CSS=interim_composite_css*15.47408+430,
             composite_CSS_SE=math_composite_sem$SEM)%>%
      left_join(math_refs,by='CAMOS')%>%
      # ss 
      mutate(composite_zscore=((composite_CSS-math_ref)/math_sd),
             composite_SS=composite_zscore*15+100,
             winsorize=case_when(composite_SS<54~'yes',
                                 composite_SS>146~'yes',
                                 .default='no'),
             composite_SS=case_when(composite_SS<54~54,
                                    composite_SS>146~146,
                                    .default=composite_SS),
             composite_SS_SE=(composite_CSS_SE/math_sd)*15,
             composite_SS_SE=case_when(winsorize=='yes'~NA,
                                       .default=composite_SS_SE))%>%
      # confidence bands
      mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
             confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
             confidence_SS_upper=(((confidence_CSS_upper-math_ref)/math_sd)*15)+100,
             confidence_SS_lower=(((confidence_CSS_lower-math_ref)/math_sd)*15)+100)%>%
      # national percentile
      mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
      mutate(natl_percentile=case_when(natl_percentile>99~99,
                                       natl_percentile<1~1,
                                       .default=round(natl_percentile,0)))%>%
      dplyr::select(-c('math_ref','math_sd'))%>%
      # round everything to 6 digits
      mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
      mutate(score='math composite')
  }else{
    math_out<-NULL
  }
  
  out<-math_out%>%
    # dplyr::select_if(~sum(!is.na(.))>0)%>%
    rename_all(~str_remove(.,'composite_'))%>%
    dplyr::select(c('score','CSS','CSS_SE','SS','SS_SE',
                   'confidence_CSS_lower','confidence_CSS_upper',
                   'confidence_SS_lower','confidence_SS_upper',
                   'natl_percentile'))
  
  return(out)
}