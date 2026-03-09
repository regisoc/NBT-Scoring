score_cog_norms<-function(mr_css=NA,me_css=NA,mr_css_se=NA,me_css_se=NA,
                           mtl_css=NA,mtt_css=NA,vdr_css=NA,mtl_css_se=NA,mtt_css_se=NA,vdr_css_se=NA,
                           whm_css=NA,sub_css=NA,counting_css=NA,whm_css_se=NA,sub_css_se=NA,counting_css_se=NA,
                           age=NA){
  require(tidyverse)
  
  load('norming/composite_sem.RData')
  load('norming/cog_wgts.RData')
  load('norming/refs.RData')
  
  cog_wgts<-cog_wgts%>%
    mutate(instrument=case_when(instrument=='Memory Task Learning'~'MTL',
                                instrument=='Memory Task Test'~'MTT',
                                instrument=='Visual Delayed Response (Touch)'~'VDR',
                                instrument=='Mullen Receptive'~'MR',
                                instrument=='Mullen Expressive - Observational/Prompted'~'ME',
                                instrument=='Subitizing'~'SUB',
                                instrument=='Who Has More'~'WHM',
                                .default=instrument))%>%
    pivot_wider(names_from='instrument',values_from=c('meanVector','sdVector','weights'))
  cog_composite_sem<-composite_sem%>%subset(Composite=='Cognition Composite')
  cog_refs<-refs%>%
    dplyr::select(c('CAMOS','cognition_ref','cognition_sd'))
  
  # scoring routine for coguage composite starts here
  if(!is.na(mr_css)&&!is.na(me_css)&&!is.na(mr_css_se)&&!is.na(me_css_se)&&
     !is.na(mtl_css)&&!is.na(mtt_css)&&!is.na(vdr_css)&&
     !is.na(mtl_css_se)&&!is.na(mtt_css_se)&&!is.na(vdr_css_se)&&
     !is.na(whm_css)&&!is.na(sub_css)&&!is.na(counting_css)&
     !is.na(whm_css_se)&&!is.na(sub_css_se)&&!is.na(counting_css_se)){
    cog_out<-data.frame(age)%>%
      rename(CAMOS=age)%>%
      left_join(cog_wgts,by=join_by(CAMOS>=minCAMOS,CAMOS<=maxCAMOS))%>%
      rowwise()%>%
      # interim composite
      mutate(interim_composite_css=sum(((mr_css-`meanVector_MR`)/`sdVector_MR`)*`weights_MR`,
                                       ((me_css-`meanVector_ME`)/`sdVector_ME`)*`weights_ME`,
                                       ((mtl_css-`meanVector_MTL`)/`sdVector_MTL`)*`weights_MTL`,
                                       ((mtt_css-`meanVector_MTT`)/`sdVector_MTT`)*`weights_MTT`,
                                       ((vdr_css-`meanVector_VDR`)/`sdVector_VDR`)*`weights_VDR`,
                                       ((whm_css-`meanVector_WHM`)/`sdVector_WHM`)*`weights_WHM`,
                                       ((sub_css-`meanVector_SUB`)/`sdVector_SUB`)*`weights_SUB`,
                                       ((counting_css-`meanVector_Counting`)/`sdVector_Counting`)*`weights_Counting`,
                                       na.rm=T))%>%
      # css composite
      mutate(composite_CSS=interim_composite_css*15.47408+430,
             composite_CSS_SE=cog_composite_sem$SEM)%>%
      left_join(cog_refs,by='CAMOS')%>%
      # ss 
      mutate(composite_zscore=((composite_CSS-cognition_ref)/cognition_sd),
             composite_SS=composite_zscore*15+100,
             winsorize=case_when(composite_SS<54~'yes',
                                 composite_SS>146~'yes',
                                 .default='no'),
             composite_SS=case_when(composite_SS<54~54,
                                    composite_SS>146~146,
                                    .default=composite_SS),
             composite_SS_SE=(composite_CSS_SE/cognition_sd)*15,
             composite_SS_SE=case_when(winsorize=='yes'~NA,
                                       .default=composite_SS_SE))%>%
      # confidence bands
      mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
             confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
             confidence_SS_upper=(((confidence_CSS_upper-cognition_ref)/cognition_sd)*15)+100,
             confidence_SS_lower=(((confidence_CSS_lower-cognition_ref)/cognition_sd)*15)+100)%>%
      # national percentile
      mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
      mutate(natl_percentile=case_when(natl_percentile>99~99,
                                       natl_percentile<1~1,
                                       .default=round(natl_percentile,0)))%>%
      dplyr::select(-c('cognition_ref','cognition_sd'))%>%
      # round everything to 6 digits
      mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
      mutate(score='cognition composite')
  }else{
    cog_out<-NULL
  }
  
  out<-cog_out%>%
    rename_all(~str_remove(.,'composite_'))%>%
    dplyr::select(c('score','CSS','CSS_SE','SS','SS_SE',
                   'confidence_CSS_lower','confidence_CSS_upper',
                   'confidence_SS_lower','confidence_SS_upper',
                   'natl_percentile'))
  
  return(out)
}