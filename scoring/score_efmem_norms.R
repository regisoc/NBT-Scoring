score_efmem_norms<-function(mtl_css=NA,mtt_css=NA,vdr_css=NA,mtl_css_se=NA,mtt_css_se=NA,vdr_css_se=NA,age=NA){
  require(tidyverse)
  
  load('norming/composite_sem.RData')
  load('norming/efmem_wgts.RData')
  load('norming/refs.RData')
  
  if(length(mtl_css)==0){mtl_css=NA}
  if(length(mtt_css)==0){mtt_css=NA}
  if(length(vdr_css)==0){vdr_css=NA}
  
  efmem_wgts<-efmem_wgts%>%
    dplyr::select(-c('scoreType','instrument+score reference'))%>%
    mutate(instrument=case_when(instrument=='Memory Task Learning'~'MTL',
                                instrument=='Memory Task Test'~'MTT',
                                instrument=='Visual Delayed Response (Touch)'~'VDR'))%>%
    pivot_wider(names_from='instrument',values_from=c('meanVector','sdVector','weights'))
  efmem_composite_sem<-composite_sem%>%subset(Composite=='Executive Function/Memory Composite')
  efmem_refs<-refs%>%
    dplyr::select(c('CAMOS','efMem_ref','efMem_sd'))
  
  # scoring routine for efmemuage composite starts here
  if(!is.na(mtl_css)&!is.na(mtt_css)&!is.na(vdr_css)&
     !is.na(mtl_css_se)&!is.na(mtt_css_se)&!is.na(vdr_css_se)){
    efmem_out<-data.frame(age)%>%
      rename(CAMOS=age)%>%
      left_join(efmem_wgts,by=join_by(CAMOS>=minCAMOS,CAMOS<=maxCAMOS))%>%
      rowwise()%>%
      # interim composite
      mutate(interim_composite_css=sum(((mtl_css-`meanVector_MTL`)/`sdVector_MTL`)*`weights_MTL`,
                                       ((mtt_css-`meanVector_MTT`)/`sdVector_MTT`)*`weights_MTT`,
                                       ((vdr_css-`meanVector_VDR`)/`sdVector_VDR`)*`weights_VDR`,
                                       na.rm=T))%>%
      # css composite
      mutate(composite_CSS=interim_composite_css*15.47408+430,
             composite_CSS_SE=efmem_composite_sem$SEM)%>%
      left_join(efmem_refs,by='CAMOS')%>%
      # ss 
      mutate(composite_zscore=((composite_CSS-efMem_ref)/efMem_sd),
             composite_SS=composite_zscore*15+100,
             winsorize=case_when(composite_SS<54~'yes',
                                 composite_SS>146~'yes',
                                 .default='no'),
             composite_SS=case_when(composite_SS<54~54,
                                    composite_SS>146~146,
                                    .default=composite_SS),
             composite_SS_SE=(composite_CSS_SE/efMem_sd)*15,
             composite_SS_SE=case_when(winsorize=='yes'~NA,
                                       .default=composite_SS_SE))%>%
      # confidence bands
      mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
             confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
             confidence_SS_upper=(((confidence_CSS_upper-efMem_ref)/efMem_sd)*15)+100,
             confidence_SS_lower=(((confidence_CSS_lower-efMem_ref)/efMem_sd)*15)+100)%>%
      # national percentile
      mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
      mutate(natl_percentile=case_when(natl_percentile>99~99,
                                       natl_percentile<1~1,
                                       .default=round(natl_percentile,0)))%>%
      dplyr::select(-c('efMem_ref','efMem_sd'))%>%
      # round everything to 6 digits
      mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
      mutate(score='efmem composite')
  }else{
    efmem_out<-NULL
  }
  
  out<-efmem_out%>%
    # dplyr::select_if(~sum(!is.na(.))>0)%>%
    rename_all(~str_remove(.,'composite_'))%>%
    dplyr::select(c('score','CSS','CSS_SE','SS','SS_SE',
                   'confidence_CSS_lower','confidence_CSS_upper',
                   'confidence_SS_lower','confidence_SS_upper',
                   'natl_percentile'))
  
  return(out)
}