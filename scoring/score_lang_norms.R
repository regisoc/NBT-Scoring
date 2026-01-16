score_lang_norms<-function(mr_css=NA,me_css=NA,mr_css_se=NA,me_css_se=NA,age=NA){
  require(tidyverse)
  
  load('norming/composite_sem.RData')
  load('norming/lang_wgts.RData')
  load('norming/refs.RData')
  
  lang_composite_sem<-composite_sem%>%subset(Composite=='Language')
  lang_wgts<-lang_wgts%>%
    mutate(instrument=case_when(instrument=='Mullen Receptive'~'MR',
                                instrument=='Mullen Expressive - Observational/Prompted'~'ME'))%>%
    pivot_wider(names_from='instrument',values_from=cs(meanVector,sdVector,weights))
  lang_refs<-refs%>%
    dplyr::select(c('CAMOS','lang_ref','lang_sd'))
  mr_refs<-refs%>%
    dplyr::select(c('CAMOS','MullenRec_ref','MullenRec_sd'))
  me_refs<-refs%>%
    dplyr::select(c('CAMOS','MullenExp_ref','MullenExp_sd'))
  
  # scoring routine for language composite starts here
  if(!is.na(mr_css)&!is.na(me_css)&!is.na(mr_css_se)&!is.na(me_css_se)){
    lang_out<-data.frame(age,mr_css,me_css)%>%
    rename(CAMOS=age)%>%
    left_join(lang_wgts,by=join_by(CAMOS>=minCAMOS,CAMOS<=maxCAMOS))%>%
    rowwise()%>%
    # interim composite
    mutate(interim_composite_css=sum(((mr_css-`meanVector_MR`)/`sdVector_MR`)*`weights_MR`,
                                     ((me_css-`meanVector_ME`)/`sdVector_ME`)*`weights_ME`,
                                     na.rm=T),)%>%
    # css composite
    mutate(composite_CSS=interim_composite_css*15.47408+430,
           composite_CSS_SE=lang_composite_sem$SEM)%>%
    dplyr::select(-c(4:(ncol(lang_wgts)+4)))%>%
    left_join(lang_refs,by='CAMOS')%>%
    # ss 
    mutate(composite_zscore=((composite_CSS-lang_ref)/lang_sd),
           composite_SS=composite_zscore*15+100,
           winsorize=case_when(composite_SS<46~'yes',
                               composite_SS>146~'yes',
                               .default='no'),
           composite_SS=case_when(composite_SS<46~46,
                                  composite_SS>146~146,
                                  .default=composite_SS),
           composite_SS_SE=(composite_CSS_SE/lang_sd)*15,
           composite_SS_SE=case_when(winsorize=='yes'~NA,
                                     .default=composite_SS_SE))%>%
    # confidence bands
    mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
           confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
           confidence_SS_upper=(((confidence_CSS_upper-lang_ref)/lang_sd)*15)+100,
           confidence_SS_lower=(((confidence_CSS_lower-lang_ref)/lang_sd)*15)+100)%>%
    # national percentile
    mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
    mutate(natl_percentile=case_when(natl_percentile>99~99,
                                     natl_percentile<1~1,
                                     .default=round(natl_percentile,0)))%>%
    dplyr::select(-cs(lang_ref,lang_sd))%>%
    # round everything to 6 digits
    mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
    mutate(score='language composite')
  }else{
    lang_out<-NULL
  }
  
  # mullen receptive scoring
  if(!is.na(mr_css)&!is.na(mr_css_se)){
    mr_out<-data.frame(age,mr_css,me_css)%>%
    rename(CAMOS=age)%>%
    # css composite
    mutate(composite_CSS=mr_css,
           composite_CSS_SE=mr_css_se)%>%
    left_join(mr_refs,by='CAMOS')%>%
    # ss 
    mutate(composite_zscore=((composite_CSS-MullenRec_ref)/MullenRec_sd),
           composite_SS=composite_zscore*15+100,
           winsorize=case_when(composite_SS<46~'yes',
                               composite_SS>146~'yes',
                               .default='no'),
           composite_SS=case_when(composite_SS<46~46,
                                  composite_SS>146~146,
                                  .default=composite_SS),
           composite_SS_SE=(composite_CSS_SE/MullenRec_sd)*15,
           composite_SS_SE=case_when(winsorize=='yes'~NA,
                                     .default=composite_SS_SE))%>%
    # confidence bands
    mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
           confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
           confidence_SS_upper=(((confidence_CSS_upper-MullenRec_ref)/MullenRec_sd)*15)+100,
           confidence_SS_lower=(((confidence_CSS_lower-MullenRec_ref)/MullenRec_sd)*15)+100)%>%
    # national percentile
    mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
    mutate(natl_percentile=case_when(natl_percentile>99~99,
                                     natl_percentile<1~1,
                                     .default=round(natl_percentile,0)))%>%
    dplyr::select(-cs(MullenRec_ref,MullenRec_sd))%>%
    # round everything to 6 digits
    mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
    mutate(score='mullen receptive')
  }else{
    mr_out<-NULL
  }
  
  # mullen expressive scoring
  if(!is.na(me_css)&!is.na(me_css_se)){
    me_out<-data.frame(age,mr_css,me_css)%>%
    rename(CAMOS=age)%>%
    # css composite
    mutate(composite_CSS=me_css,
           composite_CSS_SE=me_css_se)%>%
    left_join(me_refs,by='CAMOS')%>%
    # ss 
    mutate(composite_zscore=((composite_CSS-MullenExp_ref)/MullenExp_sd),
           composite_SS=composite_zscore*15+100,
           winsorize=case_when(composite_SS<46~'yes',
                               composite_SS>146~'yes',
                               .default='no'),
           composite_SS=case_when(composite_SS<46~46,
                                  composite_SS>146~146,
                                  .default=composite_SS),
           composite_SS_SE=(composite_CSS_SE/MullenExp_sd)*15,
           composite_SS_SE=case_when(winsorize=='yes'~NA,
                                     .default=composite_SS_SE))%>%
    # confidence bands
    mutate(confidence_CSS_lower=composite_CSS-(1.645*composite_CSS_SE),
           confidence_CSS_upper=composite_CSS+(1.645*composite_CSS_SE),
           confidence_SS_upper=(((confidence_CSS_upper-MullenExp_ref)/MullenExp_sd)*15)+100,
           confidence_SS_lower=(((confidence_CSS_lower-MullenExp_ref)/MullenExp_sd)*15)+100)%>%
    # national percentile
    mutate(natl_percentile=pnorm(composite_zscore,mean=0,sd=1)*100)%>%
    mutate(natl_percentile=case_when(natl_percentile>99~99,
                                     natl_percentile<1~1,
                                     .default=round(natl_percentile,0)))%>%
    dplyr::select(-cs(MullenExp_ref,MullenExp_sd))%>%
    # round everything to 6 digits
    mutate(across(where(is.numeric), ~round(.x, digits=6)))%>%
    mutate(score='mullen expressive')
  }else{
    me_out<-NULL
  }
  
  out<-lang_out%>%
    rbind(mr_out)%>%
    rbind(me_out)%>%
    dplyr::select_if(~sum(!is.na(.))>0)%>%
    dplyr::select(contains(c('score','composite','SS','natl')))%>%
    rename_all(~str_remove(.,'composite_'))%>%
    dplyr::select(-'zscore')
  
  return(out)
}