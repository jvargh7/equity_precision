


boot_se_ada = function(df,i){
  
  dfb = df[i,]
  
  f_c_1 = sum(dfb[dfb$fall == 1 & dfb$review_category == 1, ]$correct,na.rm=TRUE)
  f_c_2 = sum(dfb[dfb$fall == 1 & dfb$review_category == 2, ]$correct,na.rm=TRUE)
  f_c_3 = sum(dfb[dfb$fall == 1 & dfb$review_category == 3, ]$correct,na.rm=TRUE)
  f_c_4 = sum(dfb[dfb$fall == 1 & dfb$review_category == 4, ]$correct,na.rm=TRUE)
  
  f_r_1 = sum(!is.na(dfb[dfb$fall == 1 & dfb$review_category == 1, ]$correct))
  f_r_2 = sum(!is.na(dfb[dfb$fall == 1 & dfb$review_category == 2, ]$correct))
  f_r_3 = sum(!is.na(dfb[dfb$fall == 1 & dfb$review_category == 3, ]$correct))
  f_r_4 = sum(!is.na(dfb[dfb$fall == 1 & dfb$review_category == 4, ]$correct))
  
  
  s_c_1 = sum(dfb[dfb$fall == 0 & dfb$review_category == 1, ]$correct,na.rm=TRUE)
  s_c_2 = sum(dfb[dfb$fall == 0 & dfb$review_category == 2, ]$correct,na.rm=TRUE)
  s_c_3 = sum(dfb[dfb$fall == 0 & dfb$review_category == 3, ]$correct,na.rm=TRUE)
  s_c_4 = sum(dfb[dfb$fall == 0 & dfb$review_category == 4, ]$correct,na.rm=TRUE)
  
  s_r_1 = sum(!is.na(dfb[dfb$fall == 0 & dfb$review_category == 1, ]$correct))
  s_r_2 = sum(!is.na(dfb[dfb$fall == 0 & dfb$review_category == 2, ]$correct))
  s_r_3 = sum(!is.na(dfb[dfb$fall == 0 & dfb$review_category == 3, ]$correct))
  s_r_4 = sum(!is.na(dfb[dfb$fall == 0 & dfb$review_category == 4, ]$correct))
  
  t_f = sum(dfb$fall)
  t_s = nrow(dfb) - t_f
  
  # Proportion correct in Fall 2022 sample
  p_f = (f_c_1 + f_c_2 + f_c_3 + f_c_4)/(f_r_1 + f_r_2 + f_r_3 + f_r_4)
  
  # Proportion correct in Spring 2023 sample
  p_s = (s_c_1 + s_c_2 + s_c_3 + s_c_4)/(s_r_1 + s_r_2 + s_r_3 + s_r_4)
  
  # Weighted proportion within regional abstracts are primary studies in correct source population for PM in Diabetes 
  
  m = (p_f*t_f + p_s*t_s)/(t_f + t_s)
  
  # Weighted prop, prop in fall, prop in spring, count of total abstracts
  c(m,p_f,p_s,m*(t_f+t_s)) %>% 
    
    return(.)
  
}


generate_dataset = function(summary_review,selected_region = "East Asia",t0 = 201120 - 15756){
  
  if(selected_region == "Rest of World"){
  #   region_review = summary_review
    region_review = summary_review %>% dplyr::filter(region %in% c("USA","Western Europe"))
    
  #   
    t = t0
  #   
    t_f_proxy = sum(region_review[region_review$strata == "Fall 2022",]$n)
    t_s_proxy = sum(region_review[region_review$strata != "Fall 2022",]$n)
  #   
    t_f = t*t_f_proxy/(t_f_proxy+t_s_proxy)
    t_s = t*t_s_proxy/(t_f_proxy+t_s_proxy)
  #   
    
  }
  if(selected_region != "Rest of World"){
    region_review = summary_review %>% dplyr::filter(region == selected_region)
    
    t = sum(region_review$n)
    
    t_f = sum(region_review[region_review$strata == "Fall 2022",]$n)
    t_s = sum(region_review[region_review$strata != "Fall 2022",]$n)
    
  }
  
  
  fall_sampled_review = region_review %>% dplyr::filter(sampled==1,
                                                        strata == "Fall 2022")
  other_sampled_review = region_review %>% dplyr::filter(sampled==1,
                                                          strata != "Fall 2022")
  
  # Sum the reviewed abstracts
  f_r_1 = sum(fall_sampled_review[fall_sampled_review$review_strata == "Matched",]$n,na.rm=TRUE)
  f_r_2 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R1_Only",]$n,na.rm=TRUE)
  f_r_3 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R2_Only",]$n,na.rm=TRUE)
  f_r_4 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R3_Completed",]$n,na.rm=TRUE)
  
  s_r_1 = sum(other_sampled_review[other_sampled_review$review_strata == "Matched",]$n,na.rm=TRUE)
  s_r_2 = sum(other_sampled_review[other_sampled_review$review_strata == "R1_Only",]$n,na.rm=TRUE)
  s_r_3 = sum(other_sampled_review[other_sampled_review$review_strata == "R2_Only",]$n,na.rm=TRUE)
  s_r_4 = sum(other_sampled_review[other_sampled_review$review_strata == "R3_Completed",]$n,na.rm=TRUE)
  
  # Sum the correct abstracts (outcome == 1)
  f_c_1 = sum(fall_sampled_review[fall_sampled_review$review_strata == "Matched" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_2 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R1_Only" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_3 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R2_Only" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_4 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R3_Completed" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  
  s_c_1 = sum(other_sampled_review[other_sampled_review$review_strata == "Matched" & other_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_2 = sum(other_sampled_review[other_sampled_review$review_strata == "R1_Only" & other_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_3 = sum(other_sampled_review[other_sampled_review$review_strata == "R2_Only" & other_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_4 = sum(other_sampled_review[other_sampled_review$review_strata == "R3_Completed" & other_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  
  if(selected_region != "Rest of World"){
  fall_review_probs = c(t_f - (f_r_1 + f_r_2+ f_r_3 + f_r_4), f_r_1, f_r_2, f_r_3, f_r_4)/t_f
  other_review_probs = c(t_s - (s_r_1 + s_r_2+ s_r_3 + s_r_4), s_r_1, s_r_2, s_r_3, s_r_4)/t_s
  }else if(selected_region == "Rest of World"){
    
    fall_review_probs = c(t_f_proxy - (f_r_1 + f_r_2+ f_r_3 + f_r_4), f_r_1, f_r_2, f_r_3, f_r_4)/t_f_proxy
    other_review_probs = c(t_s_proxy - (s_r_1 + s_r_2+ s_r_3 + s_r_4), s_r_1, s_r_2, s_r_3, s_r_4)/t_s_proxy
    
  }
  # fall_review_probs[is.na(fall_review_probs)] <- 0
  # spring_review_probs[is.na(spring_review_probs)] <- 0
  
  r_c = c(0:4)
  # r_c[!is.na(fall_review_probs)]
  
  fall_dataset = other_dataset = data.frame()
  if(t_f > 0){
    fall_dataset = data.frame(fall = rep(1,times=t_f)) %>% 
      mutate(review_category = sample(r_c[!is.na(fall_review_probs)],size = n(),replace=TRUE,prob = na.omit(fall_review_probs))) %>% 
      mutate(correct = case_when(review_category == 1 ~ rbinom(n(),1,f_c_1/f_r_1),
                                 review_category == 2 ~ rbinom(n(),1,f_c_2/f_r_2),
                                 review_category == 3 ~ rbinom(n(),1,f_c_3/f_r_3),
                                 review_category == 4 ~ rbinom(n(),1,f_c_4/f_r_4),
                                 TRUE ~ rbinom(n(),1,0)
      ))
  }
  
  
  if(t_s > 0){
    other_dataset = data.frame(fall = rep(0,times=t_s)) %>% 
      mutate(review_category = sample(r_c[!is.na(other_review_probs)],size = n(),replace=TRUE,prob = na.omit(other_review_probs))) %>% 
      mutate(correct = case_when(review_category == 1 ~ rbinom(n(),1,s_c_1/s_r_1),
                                 review_category == 2 ~ rbinom(n(),1,s_c_2/s_r_2),
                                 review_category == 3 ~ rbinom(n(),1,s_c_3/s_r_3),
                                 review_category == 4 ~ rbinom(n(),1,s_c_4/s_r_4),
                                 TRUE ~ rbinom(n(),1,0)
      ))
  }
  
  
  bind_rows(fall_dataset,
            other_dataset) %>% 
    return(.)
  
  
  
}