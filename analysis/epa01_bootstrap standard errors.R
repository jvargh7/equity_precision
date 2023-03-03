source(".Rprofile")
library(boot)

master_list <- readRDS(paste0(path_ep_folder,"/working/master list of reviews.RDS"))
summary_master <- readRDS(paste0(path_ep_folder,"/working/summary of reviews by strata.RDS"))



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


generate_dataset = function(summary_review,selected_region = "East Asia"){

  if(selected_region == "Rest of World"){
    region_review = summary_review
    
    t = 197570 - 15756
    
    t_f_proxy = sum(region_review[region_review$strata == "Fall 2022",]$n)
    t_s_proxy = sum(region_review[region_review$strata == "Spring 2023",]$n)
    
    t_f = t*t_f_proxy/(t_f_proxy+t_s_proxy)
    t_s = t*t_s_proxy/(t_f_proxy+t_s_proxy)
    
  }
  if(selected_region != "Rest of World"){
    region_review = summary_review %>% dplyr::filter(region == selected_region)
    
    t = sum(region_review$n)
    
    t_f = sum(region_review[region_review$strata == "Fall 2022",]$n)
    t_s = sum(region_review[region_review$strata == "Spring 2023",]$n)
    
  }


  fall_sampled_review = region_review %>% dplyr::filter(sampled==1,
                                                         strata == "Fall 2022")
  spring_sampled_review = region_review %>% dplyr::filter(sampled==1,
                                                         strata == "Spring 2023")

  f_r_1 = sum(fall_sampled_review[fall_sampled_review$review_strata == "Matched",]$n,na.rm=TRUE)
  f_r_2 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R1_Only",]$n,na.rm=TRUE)
  f_r_3 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R2_Only",]$n,na.rm=TRUE)
  f_r_4 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R3_Completed",]$n,na.rm=TRUE)

  s_r_1 = sum(spring_sampled_review[spring_sampled_review$review_strata == "Matched",]$n,na.rm=TRUE)
  s_r_2 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R1_Only",]$n,na.rm=TRUE)
  s_r_3 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R2_Only",]$n,na.rm=TRUE)
  s_r_4 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R3_Completed",]$n,na.rm=TRUE)


  f_c_1 = sum(fall_sampled_review[fall_sampled_review$review_strata == "Matched" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_2 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R1_Only" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_3 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R2_Only" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  f_c_4 = sum(fall_sampled_review[fall_sampled_review$review_strata == "R3_Completed" & fall_sampled_review$outcome == 1,]$n,na.rm=TRUE)

  s_c_1 = sum(spring_sampled_review[spring_sampled_review$review_strata == "Matched" & spring_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_2 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R1_Only" & spring_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_3 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R2_Only" & spring_sampled_review$outcome == 1,]$n,na.rm=TRUE)
  s_c_4 = sum(spring_sampled_review[spring_sampled_review$review_strata == "R3_Completed" & spring_sampled_review$outcome == 1,]$n,na.rm=TRUE)

  
  fall_review_probs = c(t_f - (f_r_1 + f_r_2+ f_r_3 + f_r_4), f_r_1, f_r_2, f_r_3, f_r_4)/t_f
  spring_review_probs = c(t_s - (s_r_1 + s_r_2+ s_r_3 + s_r_4), s_r_1, s_r_2, s_r_3, s_r_4)/t_s

  # fall_review_probs[is.na(fall_review_probs)] <- 0
  # spring_review_probs[is.na(spring_review_probs)] <- 0
  
  r_c = c(0:4)
  # r_c[!is.na(fall_review_probs)]
  
  fall_dataset = spring_dataset = data.frame()
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
    spring_dataset = data.frame(fall = rep(0,times=t_s)) %>% 
      mutate(review_category = sample(r_c[!is.na(spring_review_probs)],size = n(),replace=TRUE,prob = na.omit(spring_review_probs))) %>% 
      mutate(correct = case_when(review_category == 1 ~ rbinom(n(),1,f_c_1/f_r_1),
                                 review_category == 2 ~ rbinom(n(),1,f_c_2/f_r_2),
                                 review_category == 3 ~ rbinom(n(),1,f_c_3/f_r_3),
                                 review_category == 4 ~ rbinom(n(),1,f_c_4/f_r_4),
                                 TRUE ~ rbinom(n(),1,0)
      ))
  }
  
  
  bind_rows(fall_dataset,
            spring_dataset) %>% 
    return(.)



}

gen_row = generate_dataset(summary_master,"Rest of World")
gen_ea = generate_dataset(summary_master,"East Asia")
gen_lac = generate_dataset(summary_master,"Latin America")
gen_mena = generate_dataset(summary_master,"Middle East")
gen_sa = generate_dataset(summary_master,"South Asia")
gen_seap = generate_dataset(summary_master,"South East Asia")


# East Asia ----------



bootstrap_row = boot(gen_row,boot_se_ada,R=1000)
bootstrap_ea = boot(gen_ea,boot_se_ada,R=1000)
bootstrap_lac = boot(gen_lac,boot_se_ada,R=1000)
bootstrap_mena = boot(gen_mena,boot_se_ada,R=1000)
bootstrap_sa = boot(gen_sa,boot_se_ada,R=1000)
bootstrap_seap = boot(gen_seap,boot_se_ada,R=1000)



# Global Estimates -----------
data.frame(
  c_row = bootstrap_row$t[,4],
  c_ea = bootstrap_ea$t[,4],
  c_lac = bootstrap_lac$t[,4],
  c_mena = bootstrap_mena$t[,4],
  c_sa = bootstrap_sa$t[,4],
  c_seap = bootstrap_seap$t[,4]
  
) %>% 
  mutate(total_correct = rowSums(.[,c("c_row","c_ea","c_lac","c_mena","c_sa","c_seap")],na.rm=TRUE),
         total_nonwestern = rowSums(.[,c("c_ea","c_lac","c_mena","c_sa","c_seap")],na.rm=TRUE)) %>% 
  
  mutate(prop_correct = total_correct/197570,
         prop_nonwestern = total_nonwestern/total_correct,
         
         prop_ea = (c_ea)/total_correct,
         prop_lac = (c_lac)/total_correct,
         prop_mena = (c_mena)/total_correct,
         prop_sa = (c_sa)/total_correct,
         prop_seap = (c_seap)/total_correct
         
  ) %>% 
  
  summarize_at(vars(starts_with("prop")),
               list(~quantile(.,probs=c(0.025,0.50,0.975),na.rm = TRUE)))



quantile(bootstrap_coefs$t[,1],probs = c(0.025,0.975))
quantile(bootstrap_coefs$t[,2],probs = c(0.025,0.975))
quantile(bootstrap_coefs$t[,3],probs = c(0.025,0.975))

# bootstrap_coefs2 = master_list %>% 
#   dplyr::filter(region == "East Asia") %>% 
#   
# boot(.,boot_se_ada,R=1000)
# quantile(bootstrap_coefs2$t[,1],probs = c(0.025,0.975))







