boot_se1 = function(n,s,c){
  
  dfb = df[i,]
  n1 = sum(dfb$strata == 1)
  s1 = sum(dfb[dfb$strata == 1,]$screened == 1)
  c1 = sum(dfb[dfb$strata == 1 & dfb$screened == 1,]$correct == 1)
  
  m = ((c1/s1)*n1)/(n1)
  
}




boot_se2 = function(df,i){
  
  dfb = df[i,]
  
  n1 = sum(dfb$strata == 1)
  n2 = sum(dfb$strata == 2)
  
  s1 = sum(dfb[dfb$strata == 1,]$screened == 1)
  s2 = sum(dfb[dfb$strata == 2,]$screened == 1)
  
  c1 = sum(dfb[dfb$strata == 1 & dfb$screened == 1,]$correct == 1)
  c2 = sum(dfb[dfb$strata == 2 & dfb$screened == 1,]$correct == 1)
  
  m = ((c1/s1)*n1 + (c2/s2)*n2)/(n1+n2)
  
  # data.frame(n1=n1,
  #            n2=n2,
  #            s1=s1,
  #            s2=s2,
  #            c1=c1,
  #            c2=c2,
  #            m = m) %>% 
  
  m %>% 
    return()
  
}


set.seed(100)



correct_df = bind_rows(
  
  data.frame(screened = rbinom(7377,1,p=126/7377)) %>% 
    dplyr::mutate(correct = case_when(screened == 1 ~ rbinom(n(),1,100/126),
                                      TRUE ~ rbinom(n(),1,0)),
                  strata = 1) ,
  data.frame(screened = rbinom(2472,1,p=398/2472)) %>% 
    mutate(correct = case_when(screened == 1 ~ rbinom(n(),1,300/398),
                               TRUE ~ rbinom(n(),1,0)),
           strata = 2)
  
)

# correct_df %>% group_by(strata,screened,correct) %>% tally() %>% View()

bootstrap_coefs = boot(correct_df,boot_se2,R=1000)
quantile(bootstrap_coefs$t[,1],probs = c(0.025,0.975))