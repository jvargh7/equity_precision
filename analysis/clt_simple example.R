
p = 0.487593

coin_tosses = rbinom(10000,1,p)

sampled_tosses = sample(coin_tosses,80,replace=FALSE)


est_p = mean(sampled_tosses)
est_p

se_est_p = sqrt((est_p*(1-est_p))/length(sampled_tosses))
est_p - 1.96*se_est_p
est_p + 1.96*se_est_p


boot_se2 = function(df,i){
  
  mean(df[i,]) %>% 
    return(.)
  
}