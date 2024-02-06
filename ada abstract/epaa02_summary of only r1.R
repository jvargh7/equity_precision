


source(".Rprofile")
library(boot)

source("analysis/epa_bootstrapping functions.R")

master_list <- readRDS(paste0(path_ep_folder,"/writing global equity/ada abstract/ada master list of reviews.RDS")) %>% 
  rename(region = gbd_region)
summary_master <- readRDS(paste0(path_ep_folder,"/writing global equity/ada abstract/ada summary of reviews by strata.RDS")) %>% 
  rename(region = gbd_region)


gen_row = generate_dataset(summary_master,"Rest of World")
gen_ea = generate_dataset(summary_master,"East Asia")
gen_lac = generate_dataset(summary_master,"Latin America and Caribbean")
gen_mena = generate_dataset(summary_master,"Middle East and North Africa")
gen_sa = generate_dataset(summary_master,"South Asia")
gen_seap = generate_dataset(summary_master,"South East Asia and Pacific Islands")
gen_ca = generate_dataset(summary_master,"Central Asia")
gen_ssa = generate_dataset(summary_master,"Sub Saharan Africa")
gen_cee = generate_dataset(summary_master,"Central and Eastern Europe")


# Bootstrapped estimates ----------

bootstrap_row = boot(gen_row,boot_se_ada,R=1000)
bootstrap_ea = boot(gen_ea,boot_se_ada,R=1000)
bootstrap_lac = boot(gen_lac,boot_se_ada,R=1000)
bootstrap_mena = boot(gen_mena,boot_se_ada,R=1000)
bootstrap_sa = boot(gen_sa,boot_se_ada,R=1000)
bootstrap_seap = boot(gen_seap,boot_se_ada,R=1000)
bootstrap_ca = boot(gen_ca,boot_se_ada,R=1000)
bootstrap_ssa = boot(gen_ssa,boot_se_ada,R=1000)
bootstrap_cee = boot(gen_cee,boot_se_ada,R=1000)



# Global Estimates -----------
data.frame(
  c_row = bootstrap_row$t[,4],
  c_ea = bootstrap_ea$t[,4],
  c_lac = bootstrap_lac$t[,4],
  c_mena = bootstrap_mena$t[,4],
  c_sa = bootstrap_sa$t[,4],
  c_seap = bootstrap_seap$t[,4],
  c_ca = bootstrap_ca$t[,4],
  c_ssa = bootstrap_ssa$t[,4],
  c_cee = bootstrap_cee$t[,4]
  
) %>% 
  mutate(total_correct = rowSums(.[,c("c_row","c_ea","c_lac","c_mena","c_sa","c_seap","c_ca","c_ssa","c_cee")],na.rm=TRUE),
         total_nonwestern = rowSums(.[,c("c_ea","c_lac","c_mena","c_sa","c_seap","c_ca","c_ssa","c_cee")],na.rm=TRUE)) %>% 
  
  mutate(prop_correct = total_correct/201120,
         prop_nonwestern = total_nonwestern/total_correct,
         
         prop_ea = (c_ea)/total_correct,
         prop_lac = (c_lac)/total_correct,
         prop_mena = (c_mena)/total_correct,
         prop_sa = (c_sa)/total_correct,
         prop_seap = (c_seap)/total_correct,
         prop_ca = (c_ca)/total_correct,
         prop_ssa = (c_ssa)/total_correct,
         prop_cee = (c_cee)/total_correct
         
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








