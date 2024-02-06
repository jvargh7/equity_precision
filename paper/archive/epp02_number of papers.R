abstracts_df <- readRDS(paste0(path_ep_folder,"/working/abstracts_df.RDS")) %>% 
  dplyr::filter(precision_medicine == "Yes",is_diabetes == "Yes",correct_population == "Yes")

table(abstracts_df$source_population,abstracts_df$primary_study)

# Number of articles correct
177/417
147/398
147*6.21105527638191
178/387


# Number of primary studies
152/177
136/147
136*6.21105527638191
153/178




figA = abstracts_df %>% 
  group_by(source_population,primary_study) %>% 
  tally() %>% 
  mutate(n = case_when(source_population == "East Asians" ~ as.numeric(n)*6.21105527638191,
                       TRUE ~ as.numeric(n)),
         primary_study = case_when(primary_study == "Yes" ~ "Primary Study",
                                   primary_study == "No" ~ "Review Article")) %>% 
  ggplot(data=.,aes(x=primary_study,y=n,fill=source_population,group=source_population,label=round(n,0))) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(aes(y=n+15),position = position_dodge(width=0.9)) +
  xlab("Study Type") +
  ylab("Number of articles") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("darkgreen","darkblue","red"))

figB = abstracts_df %>% 
  group_by(publication_year,source_population) %>% 
  tally() %>% 
  ungroup() %>% 
  dplyr::mutate(n = case_when(source_population == "East Asians" ~ as.numeric(n)*6.21105527638191,
                              TRUE ~ as.numeric(n))) %>% 
  ggplot(data=.,aes(x=publication_year,y=n,col=source_population)) +
  geom_point() +
  
  geom_path() +
  xlab("Year") +
  ylab("Number of articles") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(name="",values=c("darkgreen","darkblue","red"))

library(ggpubr)
ggarrange(figA,figB,
          labels=c("A","B"),
          nrow=1,ncol=2) %>% 
  ggsave(.,filename=paste0(path_ep_folder,"/figures/number of papers.png"),width=10,height=5)
