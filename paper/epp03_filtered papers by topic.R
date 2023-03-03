abstracts_df <- readRDS(paste0(path_ep_folder,"/working/abstracts_df.RDS")) %>% 
  dplyr::filter(precision_medicine == "Yes",is_diabetes == "Yes",correct_population == "Yes")


# figA <- abstracts_df %>% 
#   group_by(source_population) %>% 
#   summarize_at(vars(matches("\\smedicine")),~sum(.,na.rm=TRUE)) %>% 
#   pivot_longer(cols=matches("\\smedicine"),names_to = "name",values_to="n") %>% 
#   mutate(n = case_when(source_population == "East Asians" ~ as.numeric(n)*6.21105527638191,
#                        TRUE ~ as.numeric(n))) %>% 
#   mutate(omics = str_to_title(name)) %>%
#   ggplot(data=.,aes(x=name,y=n,fill=source_population,group=source_population,label=round(n,0))) +
#   geom_col(position = position_dodge(width=0.9)) +
#   geom_text(aes(y=n+15),position = position_dodge(width=0.9)) +
#   xlab("") +
#   ylab("Number of articles") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   scale_fill_manual(name="",values=c("darkgreen","darkblue","red"))

figB <- abstracts_df %>%
  group_by(source_population) %>%
  summarize_at(vars(ends_with("om")),~sum(.,na.rm=TRUE)) %>%
  pivot_longer(cols=ends_with("om"),names_to = "omics",values_to="n") %>%
  mutate(n = case_when(source_population == "East Asians" ~ as.numeric(n)*6.21105527638191,
                       TRUE ~ as.numeric(n))) %>%
  mutate(omics = str_to_title(omics) %>% paste0(.,"ics")) %>%
  dplyr::filter(omics != "Exposomics") %>%
  ggplot(data=.,aes(x=omics,y=n,fill=source_population,group=source_population,label=round(n,0))) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(aes(y=n+5),position = position_dodge(width=0.9)) +
  xlab("") +
  ylab("Number of articles") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("darkgreen","darkblue","red"))





library(ggpubr)
figB %>% 
  ggsave(.,filename=paste0(path_ep_folder,"/figures/number by topic.png"),width=10,height=5)
