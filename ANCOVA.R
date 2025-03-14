#ANCOVA for javadi
# for article 1403-8.R

ANCOVA = function(Time1, Time2, y.lab = NULL){
  Data$Time1 = c(Data[[Time1]])
  Data$Time2 = c(Data[[Time2]])
  
library(emmeans)
 pwc <- Data %>% 
  emmeans_test(
    Time2 ~ Group2,  covariate = Time1,
    p.adjust.method = "none"
  )
GS = GetSummary(Data, group = "Group2",
           dep.quantitative = c("Time1","Time2"))$MeanSD
res.aov <- Data %>% anova_test(Time2 ~ Time1 + Group2 + pro + fat + cho+milk+veg+veg+frut+cereal+meat+oil)
GS$`F (df1,df2)`   = NA
GS$`P-value`       = NA
GS$`F (df1,df2)`[1]   = paste0(res.aov$F[res.aov$Effect=="Group2"]," (",
                            res.aov$DFn[res.aov$Effect=="Group2"], ", ",
                            res.aov$DFd[res.aov$Effect=="Group2"],")")


GS$`P-value`[1]   = res.aov$p[res.aov$Effect=="Group2"] 


GS%>% wd.Table(y.lab)

 
res.aov$p = round(res.aov$p,3)
as.data.frame(res.aov) %>%       wd.Table(y.lab)
get_emmeans(pwc) %>% mutate(across(where(is.numeric), round, 3))  %>% wd.Table(y.lab)
 

pwc <- pwc %>% add_xy_position(x = "Group2", fun = "mean_sd")
g1 = ggbarplot( get_emmeans(pwc), x = "Group2", y = "emmean", fill="black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2 ) + 
  stat_pvalue_manual(pwc, hide.ns = FALSE, tip.length = FALSE) +
  labs(y=y.lab, x= "",
       subtitle = get_test_label(res.aov, detailed = TRUE),
       # caption = get_pwc_label(pwc)
  )

ggsave(paste0(y.lab,".jpeg"), g1, dpi = 1000, width = 5, height = 6)
}
