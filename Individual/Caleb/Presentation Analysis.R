library(lmerTest)
library(knitr)
library(tidyverse)
library(broom.mixed)
library(ggeffects)
load("data/processed/lmmdata.RData")

model_all = lmer(zhfa ~ as.factor(chsex) + 
                   time +
                   drwaterq_new + 
                   as.factor(chsex) * time + 
                   as.factor(chsex)*drwaterq_new + 
                   time*drwaterq_new + 
                   age_first_c + 
                   (1|childid),
                 data = ethiopia_df)

summ = summary(model_all)
kable(round(as.data.frame(summ$coefficients),4) %>% 
        select("Estimate", "Std. Error","Pr(>|t|)"))
attr(summ$varcor$childid,"stddev")

p1 = plot(ggpredict(model_all, terms=c("time", "chsex")),
     show_data = T)+
labs(title ="Interaction between sex and time", x = "time", y = "z-score",colour = "sex"  )

p2 = plot(ggpredict(model_all, terms=c("time", "drwaterq_new")),
          show_data = T)+
  labs(title ="Interaction between access of \n drinking water and time" , x = "time", y = "z-score",colour = "access to \n drinking water" )

p3 =plot(ggpredict(model_all, terms = c("chsex [all]", "drwaterq_new")), 
       show_data = TRUE, 
       connect_lines = TRUE) +
  labs(title = "Interaction between access of\n  drinking water and sex", x = "sex", y = "z-score",colour = "access to \n drinking water"  )

p1 + p2+p3

dev.off()
  model_younger = lmer(zhfa ~ as.factor(chsex) + 
                     time +
                     drwaterq_new + 
                     as.factor(chsex) * time + 
                     as.factor(chsex)*drwaterq_new + 
                     time*drwaterq_new + 
                     age_first_c + 
                     (1|childid),
                   data = ethiopia_yg)
  summary(model_younger)
  
  summ = summary(model_younger)
  kable(round(as.data.frame(summ$coefficients),4) %>% 
          select("Estimate", "Std. Error","Pr(>|t|)"))
  round(attr(summ$varcor$childid,"stddev"),4)
  
  
  p1 = plot(ggpredict(model_younger, terms=c("time", "chsex")),
            show_data = T)+
    labs(title ="Interaction between sex and time", x = "time", y = "z-score",colour = "sex"  )
  
  p2 = plot(ggpredict(model_younger, terms=c("time", "drwaterq_new")),
            show_data = T)+
    labs(title ="Interaction between access of\n drinking water and time" , x = "time", y = "z-score",colour = "access to \n drinking water" )
  
  p3 =plot(ggpredict(model_younger, terms = c("chsex [all]", "drwaterq_new")), 
           show_data = TRUE, 
           connect_lines = TRUE) +
    labs(title = "Interaction between access of\n drinking water and sex", x = "sex", y = "z-score",colour = "access to \n drinking water"  )
  
  p1 + p2+p3



model_older = lmer(zhfa ~ as.factor(chsex) + 
                       time +
                       drwaterq_new + 
                       as.factor(chsex) * time + 
                       as.factor(chsex)*drwaterq_new + 
                       time*drwaterq_new + 
                       age_first_c + 
                       (1|childid),
                     data = ethiopia_old)
summary(model_older)


summ = summary(model_older)
kable(round(as.data.frame(summ$coefficients),4) %>% 
        select("Estimate", "Std. Error","Pr(>|t|)"))
round(attr(summ$varcor$childid,"stddev"),4)


p1 = plot(ggpredict(model_older, terms=c("time", "chsex")),
          show_data = T)+
  labs(title ="Interaction between sex and time", x = "time", y = "z-score",colour = "sex"  )

p2 = plot(ggpredict(model_older, terms=c("time", "drwaterq_new")),
          show_data = T)+
  labs(title ="Interaction between access of\n drinking water and time" , x = "time", y = "z-score",colour = "access to \n drinking water" )

p3 =plot(ggpredict(model_older, terms = c("chsex [all]", "drwaterq_new")), 
         show_data = TRUE, 
         connect_lines = TRUE) +
  labs(title = "Interaction between access of\n drinking water and sex", x = "sex", y = "z-score",colour = "access to \n drinking water"  )

p1 + p2+p3

ethiopia_df%>% group_by(childid) %>% slice(1) %>% ungroup() %>% group_by(yc)%>% summarise(mean(agemon))
  
