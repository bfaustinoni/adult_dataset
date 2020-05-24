setwd("C:/Users/Benedito/Desktop/credito_pit_ttc/dados")
library(ggplot2)
library(dplyr)
library(reshape2)

df = read.csv("mortgage.csv", header = TRUE, sep = ';', dec =',')

View(df) ###visualizando as features

###Vamos olhar o modelo PIT primeiro

pit_model = glm(df$default_time~df$hpi_time+df$gdp_time+df$uer_time+df$FICO_orig_time+df$LTV_orig_time,
                binomial(link = "probit"))

summary(pit_model)

###Agora o modelo TTC

ttc_model = glm(df$default_time~df$FICO_orig_time+df$LTV_orig_time,
                binomial(link = "probit"))

summary(ttc_model)


###fazendo o predict 

df$pred_ttc = predict(ttc_model,df, type = 'response')
df$pred_pit = predict(pit_model,df, type = 'response')

df_2 = df

df_2 = df_2 %>%
  group_by(time) %>%
  summarise(DR = mean(default_time), PD_TTC = mean(pred_ttc), PD_PIT = mean(pred_pit))

View(df_2)

df_3 = melt(df_2, id.vars = "time")

View(df_3)

colnames(df_3) = c("time", "rate", "value")



ggplot(data=df_3,aes(x=time, y = value, group = rate)) + geom_line(aes(colour = rate), size = 0.75)+geom_point(aes(shape = rate, colour = rate), size = 1.75)+
  scale_color_manual(values=c("red2", "seagreen4", "royalblue3"))+ theme_minimal()+xlab('Time') + ylab("DR and PD")


