# library(ggplot2)
# library(dplyr)
# library(ggthemes)
# 
# rate <- seq(0.1,10,0.1)
# qaly  <- 1:100
# ds <- data.frame(rate, qaly)
# subset(ds, ds$qaly>10)
# 
# #ds %>% filter(qaly>10) %>% rename (dsdsd = rate) %>%
# 
# ggplot(ds, aes(y=qaly, x=rate)) + geom_point () +
#   ggtitle(label = "Net QALY Gain for diff", subtitle = "Azihromycin Therapy") +
#   ylab ("QALY Gain /n QALY") + xlab ("Exacerbation Rate") + theme_fivethirtyeight()

# library(ggplot2)
# colors <- c("Sepal Width" = "blue", "Petal Length" = "red", "Petal Width" = "orange")
# 
# ggplot(iris, aes(x = Sepal.Length)) +
#   geom_line(aes(y = Sepal.Width, color = "Sepal Width"), size = 1.5) +
#   geom_line(aes(y = Petal.Length, color = "Petal Length"), size = 1.5) +
#   geom_line(aes(y = Petal.Width, color = "Petal Width"), size = 1.5) +
#   labs(x = "Year",
#        y = "(%)",
#        color = "Legend") +
#   scale_color_manual(values = colors)
huron<-data.frame(year=1875:1972,level=LakeHuron)

vertDf<-data.frame(years=c(1900,1925,1950),labels=c("A","B","C"))

ggplot(huron, aes (x=year,y=level)) +
  geom_line() + 
  geom_vline(aes(xintercept=years, color=labels),data=vertDf, show.legend=T) + 
  scale_color_manual("Sample Year",
                     values=c("A"="blue","B"="red","C"="green"))
  
# # golds <- c(0.82,1.17,1.61,2.1)
# # history <- c(0.5,1.05,1.7)
# # for colors: https://htmlcolorcodes.com
#   
# ggplot(ds, aes(x=rate)) + 
#   # geom_line(color="grey") +
#   # geom_point() +
#   # stat_smooth(method = "lm", formula = y ~ exp(x), size = 1 , color ="grey")+    
#   # ylab ("Proportion of positive net QALY \n") + xlab ("\n Treatment effecg") +
#   geom_point(aes(y=prop)) + 
#   geom_point(aes(y=qaly)) +  
#   
#   scale_y_continuous(name = "Proportion of positive net QALY \n",
#   # Add a second axis and specify its features
#   sec.axis = sec_axis( trans=~.*0.1, name="Net QALY gain \n"))+
#   xlab ("\n Relative risk of exacerbation in the treatment group") +
#   theme_ipsum()+
#   # +xlim()+ylim()+
#   geom_vline(xintercept=0.75, linetype="dashed", color = "#DC7633")+
#   geom_text(aes(x=0.75, label="\n Model", y=0.5,family = "sans"), color="#DC7633") 



# geom_vline(xintercept=history, linetype="dashed", color = "#8E44AD")+
# geom_text(aes(x=0.5, label="\n History", y=0.4,family = "sans"), color="#8E44AD") +
# geom_vline(xintercept=golds, linetype="dashed", color = "#1F618D")+
# geom_text(aes(x=1.17, label="\n Gold stages", y=0.3,family = "sans"), colour="#1F618D") +
# geom_vline(xintercept=1.8, linetype="dashed", color = "#1ABC9C") +
# geom_text(aes(x=1.8, label="\n Threshold", y=0.2,family = "sans"), colour="#1ABC9C")

# ggplot(ds, aes(y=prop, x=rate)) + 
#   geom_line(color="grey") +
#   geom_point() +
#   # stat_smooth(method = "lm", formula = y ~ exp(x), size = 1 , color ="grey")+    
#   # ylab ("Proportion of positive net QALY \n") + xlab ("\n Treatment effecg") +
#   ylab ("Proportion of positive net QALY\n")+ scale_y_continuous(guide = guide_axis(position = "right")) + xlab ("\n Resistane") +
#   theme_economist() +
#   ylim(-0.1,1) +
#   xlim(0,1)+
#   geom_vline(xintercept=0.2, linetype="dashed", color = "#DC7633")+
#   geom_text(aes(x=0.2, label="\n Model", y=0.5,family = "sans"), color="#DC7633") 
