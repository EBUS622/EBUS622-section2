set.seed(100)
samplesize<-300
onstreet_waitingtime<-runif(samplesize, min=0,max=10)
p0<-0.99
p10<-0.10
intercept_coef<-log(p0/(1-p0))
slope_waitingtime<-(log(p10/(1-p10))-intercept_coef)/10
intercept_coef
slope_waitingtime
logit_value<-intercept_coef+slope_waitingtime*onstreet_waitingtime
p<-1/(1+exp(-logit_value))
choose_EV<-rbinom(samplesize,size=1,prob=p)
EV_data<-data.frame(choose_EV,onstreet_waitingtime)
head(EV_data)
summary(EV_data)
model <- glm(
  choose_EV ~ onstreet_waitingtime,
  data   = EV_data,
  family = binomial(link = "logit")
)
summary(model)
exp(coef(model)["onstreet_waitingtime"])
library(ggplot2)
library(scales)
onstreet_time<-data.frame(onstreet_waitingtime=seq(0,10,length.out=300))
onstreet_time$pred_prob<-predict(model,newdata=onstreet_time,type="response")
ggplot(onstreet_time, aes(x = onstreet_waitingtime, y = pred_prob)) +
  geom_line(size=1) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),  # show as 0%, 10%, 20%, ...
    limits = c(0, 1)
  ) +
  labs(
    x = "Minutes",
    y = "Predicted probability of choosing EV taxi (%)",
  )
  