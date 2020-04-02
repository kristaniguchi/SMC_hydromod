#SMC Hydromod data exploration: which streams are most susceptible to hydromod?
  #How do responses vary by stream type and enviro setting?

#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("plyr")
#install.packages("scales")
#install.packages("MASS")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(MASS)

#read in SMC hydrmod data
data <- read.csv("P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/Hydromod project combined variables.csv")
#create new column for vert and lat suscept 
data$vert.rating <- revalue(data$VertSuscept, c("VERY HIGH" = 4, "High"= 3, "MEDIUM" = 2, "LOW" = 1, "Unk" = NA))
  data$vert.rating <- as.numeric(as.character(data$vert.rating))
data$av.lat.rating <- revalue(data$AverageLatSuscept, c("VERY HIGH" = 4, "HIGH"= 3, "MEDIUM" = 2, "LOW" = 1, "Unk"=NA))
  data$av.lat.rating <- as.numeric(as.character(data$av.lat.rating))
data$d50[data$d50== -88] <- NA
  
#subset data to most recent year surveyed
data$sampledate <- as.Date(as.character(data$sampledate), "%m/%d/%Y")
sub <- data %>% group_by(stationcode) %>% slice(which.max(sampledate)) %>% data.frame()
#export subsetted data as csv
write.csv(sub, file="P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/Hydromod project combined variables_subsetrecentsurveyonly2.csv")

#determine if concrete, eng.softbottom, or natural (channel type: natural or engineered)
channeltype2 <- NA

for(i in 1:length(sub$stationcode)){
  if(is.na(sub$channeltype[i])){
    channeltype2[i] <- NA
  }else if(sub$channeltype[i]=="Natural"){
      channeltype2[i] <- "Natural"
  }else if(sub$bottom[i] == "Soft/Natural"){
      channeltype2[i] <- "Engineered_SoftBottom"
  }else if(sub$bottom[i] == "Concrete"){
      channeltype2[i] <- "Concrete"
  }else{
    channeltype2[i] <- NA
  }
}

#save new channel type vector into sub df
sub$channeltype2 <- channeltype2

#Stacked barplots for concrete, engineered_softbottom, and natural
vert.suscept <- data.frame(aggregate(sub, by = sub[c('channeltype2','vert.rating')], length))
  vert.suscept$count <- vert.suscept$SiteYear
lat.suscept <- data.frame(aggregate(sub, by = sub[c('channeltype2','av.lat.rating')], length))
  lat.suscept$count <- lat.suscept$SiteYear

  #vertical suscept
ggplot(data = vert.suscept) +
  geom_bar(aes(x = channeltype2, y = count, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE)) +
  ggtitle("Vertical Susceptibility, most recent survey") +
  guides(fill = guide_legend(reverse = TRUE)) +
  #scale_fill_discrete(name = "Vertical Suscept.", labels = c("Low", "Medium", "High")) +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = lat.suscept) +
  geom_bar(aes(x = channeltype2, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE)) +
  ggtitle("Lateral Susceptibility, most recent survey") +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 


#Ordinal logistic regression: looking at all channel types
lapply(sub[, c("area.SqMile",	"precip.Inch",	"valleyslope",	"valleywidth.Meter", "d50", "Q10", "vert.rating", "av.lat.rating")], table)

## fit ordered logit model and store results 'm'
m <- polr(factor(vert.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10 + channeltype2  , data = sub, Hess=TRUE)
summary(m)



# Logistic Regression GLM
# where F is a binary factor and
# x1-x3 are continuous predictors
fit <- glm(factor(vert.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10 + channeltype2 , data = sub,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
#for vertical rating --> D50 and precip.inch


fit <- glm(factor(av.lat.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10 + channeltype2 , data = sub,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
#for lateral rating --> only D50

#subset natural, lateral
natural <- sub[sub$channeltype=="Natural",]
fit <- glm(factor(av.lat.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10  , data = natural,family=binomial())
summary(fit) # display results

fit <- glm(factor(av.lat.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10 +roaddens_1k, data = natural,family=binomial())
summary(fit) # display results

#subset natural, vertical
fit <- glm(factor(vert.rating) ~ area.SqMile +	precip.Inch +	valleyslope +	valleywidth.Meter + d50 + Q10 +roaddens_1k , data = natural,family=binomial())
summary(fit) # display results


#boxplots
p <- ggplot(natural, aes(x=factor(vert.rating), y=d50)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Vertical Susceptibility") +
  ggtitle("Vertical Susceptibility, most recent survey, natural") 
p

p <- ggplot(natural, aes(x=factor(vert.rating), y=precip.Inch)) + 
  geom_boxplot()  + xlab("Vertical Susceptibility") +
  ggtitle("Vertical Susceptibility, most recent survey, natural") 
p


p <- ggplot(natural, aes(x=factor(av.lat.rating), y=d50)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p

p <- ggplot(natural, aes(x=factor(av.lat.rating), y=valleywidth.Meter)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p

p <- ggplot(natural, aes(x=factor(av.lat.rating), y=roaddens_1k)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p

p <- ggplot(natural, aes(x=factor(av.lat.rating), y=roaddens_1k)) + 
  geom_boxplot()  + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p

p <- ggplot(natural, aes(x=factor(av.lat.rating), y=Q10)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p


p <- ggplot(natural, aes(x=factor(av.lat.rating), y=area.SqMile)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Lateral Susceptibility") +
  ggtitle("Lateral Susceptibility, most recent survey, natural") 
p


ggplot(data = natural) +
  geom_bar(aes(x = channeltype2, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE)) +
  ggtitle("Lateral Susceptibility, most recent survey") +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 

