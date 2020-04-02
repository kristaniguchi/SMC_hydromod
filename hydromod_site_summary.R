#SMC Hydromod Data exploration
  #subset overall dataset into multiple different groups (all, all.unique, most recent, most recent earthen, trends sites)
  #explore trends sites patterns
  #explore land use imp and csci

library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(MASS)

##########################################################################################

#Read in SMC hydromod data all
#read in SMC hydrmod data
data <- read.csv("P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/Hydromod project combined variables_wood_KTQ.csv")
#create new column for vert and lat suscept 
data$vert.rating <- revalue(data$VertSuscept, c("VERY HIGH" = 4, "High"= 3, "MEDIUM" = 2, "LOW" = 1, "Unk" = NA))
data$vert.rating <- as.numeric(as.character(data$vert.rating))
data$av.lat.rating <- revalue(data$AverageLatSuscept, c("VERY HIGH" = 4, "HIGH"= 3, "MEDIUM" = 2, "LOW" = 1, "Unk"=NA))
data$av.lat.rating <- as.numeric(as.character(data$av.lat.rating))
data$d50[data$d50== -88] <- NA
##########################################################################################

#determine if concrete, eng.softbottom, or natural (channel type: natural or engineered)
channeltype2 <- NA
for(i in 1:length(data$stationcode)){
  if(is.na(data$channeltype[i])){
    channeltype2[i] <- NA
  }else if(data$channeltype[i]=="Natural"){
    channeltype2[i] <- "Natural"
  }else if(data$bottom[i] == "Soft/Natural"){
    channeltype2[i] <- "Engineered_SoftBottom"
  }else if(data$bottom[i] == "Concrete"){
    channeltype2[i] <- "Concrete"
  }else{
    channeltype2[i] <- NA
  }
}
#save new channel type vector into data df
data$channeltype2 <- channeltype2

##########################################################################################

#subset trends sites
unique.sites <- as.character(unique(data$stationcode)) #297 unique hydromod sites
#loop to count repeat surveys for each unique site, subset only the sites with lenght >1
#vector of lengths for each site
length <- NA

for (i in 1:length(unique.sites)){
  sub <- data[data$stationcode == unique.sites[i],]
  length[i] <- length(sub$stationcode)
}
#index of trends sites
ind <- which(length>1)
#subset of trend sites
unique.trend <- unique.sites[ind]
length(unique.trend)
sub.trend <- data[data$stationcode %in% unique.trend,]
#write subset trend data
write.csv(sub.trend, file="P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/SMC_Hydromod_Trends_Subset.csv", row.names = FALSE)


#to do look at land use of trend sites
##########################################################################################

#subset data to most recent year surveyed
data$sampledate <- as.Date(as.character(data$sampledate), "%m/%d/%Y")
recent <- data %>% group_by(stationcode) %>% slice(which.max(sampledate)) %>% data.frame()
#export subsetted data as csv
write.csv(recent, file="P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/recent_subset.csv")

#combine with GIS data and %imp
#Add in additional GIS data to sub3
gis.data <- read.csv("P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/All_GIS_Variables.csv")
#merge gis data with sub3
recent2 <- join(recent, gis.data, by = "masterid")

plot(recent2$PctImp2011Ws, recent2$csci)


#recent subset natural 
#create subset excluding concrete channels (fully)
sub2 <- recent2[recent2$fullyarmored == "No",]
grep("Concrete", sub2$channeltype2)
sub3 <- sub2[-110,] #exclude the 1 concrete row that is left in sub2
#exclude NA channeltype2
ind.NA <- which(is.na(sub3$channeltype2))
sub4 <- sub3[-ind.NA,]

#replace all -88 values as NA
sub4[sub4 == -88]<- NA

#write csv
write.csv(sub4, file="P:/KrisTaniguchi/Hydromod_SMC/Data/final_sent_to_john/recent_subset_noconcrete.csv", row.names=FALSE)

#summary of sites
total <- length(sub4$SiteYear)
high.vert <- length(grep("High", sub4$VertSuscept))/total
high.veryhigh.lat <- (length(grep("HIGH", sub4$AverageLatSuscept)) +  length(grep("VERY HIGH", sub4$AverageLatSuscept)))  /total

#Stacked barplots for concrete, engineered_softbottom, and natural
vert.suscept <- data.frame(aggregate(sub4, by = sub4[c('smc_lu','vert.rating')], length))
vert.suscept$count <- vert.suscept$SiteYear
lat.suscept <- data.frame(aggregate(sub4, by = sub4[c('smc_lu','av.lat.rating')], length))
lat.suscept$count <- lat.suscept$SiteYear
#omit SMC_out
#vert.suscept<- vert.suscept[!grepl("SMC_out", vert.suscept$smc_lu),]
#lat.suscept<- lat.suscept[!grepl("SMC_out", lat.suscept$smc_lu),]
#gsub SMC_out
vert.suscept[] <- lapply(vert.suscept, gsub, pattern = "SMC_out", replacement = "Other", fixed = TRUE)
lat.suscept[] <- lapply(lat.suscept, gsub, pattern = "SMC_out", replacement = "Other", fixed = TRUE)

#add all summary too
vert.suscept.all <- data.frame(aggregate(sub4, by = sub4[c('vert.rating')], length))
vert.suscept.all <- rbind.fill(vert.suscept, vert.suscept.all)
vert.suscept.all$smc_lu <- c(vert.suscept.all$smc_lu[1:12], rep("All", 3))
lat.suscept.all <- data.frame(aggregate(sub4, by = sub4[c('av.lat.rating')], length))
lat.suscept.all <- rbind.fill(lat.suscept, lat.suscept.all)
lat.suscept.all$smc_lu <- c(lat.suscept.all$smc_lu[1:16], rep("All", 4))

#bar plot positions
positions <- c("All", "Agricultural", "Open", "Urban", "Other")
#make sure numeric
vert.suscept.all$masterid <- as.numeric(vert.suscept.all$masterid)
lat.suscept.all$masterid <- as.numeric(lat.suscept.all$masterid)


#vertical suscept
ggplot(data = vert.suscept.all) +
  geom_bar(aes(x = smc_lu, y = masterid, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE)) +
  ggtitle("Vertical Susceptibility, Natural Channels") +
  guides(fill = guide_legend(reverse = TRUE)) +
  #scale_fill_discrete(name = "Vertical Suscept.", labels = c("Low", "Medium", "High")) +
  xlab("") + ylab("") + scale_x_discrete(limits = positions) +
  scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = lat.suscept.all) +
  geom_bar(aes(x = smc_lu, y = masterid, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE)) +
  ggtitle("Lateral Susceptibility, Natural Channels") +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("") + ylab("") +  scale_x_discrete(limits = positions) +
  scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 




#boxplots of concrete (high, med, low) for CSCI and ASCI
#recent subset all 
#exclude NA channeltype2
ind.NA <- which(is.na(recent2$channeltype2))
recent3 <- recent2[-ind.NA,]

#replace all -88 values as NA
recent3[recent3 == -88]<- NA

#change vert rating for low 1 to 0 if it is concrete
ind.conc <- grep("Concrete", recent3$channeltype2)
recent3$vert.rating[ind.conc] <- 0

#boxplots
p <- ggplot(recent3, aes(x=factor(vert.rating), y=csci)) + 
  geom_boxplot() +scale_y_continuous(trans='log10') + xlab("Vertical Susceptibility") +
  ggtitle("Vertical Susceptibility, most recent survey, natural") 
p

