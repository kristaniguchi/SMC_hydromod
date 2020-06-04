#SMC Hydromod data exploration: which streams are most susceptible to hydromod?
  #How do responses vary by stream type and enviro setting?

#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("plyr")
#install.packages("scales")
#install.packages("MASS")
#install.packages("ggpubr")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(MASS)
library(ggpubr)

#directory
dir <- "C:/Users/KristineT.SCCWRP2K/Documents/Git/SMC_hydromod/data/"

#read in SMC hydrmod data
fname <- paste0(dir, "Hydromod project combined variables_wood_KTQ.csv")
data <- read.csv(fname)

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
write.csv(sub, file=paste0(dir,"Hydromod project combined variables_subsetrecentsurveyonly2.csv"))

######################
#determine the channel type: natural, hardened all, soft.all, hardened.sides
#note: Other bottom is concrete/asphalt/rock with hardened banks
channeltype2 <- NA

for(i in 1:length(sub$stationcode)){
  if(is.na(sub$channeltype[i])){
    channeltype2[i] <- NA
  }else if(sub$channeltype[i]=="Natural"){
      channeltype2[i] <- "Natural"
  }else if(sub$bottom[i] == "Concrete" | sub$bottom[i] == "Grouted rock" | sub$bottom[i] == "Other"){
    channeltype2[i] <- "Hardened Entire"
  }else if((sub$bottom[i] == "Soft/Natural") & (sub$leftsideofstructure[i]  == "Earthen" | sub$leftsideofstructure[i]  == "Earthen bare" | sub$leftsideofstructure[i]  == "Vegetative/Natural") & (sub$rightsideofstructure[i]  == "Earthen" | sub$rightsideofstructure[i]  == "Earthen bare" | sub$rightsideofstructure[i]  == "Vegetative/Natural") ){
    channeltype2[i] <- "Earthen Engineered"
  }else if((sub$bottom[i] == "Soft/Natural") & (sub$leftsideofstructure[i]  == "Concrete" | sub$leftsideofstructure[i]  == "Grouted rock" | sub$leftsideofstructure[i]  == "Rock") | (sub$rightsideofstructure[i]  == "Concrete" | sub$rightsideofstructure[i]  == "Grouted rock" | sub$rightsideofstructure[i]  == "Rock") ){
      channeltype2[i] <- "Hardened Side(s)"
  }else{
    channeltype2[i] <- NA
  }
}



#save new channel type vector into sub df
sub$channeltype2 <- channeltype2
#levels(sub$channeltype2) <- c("Natural", "Earthen Engineered","Hardened Side(s)", "Hardened Entire")
#levels(sub$channeltype) <- c("Natural", "Engineered", "")
#Also put natural and soft all into one category: Natural/Soft All for channeltype3
sub$channeltype3 <- channeltype2
sub$channeltype3[sub$channeltype3=="Earthen Engineered"] <- "Natural & Earthen Engineered"
sub$channeltype3[sub$channeltype3=="Natural"] <- "Natural & Earthen Engineered"

#Also create coarser category based on earthen vs. engineered
sub$channeltype4 <- sub$channeltype3
#sub$channeltype4[sub$channeltype4=="Natural & Earthen Engineered"] <- "Natural & Earthhen Engineered"
sub$channeltype4[sub$channeltype4=="Hardened Side(s)"] <- "Hardened Engineered (Fully & Partially)"
sub$channeltype4[sub$channeltype4=="Hardened Entire"] <- "Hardened Engineered (Fully & Partially)"



######################
#Summary of sites, numbers per channel type

#all sites by channel type
all.channeltype3 <- data.frame(aggregate(sub, by = sub[c('channeltype3')], length))
all.channeltype3$count <- all.channeltype3$SiteYear
total <- sum(all.channeltype3$SiteYear)

#channel type 2 counts summary, better to use channel type3
vert.suscept <- data.frame(aggregate(sub, by = sub[c('channeltype2','vert.rating')], length))
  vert.suscept$count <- vert.suscept$SiteYear
lat.suscept <- data.frame(aggregate(sub, by = sub[c('channeltype2','av.lat.rating')], length))
  lat.suscept$count <- lat.suscept$SiteYear

#channel type 3 counts summary
vert.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','vert.rating')], length))
  vert.suscept.2$count <- vert.suscept.2$SiteYear
lat.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','av.lat.rating')], length))
  lat.suscept.2$count <- lat.suscept.2$SiteYear
  
#land use and channel type 4
lu.channeltype4 <- data.frame(aggregate(sub, by = sub[c('channeltype4', 'smc_lu')], length))
lu.channeltype4$count <- lu.channeltype4$SiteYear
#exlcude SMC_out land use
lu.channeltype4<- lu.channeltype4[lu.channeltype4$smc_lu != "SMC_out",]
  #create new name for lu channeltype4
lu.channeltype4$lu.ch4.names <- c("Ag Natural/Earthen", "AgFully and Partially Hardened Engineered", "Open Natural/Earthen", "OpenFully and Partially Hardened Engineered", "Urban Natural/Earthen", "UrbanFully and Partially Hardened Engineered")

#vertical by land use channel type *** use this for bar plots
#land use and channel type 4
vert.lu.channeltype4 <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype4','vert.rating')], length))
vert.lu.channeltype4$count <- vert.lu.channeltype4$SiteYear
#exlcude SMC_out land use
vert.lu.channeltype4<- data.frame(vert.lu.channeltype4[vert.lu.channeltype4$smc_lu != "SMC_out",])
#create new name for lu channeltype4
vert.lu.channeltype4$vert.lu.ch4.names <- paste0(vert.lu.channeltype4$smc_lu, " ", vert.lu.channeltype4$channeltype4)
vert.lu.channeltype4$vert.lu.ch4.names <- gsub("Agricultural", "Ag", vert.lu.channeltype4$vert.lu.ch4.names)
#sum of sites in categories
total.vert <- sum(vert.lu.channeltype4$count)
total.vert.eng <- 11+3+53
#engineered, agriculture summary by channel type3
vert.lu.channeltype4.ag.eng <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype4','vert.rating','channeltype3')], length))
pct.ag.eng.softbottom <- 7/11


  
#lateral by land use channel type *** use this for bar plots
#land use and channel type 4
lat.lu.channeltype4 <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype4','av.lat.rating')], length))
lat.lu.channeltype4$count <- lat.lu.channeltype4$SiteYear
#exlcude SMC_out land use
lat.lu.channeltype4<- data.frame(lat.lu.channeltype4[lat.lu.channeltype4$smc_lu != "SMC_out",])
#create new name for lu channeltype4
lat.lu.channeltype4$lat.lu.ch4.names <- paste0(lat.lu.channeltype4$smc_lu, " ", lat.lu.channeltype4$channeltype4)
lat.lu.channeltype4$lat.lu.ch4.names <- gsub("Agricultural", "Ag", lat.lu.channeltype4$lat.lu.ch4.names)

#count in each lu.channeltype category
name.lu.chtype4 <- lu.channeltype4$lu.ch4.names
count.lu.chtype4 <- lu.channeltype4$count
lu.channeltype4.count.df <- data.frame(cbind(name.lu.chtype4, count.lu.chtype4))

######################
###Bar Plots: vertical and lateral suscept by land use and channel type

#bar plot positions
#reorder positions of lu
vert.lu.channeltype4$smc_lu <- factor(vert.lu.channeltype4$smc_lu, levels= c("Open","Agricultural","Urban"))
lat.lu.channeltype4$smc_lu <- factor(lat.lu.channeltype4$smc_lu, levels= c("Open","Agricultural","Urban"))

#make sure count is numeric
vert.lu.channeltype4$count <- as.numeric(vert.lu.channeltype4$count)
lat.lu.channeltype4$count <- as.numeric(lat.lu.channeltype4$count)

#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
anno <- data.frame(xstar = c(1,2,3,1,2,3), ystar = rep(0, 6),
                   lab = c("(71)", "(36)","(31)","(3)","(11)","(53)"),
                   channeltype4 = c("Natural & Earthen Engineered", "Natural & Earthen Engineered","Natural & Earthen Engineered","Hardened Engineered (Fully & Partially)","Hardened Engineered (Fully & Partially)","Hardened Engineered (Fully & Partially)"))
#set levels for channel type4
anno$channeltype4 <- factor(anno$channeltype4, levels=c("Natural & Earthen Engineered", "Hardened Engineered (Fully & Partially)"))
vert.lu.channeltype4$channeltype4 <- factor(vert.lu.channeltype4$channeltype4, levels=c("Natural & Earthen Engineered", "Hardened Engineered (Fully & Partially)"))
lat.lu.channeltype4$channeltype4 <- factor(lat.lu.channeltype4$channeltype4, levels=c("Natural & Earthen Engineered", "Hardened Engineered (Fully & Partially)"))


#vertical suscept
ggplot(data = vert.lu.channeltype4) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Vertical Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~channeltype4) +
  xlab("") + ylab("Proportion of Sites") +
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = lat.lu.channeltype4) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Lateral Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~channeltype4) +
  xlab("") + ylab("Proportion of Sites") +  
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 


###############
#Revision1 to barplots in natural vs. engineered category (channel type)

#vertical by land use channel type 
#land use and channel type
sub$channeltype <- factor(sub$channeltype, levels = c("Natural", "Engineered"))
vert.lu.channeltype <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype','vert.rating')], length))
vert.lu.channeltype$count <- vert.lu.channeltype$SiteYear
#exlcude SMC_out land use
vert.lu.channeltype<- data.frame(vert.lu.channeltype[vert.lu.channeltype$smc_lu != "SMC_out",])
#create new name for lu channeltype
vert.lu.channeltype$vert.lu.ch4.names <- paste0(vert.lu.channeltype$smc_lu, " ", vert.lu.channeltype$channeltype)
vert.lu.channeltype$vert.lu.ch4.names <- gsub("Agricultural", "Ag", vert.lu.channeltype$vert.lu.ch4.names)
#sum of sites in categories
total.vert <- sum(vert.lu.channeltype$count)
total.vert.eng <- 11+3+53
#engineered, agriculture summary by channel type3
vert.lu.channeltype.ag.eng <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype','vert.rating','channeltype3')], length))
pct.ag.eng.softbottom <- 7/11

#lateral by land use channel type 
#land use and channel type
lat.lu.channeltype <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype','av.lat.rating')], length))
lat.lu.channeltype$count <- lat.lu.channeltype$SiteYear
#exlcude SMC_out land use
lat.lu.channeltype<- data.frame(lat.lu.channeltype[lat.lu.channeltype$smc_lu != "SMC_out",])
#create new name for lu channeltype
lat.lu.channeltype$lat.lu.ch4.names <- paste0(lat.lu.channeltype$smc_lu, " ", lat.lu.channeltype$channeltype)
lat.lu.channeltype$lat.lu.ch4.names <- gsub("Agricultural", "Ag", lat.lu.channeltype$lat.lu.ch4.names)

#count in each lu.channeltype category
name.lu.chtype <- lat.lu.channeltype$lat.lu.ch4.names
count.lu.chtype <- lat.lu.channeltype$count
lu.channeltype.count.df <- data.frame(cbind(name.lu.chtype, count.lu.chtype))

#count of total natural 
total.natural <- 71+31+28
total.engineered <- 3+16+56
lat.nat.h.vhigh <- 9+7+9+10+9+11
prop.lat.nat.h.vh <- lat.nat.h.vhigh/total.natural
vert.nat.h <- 15+22+19
prop.vert.h <- vert.nat.h/total.natural

#bar plot positions
#reorder positions of lu
vert.lu.channeltype$smc_lu <- factor(vert.lu.channeltype$smc_lu, levels= c("Open","Agricultural","Urban"))
lat.lu.channeltype$smc_lu <- factor(lat.lu.channeltype$smc_lu, levels= c("Open","Agricultural","Urban"))

#make sure count is numeric
vert.lu.channeltype$count <- as.numeric(vert.lu.channeltype$count)
lat.lu.channeltype$count <- as.numeric(lat.lu.channeltype$count)


#overall counts in each of 6 categories
#set factor levels for land use
sub$smc_lu <- factor(sub$smc_lu, levels=c("Open", "Agricultural","Urban","SMC_out"))
lat.lu.channeltype.overall <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype')], length))
ind.smcout <- grep("SMC_out", lat.lu.channeltype.overall$smc_lu)
lat.lu.channeltype.overall <- lat.lu.channeltype.overall[-ind.smcout,]

#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
anno2 <- data.frame(xstar = c(1,2,3,1,2,3), ystar = rep(0, 6),
                   lab = c("(71)", "(31)","(28)","(3)","(16)","(56)"),
                   channeltype = c("Natural", "Natural","Natural","Engineered","Engineered","Engineered"))
#set levels for channel type
anno2$channeltype <- factor(anno2$channeltype, levels=c("Natural", "Engineered"))

###UPDATE: add an All category for "Natural" and Engineered" to vert.lu.channeltype and lat.lu.channeltype

#channel type 4 counts summary
#remove all sites with lu smc_out
ind.scmoutsub <- grep("SMC_out", sub$smc_lu)
sub.nosmcout <- sub[-ind.scmoutsub,]
#aggregate based on channel type and vert/lat ratings
vert.suscept.4 <- data.frame(aggregate(sub.nosmcout, by = sub.nosmcout[c('channeltype','vert.rating')], length))
vert.suscept.4$count <- vert.suscept.4$SiteYear
lat.suscept.4 <- data.frame(aggregate(sub.nosmcout, by = sub.nosmcout[c('channeltype','av.lat.rating')], length))
lat.suscept.4$count <- lat.suscept.4$SiteYear
#add in vert.lu.ch4.names and column
vert.suscept.4$vert.lu.ch4.names <- paste0("All ", vert.suscept.4$channeltype)
lat.suscept.4$lat.lu.ch4.names <- paste0("All ", lat.suscept.4$channeltype)
vert.suscept.4$smc_lu <- rep("All", length(vert.suscept.4$smc_lu))
lat.suscept.4$smc_lu <- rep("All", length(lat.suscept.4$smc_lu))
#merge all site summary with vert.lu.channeltype and lat.lu.channeltype
merge.vert.lu.channeltype  <- full_join(vert.lu.channeltype,  vert.suscept.4, by = names(vert.suscept.4))
merge.lat.lu.channeltype  <- full_join(lat.lu.channeltype,  lat.suscept.4, by = names(lat.suscept.4))
#save smc_lu as factor and set levels
merge.vert.lu.channeltype$smc_lu <- factor(merge.vert.lu.channeltype$smc_lu, levels = c("All","Open","Agricultural","Urban"))
merge.lat.lu.channeltype$smc_lu <- factor(merge.lat.lu.channeltype$smc_lu, levels = c("All","Open","Agricultural","Urban"))

#update annotation n values with added all category 
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
anno3 <- data.frame(xstar = c(1,2,3,4,1,2,3,4), ystar = rep(0, 8),
                    lab = c("(130)","(71)", "(31)","(28)","(75)","(3)","(16)","(56)"),
                    channeltype = c("Natural", "Natural","Natural","Natural","Engineered","Engineered","Engineered","Engineered"))
#set levels for channel type
anno3$channeltype <- factor(anno3$channeltype, levels=c("Natural", "Engineered"))


#vertical suscept
ggplot(data = merge.vert.lu.channeltype) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Vertical Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free") +
  xlab("") + ylab("Proportion of Sites") +
  geom_text(data = anno3, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = merge.lat.lu.channeltype) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Lateral Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~channeltype) +
  xlab("") + ylab("Proportion of Sites") +  
  geom_text(data = anno3, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 

#What accounts for the low lat engineered ag?
ag.eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Agricultural" & sub.nosmcout$av.lat.rating == 1,]
ag.eng.low$channeltype2 #half were partially hardened, half were fully hardened
#what accounts for the low engineered urban? ANSW: 60% low engineered urban are fully hardened, 40% are partially
urb.eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Urban" & sub.nosmcout$av.lat.rating == 1,]
urb.eng.low. <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Urban" & sub.nosmcout$av.lat.rating == 1 & sub.nosmcout$channeltype2 == "Hardened Entire" ,]
#overall
eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered"  & sub.nosmcout$av.lat.rating == 1,]
eng.low$channeltype2 #half were partially hardened, half were fully hardened
length.eng.low.hardentire <- length(grep("Hardened Entire", eng.low$channeltype2))
length.eng.low.hardenpart <- length(grep("Hardened Side", eng.low$channeltype2))
length.eng.low.earthen <- length(grep("Earthen Engineered", eng.low$channeltype2))
pct.eng.low.hardentire <- length.eng.low.hardentire/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)
pct.eng.low.hardenpart  <- length.eng.low.hardenpart/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)
pct.eng.low.earthen <- length.eng.low.earthen/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)

#What accounts for the low vert engineered ag?
ag.eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Agricultural" & sub.nosmcout$vert.rating == 1,]
ag.eng.low$channeltype2 #half were partially hardened, half were fully hardened
#what accounts for the low engineered urban? ANSW: 60% low engineered urban are fully hardened, 40% are partially
urb.eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Urban" & sub.nosmcout$av.lat.rating == 1,]
urb.eng.low.hardentire <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered" & sub.nosmcout$smc_lu == "Urban" & sub.nosmcout$av.lat.rating == 1 & sub.nosmcout$channeltype2 == "Hardened Entire" ,]
#overall
eng.low <- sub.nosmcout[sub.nosmcout$channeltype == "Engineered"  & sub.nosmcout$vert.rating == 1,]
eng.low$channeltype2 #half were partially hardened, half were fully hardened
length.eng.low.hardentire <- length(grep("Hardened Entire", eng.low$channeltype2))
length.eng.low.hardenpart <- length(grep("Hardened Side", eng.low$channeltype2))
length.eng.low.earthen <- length(grep("Earthen Engineered", eng.low$channeltype2))
pct.eng.low.hardentire <- length.eng.low.hardentire/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)
pct.eng.low.hardenpart  <- length.eng.low.hardenpart/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)
pct.eng.low.earthen <- length.eng.low.earthen/(length.eng.low.hardentire+length.eng.low.hardenpart+length.eng.low.earthen)


######################
#Revision2 to barplots in natural vs. hardened engineered vs. other engineered categories (channel type), add all category for all 3

#create new channel type
sub$channeltype5 <- sub$channeltype2
unique(sub$channeltype5)
#update with 3 categories above
sub$channeltype5[sub$channeltype5=="Hardened Side(s)"] <- "Engineered\nEarthen & Hardened Side(s)"
sub$channeltype5[sub$channeltype5=="Hardened Entire"] <- "Engineered\nHardened Entire"
sub$channeltype5[sub$channeltype5=="Earthen Engineered"] <- "Engineered\nEarthen & Hardened Side(s)"

#vertical by land use channel type 
#land use and channel type
sub$channeltype5 <- factor(sub$channeltype5, levels = c("Natural", "Engineered\nEarthen & Hardened Side(s)", "Engineered\nHardened Entire" ))
vert.lu.channeltype5 <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5','vert.rating')], length))
vert.lu.channeltype5$count <- vert.lu.channeltype5$SiteYear
#exlcude SMC_out land use
vert.lu.channeltype5<- data.frame(vert.lu.channeltype5[vert.lu.channeltype5$smc_lu != "SMC_out",])

#create new name for lu channeltype5
vert.lu.channeltype5$vert.lu.ch4.names <- paste0(vert.lu.channeltype5$smc_lu, " ", vert.lu.channeltype5$channeltype5)
vert.lu.channeltype5$vert.lu.ch4.names <- gsub("Agricultural", "Ag", vert.lu.channeltype5$vert.lu.ch4.names)
#sum of sites in categories
total.vert <- sum(vert.lu.channeltype5$count)
total.vert.eng <- 11+3+53
#engineered, agriculture summary by channel type3
vert.lu.channeltype5.ag.eng <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5','vert.rating','channeltype3')], length))
pct.ag.eng.softbottom <- 7/11

vert.lu.channeltype5.ag.eng <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5','vert.rating','channeltype2')], length))
lat.lu.channeltype5.ag.eng <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5','av.lat.rating','channeltype2')], length))


#lateral by land use channel type 
#land use and channel type
lat.lu.channeltype5 <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5','av.lat.rating')], length))
lat.lu.channeltype5$count <- lat.lu.channeltype5$SiteYear
#exlcude SMC_out land use
lat.lu.channeltype5<- data.frame(lat.lu.channeltype5[lat.lu.channeltype5$smc_lu != "SMC_out",])
#create new name for lu channeltype5
lat.lu.channeltype5$lat.lu.ch4.names <- paste0(lat.lu.channeltype5$smc_lu, " ", lat.lu.channeltype5$channeltype5)
lat.lu.channeltype5$lat.lu.ch4.names <- gsub("Agricultural", "Ag", lat.lu.channeltype5$lat.lu.ch4.names)

#count in each lu.channeltype5 category
name.lu.chtype <- lat.lu.channeltype5$lat.lu.ch4.names
count.lu.chtype <- lat.lu.channeltype5$count
lu.channeltype5.count.df <- data.frame(cbind(name.lu.chtype, count.lu.chtype))

#count of total natural 
total.natural <- 71+31+28
total.engineered <- 3+16+56
lat.nat.h.vhigh <- 9+7+9+10+9+11
prop.lat.nat.h.vh <- lat.nat.h.vhigh/total.natural
vert.nat.h <- 15+22+19
prop.vert.h <- vert.nat.h/total.natural

#bar plot positions
#reorder positions of lu
vert.lu.channeltype5$smc_lu <- factor(vert.lu.channeltype5$smc_lu, levels= c("Open","Agricultural","Urban"))
lat.lu.channeltype5$smc_lu <- factor(lat.lu.channeltype5$smc_lu, levels= c("Open","Agricultural","Urban"))

#make sure count is numeric
vert.lu.channeltype5$count <- as.numeric(vert.lu.channeltype5$count)
lat.lu.channeltype5$count <- as.numeric(lat.lu.channeltype5$count)


#overall counts in each of 6 categories
#set factor levels for land use
sub$smc_lu <- factor(sub$smc_lu, levels=c("Open", "Agricultural","Urban","SMC_out"))
lat.lu.channeltype5.overall <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype5')], length))
ind.smcout <- grep("SMC_out", lat.lu.channeltype5.overall$smc_lu)
lat.lu.channeltype5.overall <- lat.lu.channeltype5.overall[-ind.smcout,]

#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
anno2 <- data.frame(xstar = c(1,2,3,1,2,3,1,2), ystar = rep(0, 8),
                    lab = c("(71)", "(31)","(28)","(3)","(12)","(17)","(4)","(39)"),
                    channeltype5 = c("Natural", "Natural","Natural","Engineered\nEarthen & Hardened Side(s)","Engineered\nEarthen & Hardened Side(s)","Engineered\nEarthen & Hardened Side(s)","Engineered\nHardened Entire","Engineered\nHardened Entire"))
#set levels for channel type
anno2$channeltype5 <- factor(anno2$channeltype5, levels=c("Natural", "Engineered\nEarthen & Hardened Side(s)", "Engineered\nHardened Entire"))

###UPDATE: add an All category for "Natural", other engineered, hardened engineered to vert.lu.channeltype5 and lat.lu.channeltype5

#channel type 4 counts summary
#remove all sites with lu smc_out
ind.scmoutsub <- grep("SMC_out", sub$smc_lu)
sub.nosmcout <- sub[-ind.scmoutsub,]
#aggregate based on channel type and vert/lat ratings
vert.suscept.4 <- data.frame(aggregate(sub.nosmcout, by = sub.nosmcout[c('channeltype5','vert.rating')], length))
vert.suscept.4$count <- vert.suscept.4$SiteYear
lat.suscept.4 <- data.frame(aggregate(sub.nosmcout, by = sub.nosmcout[c('channeltype5','av.lat.rating')], length))
lat.suscept.4$count <- lat.suscept.4$SiteYear
#add in vert.lu.ch4.names and column
vert.suscept.4$vert.lu.ch4.names <- paste0("All ", vert.suscept.4$channeltype5)
lat.suscept.4$lat.lu.ch4.names <- paste0("All ", lat.suscept.4$channeltype5)
vert.suscept.4$smc_lu <- rep("All", length(vert.suscept.4$smc_lu))
lat.suscept.4$smc_lu <- rep("All", length(lat.suscept.4$smc_lu))
#merge all site summary with vert.lu.channeltype5 and lat.lu.channeltype5
merge.vert.lu.channeltype5  <- full_join(vert.lu.channeltype5,  vert.suscept.4, by = names(vert.suscept.4))
merge.lat.lu.channeltype5  <- full_join(lat.lu.channeltype5,  lat.suscept.4, by = names(lat.suscept.4))
#save smc_lu as factor and set levels
merge.vert.lu.channeltype5$smc_lu <- factor(merge.vert.lu.channeltype5$smc_lu, levels = c("All","Open","Agricultural","Urban"))
merge.lat.lu.channeltype5$smc_lu <- factor(merge.lat.lu.channeltype5$smc_lu, levels = c("All","Open","Agricultural","Urban"))

#update annotation n values with added all category 
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
anno3 <- data.frame(xstar = c(1,2,3,4,1,2,3,4,1,2,3), ystar = rep(0, 11),
                    lab = c("(130)","(71)", "(31)","(28)","(32)","(3)","(12)","(17)","(43)","(4)","(39)"),
                    channeltype5 = c("Natural","Natural", "Natural","Natural","Engineered\nEarthen & Hardened Side(s)","Engineered\nEarthen & Hardened Side(s)","Engineered\nEarthen & Hardened Side(s)","Engineered\nEarthen & Hardened Side(s)","Engineered\nHardened Entire","Engineered\nHardened Entire","Engineered\nHardened Entire"))


#set levels for channel type
anno3$channeltype5 <- factor(anno3$channeltype5, levels=c("Natural", "Engineered\nEarthen & Hardened Side(s)", "Engineered\nHardened Entire"))


#vertical suscept
ggplot(data = merge.vert.lu.channeltype5) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Vertical Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~factor(channeltype5, levels=c("Natural", "Engineered\nEarthen & Hardened Side(s)", "Engineered\nHardened Entire")), scales = "free") +
  xlab("") + ylab("Proportion of Sites") +
  geom_text(data = anno3, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = merge.lat.lu.channeltype5) +
  geom_bar(color="black", aes(x = smc_lu, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Lateral Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~factor(channeltype5, levels=c("Natural", "Engineered\nEarthen & Hardened Side(s)", "Engineered\nHardened Entire")), scales = "free") +
  xlab("") + ylab("Proportion of Sites") +
  geom_text(data = anno3, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 






















######################
###Boxplots: CSCI and ASCI vs. Lateral and Vertical Susceptibility

######CSCI vs. lateral and vert suscept

#subset data to exclude medium,high,veryhigh hardened
#channel type 3 counts summary
vert.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','vert.rating')], length))
  vert.suscept.2$count <- vert.suscept.2$SiteYear
lat.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','av.lat.rating')], length))
  lat.suscept.2$count <- lat.suscept.2$SiteYear
# create new column for Channel type3 and vert/lat suscept
sub$channeltype3.vert <- paste0(sub$VertSuscept, " ",sub$channeltype3 )
sub$channeltype3.vert <- gsub("Natural & Earthen Engineered", "Natural Earthen", sub$channeltype3.vert )
sub$channeltype3.vert <- gsub("LOW", "Low", sub$channeltype3.vert )
sub$channeltype3.vert <- gsub("MEDIUM", "Medium", sub$channeltype3.vert )
# Exclude NA channel type and medium/high hardened sides(s), Unk
  #find indices of channel type NA and unk and exclude them
ind.NA <- which(is.na(sub$channeltype3))
ind.highhardsides <- grep('High Hardened Side', as.character(sub$channeltype3.vert))
ind.medhardsides <- grep("Medium Hardened Side", sub$channeltype3.vert)
ind.unk <- grep("Unk", sub$channeltype3.vert)
#subset sub2.vert
sub2.vert <- data.frame(sub[-c(ind.NA, ind.unk),])
#sub2.vert <- data.frame(sub[-c(ind.NA, ind.highhardsides, ind.medhardsides, ind.unk),])

#create order of the categories for boxplots
#sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural Earthen","Medium Natural Earthen","High Natural Earthen"))
###update this with the high hardened and med hardened sides category
sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural Earthen","Medium Natural Earthen","Medium Hardened Side(s)", "High Natural Earthen", "High Hardened Side(s)"))
#sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Medium Hardened Side(s)", "High Hardened Side(s)", "Low Natural Earthen","Medium Natural Earthen", "High Natural Earthen"))
#update this to get line breaks 
levels(sub2.vert$channeltype3.vert) <- gsub(" ", "\n", levels(sub2.vert$channeltype3.vert))


#summary of median values for each suscept/channel type category
sub2.vert$csci <- as.numeric(sub2.vert$csci)
sub2.vert.median <- data.frame(aggregate(sub2.vert, by = sub2.vert[c('channeltype3.vert')], FUN=mean))
unique(sub2.vert$channeltype3.vert )


# create new column for Channel type3 and lateral suscept
sub$channeltype3.lat <- paste0(sub$AverageLatSuscept, " ",sub$channeltype3 )
sub$channeltype3.lat <- gsub("Natural & Earthen Engineered", "Natural Earthen", sub$channeltype3.lat )
sub$channeltype3.lat <- gsub("LOW", "Low", sub$channeltype3.lat )
sub$channeltype3.lat <- gsub("MEDIUM", "Medium", sub$channeltype3.lat )
sub$channeltype3.lat <- gsub("HIGH", "High", sub$channeltype3.lat )
sub$channeltype3.lat <- gsub("VERY", "Very", sub$channeltype3.lat )
  unique(sub$channeltype3.lat)
  
# Exclude NA channel type and medium/high hardened sides(s), Unk
#find indices of channel type NA and unk and exclude them
ind.NA.lat <- which(is.na(sub$AverageLatSuscept))
ind.highhardsides.lat <- grep('High Hardened Side', as.character(sub$channeltype3.lat))
ind.medhardsides.lat <- grep("Medium Hardened Side", sub$channeltype3.lat)
ind.unk.lat <- grep("Unk", sub$channeltype3.lat)
#sub2.lat <- data.frame(sub[-c(ind.NA, ind.NA.lat, ind.highhardsides.lat, ind.medhardsides.lat, ind.unk.lat),])
sub2.lat <- data.frame(sub[-c(ind.NA, ind.NA.lat, ind.unk.lat),])
  unique(sub2.lat$channeltype3.lat)
#create order of the categories for boxplots
  #sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural/Earthen","Medium Natural/Earthen","High Natural/Earthen","Very High Natural/Earthen"))
  sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural Earthen","Medium Natural Earthen","Medium Hardened Side(s)","High Natural Earthen","High Hardened Side(s)","Very High Natural Earthen", "Very High Hardened Side(s)"))
  #update this to get line breaks 
  levels(sub2.lat$channeltype3.lat) <- gsub(" ", "\n", levels(sub2.lat$channeltype3.lat))
  
#Boxplots CSCI
  
#annotate total number of sites in each bin/category
#vert.csci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.csci <- which(is.na(sub2.vert$csci))
  sub2.csci.vert <- data.frame(sub2.vert[-ind.NA.csci,])
  #ver channel type3
  vert.channeltype3 <- data.frame(aggregate(sub2.csci.vert, by = sub2.csci.vert[c('channeltype3.vert')], length))
  vert.channeltype3$count <- vert.channeltype3$SiteYear
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.vert.csci <- data.frame(xstar = c(1:7), ystar = rep(0, 7),
                              lab = paste0("(",vert.channeltype3$count,")"))
  
#lat.csci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.csci.lat <- which(is.na(sub2.lat$csci))
  sub2.csci.lat <- data.frame(sub2.lat[-ind.NA.csci.lat,])
  #lat channel type 3
  lat.channeltype3 <- data.frame(aggregate(sub2.csci.lat, by = sub2.csci.lat[c('channeltype3.lat')], length))
  lat.channeltype3$count <- lat.channeltype3$SiteYear
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.lat.csci <- data.frame(xstar = c(1:6), ystar = rep(0, 6),
                     lab = paste0("(",lat.channeltype3$count,")"))
  
  
  #CSCI vertical
  cv <- ggplot(sub2.csci.vert, aes(x=channeltype3.vert, y=csci, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    ggtitle("CSCI vs. Vertical Susceptibility") + 
    #geom_text(data = anno.vert.csci, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
    scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  cv

  #CSCI lateral
  cl <- ggplot(sub2.csci.lat, aes(x=channeltype3.lat, y=csci, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    ggtitle("CSCI vs. Lateral Susceptibility") +
    scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  cl
  
  #summary median values in boxplots
  vert.csci.med <- aggregate(csci ~  channeltype3.vert, sub2.vert, median)
  lat.csci.med <- aggregate(csci ~  channeltype3.lat, sub2.lat, median)
  vert.csci.length <- aggregate(csci ~  channeltype3.vert, sub2.vert, length)
  lat.csci.length <- aggregate(csci ~  channeltype3.lat, sub2.lat, length)
  
#Boxplots ASCI
  
#annotate total number of sites in each bin/category
#vert.asci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.asci <- which(is.na(sub2.vert$ASCI.hybrid))
  sub2.asci.vert <- data.frame(sub2.vert[-ind.NA.asci,])
  #vert channel type 3
  vert.channeltype3.asci <- data.frame(aggregate(sub2.asci.vert, by = sub2.asci.vert[c('channeltype3.vert')], length))
  vert.channeltype3.asci$count <- vert.channeltype3.asci$SiteYear
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.vert.asci <- data.frame(xstar = c(1:5), ystar = rep(0, 5),
                               lab = c(vert.channeltype3.asci$count))
#lat.asci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.asci.lat <- which(is.na(sub2.lat$ASCI.hybrid))
  sub2.asci.lat <- data.frame(sub2.lat[-ind.NA.asci.lat,])
  #lat channel type 3
  lat.channeltype3.asci <- data.frame(aggregate(sub2.asci.lat, by = sub2.asci.lat[c('channeltype3.lat')], length))
  lat.channeltype3.asci$count <- lat.channeltype3.asci$SiteYear
#annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.lat.asci <- data.frame(xstar = c(1:6), ystar = rep(0, 6),
                              lab = c(lat.channeltype3.asci$count))
  
  #ASCI vertical boxplots
  av <- ggplot(sub2.asci.vert, aes(x=channeltype3.vert, y=ASCI.hybrid, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    ggtitle("ASCI vs. Vertical Susceptibility") +
    scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  av
  
  #ASCI lateral boxplots
  al <- ggplot(sub2.asci.lat, aes(x=channeltype3.lat, y=ASCI.hybrid, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    ggtitle("Lateral Susceptibility, ASCI") +
    scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  al
  
  
  #summary median values in boxplots
  vert.asci.med <- aggregate(ASCI.hybrid ~  channeltype3.vert, sub2.vert, median)
  lat.asci.med <- aggregate(ASCI.hybrid ~  channeltype3.lat, sub2.lat, length)
  
  
  
#################################################################    
########BOXPLOTS WITH NATURAL CHANNEL SUBSET OUT
  
  ######CSCI vs. lateral and vert suscept
  

  #subset data to exclude medium,high,veryhigh hardened
  #channel type 2 counts summary
  vert.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','vert.rating')], length))
  vert.suscept.2$count <- vert.suscept.2$SiteYear
  lat.suscept.2 <- data.frame(aggregate(sub, by = sub[c('channeltype3','av.lat.rating')], length))
  lat.suscept.2$count <- lat.suscept.2$SiteYear
  # create new column for Channel type3 and vert/lat suscept
  sub$channeltype3.vert <- paste0(sub$VertSuscept, " ",sub$channeltype3 )
  sub$channeltype3.vert <- gsub("LOW", "Low", sub$channeltype3.vert )
  sub$channeltype3.vert <- gsub("MEDIUM", "Medium", sub$channeltype3.vert )
  #for natural/earthen parse out natural from earthen engineered
  ind.natural <- grep("Natural", sub$channeltype2)
  sub$channeltype3.vert[ind.natural] <- gsub("Natural & Earthen Engineered", "Natural",   sub$channeltype3.vert[ind.natural])
  ind.earthen <- grep("Earthen Engineered", sub$channeltype2)
  sub$channeltype3.vert[ind.earthen] <- gsub("Natural & Earthen Engineered", "Earthen Engineered",   sub$channeltype3.vert[ind.earthen])

  # Exclude NA channel type and medium/high hardened sides(s), Unk
  #find indices of channel type NA and unk and exclude them
  ind.NA <- which(is.na(sub$channeltype3))
  ind.highhardsides <- grep('High Hardened Side', as.character(sub$channeltype3.vert))
  ind.medhardsides <- grep("Medium Hardened Side", sub$channeltype3.vert)
  ind.unk <- grep("Unk", sub$channeltype3.vert)
  #subset sub2.vert
  sub2.vert <- data.frame(sub[-c(ind.NA, ind.unk),])
  #sub2.vert <- data.frame(sub[-c(ind.NA, ind.highhardsides, ind.medhardsides, ind.unk),])
  unique(sub2.vert$channeltype3.vert)
  
  #create order of the categories for boxplots
  #sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural Earthen","Medium Natural Earthen","High Natural Earthen"))
  ###update this with the high hardened and med hardened sides category
  #sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Earthen Engineered","Low Natural","Medium Natural","Medium Hardened Side(s)", "High Natural", "High Earthen Engineered","High Hardened Side(s)"))
  sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Medium Hardened Side(s)","High Hardened Side(s)","Low Earthen Engineered","High Earthen Engineered", "Low Natural","Medium Natural", "High Natural"))
  
  #create new channeltype4.vert to sepearate out natural
  #update this to get line breaks 
  levels(sub2.vert$channeltype3.vert) <- gsub(" ", "\n", levels(sub2.vert$channeltype3.vert))
  
  
  #summary of median values for each suscept/channel type category
  sub2.vert$csci <- as.numeric(sub2.vert$csci)
  sub2.vert.median <- data.frame(aggregate(sub2.vert, by = sub2.vert[c('channeltype3.vert')], FUN=mean))
  unique(sub2.vert$channeltype3.vert )
  
  
  # create new column for Channel type3 and lateral suscept
  sub$channeltype3.lat <- paste0(sub$AverageLatSuscept, " ",sub$channeltype3 )
  sub$channeltype3.lat <- gsub("LOW", "Low", sub$channeltype3.lat )
  sub$channeltype3.lat <- gsub("MEDIUM", "Medium", sub$channeltype3.lat )
  sub$channeltype3.lat <- gsub("HIGH", "High", sub$channeltype3.lat )
  sub$channeltype3.lat <- gsub("VERY", "Very", sub$channeltype3.lat )
  #for natural/earthen parse out natural from earthen engineered
  ind.natural <- grep("Natural", sub$channeltype2)
  sub$channeltype3.lat[ind.natural] <- gsub("Natural & Earthen Engineered", "Natural",   sub$channeltype3.lat[ind.natural])
  ind.earthen <- grep("Earthen Engineered", sub$channeltype2)
  sub$channeltype3.lat[ind.earthen] <- gsub("Natural & Earthen Engineered", "Earthen Engineered",   sub$channeltype3.lat[ind.earthen])
  
  unique(sub$channeltype3.lat)
  
  # Exclude NA channel type and medium/high hardened sides(s), Unk
  #find indices of channel type NA and unk and exclude them
  ind.NA.lat <- which(is.na(sub$AverageLatSuscept))
  #ind.highhardsides.lat <- grep('High Hardened Side', as.character(sub$channeltype3.lat))
  #ind.medhardsides.lat <- grep("Medium Hardened Side", sub$channeltype3.lat)
  ind.unk.lat <- grep("Unk", sub$channeltype3.lat)
  #sub2.lat <- data.frame(sub[-c(ind.NA, ind.NA.lat, ind.highhardsides.lat, ind.medhardsides.lat, ind.unk.lat),])
  sub2.lat <- data.frame(sub[-c(ind.NA, ind.NA.lat, ind.unk.lat),])
  unique(sub2.lat$channeltype3.lat)
  #create order of the categories for boxplots
  #sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural/Earthen","Medium Natural/Earthen","High Natural/Earthen","Very High Natural/Earthen"))
  sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Natural","Medium Natural","Medium Hardened Side(s)","High Natural","High Earthen Engineered","High Hardened Side(s)","Very High Natural", "Very High Earthen Engineered","Very High Hardened Side(s)"))
  #sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Medium Hardened Side(s)","High Hardened Side(s)","Low Natural","Medium Natural","High Natural","High Earthen Engineered","Very High Natural", "Very High Earthen Engineered","Very High Hardened Side(s)"))
  
    #update this to get line breaks 
  levels(sub2.lat$channeltype3.lat) <- gsub(" ", "\n", levels(sub2.lat$channeltype3.lat))
  
  #Boxplots CSCI
  
  #annotate total number of sites in each bin/category
  #vert.csci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.csci <- which(is.na(sub2.vert$csci))
  sub2.csci.vert <- data.frame(sub2.vert[-ind.NA.csci,])
  #ver channel type3
  vert.channeltype3 <- data.frame(aggregate(sub2.csci.vert, by = sub2.csci.vert[c('channeltype3.vert')], length))
  vert.channeltype3$count <- vert.channeltype3$SiteYear
  #annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.vert.csci <- data.frame(xstar = c(1:8), ystar = rep(0, 8),
                               lab = paste0("(",vert.channeltype3$count,")"))
  
  
  #CSCI vertical
  cv <- ggplot(sub2.csci.vert, aes(x=channeltype3.vert, y=csci, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    ggtitle("CSCI vs. Vertical Susceptibility") + 
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.79, linetype="dashed", color = "black") +
    geom_text(aes(0,0.79, label=0.79, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    #geom_text(data = anno.vert.csci, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
    scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  cv
  
  ggsave(cv, filename="cv.jpg", dpi=300, height=4, width=8)

  
   #CSCI lateral
  #set levels for channeltype3.lat
  levels(sub2.csci.lat$channeltype3.lat)
  new.levels<- c("Low\nNatural","Medium\nNatural","High\nNatural","Very\nHigh\nNatural",
                 "Low\nHardened\nEntire","Low\nHardened\nSide(s)","Medium\nHardened\nSide(s)","High\nHardened\nSide(s)","Very\nHigh\nHardened\nSide(s)",
                 "High\nEarthen\nEngineered",
                 "Very\nHigh\nEarthen\nEngineered")
  sub2.csci.lat$channeltype3.lat <- factor(sub2.csci.lat$channeltype3.lat, levels=new.levels)
  #lat.csci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.csci.lat <- which(is.na(sub2.lat$csci))
  sub2.csci.lat <- data.frame(sub2.lat[-ind.NA.csci.lat,])
  #lat channel type 3
  lat.channeltype3 <- data.frame(aggregate(sub2.csci.lat, by = sub2.csci.lat[c('channeltype3.lat')], length))
  lat.channeltype3$count <- lat.channeltype3$SiteYear
  
  #summary of total number of sites in each bin
  vert.csci.length <- aggregate(csci ~  channeltype3.vert, sub2.csci.vert, length)
  lat.csci.length <- aggregate(csci ~  channeltype3.lat, sub2.csci.lat, length)
  
  #annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.lat.csci <- data.frame(xstar = c(1:4,1:6), ystar = rep(0, 10),
                              lab = paste0("(",lat.csci.length$csci,")"),
                              channeltype3.lat = lat.csci.length$channeltype3.lat)
  
  
  cl <- ggplot(sub2.csci.lat, aes(x=channeltype3.lat, y=csci, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.79, linetype="dashed", color = "grey41") +
    geom_text(aes(0,0.79, label=0.79, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    #scale_y_continuous(breaks=c(0.2, 0.4, 0.6, 0.79, 1)) +
    #geom_text(data = anno.lat.csci, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
    ggtitle("CSCI vs. Lateral Susceptibility") +
    scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  cl
  
  ggsave(cl, filename="cl.jpg", dpi=300, height=4, width=8)
  
  
  #summary median values in boxplots
  vert.csci.med <- aggregate(csci ~  channeltype3.vert, sub2.csci.vert, median)
  lat.csci.med <- aggregate(csci ~  channeltype3.lat, sub2.csci.lat, median)
  
  #why very high natural sites with low scores
  nat.vhigh.lat <- sub2.csci.lat[sub2.csci.lat$channeltype == "Natural" & sub2.csci.lat$av.lat.rating == 4,]
  write.csv(nat.vhigh.lat, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SMC_hydromod/data/nat.vhigh.lat.csv")
  #Boxplots ASCI
  
  #annotate total number of sites in each bin/category
  #vert.asci site counts for each category
  #omit all sites that do not have CSCI values
  ind.NA.asci <- which(is.na(sub2.vert$ASCI.hybrid))
  sub2.asci.vert <- data.frame(sub2.vert[-ind.NA.asci,])
  #vert channel type 3
  vert.channeltype3.asci <- data.frame(aggregate(sub2.asci.vert, by = sub2.asci.vert[c('channeltype3.vert')], length))
  vert.channeltype3.asci$count <- vert.channeltype3.asci$SiteYear
  #annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.vert.asci <- data.frame(xstar = c(1:5), ystar = rep(0, 5),
                               lab = c(vert.channeltype3.asci$count))
  #lat.asci site counts for each category
  #omit all sites that do not have ASCI values
  ind.NA.asci.lat <- which(is.na(sub2.lat$ASCI.hybrid))
  sub2.asci.lat <- data.frame(sub2.lat[-ind.NA.asci.lat,])
  #lat channel type 3
  lat.channeltype3.asci <- data.frame(aggregate(sub2.asci.lat, by = sub2.asci.lat[c('channeltype3.lat')], length))
  lat.channeltype3.asci$count <- lat.channeltype3.asci$SiteYear
  #annotate the total number of sites in each bin outside of plot area, will use geom_text() and coord_cartesian(clip = "off")
  anno.lat.asci <- data.frame(xstar = c(1:6), ystar = rep(0, 6),
                              lab = c(lat.channeltype3.asci$count))
  #levels for lat asci, since missing med hardened and high hardened
  unique(sub2.asci.lat$channeltype3.lat)
  levels<- levels(sub2.asci.lat$channeltype3.lat)
  new.levels <- c(levels[1:4],levels[6:7],levels[9:11])
  sub2.asci.lat$channeltype3.lat <- factor(sub2.asci.lat$channeltype3.lat, levels= new.levels)
  #set levels for channeltype3.vert asci
  levels(sub2.asci.vert$channeltype3.vert)
  new.levels.asci.vert<- c("Low\nNatural","Medium\nNatural","High\nNatural",
                 "Low\nHardened\nEntire","Low\nHardened\nSide(s)","Medium\nHardened\nSide(s)","High\nHardened\nSide(s)",
                 "High\nEarthen\nEngineered")
  sub2.asci.vert$channeltype3.vert <- factor(sub2.asci.vert$channeltype3.vert, levels=new.levels.asci.vert)
  
  
  #ASCI vertical boxplots
  av <- ggplot(sub2.asci.vert, aes(x=channeltype3.vert, y=ASCI.hybrid, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.88, linetype="dashed", color = "black") +
    geom_text(aes(0,0.88, label=0.88, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    ggtitle("ASCI vs. Vertical Susceptibility") +
    scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  av
  
  ggsave(av, filename="av.jpg", dpi=300, height=4, width=8)
  
  
  #ASCI lateral boxplots
  al <- ggplot(sub2.asci.lat, aes(x=channeltype3.lat, y=ASCI.hybrid, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.88, linetype="dashed", color = "black") +
    geom_text(aes(0,0.88, label=0.88, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    ggtitle("ASCI vs. Lateral Susceptibility") +
    scale_fill_manual(name = "Lateral Susceptibility", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  al
  
  ggsave(al, filename="al.jpg", dpi=300, height=4, width=8)
  
  

  
  
  
  
  
  ##ADDITIONAL ASCI Plots to see if different when using softbodied, hybrid, diatoms
  #ASCI vertical boxplots --> diatoms
  avd <- ggplot(sub2.asci.vert, aes(x=channeltype3.vert, y=ASCI.diatoms, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Diatoms Score") +
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.82, linetype="dashed", color = "black") +
    geom_text(aes(0,0.82, label=0.82, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    ggtitle("ASCI Diatoms vs. Vertical Susceptibility") +
    scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  avd
  
  #ASCI vertical boxplots --> soft bodied
  avd <- ggplot(sub2.asci.vert, aes(x=channeltype3.vert, y=ASCI.sba, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Soft-Bodied Score") +
    facet_grid(~factor(channeltype, levels=c("Natural", "Engineered")), scales = "free", space = "free")+
    theme(legend.position="bottom") +
    geom_hline(yintercept=0.80, linetype="dashed", color = "black") +
    geom_text(aes(0,0.80, label=0.80, vjust=-0.7, hjust=-0.2), size=3, color="grey41") +
    ggtitle("ASCI Soft-Bodied vs. Vertical Susceptibility") +
    scale_fill_manual(name = "Vertical Susceptibility", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  avd
  
  
  #summary median values in boxplots for diatoms and sba
  vert.asci.med.sba <- aggregate(ASCI.sba ~  channeltype3.vert, sub2.asci.vert, median)
  lat.asci.med.sba <- aggregate(ASCI.sba ~  channeltype3.lat, sub2.asci.lat, median)
  vert.asci.length.sba <- aggregate(ASCI.sba ~  channeltype3.vert, sub2.asci.vert, length)
  lat.asci.length.sba <- aggregate(ASCI.sba ~  channeltype3.lat, sub2.asci.lat, length)
  
  
#test sig diff between natural low/med vs. high vertical ASCI
  #subset of natural only
  sub.nat <- sub2.asci.vert[sub2.asci.vert$channeltype == "Natural",]
  sub.nat$vert.combine <- sub.nat$channeltype3.vert
  sub.nat$vert.combine <- gsub("Low\nNatural", "Low.Medium", sub.nat$vert.combine)
  sub.nat$vert.combine <- gsub("Medium\nNatural", "Low.Medium", sub.nat$vert.combine)
  
  p <- ggplot(sub.nat, aes(x = vert.combine, y = ASCI.hybrid)) + geom_boxplot() + stat_compare_means(method = "t.test")
#test sig diff between natural low vs veryhigh lat ASCI  
  #subset of natural only
  sub.nat <- sub2.asci.lat[sub2.asci.lat$channeltype == "Natural",]
  sub.nat$lat.combine <- sub.nat$channeltype3.lat
  sub.nat$lat.combine <- gsub("Low\nNatural", "Low", sub.nat$lat.combine)
  sub.nat$lat.combine <- gsub("Very\nHigh\nNatural", "VHigh", sub.nat$lat.combine)
  sub.nat.asci.lat.low.vhigh <- sub.nat[sub.nat$lat.combine == "Low" | sub.nat$lat.combine == "VHigh",]
  
  p <- ggplot(sub.nat.asci.lat.low.vhigh, aes(x = lat.combine, y = ASCI.hybrid)) + geom_boxplot() + stat_compare_means(method = "t.test")
  p
  
  
  #test sig diff between natural low/med vs. high/very high CSCI lateral categores
  #subset of natural only
  sub.nat.csci.lat <- sub2.csci.lat[sub2.csci.lat$channeltype == "Natural",]
  sub.nat.csci.lat$lat.combine <- sub.nat.csci.lat$channeltype3.lat
  sub.nat.csci.lat$lat.combine <- gsub("Low\nNatural", "Low.Medium", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine <- gsub("Medium\nNatural", "Low.Medium", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine <- gsub("Very\nHigh\nNatural", "High.VHigh", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  sub.nat.csci.lat$lat.combine <- gsub("High\nNatural", "High.VHigh", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  
  p <- ggplot(sub.nat.csci.lat, aes(x = lat.combine, y = csci)) + geom_boxplot() + stat_compare_means(method = "t.test")
  p
  
  #test sig diff between natural low vs. very high CSCI lateral categores
  #subset of natural only
  sub.nat.csci.lat <- sub2.csci.lat[sub2.csci.lat$channeltype == "Natural",]
  sub.nat.csci.lat$lat.combine2 <- sub.nat.csci.lat$channeltype3.lat
  sub.nat.csci.lat$lat.combine2 <- gsub("Low\nNatural", "Low", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine2 <- gsub("Medium\nNatural", "Medium", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine2 <- gsub("Very\nHigh\nNatural", "VHigh", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  sub.nat.csci.lat$lat.combine2 <- gsub("High\nNatural", "High", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  sub.nat.csci.lat.low.vhigh <- sub.nat.csci.lat[sub.nat.csci.lat$lat.combine2 == "Low" | sub.nat.csci.lat$lat.combine2 == "VHigh",]
  
  p <- ggplot(sub.nat.csci.lat.low.vhigh, aes(x = lat.combine2, y = csci)) + geom_boxplot() + stat_compare_means(method = "t.test")
  p
  
  #test sig diff between natural low/medium vs. very high CSCI lateral categores
  #subset of natural only
  sub.nat.csci.lat <- sub2.csci.lat[sub2.csci.lat$channeltype == "Natural",]
  sub.nat.csci.lat$lat.combine3 <- sub.nat.csci.lat$channeltype3.lat
  sub.nat.csci.lat$lat.combine3 <- gsub("Low\nNatural", "Low.Medium", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine3 <- gsub("Medium\nNatural", "Low.Medium", sub.nat.csci.lat$lat.combine)
  sub.nat.csci.lat$lat.combine3 <- gsub("Very\nHigh\nNatural", "VHigh", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  sub.nat.csci.lat$lat.combine3 <- gsub("High\nNatural", "High", sub.nat.csci.lat$lat.combine, fixed=TRUE)
  sub.nat.csci.lat.low.vhigh <- sub.nat.csci.lat[sub.nat.csci.lat$lat.combine3 == "Low.Medium" | sub.nat.csci.lat$lat.combine3 == "VHigh",]
  
  p <- ggplot(sub.nat.csci.lat.low.vhigh, aes(x = lat.combine3, y = csci)) + geom_boxplot() + stat_compare_means(method = "t.test")
  p
  
  
  sub9 <- sub2.asci.vert[sub2.asci.vert$channeltype3.vert == "Low\nHardened\nSide(s)",]
  

  
  fname <- paste0("P:/KrisTaniguchi/Hydromod_SMC/Data/", "sub2.asci.vert.csv")
   write.csv(sub2.asci.vert, fname, row.names=FALSE)

  sub9 <- sub2.asci.vert[sub2.asci.vert$channeltype3.vert == "Low\nHardened\nSide(s)",]








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

