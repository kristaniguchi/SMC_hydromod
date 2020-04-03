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
    channeltype2[i] <- "Soft Entire"
  }else if((sub$bottom[i] == "Soft/Natural") & (sub$leftsideofstructure[i]  == "Concrete" | sub$leftsideofstructure[i]  == "Grouted rock" | sub$leftsideofstructure[i]  == "Rock") | (sub$rightsideofstructure[i]  == "Concrete" | sub$rightsideofstructure[i]  == "Grouted rock" | sub$rightsideofstructure[i]  == "Rock") ){
      channeltype2[i] <- "Hardened Side(s)"
  }else{
    channeltype2[i] <- NA
  }
}

#save new channel type vector into sub df
sub$channeltype2 <- channeltype2

#Also put natural and soft all into one category: Natural/Soft All for channeltype3
sub$channeltype3 <- channeltype2
sub$channeltype3[sub$channeltype3=="Soft Entire"] <- "Natural/Soft Entire"
sub$channeltype3[sub$channeltype3=="Natural"] <- "Natural/Soft Entire"

#Also create coarser category based on earthen vs. engineered
sub$channeltype4 <- sub$channeltype3
sub$channeltype4[sub$channeltype4=="Natural/Soft Entire"] <- "Earthen"
sub$channeltype4[sub$channeltype4=="Hardened Side(s)"] <- "Engineered"
sub$channeltype4[sub$channeltype4=="Hardened Entire"] <- "Engineered"



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
lu.channeltype4$lu.ch4.names <- c("Ag Earthen", "Ag Engineered", "Open Earthen", "Open Engineered", "Urban Earthen", "Urban Engineered")

#vertical by land use channel type *** use this for bar plots
#land use and channel type 4
vert.lu.channeltype4 <- data.frame(aggregate(sub, by = sub[c('smc_lu','channeltype4','vert.rating')], length))
vert.lu.channeltype4$count <- vert.lu.channeltype4$SiteYear
#exlcude SMC_out land use
vert.lu.channeltype4<- data.frame(vert.lu.channeltype4[vert.lu.channeltype4$smc_lu != "SMC_out",])
#create new name for lu channeltype4
vert.lu.channeltype4$vert.lu.ch4.names <- paste0(vert.lu.channeltype4$smc_lu, " ", vert.lu.channeltype4$channeltype4)
vert.lu.channeltype4$vert.lu.ch4.names <- gsub("Agricultural", "Ag", vert.lu.channeltype4$vert.lu.ch4.names)
  
#vertical by land use channel type *** use this for bar plots
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
                   channeltype4 = c("Earthen", "Earthen","Earthen","Engineered","Engineered","Engineered"))

#vertical suscept
ggplot(data = vert.lu.channeltype4) +
  geom_bar(aes(x = smc_lu, y = count, fill = factor(vert.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Vertical Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~channeltype4) +
  xlab("") + ylab("Proportion of Sites") +
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 

#lateral suscept
ggplot(data = lat.lu.channeltype4) +
  geom_bar(aes(x = smc_lu, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7) +
  ggtitle("Lateral Susceptibility") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~channeltype4) +
  xlab("") + ylab("Proportion of Sites") +  
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
  scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 

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
sub$channeltype3.vert <- gsub("Natural/Soft Entire", "Earthen", sub$channeltype3.vert )
sub$channeltype3.vert <- gsub("LOW", "Low", sub$channeltype3.vert )
sub$channeltype3.vert <- gsub("MEDIUM", "Medium", sub$channeltype3.vert )
# Exclude NA channel type and medium/high hardened sides(s), Unk
  #find indices of channel type NA and unk and exclude them
ind.NA <- which(is.na(sub$channeltype3))
ind.highhardsides <- grep('High Hardened Side', as.character(sub$channeltype3.vert))
ind.medhardsides <- grep("Medium Hardened Side", sub$channeltype3.vert)
ind.unk <- grep("Unk", sub$channeltype3.vert)
sub2.vert <- data.frame(sub[-c(ind.NA, ind.highhardsides, ind.medhardsides,ind.unk),])
#create order of the categories for boxplots
sub2.vert$channeltype3.vert <- factor(sub2.vert$channeltype3.vert, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Earthen","Medium Earthen","High Earthen"))


# create new column for Channel type3 and lateral suscept
sub$channeltype3.lat <- paste0(sub$AverageLatSuscept, " ",sub$channeltype3 )
sub$channeltype3.lat <- gsub("Natural/Soft Entire", "Earthen", sub$channeltype3.lat )
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
sub2.lat <- data.frame(sub[-c(ind.NA, ind.NA.lat, ind.highhardsides.lat, ind.medhardsides.lat,ind.unk.lat),])
  unique(sub2.lat$channeltype3.lat)
#create order of the categories for boxplots
  sub2.lat$channeltype3.lat <- factor(sub2.lat$channeltype3.lat, levels= c("Low Hardened Entire","Low Hardened Side(s)","Low Earthen","Medium Earthen","High Earthen","Very High Earthen"))
  
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
  anno.vert.csci <- data.frame(xstar = c(1:5), ystar = rep(0, 5),
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
  cv <- ggplot(sub2.vert, aes(x=channeltype3.vert, y=csci, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    ggtitle("CSCI vs. Vertical Susceptibility") + 
    #geom_text(data = anno.vert.csci, aes(x = xstar,  y = ystar, label = lab), size=3, vjust = 5.5) +  coord_cartesian(clip = "off") +
    scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  cv

  #CSCI lateral
  cl <- ggplot(sub2.lat, aes(x=channeltype3.lat, y=csci, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("CSCI Score") +
    ggtitle("CSCI vs. Lateral Susceptibility") +
    scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  cl
  
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
  av <- ggplot(sub2.vert, aes(x=channeltype3.vert, y=ASCI.hybrid, fill= factor(vert.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    ggtitle("ASCI vs. Vertical Susceptibility") +
    scale_fill_manual(name = "Vertical Suscept.", labels = c("Low", "Medium", "High"), values = c("green4","yellowgreen","orange1")) 
  av
  
  #ASCI lateral boxplots
  al <- ggplot(sub2.lat, aes(x=channeltype3.lat, y=ASCI.hybrid, fill= factor(av.lat.rating))) + 
    geom_boxplot()  + xlab("") + ylab("ASCI Score") +
    ggtitle("Lateral Susceptibility, ASCI") +
    scale_fill_manual(name = "Lateral Suscept.", labels = c("Low", "Medium", "High", "Very High"), values = c("green4","yellowgreen","orange1","red3")) 
  al
  

  
  
    
  plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], timeframe = label.years)) + 
    geom_point(mapping = aes(x = x, y = y, col=timeframe, size=.5)) +
    labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
    scale_size(guide=FALSE) + theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
    guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
  
  
  
  
  

  








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

