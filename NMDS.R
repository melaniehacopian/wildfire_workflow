## ******************************************************
## Analysis - NMDS ###################################### 
##*******************************************************

pal5<-(c( "#7F83CF", "#69A9E0", "#7FCFB2", "#edd68c", "#ed736b"))
pal4<-(c( "#69A9E0", "#7FCFB2", "#edd68c", "#ed736b"))
pal2<-(c( "#69A9E0", "#ed736b"))

#### START #####
otu_wide <-otu_wide %>%
  column_to_rownames("sample_id")

#Create merged data frame
merged_data <- metadata_filtered %>%
  left_join(otu_wide, by = "sample_id") %>%
  mutate(biome = factor(biome, levels= c("grassland", "shrubland")),
         treatment = factor(treatment, levels = c("control", "drought")),
         timepostfire = factor(timepostfire, levels = c("1mo", "2mo", "4mo", "8mo")),
         soildepth = factor(soildepth, levels = c("1_2", "3_4", "5_6", "7_8", "9_10"))) 

#merged data grassland
merged_data_grassland<- merged_data %>%
  filter(biome=="grassland")

#merged data shrubland
merged_data_shrubland<- merged_data %>%
  filter(biome=="shrubland")


#Load vegan
library("vegan")

#subset the dataframe on which to base the ordination (dataframe 1)
data_1 <- merged_data[,7:3043]
data_1_grassland<- merged_data_grassland[,7:3043]
data_1_shrubland<- merged_data_shrubland[,7:3043]


#sort(rowSums(data_1)) checks if there are any 0s 

#Identify the columns that contains the descriptive/environmental data (dataframe 2)
data_2 <- merged_data[,1:6]
data_2_grassland <- merged_data_grassland[,1:6]
data_2_shrubland <- merged_data_shrubland[,1:6]


#ordination by NMDS
NMDS <- metaMDS(data_1, distance = "bray", k = 3, trymax=20)
NMDS_grassland <-metaMDS(data_1_grassland, distance = "bray", k = 3, trymax=30)
NMDS_shrubland <-metaMDS(data_1_shrubland, distance = "bray", k = 3, trymax=20)

#Extract the axes scores

coordinates <- data.frame(NMDS$points[,1:3])
coordinates_grassland <-data.frame(NMDS_grassland$points[,1:3])
coordinates_shrubland <-data.frame(NMDS_shrubland$points[,1:3])

nmds_plus_metadata <- merge(coordinates, merged_data, by.x = "row.names", by.y = "row.names") %>%
  mutate(timepostfire = gsub("1mo","0.4", timepostfire)) %>%
  mutate(timepostfire = gsub("2mo","2", timepostfire)) %>%
  mutate(timepostfire = gsub("4mo","4", timepostfire)) %>%
  mutate(timepostfire = gsub("8mo","8", timepostfire)) 

nmds_plus_metadata_grassland <- merge(coordinates_grassland, merged_data_grassland, 
                                      by.x = "row.names", by.y = "row.names") %>%
  mutate(timepostfire = gsub("1mo","0.4", timepostfire)) %>%
  mutate(timepostfire = gsub("2mo","2", timepostfire)) %>%
  mutate(timepostfire = gsub("4mo","4", timepostfire)) %>%
  mutate(timepostfire = gsub("8mo","8", timepostfire)) 

nmds_plus_metadata_shrubland <- merge(coordinates_shrubland, merged_data_shrubland, 
                                      by.x = "row.names", by.y = "row.names")%>%
  mutate(timepostfire = gsub("1mo","0.4", timepostfire)) %>%
  mutate(timepostfire = gsub("2mo","2", timepostfire)) %>%
  mutate(timepostfire = gsub("4mo","4", timepostfire)) %>%
  mutate(timepostfire = gsub("8mo","8", timepostfire))


######################### NMDS 2D plotting -- TREATMENT and TIME POINT- all samples

## MDS1 and MDS2 

centroid<- nmds_plus_metadata %>%
  dplyr::group_by(treatment, timepostfire) %>%
  summarize(MDS1=mean(MDS1), MDS2=mean(MDS2), .groups="drop")


nmds_plot_time_12<-ggplot(nmds_plus_metadata, aes(x = MDS1, y = MDS2, shape=treatment, color=timepostfire)) +
  geom_point() +
  geom_point(data=centroid, mapping=aes(x=MDS1, y=MDS2, shape=treatment), size=5)+
  #coord_fixed()+                                              
  theme_bw()+ 
  labs(col = "Time after fire (months)")+
  labs(shape = "Treatment")+
  xlab("MDS1 (unitless)") +
  ylab("MDS2 (unitless)")+
  #ggtitle("NMDS of OTUs across treatment and ecosystem", 
  #subtitle = bquote("p(Treatment)<0.001, p(Ecosystem type)<0.001, p(Treatment*Ecosystem type)<0.001"))+
theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
        #axis.text.y = element_blank(), # remove y-axis text
        #axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  scale_color_manual(values=pal4)
nmds_plot_time_12


## grassland samples 

## MDS1 and MDS2 

centroid<- nmds_plus_metadata_grassland %>%
  dplyr::group_by(treatment, timepostfire) %>%
  summarize(MDS1=mean(MDS1), MDS2=mean(MDS2), .groups="drop")

nmds_treat_timepoint_grassland_12 <- ggplot(nmds_plus_metadata_grassland, aes(x = MDS1, y = MDS2, shape=treatment, color=timepostfire)) +
  geom_point() +
  geom_point(data=centroid, mapping=aes(x=MDS1, y=MDS2, shape=treatment), size=5)+
  #coord_fixed()+                                              
  theme_bw()+ 
  labs(col = "Time Post-Fire (months)")+
  labs(shape = "Treatment")+
  xlab("MDS1 (unitless)") +
  ylab("MDS2 (unitless)")+
  ggtitle("")+
  #ggtitle("Grassland NMDS across treatment and time point", 
          #subtitle = bquote("p(Treatment)<0.001, p(Time point)<0.001, p(Treatment*Time point)= 0.002"))+
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
        #axis.text.y = element_blank(), # remove y-axis text
        #axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=15), # remove x-axis labels
        axis.title.y = element_text(size=15), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  scale_color_manual(values=pal4
nmds_treat_timepoint_grassland_12

## shrubland samples 

## MDS1 and MDS2 

centroid<- nmds_plus_metadata_shrubland %>%
  group_by(treatment, timepostfire) %>%
  summarize(MDS1=mean(MDS1), MDS2=mean(MDS2), .groups="drop")

nmds_treat_timepoint_shrubland_12 <- ggplot(nmds_plus_metadata_shrubland, aes(x = MDS1, y = MDS2, shape=treatment, color=timepostfire)) +
  geom_point() +
  geom_point(data=centroid, mapping=aes(x=MDS1, y=MDS2, shape=treatment), size=5)+
  #coord_fixed()+                                              
  theme_bw()+ 
  labs(col = "Time Post-Fire (months)")+
  labs(shape = "Treatment")+
  xlab("MDS1 (unitless)") +
  ylab("MDS2 (unitless)") +
  ggtitle("")+
  #ggtitle("Shrubland NMDS across treatment and time point", 
          #subtitle = bquote("p(Treatment)<0.001, p(Time point)<0.001, p(Treatment*Time point) < 0.001"))+
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
        #axis.text.y = element_blank(), # remove y-axis text
        #axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=15), # remove x-axis labels
        axis.title.y = element_text(size=15), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  scale_color_manual(values=pal4)
nmds_treat_timepoint_shrubland_12 


