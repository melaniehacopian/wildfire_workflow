## *******************************************************
## Analysis - Relative Abundance #########################
## *******************************************************

# Load libraries 

library(tidyverse)
library(ggplot)
library(plyr)

# color pallette

pal1<-(c( "#69A9E0", "#7FCFB2", "#7F83CF", "#252861", "#237910",
             "#F2ABCD", "#F2C5AB", "#EEF2AB", "#177876", "#913638",
             "#369167", "#913660", "#7CA3AD", "#AC284D", "#C695EA",
             "#95BEEA", "#EAC195", "#406553", "#169887", "#981627"))

## *******************************************************
## Relative abundance ~ Treatment ########################
## *******************************************************

### Relative abundances across treatment (fill = Phylum) (grassland and shrubland facet)

# Format data frame before calculating relative abundance 
otu_rel_abund_treat<-otu_long %>%
  left_join(metadata_filtered) %>%
  left_join(taxonomy_rarefied) %>%
  mutate(treatment = factor(treatment))%>%
  mutate(biome = gsub("grassland","Grassland", biome)) %>%
  mutate(biome = gsub("shrubland","Shrubland", biome))

# Calculate relative abundance

rel_abs_treat<- otu_rel_abund_treat %>%
  dplyr::group_by(biome, treatment) %>%
  mutate(relative_abundance = count/sum(count)*100)

# Plot relative abundance

relabundplot_treat_phylum<-ggplot(rel_abs_treat, aes(x=treatment, y=relative_abundance, fill=Phylum))+
  geom_col()+
  facet_grid(. ~ biome)+
  scale_y_continuous()+
  scale_x_discrete(labels=c("Ambient", "Drought"))+
  ylab("Relative Abundance (%)") +	
  xlab("Treatment")+
  ggtitle("")+
  scale_fill_manual(values=pal1)+
  theme_bw(base_size=15)


### Relative abundances across treatment (fill = Guild) (grassland and shrubland facet)


otu_rel_abund_treat<-otu_long %>%
  left_join(metadata_filtered) %>%
  left_join(taxonomy_rarefied) %>%
  mutate(treatment = factor(treatment))%>%
  mutate(biome = gsub("grassland","Grassland", biome)) %>%
  mutate(biome = gsub("shrubland","Shrubland", biome)) %>%
  dplyr::rename(Guild = trophicMode) 

rel_abs_treat<- otu_rel_abund_treat %>%
  group_by(treatment, biome) %>%
  mutate(relative_abundance = count/sum(count)*100)

relabundplot_treat_guild<-ggplot(rel_abs_treat, aes(x=treatment, y=relative_abundance, fill=Guild))+
  geom_col()+
  facet_grid(. ~ biome)+
  scale_y_continuous()+
  scale_x_discrete(labels=c("Ambient", "Drought"))+
  ylab("Relative Abundance (%)") +	
  xlab("Treatment")+
  ggtitle("")+
  scale_fill_manual(values=pal1)+
  theme_bw(base_size=15)

relabundplot_treat_guild

# Create multi-panel figure with guild and phylum

multi_abund_treat_fig<-ggarrange(relabundplot_treat_guild, relabundplot_treat_phylum,
                           labels = c("A", "B"),
                           vjust=1,
                           ncol = 1, nrow = 2, 
                           widths = c(3, 6),
                           common.legend = TRUE,
                           legend = "right")
multi_abund_treat_fig


