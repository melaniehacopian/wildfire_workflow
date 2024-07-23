## *******************************************************
## Species Indicator Anaylsis  ###########################
## *******************************************************

library(indicspecies)

otu_metadata_merge <- metadata_filtered %>%
  left_join(otu_wide_w_id, by="sample_id")

otu_metadata_merge_grassland <- otu_metadata_merge %>%
  filter( biome == "grassland")

otu_metadata_merge_shrubland <- otu_metadata_merge %>%
  filter( biome == "shrubland")

### drought and control indicators 

# grassland 
grassland_otu_counts <-otu_metadata_merge_grassland[,7:ncol(otu_metadata_merge)]
treatment_vector_grass <- otu_metadata_merge_grassland$treatment
result_grass <- multipatt(grassland_otu_counts, treatment_vector_grass, func = "r.g", control = how(nperm=999))
summary(result_grass, alpha=0.01)

# shrubland
shrubland_otu_counts <-otu_metadata_merge_shrubland[,7:ncol(otu_metadata_merge)]
treatment_vector_shrub <- otu_metadata_merge_shrubland$treatment
result_shrub <- multipatt(shrubland_otu_counts, treatment_vector_shrub, func = "r.g", control = how(nperm=999))
summary(result_shrub, alpha=0.01)


### wildfire indicators 
# grassland 
time_grassland <- otu_metadata_merge_grassland %>%
  filter (! timepostfire == "2mo") %>%
  filter (! timepostfire == "4mo")
otu_counts_fire_grass <- time_grassland[,7:ncol(time_grassland)]
treatment_vector_fire_grass <- time_grassland$timepostfire
result_fire_grass <- multipatt(otu_counts_fire_grass, treatment_vector_fire_grass, func = "r.g", control = how(nperm=999))
summary(result_fire_grass, alpha = 0.01)

# shrubland
time_shrubland <- otu_metadata_merge_shrubland %>%
  filter (! timepostfire == "2mo") %>%
  filter (! timepostfire == "4mo")
otu_counts_fire_shrub <- time_shrubland[,7:ncol(time_shrubland)]
treatment_vector_fire_shrub <- time_shrubland$timepostfire
result_fire_shrub <- multipatt(otu_counts_fire_shrub, treatment_vector_fire_shrub, func = "r.g", control = how(nperm=999))
summary(result_fire_shrub)
