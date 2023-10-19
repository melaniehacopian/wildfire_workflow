# The following Rcode is what I used to produce a dataframe giving relative abundance for each of the three guilds wihin each sample. 

## *******************************************************
## Relative abundance/ Guild community composition  #####
## *******************************************************

otu_rel_abund<-otu_long %>%
  left_join(metadata_filtered) %>%
  left_join(taxonomy_rarefied) %>%
  mutate(biome = factor(biome))%>%
  filter(Guild == "Pathotroph" | trophicMode == "Saprotroph" | trophicMode == "Symbiotroph")

# Calculate relative abundance of guilds within each sample
rel_abs<- otu_rel_abund %>%
  group_by(sample_id) %>%
  mutate(relative_abundance = count/sum(count)*100)

# Sum relative abundances based on guild and sample
summarized_rel_abs <- rel_abs %>%
  group_by(sample_id, Guild ) %>%
  summarize(Total_Abundance = sum(relative_abundance))

# Pivot the summarized data to have guild-wise columns
sum_rel_abs <- summarized_rel_abs %>%
  pivot_wider(names_from = Guild, values_from = Total_Abundance)

# Join the summarized data with the original metadata
final_rel_abs_forPRIMER <- rel_abs %>%
  distinct(sample_id, .keep_all = TRUE) %>%
  select(-OTU, -`relative_abundance`, -count, -Kingdom, -Phylum, -Class,
         -Order, -Family, -Genus, -Species, -taxon, -taxonomicLevel, -Guild, 
         -guild, -trait) %>%
  left_join(sum_rel_abs, by = "sample_id")


