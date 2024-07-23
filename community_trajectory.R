## *******************************************************
## Community trajectory analysis  ########################
## *******************************************************

library(trend)
head(metadata_filtered)
head(otu_wide)

otu_wide <-otu_wide_no_x %>%
  column_to_rownames("sample_id")

otu_data <- as.matrix(otu_wide)

## *******************************************************
## Taxonomic community composition  ######################
## *******************************************************

# biome and treatment are changeable variables 

# Sort the data by timepoint 
metadata_filtered_biome_treat <- metadata_filtered %>%
  filter(biome == "shrubland")%>%
  filter(treatment == "control")

metadata_filtered_timesorted <- metadata_filtered_biome_treat[order(metadata_filtered_biome_treat$timepostfire), ]

# Extract the common row names between otu_data and metadata_filtered_timesorted
common_row_names <- intersect(rownames(otu_data), rownames(metadata_filtered_timesorted))

# Subset otu_data and metadata_filtered_timesorted to include only the common row names
otu_data_subset <- otu_data[common_row_names, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted[common_row_names, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted_subset[order(metadata_filtered_timesorted_subset$timepostfire), ]


variable_mk <- otu_data_subset[match(rownames(metadata_filtered_timesorted_subset), rownames(otu_data_subset)), ]


mk_TCC_dist <- vegdist(variable_mk, method = "bray")

mk.test(as.vector(mk_TCC_dist))

## *******************************************************
## Species indidcator trajectory analysis  ###############
## *******************************************************

grassland_indicators_otu_subset <- c("OTU237", "OTU5", "OTU336", "OTU265", "OTU800", "OTU177", "OTU73", "OTU186", "OTU163", "OTU11603", 
                                     "OTU215", "OTU8275", "OTU1", "OTU476", "OTU8406", "OTU33", "OTU109", "OTU70", "OTU565", "OTU1098",
                                     "OTU65", "OTU36", "OTU365", "OTU737", "OTU182", "OTU578", "OTU3", "OTU80", "OTU452", "OTU85",
                                     "OTU76", "OTU348", "OTU68", "OTU135", "OTU946", "OTU294", "OTU101", "OTU1650", "OTU401", "OTU427")

shrubland_indicators_otu_subset <- c(
                               "OTU450", "OTU674", "OTU78", "OTU447", "OTU24", "OTU19", "OTU359", "OTU133", "OTU433", "OTU6058",
                               "OTU277", "OTU619", "OTU165", "OTU1483", "OTU903", "OTU260", "OTU14", "OTU188", "OTU161", "OTU596",
                               "OTU85", "OTU214", "OTU126", "OTU7", "OTU15", "OTU654", "OTU877", "OTU76", "OTU401", "OTU832",
                               "OTU20", "OTU356", "OTU100", "OTU675", "OTU528", "OTU9331", "OTU241", "OTU901", "OTU338", "OTU546")

grassland_indicator_OTUs <- otu_data[, grassland_indicators_otu_subset]
shrubland_indicator_OTUs <- otu_data[, shrubland_indicators_otu_subset]

# Sort the data by time
metadata_filtered_biome_treat <- metadata_filtered %>%
  filter(biome == "shrubland")%>%
  filter(treatment == "drought")

metadata_filtered_timesorted <- metadata_filtered_biome_treat[order(metadata_filtered_biome_treat$timepostfire), ]

# Extract the common row names between otu_data and metadata_filtered_timesorted
common_row_names_indic <- intersect(rownames(shrubland_indicator_OTUs), rownames(metadata_filtered_timesorted))

# Subset otu_data and metadata_filtered_timesorted to include only the common row names
otu_data_subset <- shrubland_indicator_OTUs[common_row_names_indic, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted[common_row_names_indic, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted_subset[order(metadata_filtered_timesorted_subset$timepostfire), ]


variable_mk <- otu_data_subset[match(rownames(metadata_filtered_timesorted_subset), rownames(otu_data_subset)), ]


mk_SI_dist <- vegdist(variable_mk, method = "bray")

mk.test(as.vector(mk_SI_dist))


## *******************************************************
## Guild CC trajectory analysis  #########################
## *******************************************************

head(guild_permanova_values)
head(guild_permanova_metadata)

# Sort the data by timepoint 
metadata_filtered_biome_treat <- metadata_filtered %>%
  filter(biome == "shrubland")%>%
  filter(treatment == "drought")

metadata_filtered_timesorted <- metadata_filtered_biome_treat[order(metadata_filtered_biome_treat$timepostfire), ]

# Extract the common row names 
common_row_names <- intersect(rownames(guild_permanova_values), rownames(metadata_filtered_timesorted))

# Subset otu_data and metadata_filtered_timesorted to include only the common row names
guild_data_subset <- guild_permanova_values[common_row_names, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted[common_row_names, ]
metadata_filtered_timesorted_subset <- metadata_filtered_timesorted_subset[order(metadata_filtered_timesorted_subset$timepostfire), ]


variable_GCC <- guild_data_subset[match(rownames(metadata_filtered_timesorted_subset), rownames(guild_data_subset)), ]


mk_guild_dist <- vegdist(variable_GCC, method = "bray")

mk.test(as.vector(mk_guild_dist))

