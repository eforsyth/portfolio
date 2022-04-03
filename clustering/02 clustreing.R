




model_cluster_filtered_sa2 <- data_filtering_sa2 %>% 
  left_join(data_clustering_sa2,
            by = "sa2") %>% 
  select(sa2, starts_with("C18_income"), geom) %>% 
  st_as_sf()

# isolate variables for clustering
# needs to be an sf object with variables
model_clustering_variables <- model_cluster_filtered_sa2 %>% 
  select(starts_with("C18_income"))


# define spatial weights --------------------------------------------------
# rgeoda implementation of the spdep nb + lw functions
queen_w <- queen_weights(data_clustering_sa2)
summary(queen_w)


# spatially constrained clustering ----------------------------------------
redcap_clusters <- redcap(k = 12,
                          w = queen_w,
                          df = model_clustering_variables,
                          method = "fullorder-wardlinkage",
                          scale_method = "raw") # data is already scaled so set to "raw", otherwise could have been scaled here

# check goodness of fit
redcap_clusters$`The ratio of between to total sum of squares`

# join clusters back to spatial data
results_redcap <- model_cluster_filtered_sa2 %>% 
  mutate(redcap_cluster = redcap_clusters$Clusters)

qtm(results_redcap_sa2,
    fill = 'redcap_cluster')


# bring all results together ----------------------------------------------
results_clustering <- results_kmeans %>% 
  left_join(as_tibble(results_redcap) %>% 
              select(SA2_ID, redcap_cluster),
            by = "SA2_ID") %>% 
  relocate(geom, .after = last_col())


#####
