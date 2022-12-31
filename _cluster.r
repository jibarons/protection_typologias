# # Cluster analysis
# First two dimensions form MCA
data_clust <- tibble::tibble(as.data.frame(m_res$ind$coord)[,1:2])

# Explore if data is clusterable (takes time with n > 100!!)
hopking <- factoextra::get_clust_tendency(data_clust, n = 20, graph = FALSE) 
# n = 1000 return 0.9461962. To speed the script we set n = 20 and adjusted coef.
hopking <- hopking$hopkins_stat - (hopking$hopkins_stat - 0.9461962)

# Cluster analysis
## Hybrid K-mean and Hierarchical (first hclus, then kmean with hclust centers)
hk <- factoextra::hkmeans(data_clust, k = 3)
p_clust_hk <- factoextra::fviz_cluster(hk)
## silhouettes for Hkmean 
hk_k <- factoextra::eclust(data_clust, "kmeans", k = hk$centers, graph = FALSE)
p_silh_hk <- factoextra::fviz_silhouette(hk_k)
silh_hk <- dplyr::group_by(p_silh_hk$data, cluster) |>
    dplyr::summarise(n = dplyr::n(), silh = mean(sil_width))

## Calculate hierarchical
h <- factoextra::eclust(data_clust, "hclust", graph = FALSE, nboot =  5)
p_clust_h <- factoextra::fviz_cluster(h)
## silhuettes for H
p_silh_h <- factoextra::fviz_silhouette(h)
silh_h <- dplyr::group_by(p_silh_h$data, cluster) |>
    dplyr::summarise(n = dplyr::n(), silh = mean(sil_width))

# Linkcluster to cualitative data

# Get key geo indicators
hh_clust <- dplyr::select(indicators_data, uuid, strata_cat, oslo_cat, 
    westbank_cat, weights, area_cat)
# Join geo indicators with cluster data
hh_clust <- dplyr::bind_cols(hh_clust, p_clust_hk$data)
# Add geo key for map (locality code)
hh_clust <- dplyr::right_join(hh_data[, c("uuid", "locality_code")], hh_clust)


# Tablas de Cluster y riesgo de proteccion
## link protection risk and cluster to see how the match
hh_clust_risk <- dplyr::left_join(hh_clust, data_risk)

hh_clust_gov <- dplyr::count(hh_clust, westbank_cat, cluster, wt = weights) |>
    dplyr::group_by(cluster) |>
    dplyr::mutate(N = sum(n), p = n / N) |>
    dplyr::mutate(cname = dplyr::case_when(cluster == 1 ~ "Menor riesgo",
        cluster == 2 ~ "Riesgo en dignidad", cluster == 3 ~ "Riesgo en seguridad"))
hh_clust_clus <- dplyr::count(hh_clust, westbank_cat, cluster, wt = weights) |>
    dplyr::group_by(westbank_cat) |>
    dplyr::mutate(N = sum(n), p = n / N) |>
    dplyr::mutate(cname = dplyr::case_when(cluster == 1 ~ "Menor riesgo",
        cluster == 2 ~ "Riesgo en dignidad", cluster == 3 ~ "Riesgo en seguridad"))
hh_clust_area <- dplyr::count(hh_clust, area_cat, cluster, wt = weights) |>
    dplyr::group_by(area_cat) |>
    dplyr::mutate(N = sum(n), p = n / N) |>
    dplyr::mutate(cname = dplyr::case_when(cluster == 1 ~ "Menor riesgo",
        cluster == 2 ~ "Riesgo en dignidad", cluster == 3 ~ "Riesgo en seguridad"))


### GIS  procesing and matching interbases
# Join shapefile with cluster data
hh_clust_sf <- dplyr::inner_join( # Select only those that match in both
  comm_wb[, c("PCODE", "X", "Y")], hh_clust_risk, by = c("PCODE" = "locality_code") 
)
hh_clust_sf <- sf::st_as_sf(hh_clust_sf, coords = c("X", "Y"), crs = pcrs)
# hh_clust_isect <- sf::st_intersection(oslo, hh_clust_sf) # get CLASS .Take long to compute, saved to rds, read from there.
# writeRDS(hh_clust_isect, "data\\gis\\hh_clust_sf_intersected.rds")
# hh_clust_isect <- readRDS("data\\gis\\hh_clust_sf_intersected.rds")

# Format qualitative list of locations
pcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
loc_qual <- sf::st_as_sf(x = admins, coords = c("lat_y", "long_x"), crs = pcrs)
loc_qual_oslo <- sf::st_intersection(oslo, loc_qual)

loc_qual_oslo <- dplyr::mutate(loc_qual_oslo, 
    area_cat = dplyr::case_when( CLASS == "A" ~ "area_a_b", CLASS == "B" ~ "area_a_b", CLASS %in% c("C", "Nature Reserve") ~ "area_c", CLASS %in% c("H1", "H2") ~ "hebron_h2", TRUE ~ "ERROR!!!"), name = tolower(name))
loc_qual_oslo <- dplyr::select(loc_qual_oslo, cluster_id, westbank_cat = name, area_cat, CLASS)

# Join cluster hh and qual location at strata level
loc_qual_df <- as.data.frame(loc_qual_oslo)
hh_clust_df <- as.data.frame(hh_clust_sf)
hh_qual_clust <- dplyr::left_join(loc_qual_df, hh_clust_df, 
    by = c("area_cat", "westbank_cat"), suffix = c(".qual", ".clust"))

# Final data that join cluster for each qualitative community by estrata+governorate
loca_qual_clust <- dplyr::group_by(hh_qual_clust, area_cat, cluster_id, weights) |>
    dplyr::summarise(cluster_avg = weighted.mean(as.numeric(as.character(cluster)), weights)) |>
    dplyr::mutate(cluster_cat = dplyr::case_when(cluster_avg < 1.66 ~ 1,
        cluster_avg >= 1.66 & cluster_avg < 2.33  ~ 2, cluster_avg > 2.33 ~ 3))

loca_qual_clust <- dplyr::filter(loca_qual_clust, !is.na(cluster_avg))









# # Vornoi polygons approach
# loc_vor <- loc_clust_sf %>%
#   st_union %>%
#   st_voronoi(bOnlyEdges = FALSE) %>%
#   st_collection_extract %>%
#   st_as_sf %>%
#   st_join(loc_clust_sf)

# loc_vor_oslo <- sf::st_intersection(oslo, loc_vor)
# loc_vor_oslo <- dplyr::mutate(loc_vor_oslo, 
#     id = dplyr::row_number(), id = as.character(id))

# loc_vor_oslo_qual <- sf::st_intersection(loc_qual, loc_vor_oslo)

# pcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# loc_qual <- sf::st_as_sf(x = admins, coords = c("lat_y", "long_x"), crs = pcrs)

# tmap::tm_shape(loc_vor_oslo) +
# tmap::tm_polygons(col = "cluster_avg") +
# tmap::tm_shape(oslo)+
# tmap::tm_borders() +
#   tmap::tm_shape(loc_vor_oslo_qual) +
#   tmap::tm_symbols(size=0.1, col = "red") +
#   tmap::tm_shape(hh_clust_sf) +
#   tmap::tm_symbols(size=0.1, col = "black", alpha = 0.5) +
#   tmap::tm_layout(frame = FALSE) 


