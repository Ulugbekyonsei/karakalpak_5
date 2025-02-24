
rm(list = ls())
library(blackmarbler)
library(geodata)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(lubridate)
library(raster)

# Load the data

bearer <- get_nasa_token(username = "Ulugbek1992", 
                         password = "Ancb?J)VT@UX5Xq")
path<-"G:/My Drive/VAC/shapefiles/uzb_admbnda_adm2_2018b"

setwd(path)


bm_raster<-rast("bm_raster.tif")

# Load the new shapefile
new_shape <- st_read("G:/My Drive/VAC/shapefiles/mahalla_shp/mahalla.shp")

# Ensure the new shapefile has the same CRS as the bm_raster
# If not, transform it to match the raster's CRS

m_shape <- st_transform(new_shape, crs(bm_raster))

# Convert the sf object to a SpatVector for terra compatibility

new_vect <- vect(m_shape)

# Crop the raster to the extent of the new shapefile
bm_raster_cropped <- crop(bm_raster, new_vect)

# Mask the cropped raster to exactly match the new shapefile boundaries
bm_raster_new <- mask(bm_raster_cropped, new_vect)

# Write the new raster to a TIFF file
writeRaster(bm_raster_new, filename = "bmahalla_raster_newshape.tif", overwrite = TRUE)

plot(bm_raster_new)

###
colnames(m_shape)
table(m_shape$region_nam)
karakalpak_shape<-m_shape[m_shape$region_nam=="Qoraqalpog'iston Respublikasi",]

#st_write(karakalpak_shape, "karakalpak_shape.shp")

karakalpak_shape <- st_transform(karakalpak_shape, crs(bm_raster))

# Convert the sf object to a SpatVector for terra compatibility

new_vect <- vect(m_shape)

# Crop the raster to the extent of the new shapefile
bm_raster_cropped_karakalpak <- crop(bm_raster, karakalpak_shape)

# Mask the cropped raster to exactly match the new shapefile boundaries
bm_raster_karakalpak <- mask(bm_raster_cropped_karakalpak, new_vect)

# Write the new raster to a TIFF file
writeRaster(bm_raster_karakalpak, filename = "bmahalla_raster_karakalpak.tif", overwrite = TRUE)

#plot(bm_raster_karakalpak)

nlyr(bm_raster_karakalpak)

bm_karakalpak_2024 <- bm_raster_karakalpak[[37:48]]

original_layer_names<-names(bm_karakalpak_2024)

bm_karakalpak_2024_r <- raster(bm_karakalpak_2024)
library(terra)
library(raster)
bm_karakalpak_2024_brick <- brick(bm_karakalpak_2024)
# Now apply approxNA from the raster package
imputed_data <- approxNA(bm_karakalpak_2024_brick, method = "linear", rule = 2, f = 0)

writeRaster(imputed_data, filename = "imputed_data_karakalpak.tif", overwrite = TRUE)

karakalpak_vect <- vect(karakalpak_shape)

# Convert imputed_data (a RasterBrick from the raster package) to a SpatRaster:
imputed_data_terra <- rast(imputed_data)

# Calculate mean values (for example) for each polygon zone:
zonal_stats <- terra::extract(imputed_data_terra, karakalpak_vect, fun = mean, na.rm = TRUE)

# Combine the zonal statistics with the attribute 'mahalla_no'
# Note: The 'ID' column in zonal_stats corresponds to the polygon order in karakalpak_shape.
zonal_stats_combined <- cbind(as.data.frame(karakalpak_shape), zonal_stats[,-1])

# View the result:
#print(zonal_stats_combined)


#colnames(zonal_stats_combined)

# Create a new variable 'avg_2024' by averaging the specified monthly columns
zonal_stats_combined$avg_2024 <- rowMeans(
  zonal_stats_combined[, c("t2024_01", "t2024_02", "t2024_03", "t2024_04", 
                           "t2024_05", "t2024_06", "t2024_07", "t2024_08", 
                           "t2024_09", "t2024_10", "t2024_11", "t2024_12")],
  na.rm = TRUE
)

summary(zonal_stats_combined$avg_2024)

#normalize zonal_stats_combined$avg_2024
min_val <- min(zonal_stats_combined$avg_2024, na.rm = TRUE)
max_val <- max(zonal_stats_combined$avg_2024, na.rm = TRUE)

zonal_stats_combined$avg_2024_norm <- (zonal_stats_combined$avg_2024 - min_val) / (max_val - min_val)

# Check the summary of the normalized values
summary(zonal_stats_combined$avg_2024_norm)


zonal_stats_combined_withoutna<-zonal_stats_combined[!is.na(zonal_stats_combined$avg_2024_norm),]

setwd("G:/My Drive/VAC/shapefiles/mahalla_shp")

###read csv file
mahalla_data<-read.csv("ndvi_polygons.csv")

colnames(mahalla_data)
colnames(zonal_stats_combined_withoutna)



#rename zonal_stats_combined_withoutna$MFY_ID to zonal_stats_combined_withoutna$mfy_id
colnames(zonal_stats_combined_withoutna)[colnames(zonal_stats_combined_withoutna)=="MFY_ID"]<-"mfy_id"
library(dplyr)

data_use <- merge(mahalla_data, dplyr::select(zonal_stats_combined_withoutna, avg_2024, avg_2024_norm, mfy_id), by = "mfy_id")


###correlation between data_use$avg_2024_norm and data_use$NDVI_mean

cor(data_use$avg_2024_norm, data_use$NDVI_mean, use="complete.obs")
cor(data_use$avg_2024_norm, data_use$density, use="complete.obs")

hist(data_use$avg_2024_norm)
hist(data_use$NDVI_mean)

data_use$avg_2024_log<-log(data_use$avg_2024_norm)
hist(data_use$avg_2024_log)
data_use$NDVI_mean_log<-log(data_use$NDVI_mean)
hist(data_use$NDVI_mean_log)

while (!is.null(dev.list())) dev.off()

#scatterplot
ggplot(data_use, aes(x=avg_2024_log, y=NDVI_mean_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of NDVI_mean and avg_2024_norm",
       x = "avg_2024_norm",
       y = "NDVI_mean")

lm1<-lm(density~NDVI_mean+avg_2024, data=data_use)
summary(lm1)




colnames(data_use)

# Install if needed:
#install.packages("scatterplot3d")

library(scatterplot3d)

# Create a 3D scatter plot
s3d <- scatterplot3d(x = data_use$NDVI_mean_1,
                     y = data_use$avg_2024,
                     z = data_use$density,
                     xlab = "NDVI Mean",
                     ylab = "Average 2024",
                     zlab = "Density",
                     main = "3D Scatter Plot with Regression Plane",
                     pch = 16,             # marker type
                     highlight.3d = TRUE)

# Add the regression plane from your linear model (lm1)
s3d$plane3d(lm1, 
            draw_polygon = TRUE, 
            polygon_args = list(col = rgb(0.1, 0.1, 0.8, 0.5)))

# Install and load necessary packages if you haven't already
# install.packages("factoextra")


# Load required libraries
library(plotly)

# Select the relevant variables
complete_idx <- complete.cases(data_use[, c("NDVI_mean_1", "avg_2024", "density")])

# Create a new dataframe with only complete cases
data_use_clean <- data_use[complete_idx, ]

# Select the variables for clustering
vars <- data_use_clean[, c("NDVI_mean_1", "avg_2024", "density")]

# Scale the data
vars_scaled <- scale(vars)

# Set the number of clusters (adjust k as needed)
set.seed(123)  # for reproducibility
k <- 3

kmeans_result <- kmeans(vars_scaled, centers = k)

# Add cluster assignments to the clean dataframe
data_use_clean$cluster <- as.factor(kmeans_result$cluster)

# Optionally, if you want to merge these cluster results back into your original data,
# you can use the row indices from complete_idx. For example:
data_use$cluster <- NA  # initialize the column
data_use$cluster[complete_idx] <- data_use_clean$cluster

# Create an interactive 3D scatterplot with plotly
fig <- plot_ly(data = data_use,
               x = ~NDVI_mean_1,
               y = ~avg_2024,
               z = ~density,
               color = ~cluster,
               colors = c("green", "blue", "red"),
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 5))

# Customize layout and axis labels
fig <- fig %>% layout(title = "",
                      scene = list(
                        xaxis = list(title = "Vegitatsiya indeksi"),
                        yaxis = list(title = "Tungi yorug'lik darajasi"),
                        zaxis = list(title = "Aholi zichligi")
                      ))

# Display the plot
fig




library(sf)
library(ggplot2)

# Convert your original shapefile object to an sf object (if needed)
karakalpak_sf <- st_as_sf(karakalpak_shape)

colnames(karakalpak_sf)
##rename karakalpak_sf$MFY_ID to karakalpak_sf$mfy_id

colnames(karakalpak_sf)[colnames(karakalpak_sf)=="MFY_ID"]<-"mfy_id"

#create karakalpak_sf without NA values in OBJECTID

karakalpak_sf<-karakalpak_sf[!is.na(karakalpak_sf$OBJECTID),]


# Merge the spatial data with the clustering results from data_use.
# Make sure that both data frames share the common id column "mfy_id"
karakalpak_sf_clustered <- merge(karakalpak_sf, data_use[, c("mfy_id", "cluster")],
                                 by = "mfy_id")
karakalpak_sf_clustered$cluster <- as.factor(karakalpak_sf_clustered$cluster)
# Plot the map, coloring regions by their cluster group.
# Create the plot and assign it to a variable
p <- ggplot(karakalpak_sf_clustered) +
  geom_sf(aes(fill = cluster), color = "black") +
  scale_fill_manual(
    values = c("green", "blue", "red"), 
    name = "Klaster",
    na.translate = FALSE
  ) +
  labs(title = "Klasterlarning xaritada ko'rinishi") +
  theme_minimal()
# Save the plot as a high-resolution image (300 dpi)
ggsave("high_res_cluster_map.png", plot = p, width = 10, height = 8, dpi = 1200)

p

library(rsconnect)
rsconnect::setAccountInfo(name = 'yxs96l-ulugbek-tursunov',
                          token = '678A3801E2EAA498A1A0F0A8D2E3A455',
                          secret = 'ufc0oUMG11NOKH0TWa/AU2zgGi2d016LKSOejXQw')


str(mahalla_data$population)
mahalla_data$population <- as.numeric(gsub(",", "", as.character(mahalla_data$population)))

num_NA <- sum(is.na(mahalla_data$population))
print(num_NA)


colnames(mahalla_data)

#mahalla_data$ratio<-mahalla_data$social_d_5/mahalla_data$population

# Ensure population is numeric (removing commas if necessary)
mahalla_data$population <- as.numeric(gsub(",", "", as.character(mahalla_data$population)))

# Convert social_d_5 to numeric (in case it is not already)
mahalla_data$social_d_5 <- as.numeric(as.character(mahalla_data$social_d_5))

# Now calculate the ratio
mahalla_data$ratio <- mahalla_data$social_d_5 / mahalla_data$population

num_NA <- sum(is.na(mahalla_data$ratio))
print(num_NA)

# Check the summary statistics of the ratio
summary(mahalla_data$ratio)




mahalla_data<-merge(mahalla_data, zonal_stats_combined_withoutna%>%select(avg_2024,avg_2024_norm, mfy_id),
                by="mfy_id")




mahalla_data_sub <- subset(mahalla_data, avg_2024_norm > 0)
mahalla_data_sub$avg_2024_log<-log(mahalla_data_sub$avg_2024_norm)
lm1<-lm(ratio~density+area+NDVI_mean+avg_2024_log+population, data=mahalla_data_sub)
summary(lm1)



# Load necessary library
library(ggplot2)

# Create a new data frame for predictions:
# We'll vary avg_2024 over its range and set density and NDVI_mean to their mean values.
# Create a new data frame for predictions with the correct variable name
newdata <- data.frame(
  avg_2024_log = seq(min(mahalla_data_sub$avg_2024_log, na.rm = TRUE), 
                     max(mahalla_data_sub$avg_2024_log, na.rm = TRUE), 
                     length.out = 100),
  density = mean(mahalla_data_sub$density, na.rm = TRUE),
  NDVI_mean = mean(mahalla_data_sub$NDVI_mean, na.rm = TRUE)
)

# Get predictions from the model with a 95% confidence interval.
#predictions <- predict(lm1, newdata, interval = "confidence", level = 0.999)


# Combine the predictions with the new data.
#pred_df <- cbind(newdata, predictions)

# Plot the original data and add the regression line with its 95% confidence interval.
#ggplot(mahalla_data_sub, aes(x = avg_2024_log, y = ratio)) +
 # geom_point(alpha = 0.6) +
  #geom_line(data = pred_df, aes(x = avg_2024_log, y = fit), color = "blue", linewidth = 1) +
  #geom_ribbon(data = pred_df, aes(x = avg_2024_log, ymin = lwr, ymax = upr), 
   #           inherit.aes = FALSE, alpha = 0.2, fill = "blue") +
  #labs(
   # title = "Regression of Ratio on avg_2024",
    #subtitle = "Holding density and NDVI_mean constant at their means",
    #x = "avg_2024_log",
    #y = "Ratio"
  #) +
  #theme_minimal()
#colnames(mahalla_data_sub)



library(ggplot2)
library(plotly)

# Build the plot with separate layers:
p <- ggplot() +
  # Points with tooltip from mahalla_data_sub
  geom_point(data = mahalla_data_sub, 
             aes(x = avg_2024_log, y = ratio, text = mahalla_no),
             alpha = 0.6) +
  # Regression line from pred_df (do not inherit global aesthetics)
  geom_line(data = pred_df, 
            aes(x = avg_2024_log, y = fit),
            color = "blue", linewidth = 1, inherit.aes = FALSE) +
  # Confidence interval ribbon from pred_df (do not inherit global aesthetics)
  geom_ribbon(data = pred_df, 
              aes(x = avg_2024_log, ymin = lwr, ymax = upr),
              inherit.aes = FALSE, alpha = 0.2, fill = "blue") +
  labs(
    title = "Regression of Ratio on avg_2024",
    subtitle = "Holding density and NDVI_mean constant at their means",
    x = "avg_2024 (log scale)",
    y = "Ratio"
  ) +
  theme_minimal()

# Convert the ggplot object into an interactive Plotly graph with tooltip displaying mahalla_no
reg1<-ggplotly(p, tooltip = "text")
reg1

# Load necessary libraries
library(plotly)

# Extract the relevant variables and remove any rows with missing values
data_cluster <- mahalla_data_sub[, c("ratio", "NDVI_mean_1", "avg_2024_log")]
data_cluster <- na.omit(data_cluster)

# Create a logical index for rows with complete data for the clustering variables
complete_rows <- complete.cases(mahalla_data_sub[, c("ratio", "NDVI_mean_1", "avg_2024_log")])

# Subset the data using the complete_rows index
data_cluster <- mahalla_data_sub[complete_rows, c("ratio", "NDVI_mean_1", "avg_2024_log")]

# Optionally, scale the variables so that each contributes equally
data_cluster_scaled <- scale(data_cluster)

# Perform k-means clustering with 3 clusters (set seed for reproducibility)
set.seed(123)
kmeans_result <- kmeans(data_cluster_scaled, centers = 3)

# Assign the cluster labels back to the original dataframe for the rows with complete data
mahalla_data_sub$cluster <- NA  # initialize the cluster column
mahalla_data_sub$cluster[complete_rows] <- as.factor(kmeans_result$cluster)
# Install and load factoextra if not already installed
if (!require("factoextra")) {
  install.packages("factoextra")
  library(factoextra)
}

# Using the elbow method (within-cluster sum of squares)
set.seed(123)
fviz_nbclust(data_cluster_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method")

# Using the silhouette method
fviz_nbclust(data_cluster_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method")

# Alternatively, using the gap statistic method
fviz_nbclust(data_cluster_scaled, kmeans, nstart = 25, method = "gap_stat") +
  labs(title = "Gap Statistic")



set.seed(123)
kmeans_result <- kmeans(data_cluster_scaled, centers = 5)

# Assign the cluster labels back to the original dataframe for the rows with complete data
mahalla_data_sub$cluster <- NA  # initialize the cluster column
mahalla_data_sub$cluster[complete_rows] <- as.factor(kmeans_result$cluster)


library(plotly)

# Create a subset for plotting that only contains rows with clustering information
plot_data <- mahalla_data_sub[complete_rows, ]

# Create an interactive 3D scatter plot
p_1 <- plot_ly(data = plot_data,
             x = ~ratio,
             y = ~NDVI_mean_1,
             z = ~avg_2024_log,
             color = ~cluster,
             colors = c("red", "green", "blue"),
             type = "scatter3d",
             mode = "markers",
             marker = list(size = 5)) %>%
  layout(title = "3D Cluster Plot",
         scene = list(xaxis = list(title = "Nafaqa oluvchilar soni"),
                      yaxis = list(title = "Vegitatsiya indeksi"),
                      zaxis = list(title = "Yorug'lik darajasi")))
p_1




setwd("G:/My Drive/VAC/shapefiles/mahalla_shp")

##read xlsx file
data2<-readxl::read_xlsx("KR.xlsx")

colnames(data2)
colnames(mahalla_data)


data2<-data2%>%rename(mfy_id=id_1)

mahalla_data<-merge(mahalla_data, data2%>%select(mfy_id, family_count, oluvchi), by="mfy_id")



mahalla_data$share_allowance<-mahalla_data$oluvchi/mahalla_data$family_count



#number of NA s in mahalla_data$share_allowance
num_NA <- sum(is.na(mahalla_data$share_allowance))
print(num_NA)


colnames(mahalla_data)


lm1<-lm(share_allowance~NDVI_mean_1+avg_2024_norm+population, data=mahalla_data)
summary(lm1)























