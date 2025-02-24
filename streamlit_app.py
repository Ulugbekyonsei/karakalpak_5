import os
import streamlit as st
import pandas as pd
import geopandas as gpd
import fiona
import rasterio
from rasterio.mask import mask
import numpy as np
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import plotly.express as px
import matplotlib.pyplot as plt
import statsmodels.api as sm

st.title("Spatial Data Analysis App")

# ----------------------------
# 1. Data Loading and Preparation
# ----------------------------

# Define the root directory and build file paths
root_dir = r"G:\My Drive\Dashboards\karakalpak"
shapefile_path = os.path.join(root_dir, "mahalla.shp")
raster_path = os.path.join(root_dir, "imputed_data_karakalpak.tif")
csv_path = os.path.join(root_dir, "ndvi_polygons.csv")
excel_path = os.path.join(root_dir, "KR.xlsx")

# Load Excel file (data2)
data2 = pd.read_excel(excel_path)

# Load CSV file (mahalla_data)
mahalla_data = pd.read_csv(csv_path)

# Load shapefile using Fiona directly and create a GeoDataFrame
with fiona.open(shapefile_path) as src:
    features = list(src)
    crs = src.crs  # Get the coordinate reference system
gdf = gpd.GeoDataFrame.from_features(features, crs=crs)

# Open the raster to get its CRS and other properties
raster = rasterio.open(raster_path)

# Reproject the shapefile to match the raster’s CRS
gdf = gdf.to_crs(raster.crs)

# Filter for the region "Qoraqalpog'iston Respublikasi"
karakalpak_shape = gdf[gdf['region_nam'] == "Qoraqalpog'iston Respublikasi"]

# ----------------------------
# 2. Zonal Statistics Extraction Without rasterstats.zonal_stats
# ----------------------------

def get_polygon_means(geom, src):
    """
    Extracts the raster values for a given polygon geometry and returns the mean value for each band.
    """
    try:
        out_image, out_transform = mask(src, [geom], crop=True)
        # Replace nodata values with np.nan (if nodata is defined)
        if src.nodata is not None:
            out_image = np.where(out_image == src.nodata, np.nan, out_image)
        # Compute mean for each band (axis 1 and 2 are spatial dimensions)
        means = np.nanmean(out_image, axis=(1, 2))
        return means.tolist()
    except Exception as e:
        return [np.nan] * src.count

# Open the raster once and loop through the polygons
band_count = raster.count
# Create empty lists to store mean values for each band
band_means = {f"imputed_data_karakalpak_{i}": [] for i in range(1, band_count + 1)}

with rasterio.open(raster_path) as src:
    for geom in karakalpak_shape.geometry:
        means = get_polygon_means(geom, src)
        for i, mean_val in enumerate(means, start=1):
            band_means[f"imputed_data_karakalpak_{i}"].append(mean_val)

# Add the computed zonal stats to the GeoDataFrame
for band, values in band_means.items():
    karakalpak_shape[band] = values

# Calculate the average over months (assuming bands 1 to 12 correspond to monthly data)
band_columns = [f"imputed_data_karakalpak_{i}" for i in range(1, 13) if f"imputed_data_karakalpak_{i}" in karakalpak_shape.columns]
karakalpak_shape["avg_2024"] = karakalpak_shape[band_columns].mean(axis=1)

# Normalize avg_2024
min_val = karakalpak_shape["avg_2024"].min()
max_val = karakalpak_shape["avg_2024"].max()
karakalpak_shape["avg_2024_norm"] = (karakalpak_shape["avg_2024"] - min_val) / (max_val - min_val)

# Remove rows with missing normalized values
zonal_stats_df = karakalpak_shape.dropna(subset=["avg_2024_norm"]).copy()

# Ensure the common ID column is named "mfy_id"
if "MFY_ID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.rename(columns={"MFY_ID": "mfy_id"})

# ----------------------------
# 3. Merge Data Sources and Feature Engineering
# ----------------------------

# Convert mfy_id columns to string to avoid merge errors
mahalla_data["mfy_id"] = mahalla_data["mfy_id"].astype(str)
zonal_stats_df["mfy_id"] = zonal_stats_df["mfy_id"].astype(str)
# Rename id_1 to mfy_id in data2, then convert to string
data2 = data2.rename(columns={"id_1": "mfy_id"})
data2["mfy_id"] = data2["mfy_id"].astype(str)

# Merge mahalla_data with zonal stats (selecting the needed columns)
data_use = pd.merge(mahalla_data, zonal_stats_df[["mfy_id", "avg_2024", "avg_2024_norm"]], on="mfy_id", how="inner")

# Merge data2 with data_use (keeping only relevant columns)
data_use = pd.merge(data_use, data2[["mfy_id", "family_count", "oluvchi"]], on="mfy_id", how="inner")

# Create new variable: share_allowance = oluvchi / family_count
data_use["share_allowance"] = data_use["oluvchi"] / data_use["family_count"]

# ----------------------------
# 4. Exploratory Analysis: Correlations and Histograms
# ----------------------------

st.subheader("Correlation Analysis")
corr_ndvi = data_use["avg_2024_norm"].corr(data_use["NDVI_mean"])
corr_density = data_use["avg_2024_norm"].corr(data_use["density"])
st.write("Correlation between avg_2024_norm and NDVI_mean:", corr_ndvi)
st.write("Correlation between avg_2024_norm and density:", corr_density)

# Log transformations (avoid log(0) by replacing zeros with NaN)
data_use["avg_2024_log"] = np.log(data_use["avg_2024_norm"].replace(0, np.nan))
data_use["NDVI_mean_log"] = np.log(data_use["NDVI_mean"].replace(0, np.nan))

def plot_histogram(data, title):
    fig, ax = plt.subplots()
    ax.hist(data.dropna(), bins=20, color="skyblue", edgecolor="black")
    ax.set_title(title)
    return fig

st.pyplot(plot_histogram(data_use["avg_2024_norm"], "Histogram of avg_2024_norm"))
st.pyplot(plot_histogram(data_use["NDVI_mean"], "Histogram of NDVI_mean"))
st.pyplot(plot_histogram(data_use["avg_2024_log"], "Histogram of avg_2024_log"))
st.pyplot(plot_histogram(data_use["NDVI_mean_log"], "Histogram of NDVI_mean_log"))

# ----------------------------
# 5. Clustering Analysis
# ----------------------------

# Select complete cases for clustering variables (ensure these columns exist in your data)
cluster_vars = ["NDVI_mean_1", "avg_2024", "density"]
data_cluster = data_use.dropna(subset=cluster_vars).copy()

# Scale the data
scaler = StandardScaler()
vars_scaled = scaler.fit_transform(data_cluster[cluster_vars])

# Run KMeans clustering (k=3)
k = 3
kmeans = KMeans(n_clusters=k, random_state=123)
data_cluster["cluster"] = kmeans.fit_predict(vars_scaled).astype(str)

# Merge cluster assignment back into the main dataset
data_use.loc[data_cluster.index, "cluster"] = data_cluster["cluster"]

# Create an interactive 3D scatter plot using Plotly
fig_scatter = px.scatter_3d(
    data_use,
    x="NDVI_mean_1",
    y="avg_2024",
    z="density",
    color="cluster",
    color_discrete_map={"0": "green", "1": "blue", "2": "red"},
    title="3D Scatter Plot of NDVI_mean_1, avg_2024, and density",
    labels={"NDVI_mean_1": "Vegetation Index", "avg_2024": "Nighttime Light Level", "density": "Population Density"},
)
st.plotly_chart(fig_scatter)

# ----------------------------
# 6. Spatial Visualization of Clusters
# ----------------------------

# Merge clustering results into the spatial dataframe
if "OBJECTID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.dropna(subset=["OBJECTID"])
if "MFY_ID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.rename(columns={"MFY_ID": "mfy_id"})
zonal_stats_df = zonal_stats_df.merge(data_use[["mfy_id", "cluster"]], on="mfy_id", how="left")

# Plot the clusters on a map using GeoPandas and Matplotlib
fig_map, ax_map = plt.subplots(figsize=(10, 10))
zonal_stats_df.plot(column="cluster", categorical=True, legend=True, cmap="Set1", ax=ax_map, edgecolor="black")
ax_map.set_title("Map of Clusters")
st.pyplot(fig_map)

# ----------------------------
# 7. Linear Regression Analysis
# ----------------------------

st.subheader("Linear Regression Analysis")

# Define regression variables (ensure these columns exist in your data)
# For example, we model share_allowance ~ NDVI_mean_1 + avg_2024_norm + family_count + area
reg_cols = ["share_allowance", "NDVI_mean_1", "avg_2024_norm", "family_count", "area"]
lm_data = data_use.dropna(subset=reg_cols)

if not lm_data.empty:
    X = lm_data[["NDVI_mean_1", "avg_2024_norm", "family_count", "area"]]
    y = lm_data["share_allowance"]
    # Add a constant term for the intercept
    X = sm.add_constant(X)
    model = sm.OLS(y, X).fit()
    st.text(model.summary())
else:
    st.write("Not enough data for regression analysis.")
