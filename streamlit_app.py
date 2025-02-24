import streamlit as st
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.mask import mask
import numpy as np
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import plotly.express as px
import matplotlib.pyplot as plt
import statsmodels.api as sm
import requests, zipfile, io, tempfile

st.title("Spatial Data Analysis App")

# -------------------------------------------------------------------
# 1) DOWNLOAD & LOAD DATA FROM GITHUB RAW URLs
# -------------------------------------------------------------------

# --- 1.1 CSV & Excel ------------------------------------------------
csv_url   = "https://raw.githubusercontent.com/Ulugbekyonsei/karakalpak_5/master/ndvi_polygons.csv"
excel_url = "https://raw.githubusercontent.com/Ulugbekyonsei/karakalpak_5/master/KR.xlsx"

mahalla_data = pd.read_csv(csv_url)
data2        = pd.read_excel(excel_url)

# --- 1.2 Shapefile: Using a Zipped Shapefile ------------------------
shapefile_zip_url = "https://raw.githubusercontent.com/Ulugbekyonsei/karakalpak_5/master/mahalla.zip"

def load_shapefile_zip(url):
    """
    Downloads a zipped shapefile, extracts to a temp folder,
    and returns a GeoDataFrame.
    """
    r = requests.get(url)
    z = zipfile.ZipFile(io.BytesIO(r.content))
    z.extractall("temp_shapefile")  # Extracts .shp, .dbf, .shx, etc.
    return gpd.read_file("temp_shapefile/mahalla.shp")

gdf = load_shapefile_zip(shapefile_zip_url)

# --- 1.3 Raster (.tif) ----------------------------------------------
raster_url = "https://raw.githubusercontent.com/Ulugbekyonsei/karakalpak_5/master/imputed_data_karakalpak.tif"

def load_raster_from_github(url):
    """
    Downloads a .tif to a temporary file and returns the local path.
    """
    r = requests.get(url, stream=True)
    r.raise_for_status()
    with tempfile.NamedTemporaryFile(suffix=".tif", delete=False) as tmp:
        for chunk in r.iter_content(chunk_size=8192):
            tmp.write(chunk)
        return tmp.name

raster_path = load_raster_from_github(raster_url)

# -------------------------------------------------------------------
# 2) OPEN THE RASTER, REPROJECT SHAPEFILE IF NEEDED, ETC.
# -------------------------------------------------------------------

with rasterio.open(raster_path) as raster:
    # Match CRS
    if gdf.crs != raster.crs:
        gdf = gdf.to_crs(raster.crs)

    # Filter for "Qoraqalpog'iston Respublikasi"
    karakalpak_shape = gdf[gdf["region_nam"] == "Qoraqalpog'iston Respublikasi"].copy()

    # ----------------------------------------------------------------
    # 3) Zonal Statistics (Manual Implementation Without `rasterstats`)
    # ----------------------------------------------------------------
    def get_polygon_means(geom, src):
        """
        Extracts raster values for a given polygon geometry
        and returns the mean value for each band.
        """
        try:
            out_image, _ = mask(src, [geom], crop=True)
            if src.nodata is not None:
                out_image = np.where(out_image == src.nodata, np.nan, out_image)
            means = np.nanmean(out_image, axis=(1, 2))  # mean over x & y
            return means.tolist()
        except Exception:
            return [np.nan] * src.count

    band_count = raster.count
    band_means = {f"imputed_data_karakalpak_{i}": [] for i in range(1, band_count+1)}

    # Loop over each polygon
    for geom in karakalpak_shape.geometry:
        means = get_polygon_means(geom, raster)
        for i, val in enumerate(means, start=1):
            band_means[f"imputed_data_karakalpak_{i}"].append(val)

# Add the band means to the GeoDataFrame
for band_col, values in band_means.items():
    karakalpak_shape[band_col] = values

# Compute avg_2024 from bands 1..12 (adjust if you have fewer or more)
band_columns = [f"imputed_data_karakalpak_{i}" for i in range(1, 13)
                if f"imputed_data_karakalpak_{i}" in karakalpak_shape.columns]
karakalpak_shape["avg_2024"] = karakalpak_shape[band_columns].mean(axis=1)

# Normalize avg_2024
min_val = karakalpak_shape["avg_2024"].min()
max_val = karakalpak_shape["avg_2024"].max()
karakalpak_shape["avg_2024_norm"] = (karakalpak_shape["avg_2024"] - min_val) / (max_val - min_val)

# Drop rows with missing
zonal_stats_df = karakalpak_shape.dropna(subset=["avg_2024_norm"]).copy()

# Rename MFY_ID -> mfy_id if needed
if "MFY_ID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.rename(columns={"MFY_ID": "mfy_id"})

# -------------------------------------------------------------------
# 4) MERGE WITH mahalla_data & data2
# -------------------------------------------------------------------

# Convert to string to avoid merges failing
mahalla_data["mfy_id"] = mahalla_data["mfy_id"].astype(str)
zonal_stats_df["mfy_id"] = zonal_stats_df["mfy_id"].astype(str)

data2 = data2.rename(columns={"id_1": "mfy_id"})
data2["mfy_id"] = data2["mfy_id"].astype(str)

# Merge to get final data
data_use = pd.merge(
    mahalla_data,
    zonal_stats_df[["mfy_id", "avg_2024", "avg_2024_norm"]],
    on="mfy_id", how="inner"
)
data_use = pd.merge(
    data_use,
    data2[["mfy_id", "family_count", "oluvchi"]],
    on="mfy_id", how="inner"
)

# Create share_allowance
data_use["share_allowance"] = data_use["oluvchi"] / data_use["family_count"]

# -------------------------------------------------------------------
# 5) EXPLORATORY ANALYSIS (Correlations, Histograms)
# -------------------------------------------------------------------
st.subheader("Correlation Analysis")

# Check columns exist
if "NDVI_mean" in data_use.columns:
    corr_ndvi = data_use["avg_2024_norm"].corr(data_use["NDVI_mean"])
    st.write("Correlation between avg_2024_norm and NDVI_mean:", corr_ndvi)

if "density" in data_use.columns:
    corr_density = data_use["avg_2024_norm"].corr(data_use["density"])
    st.write("Correlation between avg_2024_norm and density:", corr_density)

# Log transforms
data_use["avg_2024_log"] = np.log(data_use["avg_2024_norm"].replace(0, np.nan))
if "NDVI_mean" in data_use.columns:
    data_use["NDVI_mean_log"] = np.log(data_use["NDVI_mean"].replace(0, np.nan))

import matplotlib.pyplot as plt

def plot_histogram(series, title):
    fig, ax = plt.subplots()
    ax.hist(series.dropna(), bins=20, color="skyblue", edgecolor="black")
    ax.set_title(title)
    return fig

st.pyplot(plot_histogram(data_use["avg_2024_norm"], "Histogram of avg_2024_norm"))

if "NDVI_mean" in data_use.columns:
    st.pyplot(plot_histogram(data_use["NDVI_mean"], "Histogram of NDVI_mean"))

st.pyplot(plot_histogram(data_use["avg_2024_log"], "Histogram of avg_2024_log"))

if "NDVI_mean_log" in data_use.columns:
    st.pyplot(plot_histogram(data_use["NDVI_mean_log"], "Histogram of NDVI_mean_log"))

# -------------------------------------------------------------------
# 6) CLUSTERING ANALYSIS
# -------------------------------------------------------------------
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import plotly.express as px

cluster_vars = ["NDVI_mean_1", "avg_2024", "density"]
data_cluster = data_use.dropna(subset=cluster_vars).copy()

scaler = StandardScaler()
vars_scaled = scaler.fit_transform(data_cluster[cluster_vars])

kmeans = KMeans(n_clusters=3, random_state=123)
data_cluster["cluster"] = kmeans.fit_predict(vars_scaled).astype(str)

data_use.loc[data_cluster.index, "cluster"] = data_cluster["cluster"]

fig_scatter = px.scatter_3d(
    data_use,
    x="NDVI_mean_1",
    y="avg_2024",
    z="density",
    color="cluster",
    color_discrete_map={"0": "green", "1": "blue", "2": "red"},
    title="3D Scatter Plot of NDVI_mean_1, avg_2024, and density",
    labels={"NDVI_mean_1": "Vegetation Index", "avg_2024": "Nighttime Light", "density": "Population Density"},
)
st.plotly_chart(fig_scatter)

# -------------------------------------------------------------------
# 7) SPATIAL VISUALIZATION
# -------------------------------------------------------------------
if "OBJECTID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.dropna(subset=["OBJECTID"])
if "MFY_ID" in zonal_stats_df.columns:
    zonal_stats_df = zonal_stats_df.rename(columns={"MFY_ID": "mfy_id"})

zonal_stats_df = zonal_stats_df.merge(data_use[["mfy_id", "cluster"]], on="mfy_id", how="left")

fig_map, ax_map = plt.subplots(figsize=(10, 10))
zonal_stats_df.plot(column="cluster", categorical=True, legend=True, cmap="Set1", ax=ax_map, edgecolor="black")
ax_map.set_title("Map of Clusters")
st.pyplot(fig_map)

# -------------------------------------------------------------------
# 8) LINEAR REGRESSION
# -------------------------------------------------------------------
st.subheader("Linear Regression Analysis")

reg_cols = ["share_allowance", "NDVI_mean_1", "avg_2024_norm", "family_count", "area"]
lm_data = data_use.dropna(subset=reg_cols)

if not lm_data.empty:
    X = lm_data[["NDVI_mean_1", "avg_2024_norm", "family_count", "area"]]
    y = lm_data["share_allowance"]
    X = sm.add_constant(X)
    model = sm.OLS(y, X).fit()
    st.text(model.summary())
else:
    st.write("Not enough data for regression analysis.")

