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
import json
import plotly.io as pio

st.title("Spatial Data Analysis App")

# -------------------------------------------------------------------
# 1) DOWNLOAD & LOAD DATA FROM GITHUB RAW URLs
# -------------------------------------------------------------------



# URL to your JSON file hosted in your GitHub repository (using the raw link)
json_url = "https://raw.githubusercontent.com/Ulugbekyonsei/karakalpak_5/master/myplotly_fig.json"

# Fetch the JSON file from GitHub
response = requests.get(json_url)
response.raise_for_status()  # Ensure the request was successful
fig_json = response.text

# Convert JSON string back to a plotly figure
fig = pio.from_json(fig_json)

# Display the plot in Streamlit
st.plotly_chart(fig)
