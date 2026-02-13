# Hands-on Day 2
#Package basic untuk visualisasi dan pengolahan data LiDAR
#memanggil library
library(lidR)
library(terra)
library(sf)
library(ggplot2)


#Package basic untuk mengolah data tabular
#memanggil library
library(dplyr)
library(tidyr)
library(data.table)

#read las data
las<-readLAS("D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/Dataset/Dataset/als/NEON_D13_NIWO_DP1_456000_4434000_classified_point_cloud_colorized_crop.las")
# las <- readLAS("D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/Dataset/Dataset/als/plot_01.las")
plot(las)
plot(las, color= "RGB")
print(las@data)


##---------------Generate Digital Terrain Model--------------------------
# Triangular irregular network
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white") 

#Invert distance weighting
dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 


#Kriging
dtm_kriging <- rasterize_terrain(las, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 


##--------------generate topografic information-----------------
dtm<-dtm_idw
# 1. Hitung Slope (Kemiringan)
dtm_slope <- terra::terrain(dtm, v = "slope", unit = "radians")

# 2. Hitung Aspect (Arah Lereng)
dtm_aspect <- terra::terrain(dtm, v = "aspect", unit = "radians")

# 3. Buat Hillshade 
# angle = sudut matahari (biasanya 45), direction = arah matahari (biasanya 315/Barat Laut)
dtm_hillshade <- terra::shade(dtm_slope, dtm_aspect, angle = 45, direction = 315)

# 4. Visualisasi dengan Grayscale
terra::plot(dtm_hillshade, 
            col = gray(seq(0, 1, 0.01)), 
            main = "Visualisasi Hillshade (Relief Terperinci)",
            legend = FALSE,
            mar = c(3, 3, 3, 4))

# Memastikan legenda aktif dan mengatur posisi marjin agar muat
terra::plot(dtm_slope, 
            col = gray(seq(0, 1, 0.01)), 
            main = "Visualisasi Slope",
            legend = TRUE,            # Mengaktifkan legenda
            pax = list(side = 1:2),   # Menampilkan sumbu koordinat (opsional)
            plg = list(title = "Slope", title.font = 2), # Pengaturan teks legenda
            mar = c(3, 3, 3, 5))      # Menambah marjin kanan untuk legenda

terra::plot(dtm_aspect, 
            col = gray(seq(0, 1, 0.01)), 
            main = "Visualisasi Aspect",
            legend = TRUE, 
            pax = list(side = 1:2),
            plg = list(title = "Arah (°)", title.font = 2), 
            mar = c(3, 3, 3, 5))

##----------------generate DSM-------------------
dsm <- lidR::rasterize_canopy(las, res = 1, lidR::p2r())
plot(dsm)


##---------------generate CHM-----------------------
# Memanggil las file
#point cloud di Perm Mixed Forest, Rusia
point_cloud<-lidR::readLAS("D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/Dataset/Dataset/als/plot_01.las")

# --- normalisasi data-------
# Membuat DTM berbasis TIN sebagai referensi tanah
dtm_tin <- rasterize_terrain(point_cloud, res = 0.5, algorithm = tin())
# Menghitung ketinggian relatif (Above Ground Level)
las_norm <- normalize_height(point_cloud, dtm_tin)


# CHM dengan Algoritma Point-to-Raster (P2R)
chm_p2r <- rasterize_canopy(las_norm, res = 0.5, algorithm = p2r())
# Visualisasi
plot(chm_p2r, col = height.colors(50), main = "CHM (Canopy Height Model) - P2R")


# CHM dengan Algoritma Pit Free
chm_pitfree <- rasterize_canopy(las_norm, res = 0.5, algorithm = pitfree(
  subcircle = 0.15 # Radius untuk mencari titik
))
# Visualisasi
plot(chm_pitfree, col = height.colors(50), main = "CHM (Canopy Height Model) - PitFree")


# CHM dengan metode TIN
# Membuat CHM Berbasis TIN ---
chm_tin <- rasterize_canopy(las_norm, res = 0.5, algorithm = dsmtin())
# Visualisasi
plot(chm_tin, col = height.colors(50), main = "CHM Berbasis Interpolasi TIN")


#bonus: visualisasi RGB 2D
las<-readLAS("D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/Dataset/Dataset/als/NEON_D13_NIWO_DP1_456000_4434000_classified_point_cloud_colorized_crop.las")

# 1. Pastikan data memiliki atribut RGB (biasanya 16-bit)
# Kita bagi 256 jika data Anda menggunakan skala 16-bit agar menjadi 8-bit standar
rgb_raster <- pixel_metrics(las, ~list(R = mean(R)/256, G = mean(G)/256, B = mean(B)/256), res = 0.5)

# 2. Tampilkan hasilnya
# Karena objeknya sekarang adalah SpatRaster dari paket 'terra'
terra::plotRGB(rgb_raster)
