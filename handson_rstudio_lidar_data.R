# Hands-on Sesi 1
#Package basic untuk visualisasi dan pengolahan data LiDAR
#install library
#install.packages(c("lidR", "terra", "sf", "ggplot2"))
#memanggil library
library(lidR)
library(terra)
library(sf)
library(ggplot2)
#Package basic untuk mengolah data tabular
#install library
#install.packages(c("dplyr", "tidyr", "data.table"))
#memanggil library
library(dplyr)
library(tidyr)
library(data.table)

#load LiDAR data 
point_cloud <- lidR::readLAS(
  "D:\\Training Verru\\Pengolahan Data LiDAR dengan Software Open Source\\sesi1\\Dataset\\Dataset\\als\\plot_01.las"
)

#read metadata las
las_check(point_cloud)
point_cloud@header

#Membaca klasifikasi ground
table(point_cloud$Classification)

#Reset point cloud menjadi satu kelas
point_cloud_trial<-point_cloud
point_cloud_trial$Classification <- 1L

# 1. Progressive Morphological Filter (PMF)
# ws: window size (ukuran jendela filter)
# th: threshold ketinggian
las_pmf <- classify_ground(point_cloud_trial, algorithm = pmf(ws = 5 , th = 0.3))
las_pmf$Classification[las_pmf$Classification == 1] <- 5L
table(las_pmf$Classification)

# Mencoba mendekati data asli dengan tunning parameter
las_pmf_fit <- classify_ground(point_cloud_trial, algorithm = pmf(ws = c(3, 5, 
                                                                         10), th = c(0.1, 0.2, 0.3)))
las_pmf_fit$Classification[las_pmf_fit$Classification == 1] <- 5L
table(las_pmf_fit$Classification)

# 2. Cloth Simulation Filter (las_pmfCSF)
# rigidness: kekakuan kain (1 = sangat kaku, 3 = lemas/mengikuti tanah)
# cloth_resolution: resolusi jaring kain
las_csf <- classify_ground(point_cloud_trial, algorithm = csf(
  rigidness = 1, 
  cloth_resolution = 0.5, 
  class_threshold = 0.5, # Nilai ini mirip dengan 'th' pada PMF
  sloop_smooth = TRUE
))
las_csf$Classification[las_csf$Classification == 1] <- 5L


#coba dengan parameter lebih tunning parameter
las_csf_fit <- classify_ground(point_cloud_trial, algorithm = csf(
  rigidness = 3, 
  cloth_resolution = 1, # Resolusi lebih kasar untuk generalisasi
  class_threshold = 0.2
))
las_csf_fit$Classification[las_csf_fit$Classification == 1] <- 5L
table(las_csf_fit$Classification)

# 3. Multiscale Curvature Classification (MCC)
# s: scale (skala kelengkungan)
# t: curvature threshold (ambang batas kelengkungan)
# Catatan: Pastikan library(RMCC) sudah terinstal
las_mcc <- classify_ground(point_cloud_trial, algorithm = mcc(s = 1.5, t = 0.3))
las_mcc$Classification[las_mcc$Classification == 1] <- 5L
table(las_mcc$Classification)

# --- Visualisasi Perbandingan ---
#klasifikasi ground dari data asli
plot(point_cloud, 
     color = "Classification",
     bg = "black",
     axis = TRUE)
#klasifikasi ground dengan berbagai algoritma
plot(las_pmf, color = "Classification", main = "Hasil PMF", axis = TRUE)
plot(las_pmf_fit, color = "Classification", main = "Hasil PMF", axis = TRUE)
plot(las_csf, color = "Classification", main = "Hasil CSF", axis = TRUE)
plot(las_csf_fit, color = "Classification", main = "Hasil CSF", axis = TRUE)
plot(las_mcc, color = "Classification", main = "Hasil MCC", axis = TRUE)

##-----simpan las hasil klasifikasi-------------
#output_path<- "D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/output_klasifikasi"
#writeLAS(las_pmf, paste0(output_path, "ground_las_PMF.las"))
#writeLAS(las_pmf_fit, paste0(output_path, "ground_las_PMF_FIT.las"))
#writeLAS(las_csf, paste0(output_path, "ground_las_CSF.las"))
#writeLAS(las_csf_fit, paste0(output_path, "ground_las_CSF_FIT.las"))
#writeLAS(las_mcc, paste0(output_path, "ground_las_MCC.las"))
