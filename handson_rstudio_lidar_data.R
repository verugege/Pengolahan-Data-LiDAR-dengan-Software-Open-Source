# Hands-on Day 1
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
#install.packages(c("dplyr", "tidyr"))
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

plot(point_cloud)

##------ground classification----
#Membaca klasifikasi ground
table(point_cloud$Classification)

#klasifikasi ground dari data asli
plot(point_cloud, 
     color = "Classification",
     bg = "white",
     axis = TRUE)


#Reset point cloud menjadi satu kelas
point_cloud_trial<-point_cloud
point_cloud_trial$Classification <- 1L

# 1. Progressive Morphological Filter (PMF)
# ws: window size (ukuran jendela filter)
# th: threshold ketinggian
las_pmf <- classify_ground(point_cloud_trial, algorithm = pmf(ws = 5 , th = 0.3))
las_pmf$Classification[las_pmf$Classification == 1] <- 5L
table(las_pmf$Classification)
plot(las_pmf, color="Classification")

# Mencoba mendekati data asli dengan tunning parameter
las_pmf_fit <- classify_ground(point_cloud_trial, algorithm = pmf(ws = c(3, 5, 10), th = c(0.1, 0.2, 0.3)))
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
plot(las_csf, color ="Classification")

#coba dengan parameter lebih tunning parameter
las_csf_fit <- classify_ground(point_cloud_trial, algorithm = csf(
  rigidness = 3,         
  cloth_resolution = 1,    # Resolusi lebih kasar untuk generalisasi
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

plot(las_mcc, color ="Classification", axis = T)


##summary the data
# Fungsi untuk mengambil jumlah titik kelas 2 secara otomatis
get_ground_count <- function(las_obj) {
  counts <- table(las_obj$Classification)
  if ("2" %in% names(counts)) {
    return(as.numeric(counts["2"]))
  } else {
    return(0)
  }
}

# Membuat list objek yang ingin dibandingkan
las_list <- list(
  "Data Asli" = point_cloud,
  "PMF"       = las_pmf,
  "PMF Fit"   = las_pmf_fit,
  "CSF"       = las_csf,
  "CSF Fit"   = las_csf_fit,
  "MCC"       = las_mcc
)

# Ekstraksi otomatis ke dalam data frame
df_perbandingan <- data.frame(
  Metode = names(las_list),
  Ground_Count = sapply(las_list, get_ground_count),
  Total_Points = sapply(las_list, function(x) length(x$Classification))
)

# Menambah kolom kalkulasi
df_perbandingan$Persen_Ground <- (df_perbandingan$Ground_Count / df_perbandingan$Total_Points) * 100
df_perbandingan$Selisih_vs_Asli <- df_perbandingan$Ground_Count - df_perbandingan$Ground_Count[1]

# Tampilkan hasil akhir
print(df_perbandingan)


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
output_path<- "D:/Training Verru/Pengolahan Data LiDAR dengan Software Open Source/sesi1/output_klasifikasi/"

#writeLAS(las_pmf, paste0(output_path, "ground_las_PMF.las"))
#writeLAS(las_pmf_fit, paste0(output_path, "ground_las_PMF_FIT.las"))
#writeLAS(las_csf, paste0(output_path, "ground_las_CSF.las"))
#writeLAS(las_csf_fit, paste0(output_path, "ground_las_CSF_FIT.las"))
#writeLAS(las_mcc, paste0(output_path, "ground_las_MCC.las"))


