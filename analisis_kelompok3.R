library(readxl)

data <- read_excel(
  "Dataset_Tugas_Kelompok_Statistik_Mhs.xlsx",
  sheet = "Kel3_WebLoading"
)

print(data)

requests <- data$Jumlah_Requests
waktu <- data$Waktu_Muat_Detik

print(requests)
print(waktu)

rata_rata <- mean(waktu)
nilai_tengah <- median(waktu)
maksimum <- max(waktu)
minimum <- min(waktu)
standar_deviasi <- sd(waktu)

print(rata_rata)
print(nilai_tengah)
print(maksimum)
print(minimum)
print(standar_deviasi)