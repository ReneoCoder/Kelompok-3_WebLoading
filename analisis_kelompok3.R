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