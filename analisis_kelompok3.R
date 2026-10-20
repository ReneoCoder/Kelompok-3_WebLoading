# 1. MEMBACA DATA EXCEL
library(readxl)

data <- read_excel(
  "Dataset_Tugas_Kelompok_Statistik_Mhs.xlsx",
  sheet = "Kel3_WebLoading"
)

print(data)


# 2. STATISTIK DESKRIPTIF
waktu <- data$Waktu_Muat_Detik
requests <- data$Jumlah_Requests

cat("Rata-rata :", mean(waktu), "\n")
cat("Median    :", median(waktu), "\n")
cat("Minimum   :", min(waktu), "\n")
cat("Maksimum  :", max(waktu), "\n")
cat("Std Dev   :", sd(waktu), "\n")

# Grafik distribusi waktu loading
hist(
  waktu,
  main = "Distribusi Waktu Loading",
  xlab = "Waktu Loading (Detik)",
  col = "skyblue"
)


# 3. PERBANDINGAN JENIS WEBSITE
rata_jenis <- aggregate(
  Waktu_Muat_Detik ~ Jenis_Situs,
  data = data,
  FUN = mean
)

print(rata_jenis)

barplot(
  rata_jenis$Waktu_Muat_Detik,
  names.arg = rata_jenis$Jenis_Situs,
  main = "Rata-rata Loading per Jenis Website",
  ylab = "Waktu (Detik)",
  col = "lightblue"
)


# 4. HUBUNGAN REQUESTS DAN WAKTU LOADING
korelasi <- cor(requests, waktu)
cat("Korelasi :", korelasi, "\n")

plot(
  requests,
  waktu,
  main = "Hubungan Requests dan Waktu Loading",
  xlab = "Jumlah Requests",
  ylab = "Waktu Loading (Detik)",
  pch = 19,
  col = "blue"
)

abline(lm(waktu ~ requests), col = "red")