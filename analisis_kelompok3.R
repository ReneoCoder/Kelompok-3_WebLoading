
# ==========================================
# ANALISIS DATA WEBSITE LOADING
# KELOMPOK 3 - STATISTIK & PROBABILITAS
# ==========================================


# 1. MEMBACA DATA EXCEL

library(readxl)

data <- read_excel(
  "Dataset_Tugas_Kelompok_Statistik_Mhs.xlsx",
  sheet = "Kel3_WebLoading"
)

# Menampilkan isi dataset
print(data)

# Melihat struktur dataset
str(data)


# ==========================================
# 2. MENGAMBIL KOLOM DATA
# ==========================================

# Mengambil jumlah HTTP requests
requests <- data$Jumlah_Requests

# Mengambil waktu loading website
waktu <- data$Waktu_Muat_Detik

# Menampilkan masing-masing vector
print(requests)
print(waktu)


# ==========================================
# 3. STATISTIK DESKRIPTIF
# ==========================================

# Menghitung jumlah observasi
jumlah_data <- length(waktu)

# Menghitung rata-rata
rata_rata <- mean(waktu)

# Menghitung median
nilai_tengah <- median(waktu)

# Mengambil nilai maksimum
maksimum <- max(waktu)

# Mengambil nilai minimum
minimum <- min(waktu)

# Menghitung standar deviasi sampel
standar_deviasi <- sd(waktu)

# Menghitung varians sampel
varians <- var(waktu)

# Menghitung kuartil
kuartil <- quantile(waktu)

# Menampilkan hasil statistik
cat("\n=== STATISTIK DESKRIPTIF ===\n")

cat("Jumlah Data     :", jumlah_data, "\n")
cat("Rata-rata       :", rata_rata, "\n")
cat("Median          :", nilai_tengah, "\n")
cat("Maksimum        :", maksimum, "\n")
cat("Minimum         :", minimum, "\n")
cat("Standar Deviasi :", standar_deviasi, "\n")
cat("Varians         :", varians, "\n")
print(kuartil)


# ==========================================
# 4. HISTOGRAM WAKTU LOADING
# ==========================================

hist(
  waktu,
  main = "Distribusi Waktu Loading Website",
  xlab = "Waktu Loading (Detik)",
  ylab = "Frekuensi",
  col = "skyblue"
)


# ==========================================
# 5. RATA-RATA BERDASARKAN JENIS SITUS
# ==========================================

# Mengelompokkan data berdasarkan jenis situs
rata_per_jenis <- aggregate(
  Waktu_Muat_Detik ~ Jenis_Situs,
  data = data,
  FUN = mean
)

# Menampilkan hasil
cat("\n=== RATA-RATA PER JENIS SITUS ===\n")
print(rata_per_jenis)

# Membuat bar chart
barplot(
  rata_per_jenis$Waktu_Muat_Detik,
  names.arg = rata_per_jenis$Jenis_Situs,
  main = "Perbandingan Waktu Loading Website",
  xlab = "Jenis Website",
  ylab = "Rata-rata Waktu (Detik)",
  col = "lightblue"
)


# ==========================================
# 6. KATEGORI KECEPATAN WEBSITE
# ==========================================

# Menghitung jumlah setiap kategori
kategori <- table(data$Kategori_Kecepatan)

cat("\n=== KATEGORI KECEPATAN ===\n")
print(kategori)

# Membuat grafik kategori
barplot(
  kategori,
  main = "Kategori Kecepatan Website",
  xlab = "Kategori Kecepatan",
  ylab = "Jumlah Website",
  col = "orange"
)


# ==========================================
# 7. ANALISIS KORELASI
# ==========================================

# Menghitung korelasi Pearson
korelasi <- cor(
  requests,
  waktu,
  method = "pearson"
)

cat("\n=== HASIL KORELASI ===\n")
print(korelasi)


# ==========================================
# 8. SCATTER PLOT DAN REGRESI
# ==========================================

# Membuat scatter plot
plot(
  requests,
  waktu,
  main = "Hubungan Requests dan Waktu Loading",
  xlab = "Jumlah HTTP Requests",
  ylab = "Waktu Loading (Detik)",
  col = "blue",
  pch = 19
)

# Membuat model regresi linear
model <- lm(waktu ~ requests)

# Menambahkan garis regresi
abline(
  model,
  col = "red",
  lwd = 2
)

# Menampilkan ringkasan regresi
cat("\n=== HASIL REGRESI ===\n")
print(summary(model))


# ==========================================
# 9. ANALISIS OUTLIER
# ==========================================

# Membuat boxplot
boxplot(
  waktu,
  main = "Boxplot Waktu Loading Website",
  ylab = "Waktu Loading (Detik)",
  col = "lightgreen"
)

# Mengidentifikasi nilai outlier berdasarkan IQR
outlier <- boxplot.stats(waktu)$out

cat("\n=== DATA OUTLIER ===\n")
print(outlier)

# Mengambil data tanpa observasi outlier
data_normal <- data[!waktu %in% outlier, ]

# Menghitung ulang korelasi
korelasi_normal <- cor(
  data_normal$Jumlah_Requests,
  data_normal$Waktu_Muat_Detik
)

cat("\n=== KORELASI TANPA OUTLIER ===\n")
print(korelasi_normal)


# ==========================================
# 10. KESIMPULAN AWAL
# ==========================================

cat("\n=== RINGKASAN ANALISIS ===\n")

cat("Total observasi :", jumlah_data, "\n")
cat("Rata-rata waktu loading :", rata_rata, "detik\n")
cat("Korelasi awal :", korelasi, "\n")
cat("Korelasi tanpa outlier :", korelasi_normal, "\n")
