
library(readxl)

# 1. FUNCTION MEMBACA DATA EXCEL

baca_data <- function(file, sheet) {
  data <- read_excel(file, sheet = sheet)

  cat("\n=== DATASET ===\n")
  print(data)

  return(data)
}


# 2. FUNCTION STATISTIK DESKRIPTIF

statistik_deskriptif <- function(data, kolom) {

  nilai <- data[[kolom]]

  hasil <- data.frame(
    Rata_Rata = mean(nilai, na.rm = TRUE),
    Median = median(nilai, na.rm = TRUE),
    Maksimum = max(nilai, na.rm = TRUE),
    Minimum = min(nilai, na.rm = TRUE),
    Standar_Deviasi = sd(nilai, na.rm = TRUE)
  )

  cat("\n=== STATISTIK DESKRIPTIF ===\n")
  print(hasil)

  return(invisible(hasil))
}


# 3. FUNCTION HISTOGRAM

grafik_histogram <- function(data, kolom,
                             judul = "Histogram",
                             warna = "skyblue") {

  nilai <- data[[kolom]]

  hist(
    nilai,
    main = judul,
    xlab = kolom,
    ylab = "Frekuensi",
    col = warna
  )
}


# 4. FUNCTION PERBANDINGAN KELOMPOK

grafik_perbandingan <- function(data, kategori, nilai,
                               judul = "Perbandingan Rata-rata",
                               warna = "lightblue") {

  # Menghitung rata-rata berdasarkan kategori
  hasil <- aggregate(
    data[[nilai]],
    by = list(Kategori = data[[kategori]]),
    FUN = function(x) mean(x, na.rm = TRUE)
  )

  names(hasil)[2] <- "Rata_Rata"

  cat("\n=== RATA-RATA PER KATEGORI ===\n")
  print(hasil)

  # Membuat bar chart
  barplot(
    hasil$Rata_Rata,
    names.arg = hasil$Kategori,
    main = judul,
    xlab = kategori,
    ylab = paste("Rata-rata", nilai),
    col = warna
  )

  return(invisible(hasil))
}


# 5. FUNCTION ANALISIS KORELASI

analisis_korelasi <- function(data, kolom_x, kolom_y,
                              judul = "Analisis Korelasi") {

  x <- data[[kolom_x]]
  y <- data[[kolom_y]]

  # Menghitung korelasi Pearson
  korelasi <- cor(
    x, y,
    method = "pearson",
    use = "complete.obs"
  )

  cat("\n=== HASIL KORELASI ===\n")
  print(korelasi)

  # Scatter plot
  plot(
    x, y,
    main = judul,
    xlab = kolom_x,
    ylab = kolom_y,
    col = "blue",
    pch = 19
  )

  # Model regresi linear
  model <- lm(y ~ x)

  # Menambahkan garis regresi
  abline(
    model,
    col = "red",
    lwd = 2
  )

  return(invisible(list(
    korelasi = korelasi,
    model = model
  )))
}


# 6. PROGRAM UTAMA

data <- baca_data(
  "Dataset_Tugas_Kelompok_Statistik_Mhs.xlsx",
  "Kel3_WebLoading"
)

statistik_deskriptif(
  data,
  "Waktu_Muat_Detik"
)

grafik_histogram(
  data,
  "Waktu_Muat_Detik",
  "Distribusi Waktu Loading Website"
)

grafik_perbandingan(
  data,
  "Jenis_Situs",
  "Waktu_Muat_Detik",
  "Perbandingan Waktu Loading Website"
)

analisis_korelasi(
  data,
  "Jumlah_Requests",
  "Waktu_Muat_Detik",
  "Hubungan Requests dan Waktu Loading"
)
