# VISI: Visualisasi & Inferensia Kerentanan Sosial Indonesia Dashboard

## Daftar Isi
1.  [Pendahuluan](#pendahuluan)
2.  [Fitur Utama](#fitur-utama)
3.  [Data](#data)
4.  [Teknologi](#teknologi)
5.  [Cara Menggunakan](#cara-menggunakan)


## 1. Pendahuluan

Dashboard Visualisasi & Inferensia Kerentanan Sosial (VISI) Indonesia adalah sebuah aplikasi Shiny interaktif yang dirancang untuk menganalisis dan memvisualisasikan data kerentanan sosial di tingkat kabupaten/kota di seluruh Indonesia. Proyek ini merupakan bagian dari tugas akhir mata kuliah Komputasi Statistik di Politeknik Statistika STIS pada tahun 2025.

Kerentanan sosial merupakan indikator penting yang mengukur sejauh mana suatu komunitas rentan terhadap dampak negatif dari berbagai ancaman, baik itu bencana alam, krisis ekonomi, maupun guncangan sosial lainnya. Dengan menyediakan alat analisis yang komprehensif, dashboard ini bertujuan untuk membantu para pemangku kepentingan (peneliti, pemerintah daerah, LSM, dll.) dalam memahami faktor-faktor demografis, sosial, dan ekonomi yang berkontribusi terhadap kerentanan. Pemahaman yang mendalam ini diharapkan dapat memfasilitasi perancangan intervensi yang lebih tepat sasaran untuk meningkatkan ketahanan masyarakat Indonesia.

Dashboard VISI memungkinkan pengguna untuk menjelajahi data, melakukan berbagai uji hipotesis statistik, hingga membangun dan menganalisis model regresi linear berganda, semua disajikan dengan visualisasi yang intuitif dan interpretasi yang mudah dipahami.

## 2. Fitur Utama

Dashboard VISI dilengkapi dengan berbagai fitur yang terorganisir dalam beberapa tab utama:

### 2.1. Beranda
Tab ini berfungsi sebagai pintu gerbang utama dashboard, menyediakan informasi dasar dan fungsionalitas eksplorasi data awal.
* **Overview**: Deskripsi umum mengenai tujuan dashboard dan fitur-fiturnya.
* **Metadata Asli & Olahan**: Informasi detail mengenai definisi variabel, tipe data, unit pengukuran, dan sumber data yang digunakan (berasal dari Survei Sosial Ekonomi Nasional BPS tahun 2017).
* **Data**: Tampilan tabel data lengkap yang digunakan dalam aplikasi, dengan opsi untuk mengunduh data asli (`sovi_data.csv`) maupun data yang sudah dibersihkan (`sovi_data_clean.csv`).
* **Transformasi Data**: Fitur inovatif yang memungkinkan pengguna untuk mengkategorikan variabel numerik ke dalam kelompok-kelompok tertentu (misalnya, Tinggi, Sedang, Rendah) berdasarkan kuantil. Hasil kategorisasi akan ditambahkan sebagai kolom baru yang dapat digunakan dalam analisis ANOVA.
* **Eksplorasi Data**:
    * **Histogram & Density Plot**: Visualisasi distribusi variabel yang dipilih per provinsi atau nasional.
    * **Boxplot Perbandingan**: Membandingkan distribusi variabel antara provinsi yang dipilih dan keseluruhan Indonesia.
    * **Peta Sebaran (Leaflet)**: Peta interaktif yang menampilkan sebaran nilai variabel di setiap kabupaten/kota, dengan gradasi warna yang menunjukkan tingkat kerentanan.
    * **Statistik Deskriptif & Interpretasi**: Ringkasan statistik (rata-rata, median, maksimum, minimum, skewness, kurtosis) dan interpretasi naratif tentang karakteristik distribusi data serta peringkat wilayah.
* **Uji Asumsi Umum**: Melakukan uji asumsi statistik dasar pada data:
    * **Uji Normalitas**: Menggunakan uji Shapiro-Wilk (untuk n <= 5000) dan Lilliefors (untuk n >= 5) untuk memeriksa apakah data berdistribusi normal.
    * **Uji Kesamaan Varians (F-test)**: Membandingkan varians suatu variabel antara dua wilayah/provinsi.
    * Dilengkapi dengan interpretasi hasil uji secara otomatis.

### 2.2. Statistik Inferensia
Bagian ini fokus pada pengujian hipotesis statistik untuk menarik kesimpulan tentang populasi berdasarkan sampel data.
* **Uji Beda Rata-Rata**:
    * **Satu Populasi**: Menguji apakah rata-rata suatu variabel di suatu wilayah (nasional/provinsi) berbeda secara signifikan dari nilai hipotesis tertentu ($\mu_0$).
    * **Dua Populasi**: Menguji apakah ada perbedaan signifikan antara rata-rata suatu variabel di dua provinsi yang berbeda. Dilengkapi dengan Levene's Test untuk memeriksa asumsi homogenitas varians.
* **Uji Proporsi**:
    * **Satu Populasi**: Menguji proporsi suatu variabel (misalnya, persentase rumah tangga tidak bertenaga listrik) terhadap nilai hipotesis tertentu ($p_0$) pada skala nasional, provinsi, atau kabupaten/kota.
    * **Dua Populasi**: Membandingkan proporsi suatu variabel antara dua wilayah/populasi yang berbeda (Nasional, Provinsi, atau Kabupaten/Kota).
* **Uji Varians**:
    * **Satu Populasi**: Menguji apakah varians suatu variabel di suatu wilayah berbeda secara signifikan dari nilai hipotesis tertentu ($\sigma_0^2$).
    * **Dua Populasi**: Membandingkan varians suatu variabel antara dua provinsi yang berbeda (F-test).
* **ANOVA (Analysis of Variance)**:
    * **Satu Arah**: Membandingkan rata-rata variabel numerik berdasarkan satu faktor pengelompokan (misalnya, membandingkan persentase kemiskinan antar kategori tingkat pendidikan). Dilengkapi dengan pemeriksaan asumsi (Levene's & Shapiro-Wilk) dan uji lanjut Tukey HSD.
    * **Dua Arah**: Menguji pengaruh dua faktor pengelompokan secara bersamaan terhadap satu variabel numerik, termasuk pemeriksaan efek interaksi. Dilengkapi dengan uji lanjut Tukey HSD.

### 2.3. Analisis Regresi
Bagian ini didedikasikan untuk pemodelan hubungan antara variabel, dengan fokus pada regresi linear berganda.
* **Diagnosis Model Awal**:
    * **Matriks Korelasi**: Memvisualisasikan hubungan antar variabel menggunakan `corrplot`, membantu identifikasi multikolinearitas dan pemilihan variabel.
    * **Scatter Plot**: Menampilkan hubungan antara variabel terikat (Y) dengan setiap variabel bebas (X), beserta nilai R-squared sederhana.
* **Hasil Model Tetap**: Menganalisis model regresi linear berganda dengan variabel terikat yang sudah ditentukan (`POVERTY_PERCENTAGE`, `ILLITERATE_PERCENTAGE`, `LOWEDU_PERCENTAGE`) dan variabel bebas yang relevan.
    * **Uji Asumsi Klasik**: Melakukan uji Normalitas (residual), Homoskedastisitas (Breusch-Pagan), Non-Autokorelasi (Runs Test), dan Non-Multikolinearitas (VIF).
    * **Opsi Perbaikan Model**: Pengguna dapat memilih opsi perbaikan seperti penghapusan variabel dengan VIF tinggi, transformasi logaritma natural, atau penggunaan GLS/WLS untuk mengatasi heteroskedastisitas.
    * **Output Model R & Interpretasi**: Menampilkan ringkasan model regresi dari R dan interpretasi otomatis mengenai persamaan model, pengaruh variabel, R-squared, serta kesimpulan uji F dan t.
* **Regresi Kustom**: Memberikan fleksibilitas penuh kepada pengguna untuk membangun model regresi sendiri dengan memilih variabel terikat (Y) dan variabel bebas (X) secara kustom dari semua variabel yang tersedia.
    * **Uji Asumsi Klasik & Opsi Perbaikan**: Fungsi yang sama dengan "Hasil Model Tetap", memungkinkan pengguna untuk mendiagnosis dan memperbaiki asumsi pada model kustom mereka.
    * **Output Model R & Interpretasi Kustom**: Menampilkan ringkasan dan interpretasi model regresi yang dibangun pengguna.

## 3. Data

Data yang digunakan dalam dashboard ini berasal dari:
* **Sumber Data Utama**: Survei Sosial Ekonomi Nasional (SUSENAS) oleh Badan Pusat Statistik (BPS) Indonesia.
* **Cakupan Wilayah**: Data tersedia untuk seluruh kabupaten/kota di Indonesia pada tahun 2017.
* **Jenis Data**:
    * `data_uas.xlsx`: Data utama yang berisi indikator persentase kerentanan sosial (misalnya, persentase penduduk miskin, persentase penduduk berpendidikan rendah, dll.) serta data jumlah populasi dan rumah tangga.
    * `kabupaten.json`: Data GeoJSON yang berisi geometri batas wilayah kabupaten/kota di Indonesia, digunakan untuk visualisasi peta.
    * `sovi_data.csv`: Data asli (raw) yang digunakan sebagai referensi dan opsi unduhan.

Semua data telah melalui proses pembersihan dan pengolahan awal untuk memastikan kualitas dan konsistensi, termasuk penanganan `NA` (missing values) dengan menggantinya menjadi 0, serta penyesuaian tipe data.

## 4. Teknologi

Dashboard ini dibangun menggunakan kerangka kerja Shiny dalam bahasa pemrograman R, memanfaatkan berbagai paket R untuk analisis data dan visualisasi interaktif:

* **Shiny**: Kerangka kerja utama untuk membangun aplikasi web interaktif dengan R.
* **bs4Dash**: Untuk tampilan antarmuka pengguna yang modern dan responsif (berbasis Bootstrap 4).
* **readxl, dplyr, tidyr**: Untuk manipulasi dan pembersihan data.
* **DT**: Untuk tampilan tabel data interaktif.
* **ggplot2, plotly**: Untuk membuat visualisasi data statis dan interaktif (histogram, boxplot, scatter plot).
* **leaflet, sf**: Untuk visualisasi data geografis (peta).
* **car**: Untuk `leveneTest` (uji homogenitas varians).
* **EnvStats**: Untuk `varTest` (uji varians satu populasi).
* **nortest**: Untuk `lillie.test` (uji normalitas Lilliefors).
* **olsrr**: Untuk `ols_vif_tol` (analisis multikolinearitas VIF).
* **corrplot**: Untuk visualisasi matriks korelasi.
* **lmtest**: Untuk `bptest` (uji heteroskedastisitas Breusch-Pagan).
* **randtests**: Untuk `runs.test` (uji autokorelasi Runs Test).
* **e1071**: Untuk perhitungan skewness dan kurtosis.
* **agricolae**: Untuk `HSD.test` (uji lanjut Tukey HSD).
* **shinyjs**: Untuk fungsionalitas JavaScript tambahan.

## 5. Cara Menggunakan

### 5.1. Persyaratan
Untuk menjalankan aplikasi ini secara lokal, Anda memerlukan:
* **R** (versi 4.0 atau lebih baru disarankan)
* **RStudio** (disarankan, untuk lingkungan pengembangan yang terintegrasi)

### 5.2. Instalasi Paket R
Pastikan Anda telah menginstal semua paket R yang dibutuhkan. Anda bisa menginstalnya dengan menjalankan perintah berikut di konsol R atau RStudio:

```R
install.packages(c("shiny", "bs4Dash", "readxl", "dplyr", "DT", "ggplot2", "plotly",
                   "leaflet", "car", "tidyr", "shinyjs", "EnvStats", "nortest",
                   "sf", "olsrr", "corrplot", "lmtest", "randtests", "e1071",
                   "agricolae"))
