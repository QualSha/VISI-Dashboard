library(shiny)
library(bs4Dash)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(car)
library(tidyr)
library(shinyjs)
library(EnvStats)
library(nortest)
library(sf)
library(olsrr)
library(corrplot)
library(lmtest)
library(randtests)
library(e1071)
library(agricolae)

variable_defs <- list(
  'DISTRICTCODE' = "Kode unik BPS untuk setiap Kabupaten/Kota",
  'PROVINCE' = "Nama Provinsi",
  'DISTRICT' = "Nama Kabupaten/Kota",
  'CHILDREN_PERCENTAGE'   = "persentase penduduk usia di bawah lima tahun",
  'FEMALE_PERCENTAGE'     = "persentase penduduk perempuan",
  'ELDERLY_PERCENTAGE'    = "persentase penduduk usia 65 tahun ke atas",
  'FHEAD_PERCENTAGE'      = "persentase rumah tangga dengan kepala rumah tangga perempuan",
  'FAMILYSIZE_PERCENTAGE' = "rata-rata jumlah anggota rumah tangga",
  'NOELECTRIC_PERCENTAGE' = "persentase rumah tangga yang tidak menggunakan listrik",
  'LOWEDU_PERCENTAGE'     = "persentase penduduk usia 15 tahun ke atas dengan pendidikan rendah",
  'GROWTH_PERCENTAGE'     = "persentase pertumbuhan populasi",
  'POVERTY_PERCENTAGE'    = "persentase penduduk miskin",
  'ILLITERATE_PERCENTAGE' = "persentase penduduk yang tidak bisa membaca dan menulis",
  'NOTRAINING_PERCENTAGE' = "persentase rumah tangga yang tidak mendapatkan pelatihan bencana",
  'DPRONE_PERCENTAGE'     = "persentase rumah tangga yang tinggal di daerah rawan bencana",
  'RENTED_PERCENTAGE'     = "persentase rumah tangga yang menyewa rumah",
  'NOSEWER_PERCENTAGE'    = "persentase rumah tangga yang tidak memiliki sistem drainase",
  'TAPWATER_PERCENTAGE'   = "persentase rumah tangga yang menggunakan air ledeng",
  'CHILDREN_PERSON'   = "jumlah penduduk usia di bawah lima tahun",
  'FEMALE_PERSON'     = "jumlah penduduk perempuan",
  'ELDERLY_PERSON'    = "jumlah penduduk usia 65 tahun ke atas",
  'LOWEDU_PERSON'     = "jumlah penduduk usia 15 tahun ke atas dengan pendidikan rendah",
  'GROWTH_PERSON'     = "jumlah pertumbuhan populasi",
  'POVERTY_PERSON'    = "jumlah penduduk miskin",
  'ILLITERATE_PERSON' = "jumlah penduduk yang tidak bisa membaca dan menulis",
  'FHEAD_HOUSEHOLD'      = "jumlah rumah tangga dengan kepala rumah tangga perempuan",
  'FAMILYSIZE_HOUSEHOLD' = "jumlah rata-rata anggota rumah tangga",
  'NOELECTRIC_HOUSEHOLD' = "jumlah rumah tangga yang tidak menggunakan listrik",
  'NOTRAINING_HOUSEHOLD' = "jumlah rumah tangga yang tidak mendapatkan pelatihan bencana",
  'DPRONE_HOUSEHOLD'     = "jumlah rumah tangga yang tinggal di daerah rawan bencana",
  'RENTED_HOUSEHOLD'     = "jumlah rumah tangga yang menyewa rumah",
  'NOSEWER_HOUSEHOLD'    = "jumlah rumah tangga yang tidak memiliki sistem drainase",
  'TAPWATER_HOUSEHOLD'   = "jumlah rumah tangga yang menggunakan air ledeng",
  'POPULATION' = "jumlah penduduk",
  'HOUSEHOLD'  = "jumlah rumah tangga"
)

conclusion_defs <- list(
  gagal_tolak = list(
    greater   = "tidak lebih besar dari",
    less      = "tidak lebih kecil dari",
    two.sided = "sama dengan"
  ),
  tolak = list(
    greater   = "lebih besar dari",
    less      = "lebih kecil dari",
    two.sided = "tidak sama dengan"
  )
)

data_path <- "data_uas.xlsx"
geojson_path <- "kabupaten.json"
rawdata_path <- "sovi_data.csv"

sovi_data_raw <- read_excel(data_path)
kabupaten_geojson <- st_read(geojson_path)
rawdata <- read.csv(rawdata_path, check.names = FALSE)

sovi_data_clean <- sovi_data_raw %>%
  mutate(DISTRICTCODE = as.character(DISTRICTCODE)) %>%
  mutate(across(-DISTRICTCODE, as.numeric)) %>%
  mutate(across(everything(), ~replace_na(., 0)))

kabupaten_clean <- kabupaten_geojson %>%
  mutate(DISTRICTCODE = as.character(idkab))

sovi_data_joined <- kabupaten_clean %>%
  right_join(sovi_data_clean, by = "DISTRICTCODE")

sovi_data_temp <- sovi_data_joined %>%
  rename(
    DISTRICT = nmkab,
    PROVINCE = nmprov
  ) %>%
  select(
    DISTRICTCODE, PROVINCE, DISTRICT,
    ends_with("_PERCENTAGE"), ends_with("_PERSON"), ends_with("_HOUSEHOLD"),
    POPULATION, HOUSEHOLD,
    geometry
  ) %>%
  filter(!st_is_empty(geometry))

all_vars <- names(sovi_data_clean)[-1]
percent_vars <- all_vars[grepl("_PERCENTAGE$", all_vars)]
person_vars <- all_vars[grepl("_PERSON$", all_vars)]
household_vars <- all_vars[grepl("_HOUSEHOLD$", all_vars)]
count_vars <- c(person_vars, household_vars)
all_test_vars <- c(percent_vars, count_vars, "POPULATION", "HOUSEHOLD")

reg_y_vars <- c("POVERTY_PERCENTAGE", "ILLITERATE_PERCENTAGE", "LOWEDU_PERCENTAGE")
reg_x_vars <- c(
  percent_vars[!percent_vars %in% reg_y_vars],
  "POPULATION", "HOUSEHOLD"
)

sovi_data <- st_as_sf(sovi_data_temp)

provinces_list_all <- c("INDONESIA", sort(unique(sovi_data$PROVINCE)))
provinces_list_only <- sort(unique(sovi_data$PROVINCE))

custom_css <- "
  .full-width-tabs .nav-tabs {
    display: flex;
    width: 100%;
  }
  .full-width-tabs .nav-item {
    flex: 1;
    text-align: center;
  }
"

ui <- bs4DashPage(
  title = "VISI DASHBOARD",
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "VISI INDONESIA DASHBOARD",
      color = "primary",
    ),
    skin = "light",
    status = "white"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "Menu Utama",
    bs4SidebarMenu(
      id = "sidebarMenu",
      bs4SidebarHeader("Navigasi"),
      bs4SidebarMenuItem(
        "Beranda",
        tabName = "beranda",
        icon = icon("home")
      ),
      bs4SidebarMenuItem(
        "Statistik Inferensia",
        icon = icon("chart-bar"),
        startExpanded = TRUE,
        bs4SidebarMenuSubItem(
          "Uji Beda Rata-Rata",
          tabName = "uji_rata",
          icon = icon("balance-scale")
        ),
        bs4SidebarMenuSubItem(
          "Uji Proporsi",
          tabName = "uji_proporsi",
          icon = icon("percentage")
        ),
        bs4SidebarMenuSubItem(
          "Uji Varians",
          tabName = "uji_varians",
          icon = icon("chart-area")
        ),
        bs4SidebarMenuSubItem(
          "ANOVA",
          tabName = "anova",
          icon = icon("project-diagram")
        )
      ),
      bs4SidebarMenuItem(
        "Analisis Regresi",
        tabName = "regresi",
        icon = icon("chart-line")
      )
    )
  ),
  body = bs4DashBody(
    withMathJax(),
    useShinyjs(),
    tags$head(tags$style(HTML(custom_css))),
    bs4TabItems(
      bs4TabItem(
        tabName = "beranda",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "beranda_tabs",
              tabPanel(
                "Overview",
                icon = icon("info-circle"),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Deskripsi Umum Dashboard",
                           width = 12, status = "primary", solidHeader = TRUE,
                           p(
                             "Selamat datang di Dashboard Visualisasi & Inferensia Kerentanan Sosial (VISI) Indonesia. Dashboard ini dirancang sebagai platform interaktif untuk menjelajahi, menganalisis, dan memvisualisasikan data kerentanan sosial di seluruh kabupaten/kota di Indonesia. Kerentanan sosial mengukur sejauh mana suatu komunitas rentan terhadap dampak negatif dari bencana alam maupun krisis sosial-ekonomi. Dengan memahami faktor-faktor demografis, sosial, dan ekonomi yang berkontribusi terhadap kerentanan, para pemangku kepentingan dapat merancang intervensi yang lebih tepat sasaran untuk meningkatkan ketahanan masyarakat."
                           ),
                           p(
                             "Aplikasi ini menyediakan berbagai fitur analisis statistik yang komprehensif. Pada menu 'Beranda', Anda dapat melihat metadata lengkap dari data yang digunakan serta melakukan eksplorasi data awal melalui peta interaktif, histogram, dan boxplot. Menu 'Statistik Inferensia' memungkinkan Anda untuk melakukan uji hipotesis fundamental seperti Uji Beda Rata-Rata, Uji Proporsi, Uji Varians, hingga ANOVA untuk membandingkan antar kelompok. Bagi pengguna tingkat lanjut, menu 'Analisis Regresi' menyediakan alat untuk membangun model, mendiagnosis asumsi klasik, hingga membuat model regresi kustom sesuai dengan variabel yang Anda pilih."
                           )
                         ),
                         bs4Card(
                           title = "Metadata Asli (Data Persentase)",
                           width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                           p(strong("Sumber: "), "Survei Sosial Ekonomi Nasional Badan Pusat Statistik Indonesia"),
                           p(strong("Cakupan Wilayah: "), "Kabupaten/Kota Seluruh Indonesia pada Tahun 2017"),
                           p(strong("Missing Value: "), "None"),
                           hr(),
                           DTOutput("metadata_asli_table")
                         ),
                         bs4Card(
                           title = "Metadata Olahan (Data Jumlah/Atribut)",
                           width = 12, status = "purple", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                           p(strong("Sumber: "), "Olahan Data Survei Sosial Ekonomi Nasional Badan Pusat Statistik Indonesia"),
                           p(strong("Cakupan Wilayah: "), "Kabupaten/Kota Seluruh Indonesia pada Tahun 2017"),
                           p(strong("Missing Value: "), "None"),
                           hr(),
                           DTOutput("metadata_olahan_table")
                         ),
                         bs4Card(
                           title = "Data",
                           width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                           DTOutput("full_data_table"),
                           hr(),
                           div(style="text-align:center;",
                               downloadButton("download_asli_data", "Download Data Asli (.csv)", class="btn-primary"),
                               " ",
                               downloadButton("download_clean_data", "Download Data Olahan (.csv)")
                           )
                         )
                  )
                )
              ),
              tabPanel(
                "Transformasi Data",
                icon = icon("exchange-alt"),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Input Transformasi",
                           width = 12, status = "info", solidHeader = TRUE,
                           selectInput("transform_var", "Variabel:", choices = all_test_vars, selected = "POVERTY_PERCENTAGE"),
                           numericInput("transform_kategori", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                           uiOutput("transform_nama_kategori_ui"),
                           actionButton("transform_process_btn", "Proses Kategorisasi", icon = icon("cogs"), class = "btn-success")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Filter Tampilan Kolom",
                           width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           uiOutput("column_selector_ui")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Tabel Data",
                           width = 12, status = "info", solidHeader = TRUE,
                           DTOutput("transform_table_output")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Interpretasi",
                           width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                           uiOutput("transform_recap_output")
                         )
                  )
                )
              ),
              tabPanel(
                "Eksplorasi Data",
                icon = icon("search"),
                fluidRow(
                  column(12, bs4Card(title = "Filter Data", width = 12, status = "success", solidHeader = TRUE,
                                     selectInput("eksplorasi_provinsi", "Pilih Provinsi:", choices = provinces_list_all, selected = "INDONESIA"),
                                     selectInput("eksplorasi_var_hist", "Variabel:", choices = all_test_vars, selected = "POVERTY_PERCENTAGE")))
                ),
                fluidRow(
                  valueBoxOutput("vb_highest", width=3), valueBoxOutput("vb_lowest", width=3),
                  valueBoxOutput("vb_aggregate", width=3), valueBoxOutput("vb_province_pct", width=3)
                ),
                fluidRow(
                  column(6, bs4Card(title = "Histogram", width = 12, status = "success", solidHeader = TRUE,
                                    plotlyOutput("eksplorasi_histogram"),
                                    downloadButton("download_hist", "Unduh Grafik (.png)", style="width:100%; margin-top: 10px;"))),
                  column(6, bs4Card(title = "Boxplot Perbandingan Variabel", width = 12, status = "success", solidHeader = TRUE,
                                    plotlyOutput("eksplorasi_boxplot"),
                                    downloadButton("download_boxplot", "Unduh Grafik (.png)", style="width:100%; margin-top: 10px;")))
                ),
                fluidRow(
                  column(12, bs4Card(title = "Peta Sebaran (Leaflet)", width = 12, status = "success", solidHeader = TRUE,
                                     leafletOutput("eksplorasi_peta")
                  ))
                ),
                fluidRow(
                  column(12, bs4Card(title = "Interpretasi", width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                     uiOutput("eksplorasi_interpretasi_output")
                  ))
                )
              ),
              tabPanel(
                "Uji Asumsi Umum",
                icon = icon("check-double"),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Filter Data",
                           width = 12, status = "warning", solidHeader = TRUE,
                           selectInput("asumsi_provinsi", "Pilih Provinsi:", choices = provinces_list_all, selected = "INDONESIA"),
                           selectInput("asumsi_var", "Variabel:", choices = all_test_vars, selected = "POVERTY_PERCENTAGE")
                         )
                  )
                ),
                fluidRow(
                  column(6,
                         bs4Card(
                           title = "Uji Normalitas (Shapiro-Wilk & Lilliefors)",
                           width = 12, status = "warning", solidHeader = TRUE,
                           p(HTML("<b>Hipotesis Uji Normalitas:</b>")),
                           p(withMathJax(HTML("$$H_0: \\text{Data berdistribusi normal}$$ $$H_1: \\text{Data tidak berdistribusi normal}$$"))),
                           verbatimTextOutput("asumsi_shapiro_hasil")
                         )
                  ),
                  column(6,
                         bs4Card(
                           title = "Uji Kesamaan Varians (F-test)",
                           width = 12, status = "warning", solidHeader = TRUE,
                           p(HTML("<b>Hipotesis Uji Varians:</b>")),
                           p(withMathJax(HTML("$$H_0: \\sigma_1^2 = \\sigma_2^2$$ $$H_1: \\sigma_1^2 \\neq \\sigma_2^2$$"))),
                           selectInput("asumsi_var_prov2", "Bandingkan dengan Provinsi:", choices = provinces_list_all, selected = "INDONESIA"),
                           verbatimTextOutput("asumsi_variance_test")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         bs4Card(
                           title = "Interpretasi Uji Asumsi",
                           width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                           uiOutput("asumsi_interpretasi_output")
                         )
                  )
                )
              )
            )
        )
      ),
      bs4TabItem(
        tabName = "uji_rata",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "uji_rata_tabs",
              tabPanel(
                "One Population",
                icon = icon("user"),
                bs4Card(
                  title = "Input Uji t Satu Populasi",
                  width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(12, selectInput("uji_rata1_var", "Variabel (Satuan Jumlah):", choices = count_vars, selected = "POVERTY_PERSON"))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_rata1_prov", "Pilih Wilayah:", choices = provinces_list_all, selected = "INDONESIA")),
                    column(6, textInput("uji_rata1_mean", "Rata-Rata:", value = ""))
                  ),
                  fluidRow(
                    column(6, numericInput("uji_rata1_mu0", "Nilai Hipotesis (μ₀):", value = 1000)),
                    column(6, selectInput("uji_rata1_jenis", "Jenis Pengujian:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Hasil Pengujian",
                  width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_rata1_hasil")
                ),
                bs4Card(
                  title = "Interpretasi",
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_rata1_interpretasi_output")
                )
              ),
              tabPanel(
                "Two Population",
                icon = icon("users"),
                bs4Card(
                  title = "Input Uji t Dua Populasi",
                  width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(12, selectInput("uji_rata2_var", "Variabel (Satuan Jumlah):", choices = count_vars, selected = "POVERTY_PERSON"))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_rata2_prov1", "Pilih Provinsi 1:", choices = provinces_list_only, selected = "DKI JAKARTA")),
                    column(6, textInput("uji_rata2_mean1", "Rata-Rata Provinsi 1:", value = ""))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_rata2_prov2", "Pilih Provinsi 2:", choices = provinces_list_only, selected = "JAWA BARAT")),
                    column(6, textInput("uji_rata2_mean2", "Rata-Rata Provinsi 2:", value = ""))
                  ),
                  fluidRow(
                    column(12, selectInput("uji_rata2_jenis", "Jenis Pengujian:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Hasil Pengujian",
                  width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_rata2_hasil")
                ),
                bs4Card(
                  title = "Interpretasi",
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_rata2_interpretasi_output")
                )
              )
            )
        )
      ),
      bs4TabItem(
        tabName = "uji_proporsi",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "uji_proporsi_tabs",
              tabPanel(
                "One Population",
                icon = icon("user"),
                bs4Card(
                  title = "Input Uji Proporsi 1 Populasi", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(6, selectInput("uji_prop1_var", "Variabel:",
                                          choices = count_vars, selected = "POVERTY_PERSON")),
                    column(6, selectInput("uji_prop1_skala", "Skala Uji:",
                                          choices = c("Kabupaten/Kota", "Provinsi", "Nasional")))
                  ),
                  uiOutput("ui_prop1_filters"),
                  fluidRow(
                    column(12, textInput("uji_prop1_prop_amati", "Proporsi Teramati (p̂):", value = ""))
                  ),
                  fluidRow(
                    column(6, numericInput("uji_prop1_h0", "Nilai Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)),
                    column(6, selectInput("uji_prop1_jenis", "Jenis Pengujian:",
                                          choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Metodologi Singkat", width=12, status="info", solidHeader=TRUE, collapsible = TRUE, collapsed=TRUE,
                  p(strong("Proporsi Teramati (p̂)"), " dihitung sebagai (Jumlah Kasus variabel terpilih) / (Total Populasi atau Rumah Tangga) pada wilayah yang dipilih."),
                  p(strong("Skala Uji"), " menentukan lingkup wilayah (sampel) yang dianalisis:"),
                  tags$ul(
                    tags$li(strong("Nasional:"), " Menganalisis proporsi untuk seluruh Indonesia."),
                    tags$li(strong("Provinsi:"), " Menganalisis proporsi gabungan (agregat) dari seluruh kab/kota di provinsi yang Anda pilih."),
                    tags$li(strong("Kabupaten/Kota:"), " Menganalisis proporsi untuk satu kab/kota spesifik yang Anda pilih.")
                  ),
                  p(strong("Uji Hipotesis"), " membandingkan Proporsi Teramati (p̂) dari sampel Anda dengan Nilai Hipotesis (p₀) yang Anda tentukan.")
                ),
                bs4Card(
                  title = "Hasil Pengujian", width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_prop1_hasil")
                ),
                bs4Card(
                  title = "Interpretasi", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_prop1_interpretasi")
                )
              ),
              tabPanel(
                "Two Population",
                icon = icon("users"),
                bs4Card(
                  title = "Input Uji Beda Dua Proporsi", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(12, selectInput("uji_prop2_var", "Variabel:", choices = count_vars, selected = "POVERTY_PERSON"))
                  ),
                  hr(),
                  fluidRow(
                    column(6,
                           h4("Populasi 1", style="text-align: center;"),
                           uiOutput("ui_prop2_filters1")
                    ),
                    column(6,
                           h4("Populasi 2", style="text-align: center;"),
                           uiOutput("ui_prop2_filters2")
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(12, selectInput("uji_prop2_jenis", "Jenis Hipotesis Alternatif:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Metodologi Singkat", width=12, status="info", solidHeader=TRUE, collapsible = TRUE, collapsed=TRUE,
                  p("Fitur ini membandingkan proporsi dari dua populasi independen (Populasi 1 vs. Populasi 2) untuk menentukan apakah ada perbedaan yang signifikan secara statistik."),
                  tags$ul(
                    tags$li(strong("Pemilihan Populasi:"), " Anda dapat secara fleksibel memilih lingkup wilayah untuk masing-masing populasi (Nasional, Provinsi, atau Kabupaten/Kota)."),
                    tags$li(strong("Perhitungan Proporsi:"), " Untuk setiap populasi, proporsi dihitung sebagai (Total Jumlah Kasus) / (Total Populasi atau Rumah Tangga)."),
                    tags$li(strong("Pengujian Statistik:"), " Aplikasi akan melakukan uji beda dua proporsi untuk mengevaluasi hipotesis nol (H₀: p₁ = p₂), yaitu apakah proporsi di kedua populasi tersebut sama.")
                  )
                ),
                bs4Card(
                  title = "Hasil Pengujian", width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_prop2_hasil")
                ),
                bs4Card(
                  title = "Interpretasi", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_prop2_interpretasi")
                )
              )
            )
        )
      ),
      bs4TabItem(
        tabName = "uji_varians",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "uji_varians_tabs",
              tabPanel(
                "One Population",
                icon = icon("user"),
                bs4Card(
                  title = "Uji Varians 1 Populasi", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(12, selectInput("uji_var1_var", "Variabel:", choices = all_test_vars, selected = "POVERTY_PERSON"))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_var1_prov", "Pilih Wilayah:", choices = provinces_list_all, selected = "INDONESIA")),
                    column(6, textInput("uji_var1_val", "Varians:", value = ""))
                  ),
                  fluidRow(
                    column(6, numericInput("uji_var1_h0", "Nilai Hipotesis (σ₀²):", value = 1)),
                    column(6, selectInput("uji_var1_jenis", "Jenis Pengujian:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Hasil Pengujian", width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_var1_hasil")
                ),
                bs4Card(
                  title = "Interpretasi", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_var1_interpretasi")
                )
              ),
              tabPanel(
                "Two Population",
                icon = icon("users"),
                bs4Card(
                  title = "Uji Varians 2 Populasi (F-test)", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(12, selectInput("uji_var2_var", "Variabel:", choices = all_test_vars, selected = "POVERTY_PERSON"))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_var2_prov1", "Pilih Provinsi 1:", choices = provinces_list_only, selected = "DKI JAKARTA")),
                    column(6, textInput("uji_var2_val1", "Varians 1:", value = ""))
                  ),
                  fluidRow(
                    column(6, selectInput("uji_var2_prov2", "Pilih Provinsi 2:", choices = provinces_list_only, selected = "JAWA BARAT")),
                    column(6, textInput("uji_var2_val2", "Varians 2:", value = ""))
                  ),
                  fluidRow(
                    column(6, numericInput("uji_var2_h0", "Rasio Hipotesis (σ₁²/σ₂²):", value = 1)),
                    column(6, selectInput("uji_var2_jenis", "Jenis Pengujian:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Lower" = "less"), selected = "two.sided"))
                  )
                ),
                bs4Card(
                  title = "Hasil Pengujian", width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("uji_var2_hasil")
                ),
                bs4Card(
                  title = "Interpretasi", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("uji_var2_interpretasi")
                )
              )
            )
        )
      ),
      bs4TabItem(
        tabName = "anova",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "anova_tabs",
              tabPanel(
                "ANOVA Satu Arah",
                icon = icon("arrow-right"),
                fluidRow(
                  bs4Card(
                    title = "Input ANOVA Satu Arah", width = 12, status = "primary", solidHeader = TRUE,
                    p("Analisis ini membandingkan rata-rata suatu variabel numerik berdasarkan satu faktor pengelompokan."),
                    fluidRow(
                      column(3, uiOutput("anova1_factor_var_ui")),
                      column(5, uiOutput("anova1_levels_ui")),
                      column(4, selectInput("anova1_var", "Pilih Variabel Dependen (Numerik):",
                                            choices = all_test_vars, selected = "POVERTY_PERCENTAGE"))
                    )
                  )
                ),
                uiOutput("anova1_results_ui")
              ),
              tabPanel(
                "ANOVA Dua Arah",
                icon = icon("arrows-alt-h"),
                fluidRow(
                  bs4Card(
                    title = "Input ANOVA Dua Arah", width = 12, status = "primary", solidHeader = TRUE,
                    p("Analisis ini menguji pengaruh dua faktor pengelompokan secara bersamaan terhadap satu variabel numerik."),
                    uiOutput("anova2_input_ui")
                  )
                ),
                uiOutput("anova2_results_ui")
              )
            )
        )
      ),
      bs4TabItem(
        tabName = "regresi",
        div(class = "full-width-tabs",
            tabsetPanel(
              id = "regresi_tabs",
              tabPanel(
                "Diagnosis Model Awal",
                icon = icon("search-plus"),
                bs4Card(
                  title = "Filter Diagnosis", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(6, selectInput("reg_diag_y_var", "Pilih Variabel Terikat (Y):", choices = reg_y_vars)),
                    column(6, selectInput("reg_diag_prov", "Pilih Wilayah Analisis:", choices = provinces_list_all, selected = "INDONESIA"))
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Matriks Korelasi Antar Variabel Persentase", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("reg_corr_matrix_plot", height = "800px"),
                    downloadButton("download_corr_matrix", "Unduh Matriks Korelasi (.png)", style="width:100%; margin-top: 10px;")
                  )
                ),
                uiOutput("reg_scatter_plots_ui")
              ),
              tabPanel(
                "Hasil Model Tetap",
                icon = icon("cogs"),
                bs4Card(
                  title = "Filter Analisis Model Tetap", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(6, selectInput("reg_y_var", "Pilih Variabel Terikat (Y):", choices = reg_y_vars)),
                    column(6, selectInput("reg_prov", "Pilih Wilayah Analisis:", choices = provinces_list_all, selected = "INDONESIA"))
                  ),
                  uiOutput("reg_remedy_ui_selector")
                ),
                fluidRow(
                  bs4Card(
                    title = "Ringkasan Uji Asumsi Klasik", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(6, uiOutput("reg_normality_output")),
                      column(6, uiOutput("reg_hetero_output"))
                    ),
                    fluidRow(
                      column(6, uiOutput("reg_autocorr_output")),
                      column(6, uiOutput("reg_multicol_output"))
                    )
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Output Asli Model Regresi (dari R)", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    verbatimTextOutput("reg_model_raw_output")
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Hasil dan Interpretasi Model", width = 12, status = "success", solidHeader = TRUE,
                    uiOutput("reg_combined_results_ui")
                  )
                )
              ),
              tabPanel(
                "Regresi Kustom",
                icon = icon("sliders-h"),
                bs4Card(
                  title = "Filter Regresi Kustom", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(4, selectInput("custom_reg_y_var", "Pilih Variabel Terikat (Y):", choices = all_test_vars, selected="POVERTY_PERCENTAGE")),
                    column(4, uiOutput("custom_reg_x_vars_ui")),
                    column(4, selectInput("custom_reg_prov", "Pilih Wilayah Analisis:", choices = provinces_list_all, selected = "INDONESIA"))
                  ),
                  uiOutput("custom_reg_remedy_ui_selector")
                ),
                fluidRow(
                  bs4Card(
                    title = "Ringkasan Uji Asumsi (Kustom)", width = 12, status = "purple", solidHeader = TRUE,
                    fluidRow(
                      column(6, uiOutput("custom_reg_normality_output")),
                      column(6, uiOutput("custom_reg_hetero_output"))
                    ),
                    fluidRow(
                      column(6, uiOutput("custom_reg_autocorr_output")),
                      column(6, uiOutput("custom_reg_multicol_output"))
                    )
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Output Model Regresi Kustom (dari R)", width = 12, status = "purple", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    verbatimTextOutput("custom_reg_model_raw_output")
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Hasil dan Interpretasi Model Kustom", width = 12, status = "maroon", solidHeader = TRUE,
                    uiOutput("custom_reg_combined_results_ui")
                  )
                )
              )
            )
        )
      )
    )
  ),
  footer = bs4DashFooter(
    tags$div(
      class = "d-flex justify-content-between",
      "Dibuat oleh Mahasiswa Politeknik Statistika STIS - 2025",
      tags$div(
        "Project UAS Komputasi Statistik"
      )
    )
  )
)

server <- function(input, output, session) {
  
  get_friendly_name <- function(var_name, definitions = variable_defs) {
    if (is.null(var_name) || length(var_name) == 0) return("")
    if (var_name == "PROVINCE") { return("Provinsi") }
    if (grepl("_CAT\\d+$", var_name)) {
      base_var <- sub("_CAT\\d+$", "", var_name)
      friendly_base <- definitions[[base_var]]
      if (is.null(friendly_base)) return(var_name)
      return(paste("kategori", friendly_base))
    }
    friendly_name <- definitions[[var_name]]
    return(if (is.null(friendly_name)) var_name else friendly_name)
  }
  
  to_title_case <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  
  sovi_data_reactive <- reactiveVal(sovi_data)
  
  render_test_output <- function(res, hypothesis_html) {
    if (!is.null(res$error)) {
      return(p(style = "color: red;", res$error))
    }
    
    test_output_text <- paste(capture.output(print(res$result)), collapse = '\n')
    
    tagList(
      withMathJax(HTML(hypothesis_html)),
      tags$hr(),
      tags$pre(test_output_text)
    )
  }
  
  eksplorasi_data_filtered <- reactive({
    if (input$eksplorasi_provinsi == "INDONESIA") {
      sovi_data
    } else {
      sovi_data %>% filter(PROVINCE == input$eksplorasi_provinsi)
    }
  })
  
  eksplorasi_boxplot_reactive <- reactive({
    req(input$eksplorasi_provinsi, input$eksplorasi_var_hist)
    selected_prov <- input$eksplorasi_provinsi
    selected_var <- input$eksplorasi_var_hist
    
    if (selected_prov == "INDONESIA") {
      plot_data <- sovi_data %>% st_drop_geometry() %>% mutate(GROUP = "INDONESIA")
    } else {
      data_prov <- sovi_data %>% st_drop_geometry() %>% filter(PROVINCE == selected_prov) %>% mutate(GROUP = selected_prov)
      data_indonesia <- sovi_data %>% st_drop_geometry() %>% mutate(GROUP = "INDONESIA")
      plot_data <- bind_rows(data_prov, data_indonesia)
    }
    
    ggplot(plot_data, aes(x = GROUP, y = .data[[selected_var]], fill = GROUP)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Perbandingan", selected_var), x = "Wilayah", y = "Nilai") +
      theme(legend.position = "none")
  })
  
  eksplorasi_histogram_reactive <- reactive({
    req(input$eksplorasi_var_hist)
    df <- st_drop_geometry(eksplorasi_data_filtered())
    
    ggplot(df, aes(x = .data[[input$eksplorasi_var_hist]])) +
      geom_histogram(aes(y = after_stat(density)), fill = "cornflowerblue", color = "white", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      theme_minimal() +
      labs(
        title = paste("Histogram & Density Plot untuk", input$eksplorasi_var_hist),
        subtitle = paste("Wilayah:", input$eksplorasi_provinsi),
        x = input$eksplorasi_var_hist,
        y = "Density"
      )
  })
  
  output$eksplorasi_histogram <- renderPlotly({
    ggplotly(eksplorasi_histogram_reactive())
  })
  output$eksplorasi_boxplot <- renderPlotly({
    ggplotly(eksplorasi_boxplot_reactive())
  })
  
  output$download_hist <- downloadHandler(
    filename = function() {
      paste0("histogram_", input$eksplorasi_var_hist, ".png")
    },
    content = function(file) {
      ggsave(file, plot = eksplorasi_histogram_reactive(), width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_boxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$eksplorasi_var_hist, ".png")
    },
    content = function(file) {
      ggsave(file, plot = eksplorasi_boxplot_reactive(), width = 8, height = 6, dpi = 300)
    }
  )
  
  output$eksplorasi_peta <- renderLeaflet({
    req(input$eksplorasi_var_hist)
    data_spasial <- eksplorasi_data_filtered()
    
    if (nrow(data_spasial) == 0) {
      return(leaflet() %>% addTiles() %>% addControl("Tidak ada data untuk ditampilkan pada wilayah ini.", position = "topright"))
    }
    
    if (!is.numeric(data_spasial[[input$eksplorasi_var_hist]])) {
      return(leaflet() %>% addTiles() %>% addControl("Variabel yang dipilih bukan numerik.", position = "topright"))
    }
    
    pal <- colorNumeric("YlOrRd", domain = data_spasial[[input$eksplorasi_var_hist]])
    
    leaflet(data_spasial) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(get(input$eksplorasi_var_hist)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(DISTRICT, ": ", round(get(input$eksplorasi_var_hist), 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = data_spasial[[input$eksplorasi_var_hist]],
        opacity = 0.7,
        title = input$eksplorasi_var_hist,
        position = "bottomright"
      )
  })
  
  output$column_selector_ui <- renderUI({
    all_cols <- names(sovi_data_reactive())
    
    default_cols <- c("DISTRICTCODE", "PROVINCE", "DISTRICT",
                      "POVERTY_PERCENTAGE", "LOWEDU_PERCENTAGE",
                      "ILLITERATE_PERCENTAGE", "NOELECTRIC_PERCENTAGE")
    
    new_cat_cols <- all_cols[grepl("_CAT\\d+$", all_cols)]
    
    selected_cols <- if (is.null(input$column_selector)) {
      union(default_cols, new_cat_cols)
    } else {
      union(input$column_selector, new_cat_cols)
    }
    
    selectizeInput(
      "column_selector",
      "Pilih kolom yang ingin ditampilkan:",
      choices = all_cols,
      selected = selected_cols,
      multiple = TRUE,
      options = list(placeholder = 'Pilih kolom...')
    )
  })
  
  filtered_data_for_transform_table <- reactive({
    req(input$column_selector)
    st_drop_geometry(sovi_data_reactive()) %>% select(all_of(input$column_selector))
  })
  
  output$transform_table_output <- renderDT({
    datatable(
      data_to_display(),
      extensions = 'Buttons',
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        scrollY = "600px",
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
          list(extend = 'excel', exportOptions = list(modifier = list(page = "all")))
        )
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  categorization_summary_message <- reactiveVal(NULL)
  
  output$transform_nama_kategori_ui <- renderUI({
    req(input$transform_kategori)
    lapply(1:input$transform_kategori, function(i) {
      textInput(paste0("transform_nama_", i), label = paste("Nama Kategori", i), value = paste("Kategori", i))
    })
  })
  
  output$column_selector_ui <- renderUI({
    all_cols <- names(sovi_data_reactive())
    
    default_cols <- c("DISTRICTCODE", "PROVINCE", "DISTRICT",
                      "POVERTY_PERCENTAGE", "LOWEDU_PERCENTAGE",
                      "ILLITERATE_PERCENTAGE", "NOELECTRIC_PERCENTAGE")
    
    new_cat_cols <- all_cols[grepl("_CAT\\d+$", all_cols)]
    
    selected_cols <- if (is.null(input$column_selector)) {
      union(default_cols, new_cat_cols)
    } else {
      union(input$column_selector, new_cat_cols)
    }
    
    selectizeInput(
      "column_selector",
      "Pilih kolom yang ingin ditampilkan:",
      choices = all_cols,
      selected = selected_cols,
      multiple = TRUE,
      options = list(placeholder = 'Pilih kolom...')
    )
  })
  
  output$transform_table_output <- renderDT({
    datatable(
      filtered_data_for_transform_table(),
      options = list(pageLength = 50, scrollX = TRUE, scrollY = "600px"),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  categorization_summary_message <- reactiveVal(NULL)
  
  observeEvent(input$transform_process_btn, {
    current_data <- sovi_data_reactive()
    var_to_cat <- input$transform_var
    num_cat <- input$transform_kategori
    
    cat_names <- sapply(1:num_cat, function(i) input[[paste0("transform_nama_", i)]])
    
    existing_cats <- sum(grepl(paste0("^", var_to_cat, "_CAT"), names(current_data)))
    new_col_name <- paste0(var_to_cat, "_CAT", existing_cats + 1)
    
    new_column <- cut(
      current_data[[var_to_cat]],
      breaks = quantile(current_data[[var_to_cat]], probs = seq(0, 1, by = 1/num_cat), na.rm = TRUE),
      labels = cat_names,
      include.lowest = TRUE,
      right = TRUE
    )
    
    current_data[[new_col_name]] <- new_column
    
    sovi_data_reactive(current_data)
    
    summary_table <- table(new_column)
    summary_text <- paste0("<li><b>", names(summary_table), "</b>: ", summary_table, " wilayah</li>", collapse = "")
    categorization_summary_message(
      tagList(
        p(HTML(paste0("Variabel '", var_to_cat, "' telah berhasil dikategorikan menjadi kolom baru: <b>", new_col_name, "</b>."))),
        p("Berikut adalah rincian jumlah wilayah per kategori:"),
        tags$ul(HTML(summary_text))
      )
    )
    
  })
  
  output$transform_recap_output <- renderUI({
    req(input$column_selector)
    all_cols <- names(sovi_data_reactive())
    
    total_cols <- length(all_cols)
    shown_cols <- length(input$column_selector)
    hidden_cols_names <- setdiff(all_cols, input$column_selector)
    
    new_cat_cols <- all_cols[grepl("_CAT\\d+$", all_cols)]
    
    tagList(
      strong("Rekap Tampilan Tabel:"),
      tags$ul(
        tags$li(paste0("Saat ini menampilkan ", shown_cols, " dari total ", total_cols, " kolom yang tersedia.")),
        if (length(hidden_cols_names) > 0) {
          tags$li(paste0("Kolom yang disembunyikan: ", paste(hidden_cols_names, collapse = ", ")))
        },
        if (length(new_cat_cols) > 0) {
          tags$li(HTML(paste0("Variabel kategori yang telah dibuat: <b>", paste(new_cat_cols, collapse = ", "), "</b>")))
        }
      ),
      if (!is.null(categorization_summary_message())) {
        tagList(
          hr(),
          strong("Rekap Hasil Kategorisasi Terakhir:"),
          categorization_summary_message()
        )
      }
    )
  })
  
  eksplorasi_summary_stats <- reactive({
    req(input$eksplorasi_provinsi, input$eksplorasi_var_hist)
    prov_name <- input$eksplorasi_provinsi
    var_name <- input$eksplorasi_var_hist
    
    data_nasional <- sovi_data %>% st_drop_geometry()
    
    data_provinsi <- if (prov_name == "INDONESIA") {
      data_nasional
    } else {
      data_nasional %>% filter(PROVINCE == prov_name)
    }
    
    peringkat_prov <- data_nasional %>%
      group_by(PROVINCE) %>%
      summarise(MeanValue = mean(.data[[var_name]], na.rm = TRUE)) %>%
      mutate(Rank = rank(-MeanValue, ties.method = "min")) %>%
      filter(PROVINCE == prov_name)
    
    tertinggi_prov <- data_provinsi %>% filter(.data[[var_name]] == max(.data[[var_name]]))
    terendah_prov <- data_provinsi %>% filter(.data[[var_name]] == min(.data[[var_name]]))
    
    data_nasional$RankKab <- rank(-data_nasional[[var_name]], ties.method = "min")
    rank_tertinggi <- data_nasional %>% filter(DISTRICTCODE == tertinggi_prov$DISTRICTCODE[1]) %>% pull(RankKab)
    rank_terendah <- data_nasional %>% filter(DISTRICTCODE == terendah_prov$DISTRICTCODE[1]) %>% pull(RankKab)
    
    var_total_name <- "POPULATION"
    var_raw_name <- sub("_PERCENTAGE$", "_PERSON", var_name)
    if (!var_raw_name %in% names(data_nasional)) {
      var_raw_name <- sub("_PERCENTAGE$", "_HOUSEHOLD", var_name)
      var_total_name <- if (var_raw_name %in% names(data_nasional)) "HOUSEHOLD" else "POPULATION"
    }
    
    agregat_total <- sum(data_provinsi[[var_total_name]], na.rm = TRUE)
    persen_prov <- if(grepl("_PERCENTAGE$", var_name)) {
      sum(data_provinsi[[var_raw_name]], na.rm = TRUE) / agregat_total * 100
    } else { NA }
    
    vec_data <- data_provinsi[[var_name]]
    
    list(
      peringkat_prov_rank = if(nrow(peringkat_prov) > 0) peringkat_prov$Rank else NA,
      peringkat_prov_mean = if(nrow(peringkat_prov) > 0) peringkat_prov$MeanValue else NA,
      tertinggi_nama = tertinggi_prov$DISTRICT[1],
      tertinggi_nilai = max(vec_data, na.rm=TRUE),
      tertinggi_rank_nas = rank_tertinggi[1],
      terendah_nama = terendah_prov$DISTRICT[1],
      terendah_nilai = min(vec_data, na.rm=TRUE),
      terendah_rank_nas = rank_terendah[1],
      agregat_total = agregat_total,
      agregat_label = var_total_name,
      persen_prov = persen_prov,
      desk_mean = mean(vec_data, na.rm = TRUE),
      desk_median = median(vec_data, na.rm = TRUE),
      desk_skew = e1071::skewness(vec_data, na.rm = TRUE),
      desk_kurt = e1071::kurtosis(vec_data, na.rm = TRUE)
    )
  })
  
  output$vb_highest <- renderValueBox({
    stats <- eksplorasi_summary_stats()
    valueBox(
      value = tags$h3(style="font-weight: bold;", round(stats$tertinggi_nilai, 2)),
      subtitle = paste("Tertinggi:", stats$tertinggi_nama),
      icon = icon("arrow-up"),
      color = "danger"
    )
  })
  output$vb_lowest <- renderValueBox({
    stats <- eksplorasi_summary_stats()
    valueBox(
      value = tags$h3(style="font-weight: bold;", round(stats$terendah_nilai, 2)),
      subtitle = paste("Terendah:", stats$terendah_nama),
      icon = icon("arrow-down"),
      color = "success"
    )
  })
  output$vb_aggregate <- renderValueBox({
    stats <- eksplorasi_summary_stats()
    valueBox(
      value = tags$h3(style="font-weight: bold;", format(stats$agregat_total, big.mark = ".", decimal.mark=",")),
      subtitle = paste("Total", stats$agregat_label),
      icon = icon("users"),
      color = "primary"
    )
  })
  output$vb_province_pct <- renderValueBox({
    stats <- eksplorasi_summary_stats()
    val <- if (is.na(stats$persen_prov)) "N/A" else paste0(round(stats$persen_prov, 2), "%")
    valueBox(
      value = tags$h3(style="font-weight: bold;", val),
      subtitle = "Persentase Agregat Provinsi",
      icon = icon("chart-pie"),
      color = "info"
    )
  })
  
  output$eksplorasi_interpretasi_output <- renderUI({
    stats <- eksplorasi_summary_stats()
    prov_name <- input$eksplorasi_provinsi
    var_desc <- variable_defs[[input$eksplorasi_var_hist]]
    
    skew_val <- round(stats$desk_skew, 2)
    skew_text <- if (skew_val > 1) {
      paste0("Distribusi sangat condong ke kanan (positive skew) dengan nilai ", skew_val, ". Ini berarti mayoritas kabupaten/kota memiliki ", var_desc, " yang relatif rendah, namun terdapat beberapa wilayah dengan nilai sangat tinggi yang 'menarik' nilai rata-rata menjadi lebih besar dari median.")
    } else if (skew_val > 0.5) {
      paste0("Distribusi cukup condong ke kanan (positive skew) dengan nilai ", skew_val, ". Ini menunjukkan kecenderungan data terkonsentrasi pada nilai-nilai yang lebih rendah.")
    } else if (skew_val < -1) {
      paste0("Distribusi sangat condong ke kiri (negative skew) dengan nilai ", skew_val, ". Ini berarti mayoritas kabupaten/kota memiliki ", var_desc, " yang relatif tinggi, namun terdapat beberapa wilayah dengan nilai sangat rendah yang 'menarik' nilai rata-rata menjadi lebih kecil dari median.")
    } else if (skew_val < -0.5) {
      paste0("Distribusi cukup condong ke kiri (negative skew) dengan nilai ", skew_val, ". Ini menunjukkan kecenderungan data terkonsentrasi pada nilai-nilai yang lebih tinggi.")
    } else {
      paste0("Distribusi mendekati simetris dengan nilai ", skew_val, ". Ini artinya sebaran data cukup merata di sekitar nilai rata-rata, tanpa ada pencilan ekstrem yang signifikan di satu sisi.")
    }
    
    kurt_val <- round(stats$desk_kurt, 2)
    kurt_text <- if (kurt_val > 1) {
      paste0("Distribusi leptokurtik (nilai ", kurt_val, "), menunjukkan puncak yang lebih tajam dan ekor yang lebih tebal daripada distribusi normal. Ini menandakan bahwa nilai-nilai ekstrem (outlier) lebih mungkin terjadi dan sebagian besar data sangat terpusat di sekitar rata-rata.")
    } else if (kurt_val < -1) {
      paste0("Distribusi platykurtik (nilai ", kurt_val, "), menunjukkan puncak yang lebih datar dan ekor yang lebih tipis. Ini menandakan bahwa data lebih tersebar merata dan nilai-nilai ekstrem lebih jarang terjadi.")
    } else {
      paste0("Distribusi mesokurtik (nilai ", kurt_val, "), dengan puncak dan sebaran ekor yang mirip dengan distribusi normal, menunjukkan risiko adanya nilai ekstrem yang wajar.")
    }
    
    poin_utama <- tags$ol(
      if (prov_name != "INDONESIA") {
        tags$li(HTML(paste0("Secara nasional, provinsi ", strong(prov_name), " menempati ", strong("peringkat ke-", stats$peringkat_prov_rank), " untuk nilai rata-rata ", var_desc, " (", round(stats$peringkat_prov_mean, 2), ").")))
      },
      tags$li(HTML(paste0("Wilayah dengan angka ", var_desc, " tertinggi adalah ", strong(stats$tertinggi_nama), " dengan nilai ", strong(round(stats$tertinggi_nilai, 2)),
                          if(prov_name != "INDONESIA") paste0(", yang menempati ", strong("peringkat ke-", stats$tertinggi_rank_nas), " secara nasional.")))),
      tags$li(HTML(paste0("Wilayah dengan angka ", var_desc, " terendah adalah ", strong(stats$terendah_nama), " dengan nilai ", strong(round(stats$terendah_nilai, 2)),
                          if(prov_name != "INDONESIA") paste0(", yang menempati ", strong("peringkat ke-", stats$terendah_rank_nas), " secara nasional.")))),
      tags$li("Ringkasan Statistik Deskriptif:",
              tags$ul(
                tags$li(paste("Rata-rata:", round(stats$desk_mean, 2))),
                tags$li(paste("Median:", round(stats$desk_median, 2))),
                tags$li(paste("Nilai Maksimum:", round(stats$tertinggi_nilai, 2))),
                tags$li(paste("Nilai Minimum:", round(stats$terendah_nilai, 2)))
              )
      ),
      tags$li(paste("Skewness:", skew_text)),
      tags$li(paste("Kurtosis:", kurt_text))
    )
    
    tagList(poin_utama)
  })
  
  asumsi_data_filtered <- reactive({
    req(input$asumsi_provinsi, input$asumsi_var)
    if (input$asumsi_provinsi == "INDONESIA") {
      sovi_data
    } else {
      sovi_data %>% filter(PROVINCE == input$asumsi_provinsi)
    }
  })
  
  normality_tests_reactive <- reactive({
    req(input$asumsi_provinsi, input$asumsi_var)
    df <- st_drop_geometry(asumsi_data_filtered())
    data_vec <- df[[input$asumsi_var]]
    
    results <- list(shapiro = NULL, lillie = NULL, msg_shapiro = NULL, msg_lillie = NULL)
    
    if (length(data_vec) < 3) {
      results$msg_shapiro <- "Data tidak cukup untuk uji Shapiro-Wilk (n < 3)."
    } else if (length(data_vec) > 5000) {
      results$msg_shapiro <- "Data > 5000, gunakan uji Lilliefors."
    } else {
      test <- try(shapiro.test(data_vec), silent = TRUE)
      if (inherits(test, "try-error")) results$msg_shapiro <- "Error: Uji Shapiro-Wilk gagal." else results$shapiro <- test
    }
    
    if (length(data_vec) < 5) {
      results$msg_lillie <- "Data tidak cukup untuk uji Lilliefors (n < 5)."
    } else {
      test <- try(lillie.test(data_vec), silent = TRUE)
      if (inherits(test, "try-error")) results$msg_lillie <- "Error: Uji Lilliefors gagal." else results$lillie <- test
    }
    
    results
  })
  
  output$asumsi_shapiro_hasil <- renderPrint({
    res <- normality_tests_reactive()
    cat("Results of Hypothesis Test\n--------------------------\n")
    if (!is.null(res$shapiro)) print(res$shapiro) else cat(res$msg_shapiro)
    
    cat("\n\nResults of Hypothesis Test\n--------------------------\n")
    if (!is.null(res$lillie)) print(res$lillie) else cat(res$msg_lillie)
  })
  
  asumsi_variance_reactive <- reactive({
    req(input$asumsi_provinsi, input$asumsi_var, input$asumsi_var_prov2)
    
    wilayah1 <- input$asumsi_provinsi
    wilayah2 <- input$asumsi_var_prov2
    
    if (wilayah1 == wilayah2) return(list(error="Wilayah 1 dan Wilayah 2 tidak boleh sama."))
    
    data1 <- if(wilayah1 == "INDONESIA") {
      sovi_data %>% pull(!!sym(input$asumsi_var))
    } else {
      sovi_data %>% filter(PROVINCE == wilayah1) %>% pull(!!sym(input$asumsi_var))
    }
    
    data2 <- if(wilayah2 == "INDONESIA") {
      sovi_data %>% pull(!!sym(input$asumsi_var))
    } else {
      sovi_data %>% filter(PROVINCE == wilayah2) %>% pull(!!sym(input$asumsi_var))
    }
    
    if (length(data1) < 2 || length(data2) < 2) return(list(error = "Data tidak cukup untuk uji varians (n < 2)."))
    
    test <- try(var.test(data1, data2), silent = TRUE)
    if (inherits(test, "try-error")) return(list(error = "Terjadi error pada F-test."))
    
    list(result = test)
  })
  
  output$asumsi_variance_test <- renderPrint({
    res <- asumsi_variance_reactive()
    if(!is.null(res$error)) cat(res$error) else print(res$result)
  })
  
  output$asumsi_interpretasi_output <- renderUI({
    res_norm <- normality_tests_reactive()
    res_var <- asumsi_variance_reactive()
    
    interp_norm <- if (is.null(res_norm$shapiro) || is.null(res_norm$lillie)) {
      p(em("Pilih data untuk melihat interpretasi uji normalitas."))
    } else {
      p_shapiro <- res_norm$shapiro$p.value
      p_lillie <- res_norm$lillie$p.value
      var_desc <- variable_defs[[input$asumsi_var]]
      wilayah_text <- if(input$asumsi_provinsi == "INDONESIA") "di Indonesia" else paste("di Provinsi", input$asumsi_provinsi)
      
      res_shapiro_text <- if(p_shapiro >= 0.05) "berdistribusi normal" else "tidak berdistribusi normal"
      interp1 <- paste0("Berdasarkan uji Shapiro-Wilk, distribusi ", var_desc, " ", wilayah_text, " ", res_shapiro_text, ".")
      
      res_lillie_text <- if(p_lillie >= 0.05) "berdistribusi normal" else "tidak berdistribusi normal"
      interp2 <- if ((p_shapiro >= 0.05 && p_lillie >= 0.05) || (p_shapiro < 0.05 && p_lillie < 0.05)) {
        paste0("Hal yang sama terjadi pada uji Lilliefors yang menyatakan bahwa distribusi ", var_desc, " ", wilayah_text, " ", res_lillie_text, ".")
      } else {
        paste0("Hal yang berbeda terjadi pada uji Lilliefors yang menyatakan bahwa distribusi ", var_desc, " ", wilayah_text, " ", res_lillie_text, ".")
      }
      tagList(
        tags$li(HTML(interp1)),
        tags$li(HTML(interp2))
      )
    }
    
    interp_var <- if(is.null(res_var$result)) {
      p(em("Pilih data untuk melihat interpretasi uji kesamaan varians."))
    } else {
      p_value <- res_var$result$p.value
      var_desc <- variable_defs[[input$asumsi_var]]
      wilayah1 <- input$asumsi_provinsi
      wilayah2 <- input$asumsi_var_prov2
      
      conclusion <- if(p_value < 0.05) "tidak sama dengan" else "sama dengan"
      
      tags$li(paste0("Berdasarkan F-test, varians ", var_desc, " di ", wilayah1, " ", conclusion, " varians di ", wilayah2, "."))
    }
    
    tagList(
      p("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa:"),
      tags$ul(interp_norm, interp_var)
    )
  })
  
  observe({
    req(input$uji_rata1_prov, input$uji_rata1_var)
    shinyjs::disable("uji_rata1_mean")
    
    df <- if (input$uji_rata1_prov == "INDONESIA") {
      sovi_data
    } else {
      sovi_data %>% filter(PROVINCE == input$uji_rata1_prov)
    }
    
    mean_val <- df %>%
      st_drop_geometry() %>%
      summarise(mean = mean(!!sym(input$uji_rata1_var), na.rm = TRUE)) %>%
      pull(mean)
    
    updateTextInput(session, "uji_rata1_mean", value = round(mean_val, 4))
  })
  
  uji_rata1_reactive <- reactive({
    req(input$uji_rata1_var, input$uji_rata1_mu0, input$uji_rata1_jenis, input$uji_rata1_prov)
    
    x_data <- if (input$uji_rata1_prov == "INDONESIA") {
      sovi_data %>% pull(!!sym(input$uji_rata1_var))
    } else {
      sovi_data %>% filter(PROVINCE == input$uji_rata1_prov) %>% pull(!!sym(input$uji_rata1_var))
    }
    
    if (length(x_data) < 2) return(list(error = "Error: Data tidak cukup untuk t-test (n < 2)."))
    
    test <- try(t.test(x_data, mu = input$uji_rata1_mu0, alternative = input$uji_rata1_jenis), silent = TRUE)
    if (inherits(test, "try-error")) return(list(error = "Terjadi error. Pastikan data valid."))
    
    list(result = test)
  })
  
  output$uji_rata1_hasil <- renderUI({
    res <- uji_rata1_reactive()
    req(res)
    h0 <- paste0("$$H_0: \\mu = ", input$uji_rata1_mu0, "$$")
    alt_symbol <- switch(input$uji_rata1_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h1 <- paste0("$$H_1: \\mu ", alt_symbol, " ", input$uji_rata1_mu0, "$$")
    render_test_output(res, paste(h0, h1))
  })
  
  output$uji_rata1_interpretasi_output <- renderUI({
    res <- uji_rata1_reactive()
    req(res$result)
    
    p_value <- res$result$p.value
    var_desc <- variable_defs[[input$uji_rata1_var]]
    
    wilayah_text <- if(input$uji_rata1_prov == "INDONESIA") "di Indonesia" else paste("di Provinsi", input$uji_rata1_prov)
    
    test_type <- input$uji_rata1_jenis
    h0_val <- input$uji_rata1_mu0
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[test_type]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[test_type]]
    }
    
    conclusion <- paste("rata-rata", var_desc, wilayah_text, conclusion_text, h0_val)
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", conclusion, "."))
  })
  
  observe({
    req(input$uji_rata2_prov1, input$uji_rata2_prov2, input$uji_rata2_var)
    shinyjs::disable("uji_rata2_mean1"); shinyjs::disable("uji_rata2_mean2")
    
    mean_val1 <- sovi_data %>% filter(PROVINCE == input$uji_rata2_prov1) %>% st_drop_geometry() %>% summarise(mean = mean(!!sym(input$uji_rata2_var), na.rm = TRUE)) %>% pull(mean)
    mean_val2 <- sovi_data %>% filter(PROVINCE == input$uji_rata2_prov2) %>% st_drop_geometry() %>% summarise(mean = mean(!!sym(input$uji_rata2_var), na.rm = TRUE)) %>% pull(mean)
    
    updateTextInput(session, "uji_rata2_mean1", value = round(mean_val1, 4))
    updateTextInput(session, "uji_rata2_mean2", value = round(mean_val2, 4))
  })
  
  uji_rata2_reactive <- reactive({
    req(input$uji_rata2_var, input$uji_rata2_prov1, input$uji_rata2_prov2, input$uji_rata2_jenis)
    
    if (input$uji_rata2_prov1 == input$uji_rata2_prov2) return(list(error = "Error: Provinsi 1 dan 2 tidak boleh sama."))
    
    data1 <- sovi_data %>% filter(PROVINCE == input$uji_rata2_prov1) %>% pull(!!sym(input$uji_rata2_var))
    data2 <- sovi_data %>% filter(PROVINCE == input$uji_rata2_prov2) %>% pull(!!sym(input$uji_rata2_var))
    
    if (length(data1) < 2 || length(data2) < 2) return(list(error = "Error: Data tidak cukup (n < 2)."))
    
    levene_data <- data.frame(value = c(data1, data2), group = factor(c(rep("P1", length(data1)), rep("P2", length(data2)))))
    levene_test <- leveneTest(value ~ group, data = levene_data)
    var_equal <- levene_test$`Pr(>F)`[1] >= 0.05
    
    ttest <- t.test(data1, data2, mu = 0, alternative = input$uji_rata2_jenis, var.equal = var_equal)
    
    list(levene = levene_test, ttest = ttest)
  })
  
  output$uji_rata2_hasil <- renderUI({
    res <- uji_rata2_reactive()
    req(res)
    
    if (!is.null(res$error)) {
      return(p(style = "color: red;", res$error))
    }
    
    levene_text <- paste(capture.output(print(res$levene)), collapse = '\n')
    ttest_text <- paste(capture.output(print(res$ttest)), collapse='\n')
    
    levene_p_value <- res$levene$`Pr(>F)`[1]
    t_test_title <- if (levene_p_value >= 0.05) {
      "Two Sample T-Test (Equal Variance Assumed)"
    } else {
      "Welch Two Sample T-Test (Unequal Variance)"
    }
    
    d0 <- 0
    alt_symbol <- switch(input$uji_rata2_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h0 <- paste0("$$H_0: \\mu_1 - \\mu_2 = ", d0, "$$")
    h1 <- paste0("$$H_1: \\mu_1 - \\mu_2 ", alt_symbol, " ", d0, "$$")
    
    tagList(
      h4("Levene's Test for Homogeneity of Variance"),
      tags$pre(levene_text),
      tags$hr(),
      h4(t_test_title),
      withMathJax(HTML(paste(h0, h1))),
      tags$pre(ttest_text)
    )
  })
  
  output$uji_rata2_interpretasi_output <- renderUI({
    res <- uji_rata2_reactive()
    req(res$ttest)
    
    p_value <- res$ttest$p.value
    var_desc <- variable_defs[[input$uji_rata2_var]]
    prov1 <- input$uji_rata2_prov1
    prov2 <- input$uji_rata2_prov2
    test_type <- input$uji_rata2_jenis
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[test_type]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[test_type]]
    }
    conclusion <- paste("rata-rata", var_desc, "di Provinsi", prov1, conclusion_text, "rata-rata variabel tersebut di Provinsi", prov2)
    
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", trimws(conclusion), "."))
  })
  
  observe({
    req(input$uji_var1_prov, input$uji_var1_var)
    shinyjs::disable("uji_var1_val")
    
    df <- if (input$uji_var1_prov == "INDONESIA") {
      sovi_data
    } else {
      sovi_data %>% filter(PROVINCE == input$uji_var1_prov)
    }
    
    var_val <- df %>% st_drop_geometry() %>% summarise(v = var(!!sym(input$uji_var1_var), na.rm = TRUE)) %>% pull(v)
    updateTextInput(session, "uji_var1_val", value = round(var_val, 4))
  })
  
  uji_var1_reactive <- reactive({
    req(input$uji_var1_var, input$uji_var1_h0, input$uji_var1_jenis, input$uji_var1_prov)
    
    x_data <- if (input$uji_var1_prov == "INDONESIA") {
      sovi_data %>% pull(!!sym(input$uji_var1_var))
    } else {
      sovi_data %>% filter(PROVINCE == input$uji_var1_prov) %>% pull(!!sym(input$uji_var1_var))
    }
    
    if (length(x_data) < 2) return(list(error = "Error: Data tidak cukup untuk uji varians (n < 2)."))
    
    test <- try(varTest(x_data, sigma.squared = input$uji_var1_h0, alternative = input$uji_var1_jenis), silent = TRUE)
    if (inherits(test, "try-error")) return(list(error = "Terjadi error. Pastikan varians data tidak nol."))
    
    list(result = test)
  })
  
  output$uji_var1_hasil <- renderUI({
    res <- uji_var1_reactive()
    req(res)
    h0 <- paste0("$$H_0: \\sigma^2 = ", input$uji_var1_h0, "$$")
    alt_symbol <- switch(input$uji_var1_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h1 <- paste0("$$H_1: \\sigma^2 ", alt_symbol, " ", input$uji_var1_h0, "$$")
    render_test_output(res, paste(h0, h1))
  })
  
  output$uji_var1_interpretasi <- renderUI({
    res <- uji_var1_reactive()
    req(res$result)
    
    p_value <- res$result$p.value
    var_desc <- variable_defs[[input$uji_var1_var]]
    
    wilayah_text <- if(input$uji_var1_prov == "INDONESIA") "di Indonesia" else paste("di Provinsi", input$uji_var1_prov)
    test_type <- input$uji_var1_jenis
    h0_val <- input$uji_var1_h0
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[test_type]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[test_type]]
    }
    
    conclusion <- paste("varians", var_desc, wilayah_text, conclusion_text, h0_val)
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", conclusion, "."))
  })
  
  observe({
    req(input$uji_var2_prov1, input$uji_var2_prov2, input$uji_var2_var)
    shinyjs::disable("uji_var2_val1"); shinyjs::disable("uji_var2_val2")
    
    var_val1 <- sovi_data %>% filter(PROVINCE == input$uji_var2_prov1) %>% st_drop_geometry() %>% summarise(v = var(!!sym(input$uji_var2_var), na.rm = TRUE)) %>% pull(v)
    var_val2 <- sovi_data %>% filter(PROVINCE == input$uji_var2_prov2) %>% st_drop_geometry() %>% summarise(v = var(!!sym(input$uji_var2_var), na.rm = TRUE)) %>% pull(v)
    
    updateTextInput(session, "uji_var2_val1", value = round(var_val1, 4))
    updateTextInput(session, "uji_var2_val2", value = round(var_val2, 4))
  })
  
  uji_var2_reactive <- reactive({
    req(input$uji_var2_var, input$uji_var2_prov1, input$uji_var2_prov2, input$uji_var2_h0, input$uji_var2_jenis)
    if (input$uji_var2_prov1 == input$uji_var2_prov2) return(list(error = "Error: Provinsi 1 dan 2 tidak boleh sama."))
    
    data1 <- sovi_data %>% filter(PROVINCE == input$uji_var2_prov1) %>% pull(!!sym(input$uji_var2_var))
    data2 <- sovi_data %>% filter(PROVINCE == input$uji_var2_prov2) %>% pull(!!sym(input$uji_var2_var))
    
    if (length(data1) < 2 || length(data2) < 2) return(list(error = "Error: Data tidak cukup (n < 2)."))
    
    test <- try(var.test(data1, data2, ratio = input$uji_var2_h0, alternative = input$uji_var2_jenis), silent = TRUE)
    if (inherits(test, "try-error")) return(list(error = "Terjadi error. Pastikan data valid."))
    
    list(result = test)
  })
  
  output$uji_var2_hasil <- renderUI({
    res <- uji_var2_reactive()
    req(res)
    h0 <- paste0("$$H_0: \\frac{\\sigma_1^2}{\\sigma_2^2} = ", input$uji_var2_h0, "$$")
    alt_symbol <- switch(input$uji_var2_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h1 <- paste0("$$H_1: \\frac{\\sigma_1^2}{\\sigma_2^2} ", alt_symbol, " ", input$uji_var2_h0, "$$")
    render_test_output(res, paste(h0, h1))
  })
  
  output$uji_var2_interpretasi <- renderUI({
    res <- uji_var2_reactive()
    req(res$result)
    
    p_value <- res$result$p.value
    var_desc <- variable_defs[[input$uji_var2_var]]
    prov1 <- input$uji_var2_prov1
    prov2 <- input$uji_var2_prov2
    test_type <- input$uji_var2_jenis
    h0_val <- input$uji_var2_h0
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[test_type]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[test_type]]
    }
    
    conclusion <- paste("rasio varians", var_desc, "di Provinsi", prov1, "dengan Provinsi", prov2, conclusion_text, h0_val)
    
    if (p_value < 0.05 && h0_val == 1) {
      if (test_type == "two.sided") {
        conclusion <- paste0(conclusion, ". Dengan kata lain, varians ", var_desc, " dari kedua provinsi tersebut tidak bernilai sama")
      } else if (test_type == "greater") {
        conclusion <- paste0(conclusion, ". Dengan kata lain, varians ", var_desc, " di Provinsi ", prov1, " lebih besar dari varians variabel tersebut di Provinsi ", prov2)
      } else if (test_type == "less") {
        conclusion <- paste0(conclusion, ". Dengan kata lain, varians ", var_desc, " di Provinsi ", prov1, " lebih kecil dari varians variabel tersebut di Provinsi ", prov2)
      }
    } else if (p_value >= 0.05 && h0_val == 1 && test_type == "two.sided") {
      conclusion <- paste0(conclusion, ". Dengan kata lain, varians ", var_desc, " dari kedua provinsi tersebut bernilai sama")
    }
    
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", trimws(conclusion), "."))
  })
  
  get_prop_data <- function(var, skala, prov = NULL, dist = NULL) {
    var_total <- if (grepl("_PERSON$", var)) "POPULATION" else "HOUSEHOLD"
    
    df <- sovi_data
    if (skala == "Provinsi") {
      df <- df %>% filter(PROVINCE == prov)
    } else if (skala == "Kabupaten/Kota") {
      df <- df %>% filter(PROVINCE == prov, DISTRICT == dist)
    }
    
    summary_data <- st_drop_geometry(df) %>%
      summarise(x = sum(.data[[var]], na.rm = TRUE), n = sum(.data[[var_total]], na.rm = TRUE))
    
    prop_value <- if (summary_data$n > 0) summary_data$x / summary_data$n else 0
    
    list(x = round(summary_data$x), n = round(summary_data$n), p = prop_value)
  }
  
  output$ui_prop1_filters <- renderUI({
    skala <- req(input$uji_prop1_skala)
    
    if (skala == "Nasional") {
      return(NULL)
    } else if (skala == "Provinsi") {
      fluidRow(
        column(12, selectInput("uji_prop1_prov", "Pilih Provinsi:", choices = provinces_list_only))
      )
    } else {
      fluidRow(
        column(6, selectInput("uji_prop1_prov_for_kab", "Pilih Provinsi:", choices = provinces_list_only)),
        column(6, uiOutput("ui_prop1_kab_selector"))
      )
    }
  })
  
  output$ui_prop1_kab_selector <- renderUI({
    req(input$uji_prop1_prov_for_kab)
    distrik_list <- sort(unique(sovi_data$DISTRICT[sovi_data$PROVINCE == input$uji_prop1_prov_for_kab]))
    selectInput("uji_prop1_kab", "Pilih Kab/Kota:", choices = distrik_list)
  })
  
  uji_prop1_data <- reactive({
    skala <- req(input$uji_prop1_skala)
    var <- req(input$uji_prop1_var)
    
    if (skala == "Provinsi") req(input$uji_prop1_prov)
    if (skala == "Kabupaten/Kota") req(input$uji_prop1_prov_for_kab, input$uji_prop1_kab)
    
    get_prop_data(
      var = var,
      skala = skala,
      prov = if (skala == "Provinsi") input$uji_prop1_prov else if (skala == "Kabupaten/Kota") input$uji_prop1_prov_for_kab else NULL,
      dist = if (skala == "Kabupaten/Kota") input$uji_prop1_kab else NULL
    )
  })
  
  observe({
    data <- uji_prop1_data()
    shinyjs::disable("uji_prop1_prop_amati")
    updateTextInput(session, "uji_prop1_prop_amati", value = round(data$p, 6))
  })
  
  uji_prop1_reactive <- reactive({
    data <- uji_prop1_data()
    req(input$uji_prop1_h0)
    
    if (data$n == 0) return(list(error = "Jumlah total (n) pada sampel adalah nol."))
    
    test <- try(prop.test(x = data$x, n = data$n, p = input$uji_prop1_h0, alternative = input$uji_prop1_jenis, correct = FALSE), silent = TRUE)
    if(inherits(test, "try-error")) return(list(error="Input tidak valid. Pastikan 0 ≤ p₀ ≤ 1."))
    
    list(result = test)
  })
  
  output$uji_prop1_hasil <- renderUI({
    res <- uji_prop1_reactive()
    req(res)
    
    p0_val <- req(input$uji_prop1_h0)
    h0 <- paste0("$$H_0: p = ", p0_val, "$$")
    alt_symbol <- switch(input$uji_prop1_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h1 <- paste0("$$H_1: p ", alt_symbol, " ", p0_val, "$$")
    render_test_output(res, paste(h0, h1))
  })
  
  output$uji_prop1_interpretasi <- renderUI({
    res <- uji_prop1_reactive()
    req(res$result)
    
    p_value <- res$result$p.value
    var_desc <- variable_defs[[input$uji_prop1_var]]
    skala <- input$uji_prop1_skala
    
    wilayah_text <- if (skala == "Nasional") {
      "di Indonesia"
    } else if (skala == "Provinsi") {
      paste("di Provinsi", input$uji_prop1_prov)
    } else {
      paste("di Kabupaten/Kota", input$uji_prop1_kab)
    }
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[input$uji_prop1_jenis]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[input$uji_prop1_jenis]]
    }
    
    conclusion <- paste("proporsi", var_desc, wilayah_text, conclusion_text, input$uji_prop1_h0)
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", conclusion, "."))
  })
  
  output$ui_prop2_filters1 <- renderUI({
    tagList(
      selectInput("uji_prop2_skala1", "Skala Uji 1:", choices = c("Kabupaten/Kota", "Provinsi", "Nasional")),
      uiOutput("ui_prop2_prov_kab1"),
      textInput("uji_prop2_prop_amati1", "Proporsi Teramati (p̂₁):", value = "")
    )
  })
  output$ui_prop2_prov_kab1 <- renderUI({
    skala <- req(input$uji_prop2_skala1)
    if(skala == "Provinsi"){
      selectInput("uji_prop2_prov1", "Pilih Provinsi 1:", choices = provinces_list_only)
    } else if (skala == "Kabupaten/Kota") {
      tagList(
        selectInput("uji_prop2_prov_for_kab1", "Pilih Provinsi 1:", choices = provinces_list_only),
        uiOutput("ui_prop2_kab1_selector")
      )
    }
  })
  output$ui_prop2_kab1_selector <- renderUI({
    req(input$uji_prop2_prov_for_kab1)
    dist_list <- sort(unique(sovi_data$DISTRICT[sovi_data$PROVINCE == input$uji_prop2_prov_for_kab1]))
    selectInput("uji_prop2_kab1", "Pilih Kab/Kota 1:", choices = dist_list)
  })
  
  output$ui_prop2_filters2 <- renderUI({
    tagList(
      selectInput("uji_prop2_skala2", "Skala Uji 2:", choices = c("Kabupaten/Kota", "Provinsi", "Nasional")),
      uiOutput("ui_prop2_prov_kab2"),
      textInput("uji_prop2_prop_amati2", "Proporsi Teramati (p̂₂):", value = "")
    )
  })
  output$ui_prop2_prov_kab2 <- renderUI({
    skala <- req(input$uji_prop2_skala2)
    if(skala == "Provinsi"){
      selectInput("uji_prop2_prov2", "Pilih Provinsi 2:", choices = provinces_list_only, selected = provinces_list_only[2])
    } else if (skala == "Kabupaten/Kota") {
      tagList(
        selectInput("uji_prop2_prov_for_kab2", "Pilih Provinsi 2:", choices = provinces_list_only, selected = provinces_list_only[2]),
        uiOutput("ui_prop2_kab2_selector")
      )
    }
  })
  output$ui_prop2_kab2_selector <- renderUI({
    req(input$uji_prop2_prov_for_kab2)
    dist_list <- sort(unique(sovi_data$DISTRICT[sovi_data$PROVINCE == input$uji_prop2_prov_for_kab2]))
    selectInput("uji_prop2_kab2", "Pilih Kab/Kota 2:", choices = dist_list)
  })
  
  uji_prop2_data <- reactive({
    var <- req(input$uji_prop2_var)
    skala1 <- req(input$uji_prop2_skala1); skala2 <- req(input$uji_prop2_skala2)
    
    if (skala1 == "Provinsi") req(input$uji_prop2_prov1)
    if (skala1 == "Kabupaten/Kota") req(input$uji_prop2_prov_for_kab1, input$uji_prop2_kab1)
    if (skala2 == "Provinsi") req(input$uji_prop2_prov2)
    if (skala2 == "Kabupaten/Kota") req(input$uji_prop2_prov_for_kab2, input$uji_prop2_kab2)
    
    data1 <- get_prop_data(var, skala1, prov = if(skala1=="Provinsi") input$uji_prop2_prov1 else if(skala1=="Kabupaten/Kota") input$uji_prop2_prov_for_kab1, dist = if(skala1=="Kabupaten/Kota") input$uji_prop2_kab1)
    data2 <- get_prop_data(var, skala2, prov = if(skala2=="Provinsi") input$uji_prop2_prov2 else if(skala2=="Kabupaten/Kota") input$uji_prop2_prov_for_kab2, dist = if(skala2=="Kabupaten/Kota") input$uji_prop2_kab2)
    
    if(data1$n == 0 || data2$n == 0) return(list(error="Jumlah total (n) di salah satu populasi adalah nol."))
    
    list(x1=data1$x, n1=data1$n, p1=data1$p, x2=data2$x, n2=data2$n, p2=data2$p, error=NULL)
  })
  
  observe({
    data <- uji_prop2_data()
    req(data)
    shinyjs::disable("uji_prop2_prop_amati1"); shinyjs::disable("uji_prop2_prop_amati2")
    if(!is.null(data$error)) {
      updateTextInput(session, "uji_prop2_prop_amati1", value = "Error")
      updateTextInput(session, "uji_prop2_prop_amati2", value = "Error")
    } else {
      updateTextInput(session, "uji_prop2_prop_amati1", value = round(data$p1, 6))
      updateTextInput(session, "uji_prop2_prop_amati2", value = round(data$p2, 6))
    }
  })
  
  uji_prop2_reactive <- reactive({
    data <- uji_prop2_data()
    req(input$uji_prop2_jenis)
    if(!is.null(data$error)) return(list(error=data$error))
    
    test <- try(prop.test(x=c(data$x1, data$x2), n=c(data$n1, data$n2), alternative = input$uji_prop2_jenis, correct = FALSE), silent=TRUE)
    if(inherits(test, "try-error")) return(list(error="Input tidak valid untuk uji proporsi."))
    
    list(result = test)
  })
  
  output$uji_prop2_hasil <- renderUI({
    res <- uji_prop2_reactive()
    req(res)
    h0 <- "$$H_0: p_1 = p_2$$"
    alt_symbol <- switch(input$uji_prop2_jenis, "two.sided" = "\\neq", "greater" = ">", "less" = "<")
    h1 <- paste0("$$H_1: p_1 ", alt_symbol, " p_2$$")
    render_test_output(res, paste(h0, h1))
  })
  
  get_wilayah_name <- function(skala, prov=NULL, kab=NULL) {
    if (skala == "Nasional") "Nasional"
    else if (skala == "Provinsi") paste("Provinsi", prov)
    else paste("Kabupaten/Kota", kab)
  }
  
  output$uji_prop2_interpretasi <- renderUI({
    res <- uji_prop2_reactive()
    req(res$result)
    
    p_value <- res$result$p.value
    var_desc <- variable_defs[[input$uji_prop2_var]]
    
    nama1 <- get_wilayah_name(input$uji_prop2_skala1, prov = if(input$uji_prop2_skala1=="Provinsi") input$uji_prop2_prov1 else input$uji_prop2_prov_for_kab1, kab = if(input$uji_prop2_skala1=="Kabupaten/Kota") input$uji_prop2_kab1)
    nama2 <- get_wilayah_name(input$uji_prop2_skala2, prov = if(input$uji_prop2_skala2=="Provinsi") input$uji_prop2_prov2 else input$uji_prop2_prov_for_kab2, kab = if(input$uji_prop2_skala2=="Kabupaten/Kota") input$uji_prop2_kab2)
    
    if (p_value < 0.05) {
      conclusion_text <- conclusion_defs$tolak[[input$uji_prop2_jenis]]
    } else {
      conclusion_text <- conclusion_defs$gagal_tolak[[input$uji_prop2_jenis]]
    }
    
    conclusion <- paste("proporsi", var_desc, "di", nama1, conclusion_text, "proporsi variabel tersebut di", nama2)
    p(paste0("Dengan tingkat kepercayaan 95%, dapat dinyatakan bahwa ", conclusion, "."))
  })
  
  diagnosis_data <- reactive({
    req(input$reg_diag_y_var, input$reg_diag_prov)
    df <- if(input$reg_diag_prov == "INDONESIA") {
      sovi_data_reactive()
    } else {
      sovi_data_reactive() %>% filter(PROVINCE == input$reg_diag_prov)
    }
    st_drop_geometry(df) %>% na.omit()
  })
  
  corr_matrix_plot_object <- reactive({
    df_select <- sovi_data_reactive() %>%
      st_drop_geometry() %>%
      select(any_of(percent_vars), "POPULATION", "HOUSEHOLD")
    validate(need(nrow(df_select) > 1 && ncol(df_select) > 1, "Data tidak cukup untuk membuat matriks korelasi."))
    cor(df_select, use = "complete.obs")
  })
  
  output$reg_corr_matrix_plot <- renderPlot({
    corrplot(corr_matrix_plot_object(), method = "color", type = "upper", order = "hclust",
             addCoef.col = "black", tl.col="black", tl.srt=45,
             sig.level = 0.05, insig = "blank", diag=FALSE)
  })
  
  output$download_corr_matrix <- downloadHandler(
    filename = function() { "matriks_korelasi.png" },
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      corrplot(corr_matrix_plot_object(), method = "color", type = "upper", order = "hclust",
               addCoef.col = "black", tl.col="black", tl.srt=45,
               sig.level = 0.05, insig = "blank", diag=FALSE)
      dev.off()
    }
  )
  
  output$reg_scatter_plots_ui <- renderUI({
    req(input$reg_diag_y_var)
    y_var <- input$reg_diag_y_var
    x_vars_to_plot <- setdiff(c(percent_vars, "POPULATION", "HOUSEHOLD"), y_var)
    
    plot_outputs <- lapply(x_vars_to_plot, function(var) {
      plot_id <- paste0("scatter_diag_", make.names(var))
      r2_id <- paste0("r2_diag_", make.names(var))
      download_id <- paste0("download_scatter_", make.names(var))
      
      bs4Card(
        title = paste("Scatter Plot:", y_var, "vs", var),
        width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
        plotlyOutput(plot_id),
        verbatimTextOutput(r2_id),
        downloadButton(download_id, "Unduh Grafik (.png)", style="width:100%; margin-top: 10px;")
      )
    })
    fluidRow(do.call(tagList, plot_outputs))
  })
  
  observe({
    df <- diagnosis_data()
    y_var <- req(input$reg_diag_y_var)
    x_vars_to_plot <- setdiff(c(percent_vars, "POPULATION", "HOUSEHOLD"), y_var)
    
    for (var in x_vars_to_plot) {
      local({
        current_var <- var
        
        plot_id <- paste0("scatter_diag_", make.names(current_var))
        r2_id <- paste0("r2_diag_", make.names(current_var))
        download_id <- paste0("download_scatter_", make.names(current_var))
        
        scatter_plot_obj <- reactive({
          ggplot(df, aes(x = .data[[current_var]], y = .data[[y_var]])) +
            geom_point(alpha = 0.6, color = "dodgerblue") +
            geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
            labs(x = current_var, y = y_var) +
            theme_minimal(base_size = 14)
        })
        
        output[[plot_id]] <- renderPlotly({
          ggplotly(scatter_plot_obj())
        })
        
        output[[r2_id]] <- renderPrint({
          model <- lm(as.formula(paste0("`",y_var, "` ~ `", current_var, "`")), data = df)
          cat("R-squared:", round(summary(model)$r.squared, 4))
        })
        
        output[[download_id]] <- downloadHandler(
          filename = function() {
            paste0("scatter_", y_var, "_vs_", current_var, ".png")
          },
          content = function(file) {
            ggsave(file, plot = scatter_plot_obj(), width = 8, height = 6, dpi = 300)
          }
        )
      })
    }
  })
  
  fixed_regression_data <- reactive({
    req(input$reg_y_var, input$reg_prov)
    df <- if(input$reg_prov == "INDONESIA") {
      sovi_data
    } else {
      sovi_data %>% filter(PROVINCE == input$reg_prov)
    }
    st_drop_geometry(df) %>% na.omit()
  })
  
  output$reg_remedy_ui_selector <- renderUI({
    choices_available <- c( "Hapus Salah Satu Variabel Bebas" = "vif", "Transformasi Logaritma Natural (LN)" = "ln", "Estimasi Generalized Least Squares (GLS)" = "gls")
    tagList(hr(), selectizeInput(inputId = "reg_remedy_options", label = strong("Pilih Opsi Perbaikan Model:"), choices = choices_available, multiple = TRUE, options = list(placeholder = 'Tidak ada perbaikan dipilih')))
  })
  observeEvent(c(input$reg_prov, input$reg_y_var), { updateSelectizeInput(session, "reg_remedy_options", selected = "") }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  fixed_regression_analysis <- eventReactive(c(input$reg_y_var, input$reg_prov, input$reg_remedy_options), {
    validate(need(!all(c("ln", "gls") %in% input$reg_remedy_options), "Error: Silakan pilih hanya satu jenis transformasi model (LN atau GLS)."))
    df_full <- fixed_regression_data(); y_var <- req(input$reg_y_var)
    x_vars_for_model <- if (y_var == "POVERTY_PERCENTAGE") { c("LOWEDU_PERCENTAGE", "ILLITERATE_PERCENTAGE", "NOELECTRIC_PERCENTAGE") } else if (y_var == "LOWEDU_PERCENTAGE") { c("POVERTY_PERCENTAGE", "ILLITERATE_PERCENTAGE", "NOELECTRIC_PERCENTAGE") } else { c("POVERTY_PERCENTAGE", "LOWEDU_PERCENTAGE", "NOELECTRIC_PERCENTAGE") }
    req(all(c(y_var, x_vars_for_model) %in% names(df_full)))
    df_model <- df_full %>% select(all_of(c(y_var, x_vars_for_model)))
    
    remedy_log <- list()
    remedy_order <- c("vif", "ln", "gls")
    selected_remedies <- intersect(remedy_order, input$reg_remedy_options)
    
    if ("vif" %in% selected_remedies) {
      y_var_name <- names(df_model)[1]
      x_vars_df <- df_model %>% select(-all_of(y_var_name))
      if (ncol(x_vars_df) > 1) {
        if ("gls" %in% selected_remedies) {
          cor_matrix <- cor(x_vars_df); diag(cor_matrix) <- 0
          max_cor_pair_idx <- which(abs(cor_matrix) == max(abs(cor_matrix)), arr.ind = TRUE)[1, ]
          v1_name <- colnames(cor_matrix)[max_cor_pair_idx[1]]; v2_name <- colnames(cor_matrix)[max_cor_pair_idx[2]]
          cor_v1_y <- cor(df_model[[v1_name]], df_model[[y_var_name]]); cor_v2_y <- cor(df_model[[v2_name]], df_model[[y_var_name]])
          var_to_remove <- if (abs(cor_v1_y) < abs(cor_v2_y)) v1_name else v2_name
          df_model <- df_model %>% select(-all_of(var_to_remove))
          remedy_log <- append(remedy_log, paste0("Menghapus '", var_to_remove, "' (korelasi vs Y terendah dari pasangan korelasi X tertinggi)."))
        } else {
          temp_model <- lm(as.formula(paste0("`", y_var_name, "` ~ .")), data = df_model)
          vif_values <- try(vif(temp_model), silent = TRUE)
          if (!inherits(vif_values, "try-error")) {
            var_to_remove <- names(which.max(vif_values))
            df_model <- df_model %>% select(-all_of(var_to_remove))
            remedy_log <- append(remedy_log, paste0("Menghapus variabel '", var_to_remove, "' karena memiliki VIF tertinggi."))
          } else {
            remedy_log <- append(remedy_log, "Gagal menghitung VIF.")
          }
        }
      }
    }
    
    if ("ln" %in% selected_remedies) {
      validate(need(all(sapply(df_model, function(x) all(x >= 0))), "Gagal: Data negatif tidak dapat di-log.")); df_model <- df_model %>% mutate(across(everything(), ~ log(. + 0.001))); remedy_log <- append(remedy_log, "Menerapkan transformasi Logaritma Natural.")
    }
    
    validate(need(nrow(df_model) >= (ncol(df_model) + 1), "Data tidak cukup untuk pemodelan."));
    final_formula <- as.formula(paste0("`", names(df_model)[1], "` ~ ."))
    
    if ("gls" %in% selected_remedies) {
      ols_model <- lm(final_formula, data = df_model); weights_val <- 1 / (ols_model$residuals^2); final_model <- lm(final_formula, data = df_model, weights = weights_val); remedy_log <- append(remedy_log, "Menerapkan estimasi GLS/WLS.")
    } else {
      final_model <- lm(final_formula, data = df_model)
    }
    
    residuals <- final_model$residuals; validate(need(length(residuals) >= 3, "Residual tidak cukup untuk uji asumsi.")); norm_test <- if (length(residuals) <= 5000) try(shapiro.test(residuals), silent = TRUE) else "Data > 5000."; hetero_test <- try(bptest(final_model), silent = TRUE); autocorr_test <- try(randtests::runs.test(residuals), silent = TRUE); multicol_test <- if(length(coef(final_model)) > 2 && !("gls" %in% selected_remedies)) try(vif(final_model), silent = TRUE) else "not_applicable"
    list(final_model = final_model, summary = summary(final_model), norm_test = norm_test, hetero_test = hetero_test, autocorr_test = autocorr_test, multicol_test = multicol_test, remedy_log = remedy_log, error = NULL)
  })
  
  output$reg_model_raw_output <- renderPrint({
    analysis <- fixed_regression_analysis()
    validate(need(is.null(analysis$error), analysis$error))
    analysis$summary
  })
  
  output$reg_combined_results_ui <- renderUI({
    analysis <- fixed_regression_analysis(); validate(need(is.null(analysis$error), analysis$error)); summ <- analysis$summary; coefs <- coef(analysis$final_model); predictors <- names(coefs)[-1]; summary_coef <- summ$coefficients; adj_r2 <- summ$adj.r.squared; f_stat <- summ$fstatistic; y_var_original <- input$reg_y_var; is_log_transformed <- "ln" %in% input$reg_remedy_options
    eq_str <- paste0("\\hat{Y} = ", round(coefs[1], 4)); if (length(predictors) > 0) { for (i in seq_along(predictors)) { coef_val <- coefs[i + 1]; sign <- ifelse(coef_val >= 0, " + ", " - "); eq_str <- paste0(eq_str, sign, round(abs(coef_val), 4), "X_{", i, "}") } }; equation_html <- withMathJax(p(paste0("$$", eq_str, "$$"), style = "text-align: center; font-size: 1.2em; font-weight: bold;"))
    legend_points <- list(); y_legend_prefix <- if(is_log_transformed) "Log(Estimasi " else "Estimasi "; y_legend_suffix <- if(is_log_transformed) ")" else ""; legend_points <- append(legend_points, paste0("\\(\\hat{Y}\\) = ", y_legend_prefix, variable_defs[[y_var_original]], y_legend_suffix)); if (length(predictors) > 0) { x_legend_prefix <- if(is_log_transformed) "Log(" else ""; x_legend_suffix <- if(is_log_transformed) ")" else ""; for (i in seq_along(predictors)) { legend_points <- append(legend_points, paste0("\\(X_{", i, "}\\) = ", x_legend_prefix, variable_defs[[predictors[i]]], x_legend_suffix)) } }; keterangan_html <- withMathJax(tags$ul(lapply(legend_points, function(item) tags$li(HTML(item)))))
    interpretation_points <- list(); if (is_log_transformed) { y_interp_name <- paste0("<b>", variable_defs[[y_var_original]], "</b>"); intercept_val <- round(exp(summary_coef[1, "Estimate"]), 4); interpretation_points <- append(interpretation_points, paste0("Nilai rata-rata ", y_interp_name, " adalah sebesar <b>", intercept_val, "</b> ketika semua variabel bebas tidak mengalami perubahan.")); if (nrow(summary_coef) > 1) { for (i in 2:nrow(summary_coef)) { var_name <- rownames(summary_coef)[i]; est <- summary_coef[i, "Estimate"]; direction <- ifelse(est > 0, "meningkatkan", "menurunkan"); x_interp_name <- paste0("<b>", variable_defs[[var_name]], "</b>"); interpretation_points <- append(interpretation_points, paste0("Setiap kenaikan satu persen pada angka ", x_interp_name, ", akan <b>", direction, "</b> ", y_interp_name, " sebesar <b>", round(abs(est), 4), " persen</b>, dengan asumsi variabel lain konstan.")) } } } else { y_interp_name <- paste0("<b>", variable_defs[[y_var_original]], "</b>"); interpretation_points <- append(interpretation_points, paste0("Nilai rata-rata ", y_interp_name, " adalah sebesar <b>", round(summary_coef[1, "Estimate"], 4), "</b> ketika semua variabel bebas bernilai nol.")); if (nrow(summary_coef) > 1) { for (i in 2:nrow(summary_coef)) { var_name <- rownames(summary_coef)[i]; est <- summary_coef[i, "Estimate"]; direction <- ifelse(est > 0, "meningkatkan", "menurunkan"); x_interp_name <- paste0("<b>", variable_defs[[var_name]], "</b>"); interpretation_points <- append(interpretation_points, paste0("Setiap kenaikan satu unit pada ", x_interp_name, ", akan <b>", direction, "</b> nilai ", y_interp_name, " sebesar <b>", round(abs(est), 4), "</b>, dengan asumsi variabel lain konstan.")) } } }
    if (nrow(summary_coef) > 1) { all_preds <- rownames(summary_coef)[-1]; p_vals <- summary_coef[-1, "Pr(>|t|)"]; sig_vars <- all_preds[p_vals < 0.05]; if (length(sig_vars) > 0) { nonsig_vars <- all_preds[p_vals >= 0.05]; sig_text <- paste0("Variabel berpengaruh signifikan (p < 0.05) adalah: ", paste0("<b>", sapply(sig_vars, function(v) variable_defs[[v]]), "</b>", collapse = ", "), "."); nonsig_text <- if (length(nonsig_vars) > 0) paste0(" Variabel yang <b>tidak</b> berpengaruh signifikan (p ≥ 0.05) adalah: ", paste0(sapply(nonsig_vars, function(v) variable_defs[[v]]), collapse = ", "), ".") else ""; interpretation_points <- append(interpretation_points, paste(sig_text, nonsig_text)) } else { interpretation_points <- append(interpretation_points, "Tidak ada variabel bebas yang signifikan secara parsial (uji-t).") } }
    if (adj_r2 < 0) { y_interp_name_for_r2 <- paste0("<b>", variable_defs[[y_var_original]], "</b>"); r2_neg_text <- paste0("Nilai Adjusted R-squared negatif (<b>", round(adj_r2*100, 2), "%</b>) menunjukkan bahwa model ini memiliki kecocokan yang sangat buruk. Model ini lebih buruk dalam memprediksi ", y_interp_name_for_r2, " dibandingkan hanya dengan menggunakan nilai rata-ratanya."); interpretation_points <- append(interpretation_points, r2_neg_text) } else { interpretation_points <- append(interpretation_points, paste0("Sebesar <b>", round(adj_r2 * 100, 2), "%</b> keragaman pada variabel terikat dapat dijelaskan oleh model.")) }
    if(!is.null(f_stat)){ f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE); f_test_result <- if (f_p_value < 0.05) "<b>berpengaruh signifikan</b>" else "<b>tidak berpengaruh signifikan</b>"; interpretation_points <- append(interpretation_points, paste0("Berdasarkan Uji-F, semua variabel bebas secara bersama-sama ", f_test_result, " terhadap variabel terikat.")) }
    interpretasi_html <- tags$ul(lapply(interpretation_points, function(item) tags$li(HTML(item)))); violations <- c(); if (is.character(analysis$norm_test) || (is.list(analysis$norm_test) && analysis$norm_test$p.value < 0.05)) violations <- c(violations, "Normalitas"); if (!inherits(analysis$hetero_test, "try-error") && analysis$hetero_test$p.value < 0.05) violations <- c(violations, "Heteroskedastisitas"); if (!inherits(analysis$autocorr_test, "try-error") && analysis$autocorr_test$p.value < 0.05) violations <- c(violations, "Autokorelasi"); if (is.numeric(analysis$multicol_test) && any(analysis$multicol_test > 10, na.rm=TRUE)) violations <- c(violations, "Multikolinearitas"); remedy_notice <- if(length(analysis$remedy_log) > 0) p(strong("Catatan Perbaikan Model:"), tags$ul(lapply(analysis$remedy_log, tags$li))) else NULL; asumsi_html <- tagList(remedy_notice, if (length(violations) > 0) { p(style="color:red; font-weight:bold;", "Peringatan: Asumsi ", paste(violations, collapse=", "), " terlanggar pada model.") } else { p(style="color:green; font-weight:bold;", "Selamat! Tidak ada pelanggaran asumsi klasik yang terdeteksi pada model ini.") }); tagList(equation_html, hr(), keterangan_html, hr(), interpretasi_html, hr(), asumsi_html)
  })
  output$reg_normality_output <- renderUI({ tags$div(class="text-center", h5(strong("1. Uji Normalitas")), withMathJax(p(HTML("$$H_0: \\text{Residual berdistribusi normal}$$ $$H_1: \\text{Residual tidak berdistribusi normal}$$"))), verbatimTextOutput("text_normality")) })
  output$text_normality <- renderPrint({ fixed_regression_analysis()$norm_test })
  output$reg_hetero_output <- renderUI({ tags$div(class="text-center", h5(strong("2. Uji Homoskedastisitas")), withMathJax(p(HTML("$$H_0: \\text{Varians residual homogen (homoskedastisitas)}$$ $$H_1: \\text{Varians residual tidak homogen (heteroskedastisitas)}$$"))), verbatimTextOutput("text_hetero")) })
  output$text_hetero <- renderPrint({ fixed_regression_analysis()$hetero_test })
  output$reg_autocorr_output <- renderUI({ tags$div(class="text-center", h5(strong("3. Uji Non-Autokorelasi")), withMathJax(p(HTML("$$H_0: \\text{Tidak ada autokorelasi antar residual}$$ $$H_1: \\text{Ada autokorelasi}$$"))), verbatimTextOutput("text_autocorr")) })
  output$text_autocorr <- renderPrint({ fixed_regression_analysis()$autocorr_test })
  output$reg_multicol_output <- renderUI({ tags$div(class="text-center", h5(strong("4. Uji Non-Multikolinearitas")), p("Aturan: VIF > 10 menunjukkan adanya multikolinearitas."), verbatimTextOutput("reg_multicol_text")) })
  output$reg_multicol_text <- renderPrint({ fixed_regression_analysis()$multicol_test })
  
  get_var_stem <- function(var_name) {
    sub("_(PERCENTAGE|PERSON|HOUSEHOLD)$", "", var_name)
  }
  
  output$custom_reg_hypotheses_ui <- renderUI({
    withMathJax(p(HTML(
      "<b>Uji Signifikansi Parsial (Uji-t):</b> $$H_0: \\beta_i = 0$$ vs $$H_1: \\beta_i \\neq 0$$<br>
    <b>Uji Signifikansi Simultan (Uji-F):</b> $$H_0: \\beta_1 = ... = \\beta_k = 0$$ vs $$H_1: \\text{Setidaknya ada satu } \\beta_i \\neq 0$$"
    )))
  })
  
  custom_regression_data <- reactive({
    req(input$custom_reg_prov)
    df <- if(input$custom_reg_prov == "INDONESIA") { 
      sovi_data_reactive() 
    } else { 
      sovi_data_reactive() %>% filter(PROVINCE == input$custom_reg_prov) 
    }
    st_drop_geometry(df) %>% na.omit()
  })
  
  output$custom_reg_x_vars_ui <- renderUI({
    y_var <- input$custom_reg_y_var
    all_possible_x <- setdiff(all_test_vars, y_var)
    selected_x <- input$custom_reg_x_vars
    choices <- all_possible_x
    
    if (!is.null(selected_x) && length(selected_x) > 0) {
      selected_stems <-sapply(selected_x, get_var_stem)
      available_x <- all_possible_x[!sapply(all_possible_x, get_var_stem) %in% selected_stems]
      choices <- union(selected_x, available_x)
    }
    
    selectizeInput("custom_reg_x_vars", "Pilih Variabel Bebas (X):", 
                   choices = choices, selected = selected_x, multiple = TRUE,
                   options = list(placeholder = 'Pilih satu atau lebih variabel bebas'))
  })
  
  output$custom_reg_remedy_ui_selector <- renderUI({
    choices_available <- c("Hapus Salah Satu Variabel Bebas" = "vif", "Transformasi Logaritma Natural (LN)" = "ln", "Estimasi Generalized Least Squares (GLS)" = "gls")
    tagList(hr(), selectizeInput(inputId = "custom_reg_remedy_options", label = strong("Pilih Opsi Perbaikan Model:"), choices = choices_available, multiple = TRUE, options = list(placeholder = 'Tidak ada perbaikan dipilih')))
  })
  
  observeEvent(c(input$custom_reg_prov, input$custom_reg_y_var, input$custom_reg_x_vars), {
    updateSelectizeInput(session, "custom_reg_remedy_options", selected = "")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  custom_regression_analysis <- eventReactive(c(input$custom_reg_y_var, input$custom_reg_x_vars, input$custom_reg_prov, input$custom_reg_remedy_options), {
    tryCatch({
      y_var <- req(input$custom_reg_y_var)
      x_vars <- req(input$custom_reg_x_vars)
      prov <- req(input$custom_reg_prov)
      validate(need(length(x_vars) > 0, "Silakan pilih minimal satu variabel bebas (X)."))
      
      df_full <- custom_regression_data() %>% select(all_of(c(y_var, x_vars))) %>% na.omit()
      df_model <- df_full
      x_vars_current <- x_vars
      
      remedy_log <- list()
      selected_remedies <- input$custom_reg_remedy_options
      
      if ("vif" %in% selected_remedies) {
        if(ncol(df_model) > 2) { 
          if ("gls" %in% selected_remedies) {
            x_vars_df <- df_model %>% select(-all_of(y_var))
            cor_matrix <- cor(x_vars_df); diag(cor_matrix) <- 0
            max_cor_pair_idx <- which(abs(cor_matrix) == max(abs(cor_matrix)), arr.ind = TRUE)[1, ]
            v1_name <- colnames(cor_matrix)[max_cor_pair_idx[1]]; v2_name <- colnames(cor_matrix)[max_cor_pair_idx[2]]
            cor_v1_y <- cor(df_model[[v1_name]], df_model[[y_var]]); cor_v2_y <- cor(df_model[[v2_name]], df_model[[y_var]])
            var_to_remove <- if (abs(cor_v1_y) < abs(cor_v2_y)) v1_name else v2_name
            df_model <- df_model %>% select(-all_of(var_to_remove))
            x_vars_current <- setdiff(x_vars_current, var_to_remove)
            remedy_log <- append(remedy_log, paste0("Menghapus '", var_to_remove, "'."))
          } else {
            temp_model <- lm(as.formula(paste0("`", y_var, "` ~ .")), data = df_model)
            vif_values <- try(vif(temp_model), silent = TRUE)
            if (!inherits(vif_values, "try-error")) {
              var_to_remove <- names(which.max(vif_values))
              df_model <- df_model %>% select(-all_of(var_to_remove))
              x_vars_current <- setdiff(x_vars_current, var_to_remove)
              remedy_log <- append(remedy_log, paste0("Menghapus variabel '", var_to_remove, "'."))
            }
          }
        }
      }
      
      if ("ln" %in% selected_remedies) {
        validate(need(all(sapply(df_model, function(x) all(x >= 0))), "Gagal: Data negatif."))
        df_model <- df_model %>% mutate(across(everything(), ~ log(. + 0.001)))
        remedy_log <- append(remedy_log, "Menerapkan transformasi Logaritma Natural.")
      }
      
      validate(need(nrow(df_model) >= (ncol(df_model)), "Data tidak cukup untuk pemodelan."))
      
      final_formula <- as.formula(paste0("`", y_var, "` ~ `", paste(x_vars_current, collapse = "` + `"), "`"))
      
      if ("gls" %in% selected_remedies) {
        ols_model <- lm(final_formula, data = df_model)
        weights_val <- 1 / (ols_model$residuals^2)
        final_model <- lm(final_formula, data = df_model, weights = weights_val)
        remedy_log <- append(remedy_log, "Menerapkan estimasi GLS/WLS.")
      } else {
        final_model <- lm(final_formula, data = df_model)
      }
      
      residuals <- final_model$residuals
      validate(need(length(residuals) >= 3, "Residual tidak cukup."))
      norm_test <- if (length(residuals) <= 5000) try(shapiro.test(residuals), silent = TRUE) else "Data > 5000."
      hetero_test <- try(bptest(final_model), silent = TRUE)
      autocorr_test <- try(randtests::runs.test(residuals), silent = TRUE)
      multicol_test <- if(length(coef(final_model)) > 2 && !("gls" %in% selected_remedies)) try(vif(final_model), silent = TRUE) else "not_applicable"
      
      list(final_model = final_model, summary = summary(final_model), norm_test = norm_test, hetero_test = hetero_test, autocorr_test = autocorr_test, multicol_test = multicol_test, remedy_log = remedy_log, error = NULL)
      
    }, error = function(e) {
      list(error = paste("Terjadi error saat memproses model:", e$message))
    })
  })
  
  output$custom_reg_model_raw_output <- renderPrint({
    res <- custom_regression_analysis()
    if (!is.null(res$error)) { cat(res$error) } else { res$summary }
  })
  
  output$custom_reg_combined_results_ui <- renderUI({
    analysis <- custom_regression_analysis()
    validate(need(!is.null(analysis), "Menunggu input..."))
    if (!is.null(analysis$error)) {
      return(p(style="color: red; font-weight: bold;", analysis$error))
    }
    
    summ <- analysis$summary; coefs <- coef(analysis$final_model); predictors <- names(coefs)[-1]; summary_coef <- summ$coefficients; adj_r2 <- summ$adj.r.squared; f_stat <- summ$fstatistic; y_var_original <- input$custom_reg_y_var; is_log_transformed <- "ln" %in% input$custom_reg_remedy_options
    eq_str <- paste0("\\hat{Y} = ", round(coefs[1], 4)); if (length(predictors) > 0) { for (i in seq_along(predictors)) { coef_val <- coefs[i + 1]; sign <- ifelse(coef_val >= 0, " + ", " - "); eq_str <- paste0(eq_str, sign, round(abs(coef_val), 4), "X_{", i, "}") } }; equation_html <- withMathJax(p(paste0("$$", eq_str, "$$"), style = "text-align: center;")); legend_points <- list(); y_legend_prefix <- if(is_log_transformed) "Log(Estimasi " else "Estimasi "; y_legend_suffix <- if(is_log_transformed) ")" else ""; legend_points <- append(legend_points, paste0("\\(\\hat{Y}\\) = ", y_legend_prefix, get_friendly_name(y_var_original), y_legend_suffix)); if (length(predictors) > 0) { x_legend_prefix <- if(is_log_transformed) "Log(" else ""; x_legend_suffix <- if(is_log_transformed) ")" else ""; for (i in seq_along(predictors)) { legend_points <- append(legend_points, paste0("\\(X_{", i, "}\\) = ", x_legend_prefix, get_friendly_name(predictors[i]), x_legend_suffix)) } }; keterangan_html <- withMathJax(tags$ul(lapply(legend_points, function(item) tags$li(HTML(item))))); interpretation_points <- list(); if (is_log_transformed) { y_interp_name <- paste0("<b>", get_friendly_name(y_var_original), "</b>"); intercept_val <- round(exp(summary_coef[1, "Estimate"]), 4); interpretation_points <- append(interpretation_points, paste0("Nilai rata-rata ", y_interp_name, " adalah sebesar <b>", intercept_val, "</b> ketika semua variabel bebas tidak mengalami perubahan.")); if (nrow(summary_coef) > 1) { for (i in 2:nrow(summary_coef)) { var_name <- rownames(summary_coef)[i]; est <- summary_coef[i, "Estimate"]; direction <- ifelse(est > 0, "meningkatkan", "menurunkan"); x_interp_name <- paste0("<b>", get_friendly_name(var_name), "</b>"); interpretation_points <- append(interpretation_points, paste0("Setiap kenaikan satu persen pada angka ", x_interp_name, ", akan <b>", direction, "</b> ", y_interp_name, " sebesar <b>", round(abs(est), 4), " persen</b>, dengan asumsi variabel lain konstan.")) } } } else { y_interp_name <- paste0("<b>", get_friendly_name(y_var_original), "</b>"); interpretation_points <- append(interpretation_points, paste0("Nilai rata-rata ", y_interp_name, " adalah sebesar <b>", round(summary_coef[1, "Estimate"], 4), "</b> ketika semua variabel bebas bernilai nol.")); if (nrow(summary_coef) > 1) { for (i in 2:nrow(summary_coef)) { var_name <- rownames(summary_coef)[i]; est <- summary_coef[i, "Estimate"]; direction <- ifelse(est > 0, "meningkatkan", "menurunkan"); x_interp_name <- paste0("<b>", get_friendly_name(var_name), "</b>"); interpretation_points <- append(interpretation_points, paste0("Setiap kenaikan satu unit pada ", x_interp_name, ", akan <b>", direction, "</b> nilai ", y_interp_name, " sebesar <b>", round(abs(est), 4), "</b>, dengan asumsi variabel lain konstan.")) } } }; if (nrow(summary_coef) > 1) { all_preds <- rownames(summary_coef)[-1]; p_vals <- summary_coef[-1, "Pr(>|t|)"]; sig_vars <- all_preds[p_vals < 0.05]; if (length(sig_vars) > 0) { nonsig_vars <- all_preds[p_vals >= 0.05]; sig_text <- paste0("Variabel berpengaruh signifikan (p < 0.05) adalah: ", paste0("<b>", sapply(sig_vars, get_friendly_name), "</b>", collapse = ", "), "."); nonsig_text <- if (length(nonsig_vars) > 0) paste0(" Variabel yang <b>tidak</b> berpengaruh signifikan (p ≥ 0.05) adalah: ", paste0(sapply(nonsig_vars, get_friendly_name), collapse = ", "), ".") else ""; interpretation_points <- append(interpretation_points, paste(sig_text, nonsig_text)) } else { interpretation_points <- append(interpretation_points, "Tidak ada variabel bebas yang signifikan secara parsial (uji-t).") } }; if (adj_r2 < 0) { y_interp_name_for_r2 <- paste0("<b>", get_friendly_name(y_var_original), "</b>"); r2_neg_text <- paste0("Nilai Adjusted R-squared negatif (<b>", round(adj_r2*100, 2), "%</b>) menunjukkan bahwa model ini memiliki kecocokan yang sangat buruk. Model ini lebih buruk dalam memprediksi ", y_interp_name_for_r2, " dibandingkan hanya dengan menggunakan nilai rata-ratanya."); interpretation_points <- append(interpretation_points, r2_neg_text) } else { interpretation_points <- append(interpretation_points, paste0("Sebesar <b>", round(adj_r2 * 100, 2), "%</b> keragaman pada variabel terikat dapat dijelaskan oleh model.")) }; if(!is.null(f_stat)){ f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE); f_test_result <- if (f_p_value < 0.05) "<b>berpengaruh signifikan</b>" else "<b>tidak berpengaruh signifikan</b>"; interpretation_points <- append(interpretation_points, paste0("Berdasarkan Uji-F, semua variabel bebas secara bersama-sama ", f_test_result, " terhadap variabel terikat.")) }; interpretasi_html <- tags$ul(lapply(interpretation_points, function(item) tags$li(HTML(item)))); violations <- c(); if (is.character(analysis$norm_test) || (is.list(analysis$norm_test) && analysis$norm_test$p.value < 0.05)) violations <- c(violations, "Normalitas"); if (!inherits(analysis$hetero_test, "try-error") && analysis$hetero_test$p.value < 0.05) violations <- c(violations, "Heteroskedastisitas"); if (!inherits(analysis$autocorr_test, "try-error") && analysis$autocorr_test$p.value < 0.05) violations <- c(violations, "Autokorelasi"); if (is.numeric(analysis$multicol_test) && any(analysis$multicol_test > 10, na.rm=TRUE)) violations <- c(violations, "Multikolinearitas"); remedy_notice <- if(length(analysis$remedy_log) > 0) p(strong("Catatan Perbaikan Model:"), tags$ul(lapply(analysis$remedy_log, tags$li))) else NULL; asumsi_html <- tagList(remedy_notice, if (length(violations) > 0) { p(style="color:red; font-weight:bold;", "Peringatan: Asumsi ", paste(violations, collapse=", "), " terlanggar pada model.") } else { p(style="color:green; font-weight:bold;", "Selamat! Tidak ada pelanggaran asumsi klasik yang terdeteksi pada model ini.") }); tagList(equation_html, hr(), keterangan_html, hr(), interpretasi_html, hr(), asumsi_html)
  })
  
  output$custom_reg_normality_output <- renderUI({ tags$div(class="text-center", h5(strong("1. Uji Normalitas")), withMathJax(p(HTML("$$H_0: \\text{Residual berdistribusi normal}$$ $$H_1: \\text{Residual tidak berdistribusi normal}$$"))), verbatimTextOutput("custom_text_normality")) })
  output$custom_text_normality <- renderPrint({ res <- custom_regression_analysis(); if(is.null(res$error)) res$norm_test })
  
  output$custom_reg_hetero_output <- renderUI({ tags$div(class="text-center", h5(strong("2. Uji Homoskedastisitas")), withMathJax(p(HTML("$$H_0: \\text{Varians residual homogen}$$ $$H_1: \\text{Varians residual tidak homogen}$$"))), verbatimTextOutput("custom_text_hetero")) })
  output$custom_text_hetero <- renderPrint({ res <- custom_regression_analysis(); if(is.null(res$error)) res$hetero_test })
  
  output$custom_reg_autocorr_output <- renderUI({ tags$div(class="text-center", h5(strong("3. Uji Non-Autokorelasi")), withMathJax(p(HTML("$$H_0: \\text{Tidak ada autokorelasi}$$ $$H_1: \\text{Ada autokorelasi}$$"))), verbatimTextOutput("custom_text_autocorr")) })
  output$custom_text_autocorr <- renderPrint({ res <- custom_regression_analysis(); if(is.null(res$error)) res$autocorr_test })
  
  output$custom_reg_multicol_output <- renderUI({ tags$div(class="text-center", h5(strong("4. Uji Non-Multikolinearitas")), p("Aturan: VIF > 10 menunjukkan adanya multikolinearitas."), verbatimTextOutput("custom_reg_multicol_text")) })
  output$custom_reg_multicol_text <- renderPrint({ res <- custom_regression_analysis(); if(is.null(res$error)) res$multicol_test })
  
  alpha <- 0.05
  
  output$anova1_factor_var_ui <- renderUI({
    data <- sovi_data_reactive(); kategori_cols <- names(data)[grepl("_CAT\\d+$", names(data))]; choices <- c("PROVINCE", kategori_cols)
    selectInput("anova1_factor_var", "Pilih Faktor Pengelompokan:", choices = choices)
  })
  output$anova1_levels_ui <- renderUI({
    req(input$anova1_factor_var); factor_var <- input$anova1_factor_var; data <- sovi_data_reactive()
    if (factor_var == "PROVINCE") { choices <- provinces_list_only; selected <- c("ACEH", "SUMATERA UTARA", "SUMATERA BARAT"); label <- "Pilih Provinsi:" }
    else { choices <- levels(data[[factor_var]]); selected <- if(length(choices)>=3) choices else NULL; label <- paste("Pilih Level untuk", get_friendly_name(factor_var), ":") }
    selectizeInput("anova1_levels", label, choices = choices, selected = selected, multiple = TRUE, options = list(placeholder = 'Pilih minimal 3 level...'))
  })
  
  anova1_result <- reactive({
    req(input$anova1_factor_var, input$anova1_var, input$anova1_levels)
    validate(need(length(input$anova1_levels) >= 3, "Analisis ANOVA Satu Arah memerlukan minimal 3 kelompok/level untuk perbandingan."))
    factor_var <- input$anova1_factor_var; selected_levels <- input$anova1_levels
    data_filtered <- sovi_data_reactive() %>% st_drop_geometry() %>% filter(.data[[factor_var]] %in% selected_levels) %>% mutate(across(all_of(factor_var), ~factor(as.character(.))))
    validate(need(nrow(data_filtered) > length(selected_levels), "Data tidak cukup untuk level yang dipilih."))
    formula <- as.formula(paste0("`",input$anova1_var, "` ~ `", factor_var, "`")); aov_model <- aov(formula, data = data_filtered)
    levene_test <- leveneTest(formula, data = data_filtered)
    shapiro_test <- if (length(aov_model$residuals) < 3 || length(aov_model$residuals) > 5000) { "Jumlah residual di luar rentang uji Shapiro-Wilk (3-5000)." } else { shapiro.test(aov_model$residuals) }
    posthoc_result <- agricolae::HSD.test(aov_model, factor_var, group = TRUE, console = FALSE)
    list(aov_summary = summary(aov_model), posthoc = posthoc_result, data = data_filtered, levene = levene_test, shapiro = shapiro_test, factor_var = factor_var)
  })
  
  output$anova1_results_ui <- renderUI({
    guard <- anova1_result(); req(guard)
    fluidRow(
      bs4Card(title = "Pemeriksaan Asumsi", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, fluidRow(column(6, uiOutput("anova1_levene_ui")), column(6, uiOutput("anova1_shapiro_ui")))),
      bs4Card(title = "Tabel ANOVA", status = "primary", solidHeader = TRUE, width = 6, uiOutput("anova1_summary_hypo"), verbatimTextOutput("anova1_summary_output")),
      bs4Card(title = "Hasil Uji Lanjut Tukey HSD", status = "primary", solidHeader = TRUE, width = 6, p("Tabel pengelompokan berdasarkan rata-rata. Kelompok dengan huruf yang sama tidak berbeda signifikan."), verbatimTextOutput("anova1_posthoc_output")),
      bs4Card(title = "Visualisasi Perbandingan", status = "primary", solidHeader = TRUE, width = 12, plotOutput("anova1_plot_output", height = "600px")),
      bs4Card(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12, uiOutput("anova1_interpretation_output"))
    )
  })
  output$anova1_levene_ui <- renderUI({ res <- anova1_result()$levene; tagList(h5(strong("Uji Homogenitas Varians")), p(withMathJax(HTML("$$H_0: \\sigma_1^2 = \\sigma_2^2 = ... = \\sigma_k^2$$ $$H_1: \\text{Setidaknya ada satu } \\sigma_i^2 \\text{ yang berbeda}$$"))), verbatimTextOutput("anova1_levene_output")) })
  output$anova1_shapiro_ui <- renderUI({ res <- anova1_result()$shapiro; if (is.character(res)) return(p(res)); tagList(h5(strong("Uji Normalitas Residual")), p(withMathJax(HTML("$$H_0: \\text{Residual berdistribusi normal}$$ $$H_1: \\text{Residual tidak berdistribusi normal}$$"))), verbatimTextOutput("anova1_shapiro_output")) })
  output$anova1_summary_hypo <- renderUI({ withMathJax(p(HTML("$$H_0: \\mu_1 = \\mu_2 = ... = \\mu_k$$ $$H_1: \\text{Setidaknya ada satu } \\mu_i \\text{ yang berbeda}$$"))) })
  output$anova1_levene_output <- renderPrint({ anova1_result()$levene }); output$anova1_shapiro_output <- renderPrint({ anova1_result()$shapiro }); output$anova1_summary_output <- renderPrint({ anova1_result()$aov_summary }); output$anova1_posthoc_output <- renderPrint({ anova1_result()$posthoc$groups })
  output$anova1_plot_output <- renderPlot({ res <- anova1_result(); factor_var <- res$factor_var; groups_df <- data.frame(Factor = rownames(res$posthoc$groups), groups = res$posthoc$groups$groups); names(groups_df)[1] <- factor_var; plot_data <- res$data %>% group_by(.data[[factor_var]]) %>% summarise(mean_val = mean(!!sym(input$anova1_var), na.rm = TRUE), .groups = 'drop') %>% left_join(groups_df, by = factor_var); ggplot(res$data, aes_string(x = factor_var, y = input$anova1_var, fill = factor_var)) + geom_boxplot(alpha = 0.7, show.legend = FALSE) + geom_text(data = plot_data, aes(x = .data[[factor_var]], y = max(res$data[[input$anova1_var]], na.rm = TRUE), label = groups), vjust = -0.7, size=5) + labs(title = "Perbandingan Rata-rata Antar Kelompok (Uji Lanjut Tukey)", subtitle = "Kelompok dengan huruf yang sama tidak berbeda secara signifikan", x = get_friendly_name(factor_var), y = get_friendly_name(input$anova1_var)) + theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) })
  
  output$anova1_interpretation_output <- renderUI({
    res <- anova1_result()
    p_value_anova <- res$aov_summary[[1]]$`Pr(>F)`[1]
    groups_df <- res$posthoc$groups
    var_dependen_nama <- strong(get_friendly_name(input$anova1_var))
    var_faktor_nama <- strong(get_friendly_name(res$factor_var))
    
    kesimpulan_anova <- if (p_value_anova < alpha) {
      paste0("Berdasarkan analisis, <b>terdapat perbedaan rata-rata yang signifikan</b> pada ", var_dependen_nama, " di antara setidaknya dua kelompok ", var_faktor_nama, " yang diuji.")
    } else {
      paste0("Berdasarkan analisis, <b>tidak terdapat perbedaan rata-rata yang signifikan</b> pada ", var_dependen_nama, " di antara kelompok ", var_faktor_nama, " yang diuji.")
    }
    
    kesimpulan_tukey_html <- if (p_value_anova < alpha) {
      
      all_levels <- rownames(groups_df)
      interpretation_list <- lapply(all_levels, function(level_fokus) {
        
        grup_fokus <- groups_df[level_fokus, "groups"]
        
        serupa_dengan <- c()
        berbeda_dengan <- c()
        
        for (level_pembanding in all_levels) {
          if (level_fokus == level_pembanding) next
          
          grup_pembanding <- groups_df[level_pembanding, "groups"]
          
          shared_letter <- any(strsplit(grup_fokus, "")[[1]] %in% strsplit(grup_pembanding, "")[[1]])
          
          if (shared_letter) {
            serupa_dengan <- c(serupa_dengan, level_pembanding)
          } else {
            berbeda_dengan <- c(berbeda_dengan, level_pembanding)
          }
        }
        
        kalimat <- paste0("Rata-rata ", var_dependen_nama, " untuk kelompok <b>", level_fokus, "</b> ")
        if (length(serupa_dengan) > 0) {
          kalimat <- paste0(kalimat, "ditemukan <b>serupa secara statistik</b> dengan kelompok <b>", paste(serupa_dengan, collapse = "</b> dan <b>"), "</b>")
          if (length(berbeda_dengan) > 0) {
            kalimat <- paste0(kalimat, ", namun <b>berbeda signifikan</b> dengan kelompok <b>", paste(berbeda_dengan, collapse = "</b> dan <b>"), "</b>.")
          } else {
            kalimat <- paste0(kalimat, ".")
          }
        } else {
          kalimat <- paste0(kalimat, "ditemukan <b>berbeda secara signifikan</b> dengan semua kelompok lainnya.")
        }
        return(tags$li(HTML(kalimat)))
      })
      
      tagList(
        p("Detail perbandingan antar kelompok (Uji Lanjut Tukey) adalah sebagai berikut:"),
        tags$ul(interpretation_list)
      )
    }
    
    warnings <- c(); if (res$levene$`Pr(>F)`[1] < alpha) { warnings <- c(warnings, "Asumsi homogenitas varians terlanggar.") }; if (!is.character(res$shapiro) && res$shapiro$p.value < alpha) { warnings <- c(warnings, "Asumsi normalitas residual terlanggar.") }
    
    tagList(
      tags$ol(
        tags$li(HTML(kesimpulan_anova)),
        if(p_value_anova < alpha) { tags$li(kesimpulan_tukey_html) }
      ),
      if(length(warnings) > 0) {
        div(style="color: red; margin-top: 15px; font-weight: bold;",
            "Peringatan Asumsi:",
            tags$ul(style="font-weight: normal;", lapply(warnings, tags$li))
        )
      }
    )
  })
  
  output$anova2_input_ui <- renderUI({
    data <- sovi_data_reactive()
    all_factors <- c("PROVINCE", names(data)[grepl("_CAT\\d+$", names(data))])
    
    if (length(all_factors) < 2) {
      return(
        div(class = "alert alert-warning", role = "alert",
            h4(style="font-weight:bold;", "Faktor Tidak Cukup"),
            p("Analisis ANOVA Dua Arah memerlukan minimal dua variabel faktor (kategorikal)."),
            p("Saat ini, hanya ada satu faktor yang tersedia. Silakan buat variabel kategori baru di tab ",
              strong("Beranda -> Transformasi Data"), " untuk melanjutkan.")
        )
      )
    }
    
    factor2_choices <- setdiff(all_factors, input$anova2_factor1_var)
    
    tagList(
      fluidRow(
        column(3, selectInput("anova2_factor1_var", "Pilih Faktor 1:",
                              choices = all_factors, selected = "PROVINCE")),
        column(3, uiOutput("anova2_levels1_ui")),
        
        column(3, selectInput("anova2_factor2_var", "Pilih Faktor 2:",
                              choices = factor2_choices)),
        column(3, uiOutput("anova2_levels2_ui"))
      ),
      fluidRow(
        column(12, selectInput("anova2_var_numerik", "Pilih Variabel Dependen (Numerik):",
                               choices = all_test_vars, selected = "POVERTY_PERCENTAGE"))
      )
    )
  })
  
  output$anova2_levels1_ui <- renderUI({ req(input$anova2_factor1_var); factor_var <- input$anova2_factor1_var; data <- sovi_data_reactive(); if (factor_var == "PROVINCE") { choices <- provinces_list_only; selected <- c("ACEH", "SUMATERA UTARA") } else { choices <- levels(data[[factor_var]]); selected <- choices }; selectizeInput("anova2_levels1", "Pilih Level Faktor 1 (Min 2):", choices = choices, selected = selected, multiple = TRUE) })
  output$anova2_levels2_ui <- renderUI({ req(input$anova2_factor2_var); factor_var <- input$anova2_factor2_var; data <- sovi_data_reactive(); if (factor_var == "PROVINCE") { choices <- provinces_list_only; selected <- c("ACEH", "SUMATERA UTARA") } else { choices <- levels(data[[factor_var]]); selected <- choices }; selectizeInput("anova2_levels2", "Pilih Level Faktor 2 (Min 2):", choices = choices, selected = selected, multiple = TRUE) })
  
  anova2_result <- reactive({
    req(input$anova2_factor1_var, input$anova2_factor2_var, input$anova2_var_numerik, input$anova2_levels1, input$anova2_levels2)
    validate(need(length(input$anova2_levels1) >= 2 && length(input$anova2_levels2) >= 2, "Setiap faktor harus memiliki minimal 2 level/kelompok terpilih."))
    
    f1 <- input$anova2_factor1_var; f2 <- input$anova2_factor2_var; l1 <- input$anova2_levels1; l2 <- input$anova2_levels2
    
    data_filtered <- sovi_data_reactive() %>%
      st_drop_geometry() %>%
      filter(.data[[f1]] %in% l1, .data[[f2]] %in% l2) %>%
      mutate(across(all_of(c(f1, f2)), ~factor(as.character(.))))
    
    aov_results <- tryCatch({
      validate(need(nrow(data_filtered) > 0, "Tidak ada data untuk kombinasi filter yang dipilih."))
      
      tbl <- table(data_filtered[[f1]], data_filtered[[f2]])
      if (any(tbl == 0)) {
        stop("Analisis gagal karena tidak semua kombinasi kelompok memiliki data. Solusi: Pastikan setiap kombinasi kelompok (misal, 'ACEH' - 'Kategori Rendah') memiliki setidaknya satu observasi. Coba pilih level/kelompok yang berbeda.")
      }
      
      formula <- as.formula(paste0("`", input$anova2_var_numerik, "` ~ `", f1, "` * `", f2, "`"))
      aov_model <- aov(formula, data = data_filtered)
      list(aov_summary = summary(aov_model), posthoc = TukeyHSD(aov_model), error = NULL)
      
    }, error = function(e) {
      list(error = e$message)
    })
    
    return(aov_results)
  })
  
  output$anova2_results_ui <- renderUI({
    res <- anova2_result()
    req(res)
    
    if (!is.null(res$error)) {
      return(
        bs4Card(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12,
                div(style="color: red; font-weight: bold;", p(res$error))
        )
      )
    }
    
    fluidRow(
      bs4Card(title = "Tabel ANOVA", status = "primary", solidHeader = TRUE, width = 12,
              uiOutput("anova2_summary_hypo"), verbatimTextOutput("anova2_summary_output")),
      bs4Card(title = "Hasil Uji Lanjut Tukey HSD", status = "primary", solidHeader = TRUE, width = 12,
              collapsible = TRUE, collapsed = TRUE, verbatimTextOutput("anova2_posthoc_output")),
      bs4Card(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12,
              uiOutput("anova2_interpretation_output"))
    )
  })
  
  output$anova2_summary_hypo <- renderUI({ f1 <- get_friendly_name(input$anova2_factor1_var); f2 <- get_friendly_name(input$anova2_factor2_var); withMathJax(p(HTML(paste0("<b>Hipotesis Utama:</b><ul><li><b>Faktor ", f1, ":</b> $$H_0: \\alpha_i = 0$$ $$H_1: \\text{Setidaknya ada satu } \\alpha_i \\neq 0$$</li><li><b>Faktor ", f2, ":</b> $$H_0: \\beta_j = 0$$ $$H_1: \\text{Setidaknya ada satu } \\beta_j \\neq 0$$</li><li><b>Faktor Interaksi:</b> $$H_0: (\\alpha\\beta)_{ij} = 0$$ $$H_1: \\text{Setidaknya ada satu } (\\alpha\\beta)_{ij} \\neq 0$$</li></ul>")))) })
  output$anova2_summary_output <- renderPrint({ anova2_result()$aov_summary }); output$anova2_posthoc_output <- renderPrint({ anova2_result()$posthoc })
  
  output$anova2_interpretation_output <- renderUI({
    res <- anova2_result()
    p_values <- res$aov_summary[[1]]$`Pr(>F)`
    p_values <- p_values[!is.na(p_values)]
    
    f1_name_raw <- input$anova2_factor1_var
    f2_name_raw <- input$anova2_factor2_var
    
    f1 <- get_friendly_name(f1_name_raw)
    f2 <- get_friendly_name(f2_name_raw)
    var_num <- get_friendly_name(input$anova2_var_numerik)
    
    p_f1 <- p_values[1]; p_f2 <- p_values[2]; p_int <- p_values[3]
    
    kesimpulan_f1 <- if (p_f1 < alpha) "<b>berpengaruh signifikan</b>" else "<b>tidak berpengaruh signifikan</b>"
    kesimpulan_f2 <- if (p_f2 < alpha) "<b>berpengaruh signifikan</b>" else "<b>tidak berpengaruh signifikan</b>"
    kesimpulan_int <- if (p_int < alpha) {
      paste0("menemukan adanya <b>efek interaksi yang signifikan</b>. Ini adalah temuan paling penting, yang berarti pengaruh faktor ", strong(f1), " terhadap ", strong(var_num), " <b>berbeda-beda tergantung pada level/kelompok</b> dari faktor ", strong(f2), ", dan sebaliknya. Karena interaksi ini signifikan, interpretasi pengaruh utama setiap faktor secara terpisah menjadi kurang relevan.")
    } else {
      paste0("<b>tidak menemukan adanya efek interaksi yang signifikan</b>. Ini berarti pengaruh faktor ", strong(f1), " terhadap ", strong(var_num), " cenderung <b>konsisten</b> di semua level/kelompok dari faktor ", strong(f2), ".")
    }
    
    tukey_analysis_html <- tryCatch({
      tukey_results <- res$posthoc
      significant_findings <- list()
      
      for (factor_name_raw in names(tukey_results)) {
        tukey_table <- as.data.frame(tukey_results[[factor_name_raw]])
        significant_pairs <- tukey_table[tukey_table$`p adj` < alpha, , drop = FALSE]
        
        if (nrow(significant_pairs) > 0) {
          factor_title <- get_friendly_name(gsub("`", "", factor_name_raw))
          
          pair_sentences <- lapply(rownames(significant_pairs), function(pair) {
            pair_names <- strsplit(pair, "-")[[1]]
            paste0("<li>Rata-rata ditemukan berbeda signifikan antara kelompok <b>", pair_names[1], "</b> dan <b>", pair_names[2], "</b>.</li>")
          })
          
          significant_findings[[factor_title]] <- paste0("<ul>", paste(pair_sentences, collapse=""), "</ul>")
        }
      }
      
      if (length(significant_findings) == 0) {
        "Uji Lanjutan Tukey HSD tidak menemukan adanya perbedaan yang signifikan secara statistik di antara pasangan kelompok manapun."
      } else {
        narrative <- "Secara spesifik, uji Tukey HSD menemukan perbedaan signifikan pada perbandingan berikut:"
        for(title in names(significant_findings)){
          narrative <- paste0(narrative, "<br><b>Untuk faktor ", title, ":</b>", significant_findings[[title]])
        }
        narrative
      }
    }, error = function(e) {
      "Tidak dapat menghasilkan interpretasi Tukey HSD secara otomatis."
    })
    
    tagList(
      tags$ol(
        tags$li(HTML(paste0("Mengenai pengaruh utama faktor <b>", f1, "</b>, hasil analisis menunjukkan bahwa faktor ini secara umum ", kesimpulan_f1, " terhadap ", strong(var_num), "."))),
        tags$li(HTML(paste0("Mengenai pengaruh utama faktor <b>", f2, "</b>, hasil analisis menunjukkan bahwa faktor ini secara umum ", kesimpulan_f2, " terhadap ", strong(var_num), "."))),
        tags$li(HTML(paste0("Mengenai <b>interaksi</b> antara kedua faktor, analisis ", kesimpulan_int))),
        tags$li(HTML(paste0("<b>Uji Lanjutan (Tukey HSD)</b>: ", tukey_analysis_html)))
      )
    )
  })
  
  output$metadata_asli_table <- renderDT({
    vars_asli <- names(sovi_data_clean)[!grepl("_PERSON$|_HOUSEHOLD$|PROVINCE|DISTRICT", names(sovi_data_clean))]
    meta_df <- data.frame(
      `Nama Variabel` = vars_asli,
      `Tipe Data` = sapply(vars_asli, function(v) if(v == "DISTRICTCODE") "Numeric" else "Float"),
      `Label` = sapply(vars_asli, function(v) to_title_case(get_friendly_name(v, variable_defs))),
      `Unit Pengukuran` = sapply(vars_asli, function(v) case_when(v == "POPULATION" ~ "Jiwa/Orang", v == "HOUSEHOLD" ~ "Rumah Tangga", v == "DISTRICTCODE" ~ "Kode BPS", TRUE ~ "Persentase (%)")),
      `Referensi Persentase` = sapply(vars_asli, function(v) {
        if (!grepl("_PERCENTAGE", v)) return("")
        label <- get_friendly_name(v, variable_defs)
        if (grepl("rumah tangga", label)) "Jumlah Rumah Tangga (Household) Kabupaten/Kota" else "Jumlah Populasi (Population) Kabupaten/Kota"
      }),
      `Measure` = sapply(vars_asli, function(v) if(v == "DISTRICTCODE") "Nominal" else "Scale (Ratio)"),
      check.names = FALSE
    )
    datatable(meta_df, rownames = FALSE, extensions = 'Buttons',
              options = list(
                pageLength = 50,
                scrollX=TRUE,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
                  list(extend = 'excel', exportOptions = list(modifier = list(page = "all"))),
                  'print'
                )
              ))
  })
  
  output$metadata_olahan_table <- renderDT({
    vars_olahan <- c("PROVINCE", "DISTRICT", names(sovi_data_clean)[grepl("_PERSON$|_HOUSEHOLD$", names(sovi_data_clean))])
    meta_df <- data.frame(
      `Nama Variabel` = vars_olahan,
      `Tipe Data` = sapply(vars_olahan, function(v) if(v %in% c("PROVINCE", "DISTRICT")) "String" else "Float"),
      `Label` = sapply(vars_olahan, function(v) to_title_case(get_friendly_name(v, variable_defs))),
      `Unit Pengukuran` = sapply(vars_olahan, function(v) case_when(grepl("_PERSON", v) ~ "Jiwa/Orang", grepl("_HOUSEHOLD", v) ~ "Rumah Tangga", TRUE ~ "")),
      `Measure` = sapply(vars_olahan, function(v) case_when(v == "PROVINCE" ~ "Nominal", v == "DISTRICT" ~ "Nominal", TRUE ~ "Scale (Ratio)")),
      check.names = FALSE
    )
    datatable(meta_df, rownames = FALSE, extensions = 'Buttons',
              options = list(
                pageLength = 50,
                scrollX=TRUE,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
                  list(extend = 'excel', exportOptions = list(modifier = list(page = "all"))),
                  'print'
                )
              ))
  })
  
  output$full_data_table <- renderDT({
    datatable(sovi_data_clean, options = list(pageLength = 50, scrollX = TRUE, scrollY = "600px"), filter = 'top', rownames = FALSE)
  })
  
  output$download_asli_data <- downloadHandler(
    filename = function() {
      paste0("sovi_data_asli_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rawdata, file, row.names = FALSE)
    }
  )
  
  output$download_clean_data <- downloadHandler(
    filename = function() {
      paste0("sovi_data_clean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sovi_data_clean, file, row.names = FALSE)
    }
  )
  
}
shinyApp(ui, server)