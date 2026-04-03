CO2
View(CO2)


library(readxl)

# 1. Excel dosyasını oku
co2_data  <- read_excel("CO2.xlsx")
View(co2_data)

# 2. Türkiye’ye ait verileri seç
turkiye_co2 <- co2_data [co2_data $`Country Name` == "Turkiye", ]
View(turkiye_co2)

# 3. 1970–2023 yıllarını seç
years <- as.character(1970:2023)
turkiye_years <- turkiye_co2[, years]
View(turkiye_years)

# 4. Tek sütun hâline getir
df_turkiye_co2 <- data.frame(
  CO2 = as.numeric(t(turkiye_years))
)
View(df_turkiye_co2)

# ----   ----   ----   ----   ----

# 1. Excel dosyasını oku
sanayi_data  <- read_excel("Sanayi.xlsx")
View(sanayi_data)

# 2. Türkiye’ye ait verileri seç
turkiye_sanayi <- sanayi_data[sanayi_data $`Country Name` == "Turkiye", ]
View(turkiye_sanayi)

# 3. 1970–2023 yıllarını seç
years <- as.character(1970:2023)
turkiye_sanayi_years <- turkiye_sanayi[, years]
View(turkiye_sanayi_years)

# 3. 1970–2023 yıllarını seç
years <- as.character(1970:2023)
turkiye_years <- turkiye_co2[, years]
View(turkiye_years)

# 4. Tek sütun hâline getir
df_turkiye_sanayi <- data.frame(
  SANAYI = as.numeric(t(turkiye_sanayi_years))
)
View(df_turkiye_sanayi)


# ----   ----   ----   ----   ----

ortak_data <- cbind(df_turkiye_sanayi, df_turkiye_co2)
View(ortak_data)

# 5. Basit lineer regresyon
model <- lm(CO2 ~ SANAYI, data = ortak_data)

# 6. Model özetini göster
summary(model)

# Katsayılar
coef(model)

# Tahmin
predict(model, data.frame(SANAYI = 6))


# 5. Grafik
plot(ortak_data$SANAYI, ortak_data$CO2,
     main = "Basit Doğrusal Regresyon",
     xlab = "Sanayi Verileri", ylab = "CO2 Verileri",
     pch = 15, col = "blue")

# Regresyon Doğrusu
abline(model, col = "green", lwd = 4)
