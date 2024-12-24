# Set seed untuk reproduktifitas
set.seed(789)

# Membuat data
n <- 200
tinggi <- rnorm(n, mean = 170, sd = 10)  # Variabel tinggi badan
berat <- 0.5 * tinggi + rnorm(n, mean = 0, sd = 5) + 50  # Variabel berat badan dengan korelasi positif

# Menggabungkan data ke dalam data frame
data <- data.frame(tinggi, berat)

# Menambahkan beberapa outlier untuk variasi
data <- rbind(data, data.frame(tinggi = c(150, 160, 180, 190, 200), berat = c(40, 50, 90, 100, 110)))
# Uji Normalitas
shapiro_test_tinggi <- shapiro.test(data$tinggi)
shapiro_test_berat <- shapiro.test(data$berat)

# Uji Linearitas (scatter plot)
plot(data$tinggi, data$berat, main = "Scatter Plot Tinggi vs Berat", xlab = "Tinggi Badan (cm)", ylab = "Berat Badan (kg)", col = "blue", pch = 19)

# Uji Homoskedastisitas (plot residual)
model <- lm(berat ~ tinggi, data = data)
plot(model$residuals ~ model$fitted.values, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
# Menghitung koefisien korelasi Pearson
correlation <- cor(data$tinggi, data$berat)
correlation
# Memasang paket ggplot2
library(ggplot2)

# Visualisasi dengan ggplot2
ggplot(data, aes(x = tinggi, y = berat)) +
  geom_point(aes(color = ifelse(tinggi > 180, "Outlier", "Normal")), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Korelasi antara Tinggi Badan dan Berat Badan",
       x = "Tinggi Badan (cm)",
       y = "Berat Badan (kg)",
       color = "Keterangan") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "orange"))

