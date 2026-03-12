library(readxl)
library(dplyr)
library(ggplot2)


df <- read_excel("IPCA2025.xlsx")

head(df)

df <- df %>%
  filter(Subitem != "ÍNDICE GERAL") %>%
  filter(Capital != "NACIONAL")

media <- df %>%
  group_by(Data) %>%
  summarise(Media = mean(Valor, na.rm = TRUE))

ggplot(media, aes(x = Data, y = Media)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Inflação média mensal dos subitens do IPCA - 2025",
    x = "Data",
    y = "Inflação média"
  ) +
  theme_minimal()

serie <- ts(media$Media, frequency = 12)


library(forecast)

modelo <- Arima(serie, order = c(1,0,0))
previsao <- forecast(modelo, h = 6)


autoplot(previsao) +
  labs(
    title = "Previsão da inflação média dos subitens do IPCA",
    x = "Tempo",
    y = "Inflação média"
  )


