library(readr)
data <- read_csv("calidad_de_vida.csv")
data <- as.data.frame(data)

data <- data[, -which(names(data) %in% c('Overall rank'))]
View(data)

summary(data)


# Análisis descriptivo ----------------------------------------------------

# Matriz de correlación 
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(data[,-1], histogram=TRUE, pch=19)

colnames(data) <- c('País','Score', 'GDP per capita','Apoyo social','Esperanza de vida','Libertad','Generosidad', 'Percepciones de corrupción')

# Boxplot
library(tidyr)
df1<-gather(data,key = "Variable", value = "Valor",`Score`,`GDP per capita`, `Apoyo social`, `Esperanza de vida`,`Libertad`,`Generosidad`, `Percepciones de corrupción`)

library(ggplot2)
ggplot(df1, aes(x= Variable, y=Valor, fill = Variable)) + 
  geom_boxplot(position=position_dodge(1)) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

# MDS ----------------------------------------------------
library(ggpubr)

data <- data[, -which(names(data) %in% c('Score'))]

distancia <- dist(data[,-1])
resul_mds_clas<-cmdscale(distancia, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)

# se extrae la configuración
config<-resul_mds_clas$points
dimnames(config)[[2]]<-c('Dim. 1', 'Dim. 2')

#se grafica la configuración obtenida mediante MDS clásico

ggscatter(as.data.frame(config), x = "Dim. 1", y = "Dim. 2", color = "blue",
          label = data$País,
          size = 1,
          repel = TRUE)

# se obtiene la proporción de la varianza total explicada por las dos dimensiones
resul_mds_clas$GOF


clust <- kmeans(config, 2)$cluster %>%
  as.factor()
config <- as.data.frame(config)%>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(config, x = "Dim. 1", y = "Dim. 2", 
          label = data$País,
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)
