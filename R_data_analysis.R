
library(dplyr)
library(ggpubr)

df = read.csv("C:/Users/nicco/Documents/TeX/AI-Proj/data_new.csv")

df$Frontier = as.numeric(df$Frontier);
df$Explored = as.numeric(df$Explored);
df$Generated = df$Frontier + df$Explored;

summarizedDf = df %>% group_by(Algorithm, Polygons)%>%
  summarise(meanEBF = mean(EBF),
            maxEBF = max(EBF),
            minEBF = min(EBF),
            meanRuntime = mean(Time),
            maxRuntime = max(Time),
            minRuntime = min(Time),
            meanMemory = mean(Memory),
            maxMemory = max(Memory),
            minMemory = min(Memory),
            meanFrontier = mean(Frontier),
            maxFrontier = max(Frontier),
            minFrontier = min(Frontier),
            maxExplored = max(Explored),
            minExplored = min(Explored),
            meanExplored = mean(Explored),
            avgVertices = mean(Avg.vertices),
            Depth = mean(Depth),
            generated = mean(Generated),
            .groups = 'drop')

UC_Data <- summarizedDf[summarizedDf$Algorithm == 'UC',];
A2_Data <- summarizedDf[summarizedDf$Algorithm == 'A*-d2',];
A6_Data <- summarizedDf[summarizedDf$Algorithm == 'A*-d6',];

UCCorrel <- UC_Data %>% summarize(correlazione = cor(meanEBF, Polygons)) %>% .$correlazione;
A2Correl <- A2_Data %>% summarize(correlazione = cor(meanEBF, Polygons)) %>% .$correlazione;
A6Correl <- A6_Data %>% summarize(correlazione = cor(meanEBF, Polygons)) %>% .$correlazione;

UC_Data %>%
  ggline(x="Polygons", y="meanEBF");

A2_Data %>%
  ggline(x="Polygons", y="meanEBF");

A6_Data %>%
  ggline(x="Polygons", y="meanEBF");

df %>% 
  group_by(Algorithm, Polygons) %>% 
  ggline(x = "Polygons", y = "Generated", add = "mean_se", color = "Algorithm")

