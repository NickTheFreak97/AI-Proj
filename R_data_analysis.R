
library(dplyr)
library(ggpubr)
library(xtable)

df = read.csv("C:/Users/nicco/Documents/TeX/AI-Proj/data_new.csv")

df$Frontier = as.numeric(df$Frontier);
df$Explored = as.numeric(df$Explored);
df$Generated = df$Frontier + df$Explored;

UC_Data_all <- df[df$Algorithm == 'UC',];
A2_Data_all <- df[df$Algorithm == 'A*-d2',];
A6_Data_all <- df[df$Algorithm == 'A*-d6',];

UCCorrel <- UC_Data_all %>% summarize(correlazione = cor(Cost, Time)) %>% .$correlazione;
A2Correl <- A2_Data_all %>% summarize(correlazione = cor(Cost, Time)) %>% .$correlazione;
A6Correl <- A6_Data_all %>% summarize(correlazione = cor(Cost, Time)) %>% .$correlazione;

summarizedDf = df %>% group_by(Algorithm, Polygons)%>%
  summarise(meanEBF = mean(EBF),
            EBFsd = sd(EBF),
            meanRuntime = mean(Time),
            runtimeSd = sd(Time),
            meanMemory = mean(Memory),
            meanFrontier = mean(Frontier),
            meanExplored = mean(Explored),
            avgVertices = mean(Avg.vertices),
            meanDepth = mean(Depth),
            generated = mean(Generated),
            meanCost = mean(Cost),
            .groups = 'drop')

#UC_df <- df[df$Algorithm == "UC",];

#print(xtable(summarizedDf, type = "latex"), file = "filename2.tex")

UC_Data <- summarizedDf[summarizedDf$Algorithm == 'UC',];
A2_Data <- summarizedDf[summarizedDf$Algorithm == 'A*-d2',];
A6_Data <- summarizedDf[summarizedDf$Algorithm == 'A*-d6',];

summaryFrontierIncrease <- data.frame(t((A6_Data$meanFrontier - A2_Data$meanFrontier)/(A2_Data$meanFrontier)));
summaryExploredIncrease <- data.frame(t((A6_Data$meanExplored - A2_Data$meanExplored)/(A2_Data$meanExplored)));

UC_summarizedDf <- UC_Data %>% summarise(
  meanEFB = sd(meanEBF)
)

A2_summarizedDf <- A2_Data %>% summarise(
  meanEFB = sd(meanEBF)
)

A6_summarizedDf <- A6_Data %>% summarise(
  meanEFB = sd(meanEBF)
)

UCCorrel <- UC_Data %>% summarize(correlazione = cor(Polygons, meanEBF)) %>% .$correlazione;
A2Correl <- A2_Data %>% summarize(correlazione = cor(Polygons, meanEBF)) %>% .$correlazione;
A6Correl <- A6_Data %>% summarize(correlazione = cor(Polygons, meanEBF)) %>% .$correlazione;

UC_Data %>%
  ggline(x="Polygons", y="generated");

A2_Data %>%
  ggline(x="Polygons", y="generated");

A6_Data %>%
  ggline(x="Polygons", y="generated");

memoryPlot <- df %>% 
  group_by(Algorithm, Polygons) %>% 
  ggline(x = "Polygons", y = "Memory", add = "mean_se", color = "Algorithm", xlim=c(0,72))

generatedNodesPlot <- df %>% 
  group_by(Algorithm, Polygons) %>% 
  ggline(x = "Polygons", y = "Generated", add = "mean_se", color = "Algorithm", xlim=c(0,72))

timePlot <- df %>% 
  group_by(Algorithm, Polygons) %>% 
  ggline(x = "Polygons", y = "Time", add = "mean_se", color = "Algorithm", xlim=c(0,72), ylim=c(0,80000))

ggarrange(timePlot, memoryPlot, generatedNodesPlot, ncol = 2, nrow = 2)


plot(A2_Data$Polygons,A2_Data$meanRuntime,
     main='Retta di regressione lineare poligoni-tempo esecuzione per A*(d2)',
     xlab='Polygons',ylab='meanRuntime')

# plot a regression line
abline(lm(meanRuntime~Polygons,data=A2_Data),col='red')

df25 <- df[df$Polygons == 25,];

df25 %>% 
  group_by(Algorithm, Polygons) %>% 
  ggline(x = "Polygons", y = "Time", color = "Algorithm")

df25$Memory = df25$Memory/1024;
df25$Time = df25$Time/1000;

UC25quartilesMemo = quantile(df25[df25$Algorithm == 'UC',]$Memory) 
A225quartilesMemo = quantile(df25[df25$Algorithm == 'A*-d2',]$Memory) 
A625quartilesMemo = quantile(df25[df25$Algorithm == 'A*-d6',]$Memory) 

UC25quartilesGen = quantile(df25[df25$Algorithm == 'UC',]$Generated) 
A225quartilesGen = quantile(df25[df25$Algorithm == 'A*-d2',]$Generated) 
A625quartilesGen = quantile(df25[df25$Algorithm == 'A*-d6',]$Generated)

UC25quartilesTime = quantile(df25[df25$Algorithm == 'UC',]$Time) 
A225quartilesTime = quantile(df25[df25$Algorithm == 'A*-d2',]$Time) 
A625quartilesTime = quantile(df25[df25$Algorithm == 'A*-d6',]$Time)

summarizedDF25 <-df25 %>% group_by(Algorithm) %>% summarise(
  minTime = min(Time),
  maxTime = max(Time),
  meanTime = mean(Time),
  
  minMemory = min(Memory),
  maxMemory = max(Memory),
  meanMemory = min(Memory),
  
  minGenerated = min(Generated),
  maxGenerated = max(Generated),
  meanGenerated = mean(Generated),
)

boxplot(Generated~Algorithm,data = df25, col=c("green","yellow","orange"), boxwex = 0.5, medlwd=1)
boxplot(Time~Algorithm,data = df25, col=c("green","yellow","orange"), boxwex = 0.5, medlwd=1)
boxplot(Memory~Algorithm,data = df25, col=c("green","yellow","orange"), boxwex = 0.5, medlwd=1)

ggarrange(df25GeneratedBox, df25TimeBox, df25MemoryBox, ncol = 1, nrow = 3)


