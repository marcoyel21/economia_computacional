## EDA del proyecto final

# Wheelie Wonka

# Equipo 7


library(reshape2)
library(ggthemes)
library(grid)
library('heatmaply')


#Una vez construidas las variables temporales, procedemos a hacer el análisis exploratorio de los datos, teniendo en mente que la variable objetivo es la duración de los viajes.

### Análisis univariado

# Primero, revisamos la distribución de las variables.


# histograma para variables continuas
myhist <- function(yvar){
  ggplot(trips, aes_(x=as.name(yvar)))+
    geom_histogram()+
    ggtitle(paste0(as.name(yvar)))+
    xlab("")+
    ylab("")+ geom_rangeframe()+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
}

hists<- trips %>%
  select(
    duration,
    start_hour,
    age
  ) %>%
  names() %>%
  lapply(myhist)

# grafico las variables
grid.arrange(grobs=hists,ncol=3,
             top=textGrob("Distribución de las variables"))


# barras para las categóricas
# histograma para variables continuas
mybar <- function(yvar){
  ggplot(trips, aes_(x=as.name(yvar)))+
    geom_bar()+
    ggtitle(paste0(as.name(yvar)))+
    xlab("")+
    ylab("")+ geom_rangeframe()+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
}

bars <- trips %>%
  select(
    start_year,
    start_month,
    strt_statn,
    end_statn,
    start_weekday,
    subsc_type,
    gender,
    HPCP,
    Measurement_Flag
  ) %>%
  names() %>%
  lapply(mybar)

# grafico las variables
grid.arrange(grobs=bars,ncol=4,
             top=textGrob("Distribución de las variables"))




## Análisis bivariado

# Mapa de calor start y end station
x <- trips %>% count(strt_statn, end_statn)
summary(x)

y <- pivot_wider(x, id_cols=c("strt_statn"), 
                 names_from=c("end_statn"),
                 values_from=c("n"),
                 values_fill=0)
class(y)

# Matrix format. nota. las columnas son end stations y las filas start stations
rownames(y) <- y[,1] %>% unlist()
y <- y %>% dplyr::select(c(-1))
y <- as.matrix(y)


heatmaply(y,
          dendrogram = "none",
          colors = c("wheat","navyblue"),
          grid_color = "white",
          fontsize_row = 7, fontsize_col = 7,
          xlab = "",
          ylab = "",
)




# Se muestra la relación entre edad y duración (acotada superiormente por su percentil 99%)
ggplot(trips, aes(age, duration)) +
  geom_point(alpha=0.3) +
  geom_smooth() +
  ylim(0, quantile(trips$duration, probs=0.99))

corr_age_duration <- cor(trips$age, trips$duration, use = "complete.obs")



# Histograma de la edad para todas las observaciones
ggplot(data=subset(trips, !is.na(age)), aes(x=age)) + 
  geom_histogram() +
  geom_vline(aes(xintercept=mean(age, na.rm=T)), color="black", size=1) +
  geom_vline(aes(xintercept=median(age, na.rm=T)), color="black", linetype="dashed", size=1) 


# Histograma de la edad por género
ggplot(data=subset(trips, !is.na(age)), aes(x=age, color=gender, fill=gender)) + 
  geom_histogram(alpha=0.2, position="identity") +
  geom_vline(aes(xintercept=mean(age, na.rm=T)), color="orange", size=1) +
  geom_vline(aes(xintercept=median(age, na.rm=T)), color="black", linetype="dashed", size=1) +
  annotate("text",                        # Add text for mean
           x = 25,
           y = 30000,
           label = paste("Median =", median(trips$age, na.rm=T)),
           col = "black",
           size = 4) + 
  annotate("text",                        # Add text for mean
           x = 42,
           y = 30000,
           label = paste("Mean =", trunc(mean(trips$age, na.rm=T)*100)/100),
           col = "orange",
           size = 4)
