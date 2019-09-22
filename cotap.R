library(dplyr)
library(ggplot2)
library(gganimate)
library(colorspace)
library(png)
library(gifski)

url <- "http://www.camara.leg.br/cotas/Ano-2019.csv.zip"
local <- file.path("C:~/Ano-2019-a.zip")
download.file(url, local)

zipF<- "C:~/Ano-2019-a.zip"
outDir<-"C:~/"
unzip(zipF, exdir = outDir)

cp_df <- read.csv2("C:~/Ano-2019-a.csv", 
                   na.strings = c("", "NA"))

str(cp_df)
df1 <- cp_df[,c("sgPartido", "vlrLiquido", "numMes", "ideCadastro")]

df1 <- na.omit(df1)

df1$numMes <- as.numeric(df1$numMes)
df1$vlrLiquido <- as.numeric(df1$vlrLiquido)
df1$sgPartido <- as.character(df1$sgPartido)

df2 <- group_by(df1, numMes, sgPartido)
df3 <- summarise(df2,valor_total= sum(vlrLiquido))

df3$valor_totalMilhoes <- df3$valor_total/1000000

df4<- df3 %>% group_by(sgPartido) %>% 
  arrange(numMes, .by_group = TRUE)%>%
  mutate(valor_totalMilhoes = cumsum(valor_totalMilhoes))

#* 1 ensures we have non-integer ranks while sliding
df5<- df4 %>% group_by(numMes) %>% 
  mutate(rank = min_rank(-valor_totalMilhoes) * 1) %>%
  filter(rank <=10) %>%
  ungroup()



#plotting static plot
static_plot<-ggplot(df5,aes(rank,group=sgPartido,
                            fill=as.factor(sgPartido),
                            color=as.factor(sgPartido))) +
  geom_tile(aes(y = valor_totalMilhoes/2,
                height = valor_totalMilhoes,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(sgPartido, " ")), 
            vjust = 0.2, hjust = 1) +
  geom_text(aes(y=valor_totalMilhoes,
                label = paste(" ",valor_totalMilhoes)), hjust=0)+
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", 
                            colour= "grey", vjust=-1),
    plot.subtitle=element_text(size=18, hjust=0.5, 
                               face="italic", color="grey"),
    plot.caption =element_text(size=8, hjust=0.5, face="italic", 
                               color="grey"),
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(), 
    plot.margin = margin(1,1,1,4, "cm")
  )

plt<-static_plot + transition_states(states = numMes, 
                                     transition_length = 4, 
                                     state_length = 1) + 
  ease_aes('cubic-in-out') +
  #view_follow(fixed_x = TRUE) +
  labs(title = 'Total gasto por Mês: {closest_state}', 
       subtitle = "Top 10",
       caption = "Data Source: Câmara dos deputados",
       x="",y="Valor liquido gasto em milhões")

plt


anim_save("C:~/top10.gif")
