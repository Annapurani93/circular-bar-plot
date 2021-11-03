library(tidyverse)
library(readxl)
library(ggtext)
library(png)
library(cowplot)

read_excel("100 Most Spoken Languages.xlsx")->data
str_replace_all(data$`Total Speakers (in million)`,"M","")->data$`Total Speakers (in million)`
str_replace_all(data$`Native Speakers (in million)`,"M","")->data$`Native Speakers (in million)`
str_replace_all(data$`Native Speakers (in million)`,"NA","0")->data$`Native Speakers (in million)`

data$Rank<-as.numeric(data$Rank)
data$`Total Speakers (in million)`<-parse_number(data$`Total Speakers (in million)`)
data$`Native Speakers (in million)`<-parse_number(data$`Native Speakers (in million)`)

colnames(data)<-c("Rank","Language","Total","Native","Origin")

data%>%
  group_by(Origin, Language)%>%
  select(Origin,Language,Total)%>%
  arrange(Origin,.by_group = TRUE)->d2

d2$id <- seq(1, nrow(d2))
d2 <- d2 %>% arrange(Origin)

label_data <- d2
number_of_bar <- nrow(d2)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar     
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


ggplot(d2, aes(x=as.factor(id), y=Total, fill=Origin)) +       
  geom_bar(aes(x=as.factor(id), y=Total, fill=Origin), colour="white",stat="identity", alpha=1) +
  scale_fill_manual(values=c("#ee4035","#f37736","#fdf498","#7bc043","#0392cf",
                             "#96ceb4","#205566","#69587b","#f16a6f","#fea889",
                             "#b17373"))+
  ylim(-1200,1200) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=Total+10, label=paste(Language,",",Total,"million"),hjust=hjust), color="white", fontface="bold",alpha=0.8, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(colour="white",size=10,face="bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "white", face="bold"),
        plot.subtitle=element_markdown(size=12, colour="white", margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=9, colour="white",margin=margin(t=15)))+
  labs(title="42 OF THE 100 MOST-SPOKEN LANGUAGES ARE OF INDO-EUROPEAN ORIGIN",
       subtitle="The below data visualization shows the top 100 most-spoken languages in the world coloured based on their origin.<br> The size of the bar shows the number of people who speak the language",
       caption = "Data: @DiversityinData | Design: @annapurani93")->cbp


logo <- readPNG("C:/Users/Annapurani/Desktop/800px-Wikipedia-logo-v2.png")
ggdraw(cbp) +
  draw_image(logo, x = +.003, y =-.07, scale = .27)->cbp1

ggsave("cbp.png",cbp1,width=9.5,height=11)

