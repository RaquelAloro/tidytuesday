library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)


gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')


#First I separate the colums I want to keep. I'm interested in the articles violated but they are in the same column. 
#I separate them using str_split in the | and then, with unnest, I create a row for each article violated.
#Then I use str_sub to get only the numbers, as I'm not interested in the subarticles violated. Besides, each has a different notation.


violations_art <- gdpr_violations %>% select(id, 
                                          name,
                                          price, 
                                          date, 
                                          controller,
                                          article_violated,
                                          type) %>%
  mutate(article_violated= str_split(article_violated, "\\|"))%>%
  unnest(article_violated) %>%
  mutate(article_violated = str_sub (article_violated, 4, 8)) 

#In order to get the number I convert the articles into a vector, and apply gsub so I get only the digits, then I put it back on
#the data frame.I adjust to remove the . and then remove those rows that didn't have an article number (4)

vector_art <- violations_art$article_violated

vector_art_digit <- gsub("[^[:digit:].]", "",  vector_art)


violations_art <- cbind(violations_art, vector_art_digit) %>%
  select(-article_violated)%>%
  mutate(article = str_sub (vector_art_digit, 2))


violations_art <- violations_art%>% 
  select (-vector_art_digit, -type) %>% 
  filter(article != "") %>% 
  rename (article_violated = article)


#Here I group by article violated and obtain the number of times than an article was violated, globally. 

violations_gr_art<- violations_art %>% group_by(article_violated)%>%
  summarize (cases_per_article = n_distinct(id)) %>%
  arrange(desc(cases_per_article))


#I can see the article that gets violated the most is 5. Now I want to represent it. 

plot_violations_art <- ggplot(violations_gr_art, 
                              aes(x = reorder(article_violated, cases_per_article),
                                  y = cases_per_article))+
  geom_point(size = 4, color="navyblue")+
  geom_text(label = violations_gr_art$cases_per_article,
            nudge_y=4)+
  geom_segment(aes(x = reorder(article_violated, cases_per_article), 
                   y = 0, 
                   xend = reorder(article_violated, cases_per_article), 
                   yend = cases_per_article),
                   color="navyblue",
               size=1.5)+
  labs (title = "Most violated articles in Europe",
        y = "Number of violations",
        x = "Article")+
  theme(
    text = element_text(color = "black"),
    axis.line = element_line(color="black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    strip.background = element_blank()
  ) +
  coord_cartesian (ylim=c(0,115))+
  expand_limits(x = 0, y = 0)+
  scale_y_continuous(expand = c(0, 0))+
  annotate("text", label = "Articles 5, 6, and 32 are the",
           x=20, y=80, size=4.5, fontface= "bold")+
  annotate("text", label = "most violated in Europe",
           x=20, y=76, size=4.5, fontface= "bold")+
  annotate("curve", x=20, y = 85, xend=25, yend=100, curvature=-0.3,
           arrow = arrow(length=unit(2, "mm")), size=1.001)
plot_violations_art



#Next I want to see which articles were the most expensive to violate. For that, I group by article and obtain both the number of times that
#the article was violated and the total amount of fines. Then I get the average price paid for each fine of each article.

violations_price<- violations_art %>% group_by(article_violated)%>%
  summarize (cases_per_article = n_distinct(id),
             amount_per_article = sum(price)) %>%
  mutate(avg_article = amount_per_article/cases_per_article) %>%
  arrange(desc(cases_per_article))


#Finally, I represent it in a bar plot (using geom_col instead of geom_bar). The height represents the price of each article, they were very diverse
#so I used the logaritmic scale. 
#The color represents the number of times the article was violated. It's mostly blue because the majority of the articles were violated
#just a few times (except 6, 5, and 32, as we saw before)

 plot_price<- ggplot(violations_price, 
                              aes(x = reorder(article_violated, avg_article),
                                  y = avg_article,
                                  fill=violations_price$cases_per_article))+
   geom_col(position="stack")+
   scale_y_log10()+
   labs (title = "Average fine for each article",
         y = "Average price fine (???)",
         x = "Article", 
         fill="Violations per article")+
   scale_fill_gradientn(colours = c("#0A0094", "#35009E", "#6600A0",
                                    "#9D01B4", "#BE01A3", "#C90176", 
                                    "#D40244", "#DE030D", "#E93603",
                                    "#F47A04", "#FFC205"))+
   theme_minimal()+
   annotate("rect", xmin= 4, ymin=2e6, xmax=16, ymax=2e7, alpha=0.2,
            fill="yellow")+
   annotate("rect", xmin= 19.5, ymin=2e7, xmax=26.3, ymax=1.5e8, alpha=0.2,
            fill="navyblue")+
   annotate("text", label = "Most violated articles",
            x=10, y=1e7)+
   annotate("text", label = "are not the most expensive",
            x=10, y=5e6)+
   annotate("curve", x=10, y = 3e6, xend=17, yend=7e5, curvature=0.3,
            arrow = arrow(length=unit(2, "mm")))+
   annotate("text", label = "Art. 7 is the most", size=3.5,
            x=23, y=7.5e7)+
   annotate("text", label = "expensive one", size=3.5,
            x=23, y=4e7)
 
plot_price

     
  
