library(dplyr)
library(readr)
library(haven)
library(janitor)
library(ggplot2)
library(viridis)
library(ingredients)
library(shiny)




#############event time 
library(haven)
event <- read_sav("C:/Users/xglale/Desktop/SFL EFFECTS/event_all.sav")
event <- event %>%
  mutate(
    color_group = ifelse(time >= 0, "post", "pre")
  )
k<-subset(event, group=="all")
grade<-subset(k, test=="Grade")
test<-subset(k, test=="NT")



windows()
p <- ggplot(grade, aes(x = time, y = est, color = color_group)) +
  geom_linerange(
    aes(ymin = cil, ymax = ciu, color = color_group),
    size = 8,
    alpha = 0.25,
    lineend = "butt"
  ) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~subject) +
  xlab("") +
  ylab("ATT") +
  ggtitle("A) Grade")+
  scale_y_continuous(
    breaks = c( -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15)
  )+
  scale_x_continuous(
    breaks = c(-4, -3,-2,-1,0,1,2,3,4)
  )+
  scale_color_manual(values = c("pre" = "grey70", "post" = "black")) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 20),
    strip.text   = element_text(size = 20),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none")+
  coord_cartesian(ylim = c(-0.15,0.15))
p

windows()
t <- ggplot(test, aes(x = time, y = est, color=color_group)) +
  geom_linerange(
    aes(ymin = cil, ymax = ciu, color = color_group),
    size = 8,        # thickness of the bar
    alpha = 0.25,    # transparency
    lineend = "butt" # squared-off ends like bars
  ) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~subject) +
  xlab("") +
  ylab("ATT (g,t)") +
  ggtitle("B) National test")+
  coord_cartesian(ylim = c(-0.30,0.30))+
  scale_y_continuous(
    breaks = seq(-0.30, 0.30, by = 0.05),
    labels = scales::label_number(accuracy = 0.001)
    
  ) +
  scale_x_continuous(
    breaks = c(-4, -3,-2,-1,0,1,2,3,4),
  )+
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    strip.text   = element_text(size = 20),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none")+
  scale_color_manual(values = c("pre" = "grey25", "post" = "grey10"))

t


library(ggpubr)

windows()
k1<-ggarrange(
  p, t,
  ncol = 1, nrow = 2,
  widths = c(3.5, 1),  
  align = "hv"
)
k1












############ATT
library(haven)
effects_att <- read_sav("C:/Users/xglale/Desktop/SFL EFFECTS/effects_att.sav")

h<-subset(effects_att, model=="btcert"|model=="ttcert"|model=="btexp"|model=="ttexp")


l<-subset(effects_att, het==3)

l<-subset(effects_att, model=="public"|model=="ind")

l<- l%>%
  mutate(
    sub2=
      case_when(
        subject== "mathgrad" ~ "Math (Grade)",
        subject== "mathnt" ~ "Math (NT)",
        subject== "swent" ~ "Swedish (NT)",
        subject== "swegrade" ~ "Swedish (Grade)",
        T~"other"))


l<- l%>%
  mutate(
    type=
      case_when(
        model== "public" ~ "Public schools",
        model== "ind" ~ "Independent schools"))


l<- l%>%
  mutate(
    stud=
      case_when(
        model== "iep" ~ "A) IEP",
        model== "childimm" ~ "E) Childhood immigrant",
        model=="migrant"~"D) Migrant background",
        model== "new5" ~ "F) Arrived within 5 years",
        model=="males"~"B) Males",
        model=="female"~"C) Females",
        model=="females"~"C) Females"))

l<- l%>%
  mutate(
    stud=
      case_when(
        model== "lowsupp" ~ "Low support",
        model== "highsupp" ~ "High support"))


l2<-subset(l, model!="IEP")
l3<-subset(l2, model!="migrant")
l4<-subset(l3, model!="iep")


h<- h%>%
  mutate(
    mod=
      case_when(
        model== "btcert" ~ "Certified: low",
        model== "ttcert" ~ "Certified: high",
        model== "btexp" ~ "Experience: low",
        model== "ttexp"~"Experience: high"))


l4$stud <- factor(
  l4$stud,
  levels = c("Childhood immigrant", "Arrived since 2010", "Newly arrived")
)


lc <- l %>% tidyr::drop_na()

windows()
p <- ggplot(l,
            aes(x = sub2, y = att)) +
  
  geom_errorbar(aes(ymin = cil, ymax = ciu),
                width = 0.15, size = 0.9) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~type) +
  xlab("") +
  ylab("ATT") +
  coord_cartesian(ylim = c(-0.15,0.15))+
  scale_y_continuous(
    breaks = c(-0.15,-0.10, -0.05,  0, 0.05, 0.10, 0.15),
    labels = scales::number_format(accuracy = 0.001)
  )+
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 12, angle=90),
    axis.text.y=element_text(size=12),
    strip.text   = element_text(size = 20)
  )
p




##########SCHOOL AQURT
k1<-subset(effects_att, ach!=0)
k<-subset(k1, ach!=5)

k<- k%>%
  mutate(
    quart=
      case_when(
        ach== 1 ~ "Quartile 1",
        ach== 2 ~ "Quartile 2",
        ach== 3 ~ "Quartile 3",
        ach== 4 ~ "Quartile 4"))

windows()
p <- ggplot(k,
            aes(x = sub2, y = att)) +
  
  geom_errorbar(aes(ymin = cil, ymax = ciu),
                width = 0.15, size = 0.9) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ quart) +
  xlab("") +
  ylab("ATT") +
  scale_y_continuous(
    breaks = c(-0.15,-0.125,-0.10,-0.0750, -0.05, -0.025, 0, 0.025, 0.05,0.0750, 0.10,0.125, 0.15),
    labels = scales::number_format(accuracy = 0.001)
  )+
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 12),
    strip.text   = element_text(size = 20)
  )
p









View(effects_att)

k<- k%>%
  mutate(
  sub2=
    case_when(
      subject== "mathgrad" ~ "Math (Grade)",
      subject== "mathnt" ~ "Math (NT)",
      subject== "swent" ~ "Swedish (NT)",
      subject== "swegrade" ~ "Swedish (Grade)",
      T~"other"))

effects_att<- effects_att%>%
  mutate(
    test=
      case_when(
        subject== "mathgrad" ~ "Grade",
        subject== "mathnt" ~ "NT",
        subject== "swent" ~ "NT",
        subject== "swegrade" ~ "Grade",
        T~"other"))




effects_att <- effects_att %>%
  mutate(
    xlab_clean = case_when(
      model == "all"    & test == "Grade" ~ "All\n(Grade)",
      model == "ind"    & test == "Grade" ~ "Independent\n(Grade)",
      model == "public" & test == "Grade" ~ "Public\n(Grade)",
      model == "all"    & test == "NT"    ~ "All\n(NT)",
      model == "ind"    & test == "NT"    ~ "Independent\n(NT)",
      model == "public" & test == "NT"    ~ "Public\n(NT)"
    )
  )

effects_att <- effects_att %>%
  mutate(
    sub22 = case_when(
      sub == "math" ~ "Math",
      sub == "swedish" ~ "Swedish"
    )
  )


windows()
p <- ggplot(effects_att,
            aes(x = xlab_clean, y = att)) +
  
  geom_errorbar(aes(ymin = cil, ymax = ciu),
                width = 0.15, size = 0.9) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  facet_wrap(~ sub22) +
  
  xlab("") +
  ylab("ATT") +
  scale_y_continuous(
    breaks = c(-0.15,-0.125,-0.10,-0.0750, -0.05, -0.025, 0, 0.025, 0.05,0.0750, 0.10,0.125, 0.15),
    labels = scales::number_format(accuracy = 0.001)
  )+
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 12),
    strip.text   = element_text(size = 20)
  )
p










windows()

p <- ggplot(effects_att,
            aes(x = interaction(model, test),
                y = att)) +
  geom_errorbar(aes(ymin = cil, ymax = ciu),
                width = 0.15,
                size = 0.9) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ sub) +
  xlab("School type × Outcome type") +
  ylab("ATT") +
  theme_minimal(base_size = 20) +
  theme(
    panel.border   = element_rect(colour = "black", fill = NA),
    axis.text.x    = element_text(angle = 45, hjust = 1, size = 20),
    axis.text.y    = element_text(size = 20),
    strip.text     = element_text(size = 22),
    axis.title     = element_text(size = 22)
  )
p
















