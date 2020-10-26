mpg %>%
  group_by(manufacturer, class) %>%
  summarise(models=n()) %>%
  plot_ly(x=~manufacturer, y=~models, type="scatter", mode="lines+marker", color=~class)


mpg %>%
  group_by(class,manufacturer) %>%
  summarise(models=n()) %>%
  plot_ly(x=~manufacturer, y=~models, group=~class,
          type="scatter",color=~class, mode="lines+markers")


mpg %>%
  group_by(manufacturer,class) %>%
  summarise(models=n()) %>%
  plot_ly(x=~manufacturer, y=~models, group=~class,
          type="scatter",color=~class, mode="lines+markers")
