segye.data<- read.csv('D:/R/data/general analysis.csv', stringsAsFactors = F)
glimpse(segye.data)

segye.data$year <- as.factor(substr(segye.data$year, 1, 4))

segye.data <- segye.data %>% filter(existance != '폐(원)교')

segye.data <- segye.data %>% filter(kind %in% c('초등학교', '중학교', '고등학교'))

segye.data$kind <- factor(segye.data$kind, levels = c('초등학교', '중학교', '고등학교'), ordered = T)

segye.data$province <- factor(segye.data$province, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)

segye.data$estkind <- factor(segye.data$estkind, levels = c('국립', '공립', '사립'), ordered = T)

segye.data$scale <- factor(segye.data$scale, levels = c('특별/광역시', '시', '읍지역', '면지역', '특수지역'), ordered = T)

segye.data$std <- as.numeric(sub(",", "", segye.data$std, fixed = TRUE))


View(segye.data)
class(segye.data)

segye.data %>%
  group_by(kind, year) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% 
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~kind, color = ~kind, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '학교급'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

############## 지역규모별
segye.data %>%
  filter(kind == '초등학교') %>%
  group_by(year, scale) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~scale, color = ~scale, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(초등학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '지역규모'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )


segye.data %>%
  filter(kind == '중학교') %>%
  group_by(year, scale) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~scale, color = ~scale, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(중학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '지역규모'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

segye.data %>%
  filter(kind == '고등학교') %>%
  group_by(year, scale) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~scale, color = ~scale, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(고등학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '지역규모'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )


############## 설립별
segye.data %>%
  filter(kind == '초등학교') %>%
  group_by(year, estkind) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~estkind, color = ~estkind, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(초등학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '설립'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )


segye.data %>%
  filter(kind == '중학교') %>%
  group_by(year, estkind) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~estkind, color = ~estkind, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(중학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '설립'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

segye.data %>%
  filter(kind == '고등학교') %>%
  group_by(year, estkind) %>%
  summarise(std.per.sch = mean(std, na.rm = T)) %>% as.data.frame() %>%
  plot_ly(x = ~ year, y = ~std.per.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~estkind, color = ~estkind, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~round(std.per.sch, 1), textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교당 학생수(명)'
  ), 
  title = list(text = '연도별 학교당 학생수 추이(고등학교)', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '설립'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

  
  ####################  학교수
segye.data %>%
  group_by(year, kind) %>%
  summarise(n.sch = n()) %>% as.data.frame() %>% write.table('clipboard', sep = '\t')
  plot_ly(x = ~ year, y = ~n.sch) %>%
  add_trace(type = 'scatter', mode = 'markers+lines', name = ~kind, color = ~kind, colors = 'Dark2') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~n.sch, textposition = "top", showlegend = F) %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black'))
  ), 
  yaxis = list(title = '학교수(개)'
  ), 
  title = list(text = '연도별 학교수 추이', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '학교급'
    ), 
    y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )


####################  학생수
df.temp <- segye.data %>%
  filter(kind == '초등학교') %>%
  group_by(year, kind) %>%
  summarise(sum.1.std = sum(std.1, na.rm = T), 
            sum.2.std = sum(std.2, na.rm = T), 
            sum.3.std = sum(std.3, na.rm = T), 
            sum.4.std = sum(std.4, na.rm = T), 
            sum.5.std = sum(std.5, na.rm = T), 
            sum.6.std = sum(std.6, na.rm = T), 
            ) %>% as.data.frame()

segye.data %>%
  filter(kind == '중학교') %>%
  group_by(year, kind) %>%
  summarise(sum.7.std = sum(std.1, na.rm = T), 
            sum.8.std = sum(std.2, na.rm = T), 
            sum.9.std = sum(std.3, na.rm = T), 
  ) %>% as.data.frame() %>% select(3:5) %>% cbind(df.temp) -> df.temp

segye.data %>%
  filter(kind == '고등학교') %>%
  group_by(year, kind) %>%
  summarise(sum.10.std = sum(std.1, na.rm = T), 
            sum.11.std = sum(std.2, na.rm = T), 
            sum.12.std = sum(std.3, na.rm = T), 
  )  %>% as.data.frame() %>% select(3:5) %>% cbind(df.temp)  -> df.temp

df.temp <- df.temp[, c(7, 9:14, 4:6, 1:3)]

df.temp <- df.temp[, c(1, seq(13, 2, -1))]

colnames(df.temp) <- c('year', as.character(2020:2031))

df.temp <- df.temp %>% gather(key = adm.year, value = adm.num, 2:13) %>% filter(year == 2020)


df.temp %>%
  plot_ly(y = ~adm.num, x = ~adm.year) %>%
  add_trace(type = 'bar', name = '연도별 고3학생수') %>%
  add_trace(y = 533492, type = 'scatter', mode = 'lines', name = '19년도 입학정원<br>533,492명') %>%
  add_trace(type = 'scatter', mode = 'text', text = ~(scales::number_format(big.mark = ',')(adm.num)), textposition = "top", showlegend = F)
