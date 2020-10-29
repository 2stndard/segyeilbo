segye.data<- read.csv('c:/R/data/general analysis1.csv', stringsAsFactors = F)
glimpse(segye.data)

segye.data$year <- as.factor(substr(segye.data$year, 1, 4))

segye.data <- segye.data %>% filter(existance != '폐(원)교')

segye.data <- segye.data %>% filter(kind %in% c('초등학교', '중학교', '고등학교'))

segye.data$kind <- factor(segye.data$kind, levels = c('초등학교', '중학교', '고등학교'), ordered = T)

segye.data$province <- factor(segye.data$province, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)

segye.data$estkind <- factor(segye.data$estkind, levels = c('국립', '공립', '사립'), ordered = T)

segye.data$scale <- factor(segye.data$scale, levels = c('특별/광역시', '시', '읍지역', '면지역', '특수지역'), ordered = T)

segye.data$std <- as.numeric(sub(",", "", segye.data$std, fixed = TRUE))


summary(segye.data)
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
    traceorder = 'reversed', 
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
    traceorder = 'reversed', 
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


####################  연도별 예상 고3 학생수
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

k12.std <- read.table('clipboard', sep = '\t')

colnames(k12.std) <- c('adm.year', 'adm.num')

rbind(df.temp, cbind(year = '2020', k12.std[, 1:2])) %>% arrange(adm.year) -> df.temp

factor(df.temp$adm.year)

df.temp %>%
  plot_ly(y = ~adm.num, x = ~adm.year) %>%
  add_bars(data = df.temp %>% filter(adm.year <= 2019), name = '연도별 고3학생수(실측)') %>%
  add_bars(data = df.temp %>% filter(adm.year > 2019), name = '연도별 고3학생수(예상)') %>%
  add_lines(y = 533492, name = '19년도 입학정원') %>%
  add_lines(y = 221742, name = '19년도 입학정원(수도권)', 
            line = list(shape = 'hv')
            ) %>%
  add_text(text = ~(scales::number_format(big.mark = ',')(adm.num)), textposition = "top", showlegend = F, 
           textfont = list(size = 9)) %>%
  layout(annotations = list(x = 2031, 
                           y = 533492, 
                           text = '533,492',
                           showarrow = T, 
                           showahead = 7, 
                           ax = 45,
                           ay = 25),
         yaxis = list(
           title = '학생수'
         ),
         xaxis = list(
           title = '연도'
         ), 
         title = list(text = '연도별 예상 고3학생수', 
                      font = list(size = 30, 
                                  color = toRGB('black')
                      ), 
                      x = 0, 
                      y = 0.97, 
                      xref = 'paper'
         )
  ) %>%
  layout(annotations = list(x = 2031, 
                            y = 221742, 
                            text = '221,742',
                            showarrow = T, 
                            showahead = 7, 
                            ax = 45,
                            ay = 25)
  ) %>%
  layout(annotations = 
           list(x = 0, y = 1.01, text = "2020년 초1~고3학생들이 중도탈락없이 진급함을 전제하며 입학정원은 전문대, 대학을 포함함", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='left', yanchor='top', xshift=0, yshift=0,
                font=list(size=10, color="red"))
  ) %>%
  layout(margin = list(
    t = 50, automargin = T
    ), 
    legend = list(orientation = 'h', xanchor = 'left', y = -0.15), 
    autosize = T
  )
  


####################  학급수


segye.data %>%
  group_by(kind, year) %>%
  summarise(n.cls = sum(class, na.rm = T), 
            n.teacher = sum(teacher, na.rm = T), 
            n.std = sum(std, na.rm = T)) %>% 
plot_ly(x = ~ year, colors = 'Dark2') %>%
  add_trace(type = 'bar', y = ~n.teacher, color = ~kind, name = ~kind, showlegend = T, opacity = 0.5, legendgroup = '교원수') %>%
  add_trace(type = 'scatter', y = ~n.cls, mode = 'markers+lines', name = ~kind, color = ~kind, yaxis = 'y2', legendgroup = '학급수') %>%
  add_trace(type = 'scatter', mode = 'text', y = ~n.cls, text = ~(scales::number_format(big.mark = ',')(n.cls)), textposition = "top", showlegend = F, yaxis = 'y2') %>%
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black')
                                   )
                      ), 
         yaxis = list(title = '교원수(명)'),
         yaxis2 = list(showticklabels = TRUE,
                       overlaying = "y",
                       side = "right",
                       title = "학급수(개)",
                       automargin = T, # This do the trick, 
                       showgrid = F, 
                       rangemode = 'tozero'
                       ),
  title = list(text = '연도별 학급수 추이', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    traceorder = 'reversed', x = 1.1, y = 0.5
  ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t = 50)
  )


k12.std <- read.table('clipboard', sep = '\t')

colnames(k12.std) <- c('adm.year', 'adm.num')

rbind(df.temp, cbind(year = '2020', k12.std[, 1:2])) %>% arrange(adm.year) -> df.temp
