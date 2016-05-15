library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(igraph)
library(ggthemes)
####本文档主要对上月的微博热点问题‘和颐酒店女生遇袭问题‘进行深度剖析，主要针对其第一条微博####
##读取数据及初步清理
###评论数据
comment_raw = read_csv("D:/Workspace/MyPython/microblog/ext/comment/wanwan.csv")
#统一时间格式
unifine_time_format <- function(time) {
    SHIJIAN <- gsub("月","-",time)
    SHIJIAN <- gsub("日","",SHIJIAN)
    SHIJIAN <- gsub("今天","05-02",SHIJIAN)
    SHIJIAN <- paste("2016-",SHIJIAN,sep = "")
    SHIJIAN <-
        as.POSIXct(SHIJIAN,format = "%Y-%m-%d %H:%M",tz = "Asia/Taipei")
    return(SHIJIAN)
}
#互动类型回复或者艾特
reply <- function(text) {
    if (grepl("//",text)) {
        text <- strsplit(text,"//")[[1]][1]
        replies <- NA
        type <- "comment_forward"
    }
    if (grepl("(回复)?@[\u4e00-\u9fa5a-zA-Z0-9_-]{1,30}",text,perl = T)) {
        replies <-
            str_c(
                str_extract_all(
                    text, "(回复)?@[\u4e00-\u9fa5a-zA-Z0-9_-]{1,30}", simplify = FALSE
                )[[1]],sep = " ",collapse = "  "
            )
        text <-
            str_replace_all(text,"(回复)?@[\u4e00-\u9fa5a-zA-Z0-9_-]{1,30}","")
        type <- "comment_spread"
    }
    else{
        text <- text
        replies <- NA
        type <- "comment_normal"
    }
    return(c(replies,text,type))
}

comment <- comment_raw %>%
    separate(text,c("user","text"),"：",extra = "merge") %>%
    mutate(time = unifine_time_format(time)) %>%
    mutate(
        week = week(time),day = day(time),hour = hour(time),minute = minute(time)
    ) %>%
    separate(
        time,c("date","daytime"),sep = " ",extra = "merge",remove = FALSE
    ) %>%
    mutate(day_hour = as.POSIXct(paste(date," ",hour,":00:00",sep = "")))
comment <- comment[!duplicated.data.frame(comment),]
comment$date <- as.Date(comment$date)
###转发数据
file <- "D:/Workspace/MyPython/microblog/ext/forward"
fil <- list.files(file,full.names = TRUE)
forward_raw <- ldply(
    fil,read_csv,col_types = cols(
        forward = col_integer(),
        like = col_integer(),
        mid = col_character(),
        name = col_character(),
        text = col_character(),
        time = col_datetime(),
        uid = col_character()
    )
)
forward <- forward_raw %>%
    mutate(
        week = week(time),day = day(time),hour = hour(time),minute = minute(time)
    ) %>%
    separate(
        time,c("date","daytime"),sep = " ",extra = "merge",remove = FALSE
    ) %>%
    mutate(day_hour = as.POSIXct(paste(date," ",hour,":00:00",sep = "")))
forward <- forward[!duplicated.data.frame(forward),]
forward$date <- as.Date(forward$date)
##寻找转发与评论的热点
sort(table(comment$day_hour),decreasing = TRUE)[1:10]
table(forward$day_hour)
events <-
    data.frame(
        time = as.POSIXct(
            c(
                "2016-04-06 00:00:00","2016-04-06 08:00:00","2016-04-07 09:00:00","2016-04-07 22:00:00"
            )
        ),event = c("评论高潮1","评论高潮2","评论高潮3","评论高潮4"),point = as.numeric(table(forward$day_hour)[c(
            "2016-04-06 00:00:00","2016-04-06 08:00:00","2016-04-07 09:00:00","2016-04-07 22:00:00"
        )])
    )
ggplot(comment,aes(x = day_hour)) +
    geom_line(
        stat = "count",colour = "red",show.legend = TRUE,size = 1
    ) + labs(x = "时间",y = "评论/转发数") +
    geom_line(
        data = forward,aes(day_hour),colour = "blue",show.legend = TRUE,stat = "count",size = 1
    ) +
    geom_text(
        aes(time, point, label = event),data = events,vjust = 2,size = 5,colour = alpha('black',0.5)
    ) +
    scale_x_datetime(limits = as.POSIXct(c(
        "2016-04-05 08:00:00","2016-04-09 00:00:00"
    )),date_labels = "%b %d") +
    geom_point(
        aes(time,point),data = events,size = 5,colour = alpha('yellow',0.5)
    )+
    theme_wsj()
ggsave("heatpoint.jpg",width = 14,height = 14,units = "in")
###得出热点产生的几个阶段，第一阶段:4-5 22:00~24:00;第二阶段4-6 4:00~9:00,第三阶段4-7 9:00~22:00###
##第一阶段
ggplot(comment,aes(x = time)) +
    geom_line(
        stat = "count",colour = "red",show.legend = TRUE,size = 1
    ) + labs(x = "时间",y = "评论/转发数") +
    geom_line(
        data = forward,aes(time),colour = "blue",show.legend = TRUE,stat = "count",size = 1
    ) +
    scale_x_datetime(limits = as.POSIXct(c(
        "2016-04-05 20:00:00","2016-04-06 00:00:00"
    )))
ggsave("stage_1.jpg",width = 14,height = 14,units = "in")
forward_1 <-
    forward %>% filter(day == 5,as.numeric(format(time,"%H")) >= 20 &
                           as.numeric(format(time,"%H")) <= 23) %>% arrange(desc(forward))

##第二阶段
ggplot(comment,aes(x = time)) +
    geom_line(
        stat = "count",colour = "red",show.legend = TRUE,size = 1
    ) + labs(x = "时间",y = "评论/转发数") +
    geom_line(
        data = forward,aes(time),colour = "blue",show.legend = TRUE,stat = "count",size = 1
    ) +
    scale_x_datetime(limits = as.POSIXct(c(
        "2016-04-06 04:00:00","2016-04-06 09:00:00"
    )))+
    theme_igray()
ggsave("stage_2.jpg",width = 14,height = 14,units = "in")
forward_2 <-
    forward %>% filter(day == 6,as.numeric(format(time,"%H")) >= 4 &
                           as.numeric(format(time,"%H")) <= 9) %>% arrange(desc(forward))





##replies <- adply(as.array(comment$text),1,reply,.progress = "tk")

#####2.用户转发网络分析#####

##解析转发数据
forward_raw_1 <- forward_raw %>% filter(forward >= 1)
forward_raw_net <-
    str_extract_all(forward_raw_1$text, "@[\u4e00-\u9fa5a-zA-Z0-9_-]{1,30}:", simplify =
                        TRUE)
##理清转发层次
forward_raw_network <-
    aaply(forward_raw_net,1,str_replace_all,"@|:","",.progress = "text")
forward_network <- cbind(forward_raw_1$name,forward_raw_network)
forward_network <-
    data.frame(forward_network,stringsAsFactors = FALSE)

##转换为两两对应的关系
chain_user <- function(vector) {
    df <- c()
    for (v in 1:8) {
        if (as.character(vector[1,v + 1]) == "" ||
            is.na(as.character(vector[1,v + 1]))) {
            break
        }
        df_v <-
            c(as.character(vector[1,v]),as.character(vector[1,v + 1]))
        df <- rbind(df_v,df)
    }
    return(df)
}
user_chain <- c()
for (i in 1:nrow(forward_network)) {
    chain <- chain_user(forward_network[i,])
    user_chain <- rbind(user_chain,chain)
    cat(i,"\n")
}
##转发网络##

labels <- union(unique(user_chain[,1]),unique(user_chain[,2]))
ids <- 1:length(labels)
names(ids) <- labels
from <- as.character(user_chain[,1])
to <- as.character(user_chain[,2])
edges <- matrix(c(ids[from],ids[to]),ncol = 2)
g <- graph.empty(directed = T)
g <- add.vertices(g,length(labels))
g <- add.edges(g,t(edges))
V(g)$label <- labels
E(g)$weight <- count.multiple(g)
#g <- g - V(g)[degree(g,mode = "out") < 1]
g <-
    simplify(
        g,remove.multiple = TRUE,remove.loops = TRUE,edge.attr.comb = "mean"
    )
g <- graph.neighborhood(g, order = 5)[[1]]
member <- walktrap.community(g)
V(g)$member <- member$membership
std.degree.user <- 3
words.index <- (degree(g,mode = "in") >= std.degree.user)
words <- degree(g,mode = "in")[words.index]
names(words) <- V(g)[words.index]$label
#E(g)$weight <- count.multiple(g)
label <- NA
label[words.index] <- names(words)
V(g)$size = .5
max.d <- max(words)
min.d <- min(words)
V(g)[words.index]$size = 2 * (words - min.d) / (max.d - min.d) + 4
V(g)$color <- "White"
V(g)[words.index]$color <- "red"
E(g)$curved <- 0
member.num <- length(table(V(g)$member))
member.list <- list()
for (i in 1:member.num) {
    member.list <- c(member.list,list(which(V(g)$member == i)))
}
svg(filename = "de111.svg",width = 14,height = 14)
plot(
    g,
    vertex.size = V(g)$size/2,vertex.label.cex = V(g)$size / 4,layout = layout.fruchterman.reingold,vertex.label =
        label,vertex.color = V(g)$color,vertex.label.family = "GB1",edge.arrow.size =
        0,edge.width = E(g)$weight/4,mark.groups = member.list[1:7]
)
dev.off()
