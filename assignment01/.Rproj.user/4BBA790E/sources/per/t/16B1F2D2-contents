# 清空工作环境
rm(list=ls())

# 导入包
library(pacman)
p_load(readxl, dplyr, tidyr, ggplot2, lubridate, data.table)
     

# 数据读取 --------------------------------------------------------------------

xls_path <- paste('./data/tmp00', 1:5, ".xls", sep="")

# 读取excel多个sheet并合并
excel_msheets <- function(path){
  sheet_names_ls = excel_sheets(path)  
  sheets = lapply(sheet_names_ls, read_excel, path=path)
  df_n = bind_rows(sheets[-length(sheets)])
  df_n                                                                                                                                                                
}
# 读取全部的5个excel表格
df <- bind_rows(lapply(xls_path, excel_msheets))

colnames(df)
df_detector <- df[df$FSTR_LOOPGROUPID=='NHNX40(1)',]



# 冗余和缺失数据处理 ---------------------------------------------------------------
# 去除多余列
df_detector <- select(df_detector, -c('...1')) 
# 冗余数据查找
dup_index <- df_detector$FDT_TIME %in% (df_detector$FDT_TIME[which(duplicated(df_detector$FDT_TIME))])
unique_df <- df_detector[!dup_index,]
dup_df <- df_detector[dup_index,]

# 冗余数据合并
mean_colnames <- c('FINT_VOLUME', 'FINT_SPEED', 'FINT_OCCUPY')
rows_to_add <- dup_df %>% group_by('FDT_TIME') %>% summarise(across(!all_of(mean_colnames), first), across(all_of(mean_colnames), mean))
# 去除\"FDT_TIME\"列
colnames(rows_to_add)
rows_to_add <- select(rows_to_add, -c('\"FDT_TIME\"'))

# 处理后的冗余数据和其他数据合并
dup_pro_df = bind_rows(unique_df, rows_to_add)

# 清除变量空间，释放内存
rm(rows_to_add, dup_df, unique_df)

# 按时间重新排序
dup_pro_df <- dup_pro_df %>% arrange(FDT_TIME)


# 缺失数据处理 ------------------------------------------------------------------

# 生成标准时间序列
start_time <- make_datetime(2010, 4, 18, 0, 0, 0)
end_time <- make_datetime(2010, 4, 24, 23, 59, 40)
interval_nums <- as.integer(time_length(time_length(interval(start_time, end_time)), unit='second') / 20)
standard_seq <- start_time + 20 * (0: interval_nums)
standard_seq_df <- data.frame(FDT_TIME=standard_seq)

# 缺失数据补全
non_lost_df <- left_join(standard_seq_df, dup_pro_df, by="FDT_TIME")
# 统计每天缺失数量
non_lost_df$DATE <- as.Date(non_lost_df$FDT_TIME)
lost_df <- non_lost_df %>% filter(is.na(FINT_VOLUME) | is.na(FINT_SPEED) | is.na(FINT_OCCUPY))
lost_day <- lost_df %>% group_by(DATE) %>% summarise(LOST_NUM = n())

# 用平均值法对na位置进行补全
# 确定缺失值在表中的位置
lost_location <- which(is.na(non_lost_df$FINT_VOLUME))
for (i in lost_location){
  if (i > 3){
    # 这里i-3和i-1要加括号
    non_lost_df[i, 3:5] <- as.integer(colMeans(non_lost_df[(i-3):(i-1), 3:5]))
    non_lost_df[i, 2] <- 'NHNX40(1)'
  }
}
# 获取补全后对应位置的数据
impute_res <- non_lost_df[lost_location,]


# 数据聚集 --------------------------------------------------------------------

# 速度数据公式
speed_converge <- function(volume_seq, speed_seq){
  vs_seq <- volume_seq * speed_seq
  converge_speed <- sum(vs_seq) / sum(speed_seq)
  round(converge_speed, 2)
}

# 数据聚集函数
data_converge <- function(original_df, t){
  # t为以秒做单位的时间间隔
  interval_nums <- t %/% 20
  original_index <- 0:(nrow(original_df) - 1)
  converge_index <- original_index %/% interval_nums + 1
  original_df$CONVERGE_INDEX <- converge_index
  original_df
}

integrate_df <- copy(non_lost_df)
integrate_df <- data_converge(integrate_df, 60)
#####yyr
#####
##########
##