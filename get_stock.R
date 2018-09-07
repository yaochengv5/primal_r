# 此文件提供若干函数用于从wind中获取数据 
# 本文将实现quantmod中，getSymbol函数的功能

# get_macro 用于获取宏观数据 ############################
get_macro <- function(type, from = '2016-1-1', to = now()){
  # type 列示gdp, cpi等宏观指标名称，全部用小写字符串列示；
  # from， to 列示起止日，
  # 函数需要wind接口
  # 函数输出格式为xts
  
  # tpye可选项如下
  # 1. gdp_yoy ：gdp年度增长率
  # 2. gdp_qoq ：gdp季度增长率
  # 3. gdp ： 名义gpd，年度值, 单位为亿元
  
  
  require(WindR)
  require(tidyverse)
  require(lubridate)
  require(xts)
  w.start(showmenu = F)
  
  # 1. gdp_yoy ：gdp年度增长率
  if(type == 'gdp_yoy'){
    # 获取原始数据
    data <- w.edb('M0045788',from,to,'Fill=Previous') $ Data
    
    # 转换为xts格式
    data_xts <- xts(data$CLOSE, order.by  = ymd(data$DATETIME))
    names(data_xts) <- "gdp_yoy"
    
    # 返回数据
    return(data_xts)
  }
  
  # 2. gdp_qoq ：gdp季度增长率
  if(type == 'gdp_qoq'){
    # 获取原始数据
    data <- w.edb('M0039354',from,to,'Fill=Previous') $ Data
    
    # 转换为xts格式
    data_xts <- xts(data$CLOSE, order.by  = ymd(data$DATETIME))
    names(data_xts) <- "gdp_qoq"
    
    # 返回数据
    return(data_xts)
  }
  
  # 3. gdp ： 名义gpd，年度值
  if(type == 'gdp'){
    # 获取原始数据
    data <- w.edb('M0001395',from,to,'Fill=Previous') $ Data
    
    # 转换为xts格式
    data_xts <- xts(data$CLOSE, order.by  = ymd(data$DATETIME))
    names(data_xts) <- "gdp"
    
    # 返回数据
    return(data_xts)
  }
  
  
  # 0. else type输入错误
  else print("请输入正确的tpye类型")
  
}

# get_stock 用于获取股票数据 ###############################
get_stock <- function(code, from = '2016-1-1', to = now()){
  
}
# code2name 用于将股票代码转换为公司名称，例如将“600030”或“600030.sh”转换为中信证券#######
code2name <- function(code){
  
}
# name2code 用于将公司名称转换为wind代码######
name2code <- function(stockname){
  
}