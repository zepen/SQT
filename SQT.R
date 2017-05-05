#-------------------------------------------------------------------------------------------------------------
cat("Name: 单支股票模拟交易\n")
cat("version: 1.0\n")
cat("author: Flycat\n") 
cat("Time: 2017-03-01\n")
cat("=====================================================================================================\n")
cat("|---------------------------------------------------------------------------------------------------\n")
cat("|                      ======= ==    ==    ==   =====     ==   ========                            \n")
cat("|                      ||      ||    ||    ||  =         //\\\\     ||                               \n")
cat("|                      ||===   ||     \\\\  //  =         //  \\\\    ||                               \n")
cat("|                      ||      ||       ||    =        //====\\\\   ||                               \n")
cat("|                      ||      ======|  ||     ======|//      \\\\  ||                               \n")
cat("|----------------------------------------------------------------------------------------------------\n")
cat("=====================================================================================================\n")

#----------------------加载程序包---------------------------------------------------------------------
if(!any("readxl" == installed.packages()[, 1])){install.packages("readxl");cat("正在加载程序包readxl...\n")
  
  library(readxl);cat("程序包readxl加载完成!\n")

  } else {library(readxl)}

if(!any("ggplot2" == installed.packages()[, 1])){install.packages("ggplot2");cat("正在加载程序包ggplot2...\n")
  
  library(ggplot2);cat("程序包ggplot2加载完成!\n")

  } else { library(ggplot2)}
#-----------------------------------------------------------------------------------------------------
options(scipen = 20) #取消科学计数法

show_fun <- function(mytext = mytext)
{
  cat("=====================================================================================================\n")
  cat("\t", mytext, "\n")
  cat("=====================================================================================================\n")
}

cat("---------必须程序包已加载---------\n")

show_fun(mytext = "Welcome to use my R progrom for trade!")

#--------------------数据读取-------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
source_data <- read_excel("data/data.xlsx") # 数据源

show_fun(mytext = "数据源已经成功加载到内存当中")

attributes(source_data)$class <- "data.frame" # 去掉数据源当中其他属性,只保留data.frame属性

source_data <- source_data[!duplicated(source_data$Time), ] #去掉重复时间
#-----------------------------------------------------------------------------------------------------
data_dim <- dim(source_data) # 获取数据维度

source_data[, 1] <- as.Date(source_data$Time) # 将时间转换为Date格式

source_time <- source_data[, 1] # 获取交易时间

data_names <- names(source_data) # 获取数据源列名
#--------------------数据查看-------------------------------------------------------------------------
cat("数据源为:", data_dim[1], "行,", data_dim[2], "列\n")

cat("是否查看数据源?(Y/N)(回车完成输入)\n")

while (TRUE)
{
  seedata <- scan(what = character(), nlines = 1L, quiet = TRUE)
  
  if (!length(seedata)){
    
    message("警告:非法输入,必须输入一个参数!\n")
    
    next
  
    } else {
    
      if(seedata == "Y") {
      
        View(source_data);break
    
        } else if(seedata == "N") {
          
          break 
    
        } else {
      
          message("警告:输入错误,请重新输入!\n")
          
          next
     }
  }
}
#--------------------创建交易策略函数-----------------------------------------------------------------
my_way <- function(buycondition = buycondition , sellcondition = sellcondition)
{
  attach(source_data)
  
  data_nrow1 <- nrow(source_data[which(buycondition), ]) # 判断在买入策略条件下数据源是否为空
  
  data_nrow2 <- nrow(source_data[which(sellcondition), ]) # 判断在卖出策略条件下数据源是否为空

  if (data_nrow1 != 0 && data_nrow2 != 0){
    
    source_data[which(buycondition), data_dim[2] + 1] <<- 1  # 标记买入信号
    
    source_data[which(sellcondition), data_dim[2] + 1] <<- 0 # 标记卖出信号
    
    data_dim <<- dim(source_data) # 重新获取标记买入卖出信号后的数据源维度
    
    show_fun(mytext = "交易策略创建完成!")
    
    detach(source_data)
  
  } else if(data_nrow1 == 0) {
    
    message("警告:数据源不满足你的买入策略,买入交易无法进行!\n")
    
    detach(source_data)
  
  } else if(data_nrow2 == 0) {
    
    message("警告:数据源不满足你的卖出策略,卖出交易无法进行!\n")
    
    detach(source_data)
  
  } else {
    
    detach(source_data)
  }
  
}
#-------------------创建建仓函数----------------------------------------------------------------------
my_trade <- function(cash = cash, firstday = firstday){
  
  #初始化交易明细
  trade_data <- data.frame(Time = source_time, N = rep(0, data_dim[1]), Closeprise = source_data[, which("Close" == data_names)], 
                           
                           Mv = rep(0, data_dim[1]), cash = rep(0, data_dim[1]), Ta = rep(0, data_dim[1]), point = source_data[, data_dim[2]], 
                           
                           status = rep("无交易", data_dim[1]), stringsAsFactors = FALSE)
  # Time ------ 时间 
  # N ------ 持有股数 
  # Mv ------ 市值 
  # Ta ------ 总资产
  # point ------- 买卖信号 1:买入 0:卖出 NA:持有
  # status ------  买卖状态
  
  trade_data[which(trade_data$Time == firstday), which("Ta" == names(trade_data))] <- cash
  
  with(data = trade_data, trade_data[which(Time >= firstday), ])
}

#-----------------------------------------------------------------------------------------------------
#-------------------买入函数--------------------------------
buyfun <- function(tradedate = tradedate){
  
  Closeprice <- source_data[which(source_time == tradedate), which("Close" == data_names)] # 获取该天的收盘价

  if (tradedate == first_buy){
    
    Ta <- my_tradelist[which(my_tradelist$Time == tradedate), which("Ta" == data_names2)] # 获取明细表当中总资产
    
    N <- my_tradelist[which(my_tradelist$Time == tradedate), which("N" == data_names2)] <<- trunc((Ta * 0.3 / Closeprice) /100) * 100 # 获取最大股数
    
    Mv <- my_tradelist[which(my_tradelist$Time == tradedate), which("Mv" == data_names2)] <<- N * Closeprice # 买入股票后的市值
    
    my_tradelist[which(my_tradelist$Time == tradedate), which("cash" == data_names2)] <<- Ta - Mv # 剩余现金
    
    my_tradelist[which(my_tradelist$Time == tradedate), which("status" == data_names2)] <<- "成功买入"

  } else {
    
    Ta <- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("Ta" == data_names2)] # 获取上次明细表当中总资产
    
    ncash <- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("cash" == data_names2)] # 获取前一天的现金

    if(trunc((Ta * 0.3 / Closeprice) /100) >= 1 & ncash >= Ta * 0.3) # 判断现金是否可买入最低100股, 同时保证现金大于等于总资产的0.3
    {
      
      #清算前一日的股数, 市值, 总资产
      
      N <- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("N" == data_names2)] # 上次买入的股数
      
      Mv <- N * Closeprice #重新计算当日的市值
      
      Ta <- ncash + Mv #重新计算当日的总资产
      
      #进行再次买入操作
      
      NN <- trunc((Ta * 0.3 / Closeprice) /100) * 100 #新买入的股数
        
      my_tradelist[which(my_tradelist$Time == tradedate), which("N" == data_names2)] <<- N + NN  # 新买入的股数于上次买入股数叠加
      
      NMv <- my_tradelist[which(my_tradelist$Time == tradedate), which("Mv" == data_names2)] <<- NN * Closeprice + Mv # 买入股票后的市值于前一日市值叠加
      
      cash <- my_tradelist[which(my_tradelist$Time == tradedate), which("cash" == data_names2)] <<- Ta - NMv # 剩余现金
      
      my_tradelist[which(my_tradelist$Time == tradedate), which("Ta" == data_names2)] <<- Ta #当日总资产
      
      my_tradelist[which(my_tradelist$Time == tradedate), which("status" == data_names2)] <<- "成功买入"
    
    } else {
       
       # 现金不足总资产的0.3, 无法再买入
      
       my_tradelist[which(my_tradelist$Time == tradedate), which("status" == data_names2)] <<- "无法买入"
       
       #清算当日的股数, 市值, 总资产
       
       N <- my_tradelist[which(my_tradelist$Time == tradedate), which("N" == data_names2)] <<- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("N" == data_names2)] # 上次买入的股数
       
       Mv <- my_tradelist[which(my_tradelist$Time == tradedate), which("Mv" == data_names2)] <<- N * Closeprice  # 计算当日的市值
       
       cash <- my_tradelist[which(my_tradelist$Time == tradedate), which("cash" == data_names2)] <<- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("cash" == data_names2)] # 获取前一日现金并赋值给当日
       
       my_tradelist[which(my_tradelist$Time == tradedate), which("Ta" == data_names2)] <<-  cash + Mv # 计算当日的总资产
       
      }
  }
  
}
#-------------------卖出函数--------------------------------
sellfun <- function(tradedate = tradedate){
  
  Closeprice <- source_data[which(source_time == tradedate), which("Close" == data_names)] # 计算当日收盘价
  
  N <- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("N" == data_names2)] # 获取前一日持有股数
  
  # 当持有股数不为空时
  
  if (N != 0){
    
    cash <- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("cash" == data_names2)] # 获取前一日现金
  
    ncash <- my_tradelist[which(my_tradelist$Time == tradedate), which("cash" == data_names2)] <<- Closeprice * N + cash  # 按当日收盘价卖出全部持有股数并获得现金
  
    # 重新计算股数, 市值, 总资产
  
    N <- my_tradelist[which(my_tradelist$Time == tradedate), which("N" == data_names2)] <<- 0 # 股数全数卖出
  
    Mv <- my_tradelist[which(my_tradelist$Time == tradedate), which("Mv" == data_names2)] <<- 0 # 市值为零

    my_tradelist[which(my_tradelist$Time == tradedate), which("Ta" == data_names2)] <<- ncash # 此时总资产等于现金
  
    my_tradelist[which(my_tradelist$Time == tradedate), which("status" == data_names2)] <<- "成功卖出"
  
  # 当股数为空, 保留现金与总资产
    
  } else {
   
    my_tradelist[which(my_tradelist$Time == tradedate), which("N" == data_names2)] <<- 0 # 股数全数卖出
    
    my_tradelist[which(my_tradelist$Time == tradedate), which("Mv" == data_names2)] <<- 0 # 市值为零
    
    cash <- my_tradelist[which(my_tradelist$Time == tradedate), which("cash" == data_names2)] <<- my_tradelist[which(my_tradelist$Time == tradedate) - 1, which("cash" == data_names2)] # 获取奇前一日现金赋值给当日
    
    my_tradelist[which(my_tradelist$Time == tradedate), which("Ta" == data_names2)] <<- cash # 此时总资产等于现金
    
    my_tradelist[which(my_tradelist$Time == tradedate), which("status" == data_names2)] <<- "无法卖出"
    
 }
  
}
#-------------------交易函数----------------------------------------------------------------------------
tradefun <- function(){
  
  trade_date <- source_data[which(source_time >= first_buy), 1] 
  
  cat("===========模拟交易开始=================================\n")
  for (i in seq_along(trade_date))
  {
    signal <- source_data[which(source_time == trade_date[i]), data_dim[2]] #买入信号判断
    
    # 当满足买入信号时
    
    if (signal == 1 & !is.na(signal)) {
      
      buyfun(tradedate = trade_date[i]) # 调用买入函数

    # 当满足卖出信号时
      
    } else if (signal == 0 & !is.na(signal)) {
      
      sellfun(tradedate = trade_date[i]) # 调用卖出函数
      
    # 持仓状态, 持有股数不变 
      
    } else {
      
      Closeprice <- source_data[which(source_time == trade_date[i]), which("Close" == data_names)] # 获取该天收盘价
     
      CN <- my_tradelist[which(my_tradelist$Time == trade_date[i]), which("N" == data_names2)] <<- my_tradelist[which(my_tradelist$Time == trade_date[i]) - 1, which("N" == data_names2)] # 获取前一日股数赋值给当日
      
      CMv <- my_tradelist[which(my_tradelist$Time == trade_date[i]), which("Mv" == data_names2)] <<- CN * Closeprice # 计算当日市值
      
      ccash <- my_tradelist[which(my_tradelist$Time == trade_date[i]), which("cash" == data_names2)] <<- my_tradelist[which(my_tradelist$Time == trade_date[i]) - 1, which("cash" == data_names2)] # 获取前一现金赋值给当日
      
      my_tradelist[which(my_tradelist$Time == trade_date[i]), which("Ta" == data_names2)] <<- ccash + CMv # 清算当日总资产
      
    }
  }
  
  my_tradelist # 最终返回交易明细表
  
}

#-------------------存储结果函数--------------------------------------------------------------------------

save_result <- function(){
  
  cat("是否要保存交易明细至于我的文件当中?(Y/N)\n")
  
  while(TRUE){
    
    ifsave <- scan(what = character(), nlines = 1L, quiet = TRUE)
    
    if(!length(ifsave)) {
      
      message("警告:非法输入,必须输入一个参数!\n")
      
      next
      
    } else {
      
      if (ifsave == "Y"){
        
        while(TRUE){
          
          cat("请输入你要存放的磁盘: (eg: D)\n")
          
          myhard <- scan(what = character(), nlines = 1L, quiet = TRUE)
          
          ifelse(length(myhard), {break},{message("警告:非法输入,必须输入一个参数!\n");next})
        }
        
        while(TRUE){
          
          cat("请输入你的文件名: (eg: mydata)\n")
          
          myfile <- scan(what = character(), nlines = 1L, quiet = TRUE)
          
          ifelse(length(myfile), {break},{message("警告:非法输入,必须输入一个参数!\n");next})
        }
    
        path <- paste0(myhard, ":/", myfile, ".csv")
    
        write.csv(my_tradelist, file = path, row.names = FALSE)
    
        show_fun(mytext = paste0("交易明细表已经存入,请在磁盘", myhard, "中查找!"))
    
        show_fun(mytext = "Thank you for using")
    
        break
    
      } else if (ifsave == "N") {
        
        show_fun(mytext = "Thank you for using")
    
        break
    
      } else {
        
        message("警告:输入错误,请重新输入!\n")
      
        next
      }
  }
 }
}

#-----------------绘图函数-----------------------------------
tradeplot <- function(){
  
  plot.new() #开启绘图版面
  
  # 资产金随着时间变化曲线
  
  g <- ggplot(data = NULL) + labs(title = "总资产随时间变化曲线", x = "时间/日",  y = "总资产") +
  
  theme(panel.background = element_rect(fill = "seashell"), plot.title = element_text(colour = "gold4", size = 12), 
        
        axis.title.x = element_text(colour = "blue4", size = 10), axis.title.y = element_text(colour = "gold3", size = 10),
        
        axis.text.x = element_text(colour = "blue", size = 10), axis.text.y = element_text(colour = "gold3", size = 10)) +
    
  geom_line(data = my_tradelist, mapping = aes(x = Time, y = Ta), col = "dodgerblue3", lwd = 0.8, alpha = 0.7) + 
    
  geom_point(data = my_tradelist[which(my_tradelist$point == 1), ],
             
             mapping = aes(x = my_tradelist[which(my_tradelist$point == 1), "Time"], y = my_tradelist[which(my_tradelist$point == 1), "Ta"], 
                           
                           colour = factor(1)), col = "red", cex = 2, alpha = 0.7) +
    
  geom_point(data = my_tradelist[which(my_tradelist$point == 0), ],
    
             mapping = aes(x = my_tradelist[which(my_tradelist$point == 0), "Time"], y = my_tradelist[which(my_tradelist$point == 0), "Ta"]), 
               
             col = "green", cex = 2, shape = 3, alpha = 0.5) +
  
  geom_abline(mapping = aes(intercept = as.numeric(setcash), slope = 0), col = "red", lty = 2) +  # 初始资金线
    
  geom_text(mapping = aes(x = my_tradelist[1, "Time"], y = my_tradelist[1, "Ta"], 
                          
                          label = paste0("首次买入时间:\n", my_tradelist[1, "Time"])), size = 2.7, col = "blue")
  
  print(g) # 打印图像

}
#=======================================================================================================
#-------------------开始模拟交易----------------------------------------------------------------------
 
first_buy <- NULL # 初始化买入时间
 
my_tradelist <- NULL # 初始化交易明细表

data_names2 <- NULL # 初始化交易明细表列名向量

#-------------------创建策略--------------------------------------------------------------------------

show_fun(mytext = "是否创建新策略?(Y/N)(Y创建新策略,N启用默认策略)")

while(TRUE){
  
  setway <- scan(what = character(), nlines = 1L, quiet = TRUE)
  
  if (!length(setway)){
    
    message("警告:非法输入,必须输入一个参数!\n")
    
    next
    
  } else {
    
    if (setway == "Y"){
      
      while(TRUE){
        
        cat("请输入你的买入条件: \n")
        
        mybuycond <- scan(what = character(), nlines = 1L, quiet = TRUE)
        
        if (!length(mybuycond)){
          
          message("警告:非法输入,必须输入一个参数!\n")
          
          next
          
        } else {
          
          mybuycond <- with(data = source_data, eval(parse(text = mybuycond))) # 解析买入条件
          
          ifelse(is.logical(mybuycond[1]) & !is.function(mybuycond), {
            
              show_fun(mytext = "买入条件输入完成!")
            
              break
            
            }, {
              
              message("警告:输入错误,请重新输入!\n")
              
              next
              
            })
        }
     }  
        
     while(TRUE){
       
       cat("\n请输入你的卖出条件: \n")
  
       mysellcond <- scan(what = character(), nlines = 1L, quiet = TRUE)
      
       if (!length(mysellcond)){
         
         message("警告:非法输入,必须输入一个参数!\n")
        
         next
        
       } else {
         
         mysellcond <- with(data = source_data, eval(parse(text = mysellcond))) # 解析卖出条件
         
         ifelse(is.logical(mysellcond[1]) & !is.function(mysellcond), {
           
             show_fun(mytext = "卖出条件输入完成!")
           
             break
           
           }, {
             
             message("警告:输入错误,请重新输入!\n")
             
             next
             
          })
        }
     }
      
     my_way(buycondition = mybuycond, sellcondition = mysellcond) # 使用交易策略,参数buycondition表示买入条件,参数sellcondition表示卖出条件
  
     first_buy <- source_data[which(source_data[, data_dim[2]] == 1), ][1, 1] # 获取首次买入时间
  
     break

   } else if(setway == "N"){
    
      show_fun(mytext = "启用默认策略(买入条件MA30 < MA5,卖出条件Close > MA5)")
  
      my_way(buycondition = MA30 < MA5, sellcondition = Close > MA5)
  
      first_buy <- source_data[which(source_data[, data_dim[2]] == 1), ][1, 1] # 获取首次买入时间
  
      break
  
   } else {
    
      message("警告:输入错误,请重新输入!\n")
  
      next
   }

 }

}
#------------------建仓------------------------------------------------------------------------------

show_fun(mytext = "是否建仓?(Y/N)")

while(TRUE){
  
  ifbulid <- scan(what = character(), nlines = 1L, quiet = TRUE)

  if (ifbulid == "Y"){
    
    while(TRUE){
      
      cat("请输入初始资产金额: \n")
      
      setcash <- scan(what = numeric(), nlines = 1L, quiet = TRUE)
  
      if (setcash >= 1000){
        
        my_tradelist <- my_trade(cash = setcash, firstday = first_buy) # 建立交易明细表
  
        data_names2 <- names(my_tradelist) # 获取交易明细表的列名
  
        show_fun(mytext = "建仓成功!")
      
        my_tradelist <- tradefun() # 最终交易明细表
      
        show_fun("交易已经完成,交易明细已经读入交易明细表当中")
      
        save_result()   #调用存储结果函数
        
        tradeplot() #调用绘图函数
      
        break
      
     } else if (setcash >= 10000000000){
       
       show_fun(mytext = "+_+！这么有钱,给我吧！！！(钱太多,算不过来呀)")
      
       show_fun(mytext = "是否重新建仓？(Y/N)")
      
       rebulid <- scan(what = character(), nlines = 1L, quiet = TRUE)
      
       if (rebulid == "Y"){
         
         next
        
       } else if (rebulid == "N"){
         
         show_fun("O(∩_∩)O~别灰心, 下次多准备些钱来!↖(^ω^)↗")
         
         show_fun(mytext = "Thank you for using")
        
         break
        
       } else {
        
         break
       }
    
    } else {
      
      message("初始资金太低,建仓失败!(没钱就不要玩投资!)\n")
  
      show_fun(mytext = "是否重新建仓？(Y/N)")
      
      rebulid <- scan(what = character(), nlines = 1L, quiet = TRUE)
      
      if (rebulid == "Y"){
        
        next
        
      } else if (rebulid == "N"){
        
        show_fun("O(∩_∩)O~别灰心, 下次多准备些钱来!↖(^ω^)↗")
        
        show_fun(mytext = "Thank you for using")
        
        break
        
      } else {
        
        break
      }
     }
   }
    break

  } else if(ifbulid == "N"){
    
    show_fun(mytext = "不花钱,怎么投资,拜拜( ^_^ )/~~拜拜")
    
    show_fun(mytext = "Thank you for using")
  
    break
   
  } else {
    
    message("警告:输入错误,请重新输入!\n")
  
    next
  
  }
}

#-------------------清理空间对象----------------------------------------------------------------------
remove(list = ls())
#-----------------------------------------------------------------------------------------------------
#  该脚本最终解释权归本人所有, 此脚本用于理论研究, 若经商用需经本人同意
#-----------------------------------------------------------------------------------------------------


















































































































































































































































































