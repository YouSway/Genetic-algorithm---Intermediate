#資料設定
N <- 8 #隊伍總數
N_count <- 2 #比賽球場總數
N_day <- 7 #比賽天數
Q <- 4 #總時段數

#限制要求懲罰比重
RTR <- 100 #每天只能一場比賽

#設定數據名稱
N_name <- c("A","B","C","D","E","F","G","H")
N_count_name <- c("Taipei","Taichung")
Q_time <- c("13:00","14:45","16:30","18:15")


#定義變數，會依照場地數量決定chromosomes與fitness大小，並且chromosomes為日期時段表示，fitnessF為隊伍時段表示
P_count <- 0 #建立比賽數量
pair <- 0 #建立比賽隊伍
#建立染色體模型
time_chromosomes01 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
time_chromosomes02 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
time_chromosomes03 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
time_chromosomes04 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
elite_chromosomes <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
mutation_chromosomes <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
#建立適應值模型
time_fitness01 <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
time_fitness02 <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
time_fitness03 <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
time_fitness04 <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
elite_fitness <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
mutation_fitness <- matrix(c(1:(N*Q*N_count)), nrow = Q, ncol = (N*N_count))
fitness_value <- rep(0, times = 6, each = 1)
#建立選種模型
select_c01 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
select_c02 <- matrix(c(1:(N_day*Q*N_count)), nrow = Q, ncol = (N_day*N_count))
#定義突變機率(1~100)
mutation_rate <- 100



#主程式
main <- function(){
  
  initialization()#初始化
  for(i in c(1:1000)){#執行n次，也可將條件以低於限制條件後數據
    
    fitness()#計算fitness_value
    select_chromosomes()#選種
    elite()#菁英政策
    crossover()#交配
    mutation()#突變

  }
  fitness()#計算最後一次fitness_value(適應值)
  print(fitness_value[5])#輸出這次計算時最好結果的適應值
  print(elite_chromosomes)#輸出這次計算時最好結果的放入順序
  View(elite_chromosomes)
  View(elite_fitness)
}


#初代設定()
initialization <- function(){
  
  #清除變數
  P_count <<- 0
  pair <<- 0
  time_chromosomes01[1:length(time_chromosomes01)] <<- 0
  time_chromosomes02[1:length(time_chromosomes02)] <<- 0
  time_chromosomes03[1:length(time_chromosomes03)] <<- 0
  time_chromosomes04[1:length(time_chromosomes04)] <<- 0
  elite_chromosomes[1:length(elite_chromosomes)] <<- 0
  time_fitness01[1:length(time_fitness01)] <<- 0
  time_fitness02[1:length(time_fitness02)] <<- 0
  time_fitness03[1:length(time_fitness03)] <<- 0
  time_fitness04[1:length(time_fitness04)] <<- 0
  elite_fitness[1:length(elite_fitness)] <<- 0
  select_c01[1:length(select_c01)] <<- 0
  select_c02[1:length(select_c02)] <<- 0
  
  #判斷總場數
  P_count <<- 0
  for(i in c(1:(N-1))){
    for(j in c(i:(N-1))){
      P_count <<- P_count+1
    }
  }
  
  #建立隊伍配對模型
  pair <<- rep(0, times = P_count, each = 1) 
  a <- 0
  
  #此迴圈建立單循環賽表，並將組何放入pair
  for(i in c(1:(N-1))){ 
    for(j in c((i+1):N)){
      a <- a+1
      pair[a] <<- paste(i, j, sep = ",")
    }
  }
  
  #依照比賽場地數量來分配每個場地需對戰場數
  a <- rep(floor(P_count/N_count), times = N_count, each = 1)
  if(P_count-sum(a)!=0){
    for(i in c(1:(P_count-sum(a)))){
      a[i] <- a[i]+1
    }
  }
  
  #將比賽組何打亂後依造場地數量方配隨機放入chromosomes
  r1 <- sample(P_count,P_count,replace = FALSE)
  r2 <- sample(P_count,P_count,replace = FALSE)
  r3 <- sample(P_count,P_count,replace = FALSE)
  r4 <- sample(P_count,P_count,replace = FALSE)
  r5 <- sample(P_count,P_count,replace = FALSE)
  tem <- 0
  tem2 <- 0
  for(i in c(1:N_count)){
    a1 <- sample(a[i],a[i],replace = FALSE)
    a2 <- sample(P_count,a[i],replace = FALSE)
    a1_2 <- sample(a[i],a[i],replace = FALSE)
    a2_2 <- sample(P_count,a[i],replace = FALSE)
    a1_3 <- sample(a[i],a[i],replace = FALSE)
    a2_3 <- sample(P_count,a[i],replace = FALSE)
    a1_4 <- sample(a[i],a[i],replace = FALSE)
    a2_4 <- sample(P_count,a[i],replace = FALSE)
    a1_5 <- sample(a[i],a[i],replace = FALSE)
    a2_5 <- sample(P_count,a[i],replace = FALSE)
    
    for(j in c(1:a[i])){
      time_chromosomes01[a2[a1[j]]+tem2] <<- pair[r1[a1[j]+tem]]
      time_chromosomes02[a2_2[a1_2[j]]+tem2] <<- pair[r2[a1_2[j]+tem]]
      time_chromosomes03[a2_3[a1_3[j]]+tem2] <<- pair[r3[a1_3[j]+tem]]
      time_chromosomes04[a2_4[a1_4[j]]+tem2] <<- pair[r4[a1_4[j]+tem]]
      elite_chromosomes[a2_5[a1_5[j]]+tem2] <<- pair[r5[a1_5[j]+tem]]
    }
    tem <- tem+a[i]
    tem2 <-tem2+P_count
  }
  mutation_chromosomes <<- time_chromosomes01
  
  
}


#計算適應值
fitness <- function(){
  
  
  #將chromosomes轉化為fitness
  s <- 0
  for(i in c(1:N_count)){
    for(j in c(1:(N*Q))){
      
      s <- j%%Q #確認時段
      if(s==0){s <- Q}
      g <- 1+(N_day*(i-1))
      time_fitness01[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),time_chromosomes01[s,c(g:(N_day*i))]))[2]
      time_fitness02[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),time_chromosomes02[s,c(g:(N_day*i))]))[2]
      time_fitness03[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),time_chromosomes03[s,c(g:(N_day*i))]))[2]
      time_fitness04[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),time_chromosomes04[s,c(g:(N_day*i))]))[2]
      elite_fitness[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),elite_chromosomes[s,c(g:(N_day*i))]))[2]
      mutation_fitness[j+((i-1)*Q*N)] <<- table(grepl(ceiling(j/Q),mutation_chromosomes[s,c(g:(N_day*i))]))[2]
      
    }
    
  }
  
  #缺失值處理，如果NA補0
  for(i in c(1:(N*Q*N_count))){
    if(is.na(time_fitness01[i])){
      time_fitness01[i] <<- 0
    }
  }
  for(i in c(1:(N*Q*N_count))){
    if(is.na(time_fitness02[i])){
      time_fitness02[i] <<- 0
    }
  }
  for(i in c(1:(N*Q*N_count))){
    if(is.na(time_fitness03[i])){
      time_fitness03[i] <<- 0
    }
  }
  for(i in c(1:(N*Q*N_count))){
    if(is.na(time_fitness04[i])){
      time_fitness04[i] <<- 0
    }
  }
  for(i in c(1:(N*Q*N_count))){
    if(is.na(elite_fitness[i])){
      elite_fitness[i] <<- 0
    }
  }
  for(i in c(1:(N*Q*N_count))){
    if(is.na(mutation_fitness[i])){
      mutation_fitness[i] <<- 0
    }
  }
  
  
  
  #懲罰判斷
  
  #針對同一天打兩次比賽做判斷，若同天有同隊比賽超過一次，給與設定值懲罰
  P_1 <- rep(1, times = 6, each = 1)
  for(i in c(1:(N_day*N_count))){
    for(j in c(1:N)){
      tmp <- table(grepl(j,time_chromosomes01[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[1] <- P_1[1]+RTR
      }
      tmp <- table(grepl(j,time_chromosomes02[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[2] <- P_1[2]+RTR
      }
      tmp <- table(grepl(j,time_chromosomes03[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[3] <- P_1[3]+RTR
      }
      tmp <- table(grepl(j,time_chromosomes04[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[4] <- P_1[4]+RTR
      }
      tmp <- table(grepl(j,elite_chromosomes[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[5] <- P_1[5]+RTR
      }
      tmp <- table(grepl(j,mutation_chromosomes[,i]))[2]
      if(is.na(tmp)){
        tmp <- 0
      }
      if(tmp>1){
        P_1[6] <- P_1[6]+RTR
      }
    }
  }
  
  #適應值計算，共4個染色體[(最大時段-最小時段)*懲罰]
  fitness_value[1] <<- (time_fitness01[order(time_fitness01,decreasing=TRUE)[1:(N*Q*N_count)][1]]-time_fitness01[order(time_fitness01,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[1]
  fitness_value[2] <<- (time_fitness02[order(time_fitness02,decreasing=TRUE)[1:(N*Q*N_count)][1]]-time_fitness02[order(time_fitness02,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[2]
  fitness_value[3] <<- (time_fitness03[order(time_fitness03,decreasing=TRUE)[1:(N*Q*N_count)][1]]-time_fitness03[order(time_fitness03,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[3]
  fitness_value[4] <<- (time_fitness04[order(time_fitness04,decreasing=TRUE)[1:(N*Q*N_count)][1]]-time_fitness04[order(time_fitness04,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[4]
  fitness_value[5] <<- (elite_fitness[order(elite_fitness,decreasing=TRUE)[1:(N*Q*N_count)][1]]-elite_fitness[order(elite_fitness,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[5]
  fitness_value[6] <<- (mutation_fitness[order(mutation_fitness,decreasing=TRUE)[1:(N*Q*N_count)][1]]-mutation_fitness[order(mutation_fitness,decreasing=TRUE)[1:(N*Q*N_count)][(N*Q*N_count)]])*P_1[6]
  
}


#選種(可變動，目前先依適應值最理想的兩個染色體做選擇)
select_chromosomes <- function(){
  
  r <- order(fitness_value[c(1:5)])
  
  if(r[1]==1){
    select_c01 <<- time_chromosomes01
  }else if(r[1]==2){
    select_c01 <<- time_chromosomes02
  }else if(r[1]==3){
    select_c01 <<- time_chromosomes03
  }else if(r[1]==4){
    select_c01 <<- time_chromosomes04
  }else if(r[1]==5){
    select_c01 <<- elite_chromosomes
  }
  
  if(r[2]==1){
    select_c02 <<- time_chromosomes01
  }else if(r[2]==2){
    select_c02 <<- time_chromosomes02
  }else if(r[2]==3){
    select_c02 <<- time_chromosomes03
  }else if(r[2]==4){
    select_c02 <<- time_chromosomes04
  }else if(r[2]==5){
    select_c02 <<- elite_chromosomes
  }
  
}


#交配(使用雙點交配，交配兩次)
crossover <- function(){
  
  #選出兩個染色體須交配的比賽隊伍，此選擇為亂數不重複，且須有比賽隊伍，並不能一樣
  r1 <-  sample(length(select_c01),2,replace = FALSE)
  r2 <-  sample(length(select_c02),2,replace = FALSE)
  while (select_c01[r1[1]]=="0" || select_c02[r1[2]]=="0" || select_c01[r1[1]]==select_c02[r1[2]]){
    r1 <-  sample(length(select_c01),2,replace = FALSE)
  }
  while (select_c01[r2[1]]=="0" || select_c02[r2[2]]=="0" || select_c01[r2[1]]==select_c02[r2[2]]){
    r2 <- sample(length(select_c02),2,replace = FALSE)
  }
  
  
  #開始交配
  #第一次交配(產生兩個後代)
  t1 <- rep(0, times = 2, each = 1)
  t1[1] <- grep(select_c02[r1[2]],select_c01)
  t1[2] <- grep(select_c01[r1[1]],select_c02)
  time_chromosomes01 <<- select_c01
  time_chromosomes02 <<- select_c02
  time_chromosomes01[r1[1]] <<- select_c01[t1[1]]
  time_chromosomes01[t1[1]] <<- select_c01[r1[1]]
  time_chromosomes02[r1[2]] <<- select_c02[t1[2]]
  time_chromosomes02[t1[2]] <<- select_c02[r1[2]]
  
  #第二次交配(產生兩個後代)
  t2 <- rep(0, times = 2, each = 1)
  t2[1] <- grep(select_c02[r2[2]],select_c01)
  t2[2] <- grep(select_c01[r2[1]],select_c02)
  time_chromosomes03 <<- select_c01
  time_chromosomes04 <<- select_c02
  time_chromosomes03[r2[1]] <<- select_c01[t2[1]]
  time_chromosomes03[t2[1]] <<- select_c01[r2[1]]
  time_chromosomes04[r2[2]] <<- select_c02[t2[2]]
  time_chromosomes04[t2[2]] <<- select_c02[r2[2]]
  
  fitness()
  
}


#突變(隨機選擇一個數字，再依機率選擇要不要突變)
mutation <- function(){
  
  #突變機率
  r <- rep(0, times = 4, each = 1)
  for(i in c(1:4)){
    r[i] <- sample(100,1,replace = FALSE)
  }
  
  
  for(i in c(1:4)){

    
    if(r[i]<=mutation_rate){
      
      #突變第一個染色體
      if(i==1){
        
        #暫存未突變前染色體
        mutation_chromosomes <<- time_chromosomes01
        #可以判斷哪個時段哪個隊伍參賽次數過多，若有多組隊伍同樣則亂數決定
        tmp_1 <- grep(time_fitness01[order(time_fitness01)[N*Q*N_count]],time_fitness01)[sample(length(grep(time_fitness01[order(time_fitness01)[N*Q*N_count]],time_fitness01)),1)]
        #確認隊伍
        team_1 <- ceiling(tmp_1/Q)-(N*floor(floor(tmp_1/Q)/N))
        #確認時段
        time_1 <- tmp_1%%Q
        if(time_1==0){time_1 <- 4}
        #確認場地
        site_1 <- ceiling(tmp_1/(N*Q))
        
        #突變對象天數
        mutation_day_1 <- grep(team_1,time_chromosomes01[seq(time_1, Q*N_day*site_1, Q)])[sample(length(grep(team_1,time_chromosomes01[seq(time_1, Q*N_day*site_1, Q)])),1)]
        #突變對象位置
        mutation_location_1 <- (Q*(mutation_day_1-1))+time_1
        #突變交換位置
        mr <- sample(Q,1)
        while(mr==time_1){mr <- sample(Q,1)}
        mutation_switch_1 <- (Q*(mutation_day_1-1))+mr
        #突變
        tem <- mutation_chromosomes[mutation_location_1]
        mutation_chromosomes[mutation_location_1] <<- mutation_chromosomes[mutation_switch_1]
        mutation_chromosomes[mutation_switch_1] <<- tem
        fitness()
        if(fitness_value[6] <= fitness_value[1]){time_chromosomes01 <<- mutation_chromosomes}
        
      }
      
      #突變第二個染色體
      if(i==2){
        
        #暫存未突變前染色體
        mutation_chromosomes <<- time_chromosomes02
        #可以判斷哪個時段哪個隊伍參賽次數過多，若有多組隊伍同樣則亂數決定
        tmp_1 <- grep(time_fitness02[order(time_fitness02)[N*Q*N_count]],time_fitness02)[sample(length(grep(time_fitness02[order(time_fitness02)[N*Q*N_count]],time_fitness02)),1)]
        #確認隊伍
        team_1 <- ceiling(tmp_1/Q)-(N*floor(floor(tmp_1/Q)/N))
        #確認時段
        time_1 <- tmp_1%%Q
        if(time_1==0){time_1 <- 4}
        #確認場地
        site_1 <- ceiling(tmp_1/(N*Q))
        
        #突變對象天數
        mutation_day_1 <- grep(team_1,time_chromosomes02[seq(time_1, Q*N_day*site_1, Q)])[sample(length(grep(team_1,time_chromosomes02[seq(time_1, Q*N_day*site_1, Q)])),1)]
        #突變對象位置
        mutation_location_1 <- (Q*(mutation_day_1-1))+time_1
        #突變交換位置
        mr <- sample(Q,1)
        while(mr==time_1){mr <- sample(Q,1)}
        mutation_switch_1 <- (Q*(mutation_day_1-1))+mr
        #突變
        tem <- mutation_chromosomes[mutation_location_1]
        mutation_chromosomes[mutation_location_1] <<- mutation_chromosomes[mutation_switch_1]
        mutation_chromosomes[mutation_switch_1] <<- tem
        fitness()
        if(fitness_value[6] <= fitness_value[2]){time_chromosomes02 <<- mutation_chromosomes}
        
      }
      
      #突變第三個染色體
      if(i==3){
        
        #暫存未突變前染色體
        mutation_chromosomes <<- time_chromosomes03
        #可以判斷哪個時段哪個隊伍參賽次數過多，若有多組隊伍同樣則亂數決定
        tmp_1 <- grep(time_fitness03[order(time_fitness03)[N*Q*N_count]],time_fitness03)[sample(length(grep(time_fitness03[order(time_fitness03)[N*Q*N_count]],time_fitness03)),1)]
        #確認隊伍
        team_1 <- ceiling(tmp_1/Q)-(N*floor(floor(tmp_1/Q)/N))
        #確認時段
        time_1 <- tmp_1%%Q
        if(time_1==0){time_1 <- 4}
        #確認場地
        site_1 <- ceiling(tmp_1/(N*Q))
        
        #突變對象天數
        mutation_day_1 <- grep(team_1,time_chromosomes03[seq(time_1, Q*N_day*site_1, Q)])[sample(length(grep(team_1,time_chromosomes03[seq(time_1, Q*N_day*site_1, Q)])),1)]
        #突變對象位置
        mutation_location_1 <- (Q*(mutation_day_1-1))+time_1
        #突變交換位置
        mr <- sample(Q,1)
        while(mr==time_1){mr <- sample(Q,1)}
        mutation_switch_1 <- (Q*(mutation_day_1-1))+mr
        #突變
        tem <- mutation_chromosomes[mutation_location_1]
        mutation_chromosomes[mutation_location_1] <<- mutation_chromosomes[mutation_switch_1]
        mutation_chromosomes[mutation_switch_1] <<- tem
        fitness()
        if(fitness_value[6] <= fitness_value[3]){time_chromosomes03 <<- mutation_chromosomes}
        
      }
      
      #突變第四個染色體
      if(i==4){
        
        #暫存未突變前染色體
        mutation_chromosomes <<- time_chromosomes04
        #可以判斷哪個時段哪個隊伍參賽次數過多，若有多組隊伍同樣則亂數決定
        tmp_1 <- grep(time_fitness04[order(time_fitness04)[N*Q*N_count]],time_fitness04)[sample(length(grep(time_fitness04[order(time_fitness04)[N*Q*N_count]],time_fitness04)),1)]
        #確認隊伍
        team_1 <- ceiling(tmp_1/Q)-(N*floor(floor(tmp_1/Q)/N))
        #確認時段
        time_1 <- tmp_1%%Q
        if(time_1==0){time_1 <- 4}
        #確認場地
        site_1 <- ceiling(tmp_1/(N*Q))
        
        #突變對象天數
        mutation_day_1 <- grep(team_1,time_chromosomes04[seq(time_1, Q*N_day*site_1, Q)])[sample(length(grep(team_1,time_chromosomes04[seq(time_1, Q*N_day*site_1, Q)])),1)]
        #突變對象位置
        mutation_location_1 <- (Q*(mutation_day_1-1))+time_1
        #突變交換位置
        mr <- sample(Q,1)
        while(mr==time_1){mr <- sample(Q,1)}
        mutation_switch_1 <- (Q*(mutation_day_1-1))+mr
        #突變
        tem <- mutation_chromosomes[mutation_location_1]
        mutation_chromosomes[mutation_location_1] <<- mutation_chromosomes[mutation_switch_1]
        mutation_chromosomes[mutation_switch_1] <<- tem
        sss <<- mutation_chromosomes
        sss1 <<- time_chromosomes04
        fitness()
        if(fitness_value[6] <= fitness_value[4]){time_chromosomes04 <<- mutation_chromosomes}
        
      }
      
      
    }
  }
  fitness()
  
  
}



#菁英政策
elite <- function(){
  #可在變動，目前以染色體中最優質一項做保留
  elite_chromosomes<<-select_c01
}