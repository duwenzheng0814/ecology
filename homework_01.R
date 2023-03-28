#第一部分
#安装“ade4”包并加载
install.packages("ade4")
library(ade4)
#安装“tidyverse”包并加载
install.packages("tidyverse")
library(tidyverse)
#从“ade4”中调用“doubs”数据集
data(doubs, package="ade4")
doubs
#查看“doubs”数据结构
str(doubs)
#查看“doubs”数据类型
class(doubs)

#第二部分
#从“doubs”中提取“env”数据
env <- doubs$env
env
#将“env”的行名提取出来并命名为“site”
site <- row.names(env)
#将“site”作为新列
env_df <- cbind(site,env)
env_df
#将数据类型从data frame改为tibble
class(env_df)
env_tb <- as_tibble(env_df)
class(env_tb)
env_tb

#第三部分
#将“env_tb”的“dfs”列大于1000的数据提取出来
env_tb[env_tb$dfs>1000,]
#在env_final里完成：
#保留dfs>1000的数据
env_final <- env_tb[env_tb$dfs>1000,] %>%
  #将site, dfs, slo, flo, pH, nit, oxy列提取出来
  select(site, dfs, slo, flo, pH, nit, oxy)%>%
  #重命名列
  rename(distsour=dfs, slope=slo, flowrate=flo, nitrogen=nit, oxygen=oxy )%>%
  #重新排序
  arrange(slope, desc(pH))
env_final
