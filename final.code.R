library(rvest)
library(igraph)
library(stringr)
library(data.table)

#Obtaining links to different career webpages
career.cluster = data.frame(cbind(
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed:nth-child(1)") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed+ .report2ed") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".reportrtd") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed a:nth-child(1)") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed a:nth-child(1)") %>% 
    html_attr("href")
))

#Some career pages have nested links
#function to obtain nested links
get.links = function(x){
  pasted.x = paste(x)
  X5 = read_html(pasted.x[5]) %>% 
    html_nodes(".excitem a:nth-child(1)") %>% 
    html_attr("href")
  if(length(X5)>0){
    return(cbind(t(replicate(x[1:4],n = length(X5))),X5))}
  else if(length(X5) == 0)
    return(t(x))
}

complete.links = data.frame(do.call("rbind",apply(X = career.cluster,MARGIN = 1,FUN = get.links)))

complete.links = apply(X = complete.links,2,as.character)

complete.links[,5] = gsub(pattern = "summary",replacement = "details",x = complete.links[,5])

complete.links = data.frame(complete.links)

#complete.links = complete.links[unique(complete.links$X5),]

#complete.links$X5 = as.character(complete.links$X5)

#Function to return RIASEC values by job

riasec = function(x)
{
  numbers = read_html(x) %>%
    html_nodes("#wrapper_Interests .report2a b") %>%
    html_text() %>%
    matrix()
  
  interests = read_html(x) %>%
    html_nodes("#wrapper_Interests .moreinfo b") %>%
    html_text() %>%
    matrix()
  
  wage = read_html(x) %>%
    html_nodes("#wrapper_WagesEmployment tr:nth-child(1) .report2") %>%
    html_text()
  
  title = read_html(x) %>%
    html_nodes(".titleb") %>%
    html_text()  
  
  #wage = str_split(wage,pattern = " ")
  #wage = wage[[1]][3]
  #wage = str_replace_all(wage, "[[:punct:]]", "")
  #wage = as.numeric(gsub("[\\$,]", "", wage))
  
  dat = cbind(interests,numbers)
  dat = dat[order(dat[,1]),]
  dat = rbind(dat,wage,x,title)
  
  row.names(dat) = NULL
  
  if(length(dat) > 6){
    dat[7,1] = paste("income")
    dat[8,1] = paste("X5")
    dat[8,2] = x
    dat[9,1] = paste("title")
    dat[9,2] = title
    rownames(dat) = c("Artistic","Conventional","Enterprising","Investigative","Realistic","Social","income","X5","title")
    return(t(dat[,-1]))}
  else if(length(dat) < 6){
    return(NULL)}
}

riasec.data = do.call("rbind",t(apply(X = matrix(as.character(complete.links$X5)),MARGIN = 1,FUN = riasec)))

riasec.data = data.frame(riasec.data)

final.data = merge(riasec.data[!duplicated(riasec.data$X5),], complete.links[!duplicated(complete.links$X5),], by = "X5")

final.data = cbind(final.data, income = do.call(rbind, str_split(final.data$income, pattern = " "))[,c(3,4)])

final.data = final.data[which(final.data$income.2 == "annual"),]
final.data$income = NULL
final.data$income.2 = NULL
final.data$income.1 = as.numeric(gsub("[\\$,]", "", final.data$income.1))
final.data[,c(2:7)] = apply(X = final.data[,c(2:7)], MARGIN = 2, FUN = function(x){as.numeric(as.character(x))})
final.data = melt(final.data, id.vars = c("X5", "title", "X1", "X2", "X3", "X4", "income.1"))

colnames(final.data) = c("link", "title","career.cluster", "career.pathway", "code","occupation","income","interest","score")

head(final.data)
