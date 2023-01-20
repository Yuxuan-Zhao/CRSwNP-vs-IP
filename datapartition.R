classes = lapply(levels(m[,1]), function(x) which(m[,1]==x))   #list:1-0
train = lapply(classes, function(class) sample(class, 0.7*length(class), replace = F))#无放回随机选择70%做训练
train = unlist(train)            
test = (1:nrow(m))[-train]       
A = m[train,]                    
B = m[test, ]
