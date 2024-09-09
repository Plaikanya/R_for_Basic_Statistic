

# Do some basic stats
#สุ่มกลุ่มตัวอย่างที่มีขนาด n = 10 และ n=30 กลุ่มละ 500 ครั้งโดยนำ Mean และ SD ของขนาดไข่ของนก
#ในแต่ละครั้งมา Plot Histrograms เพื่อดูความแตกต่างของในการกระจายตัวของข้อมูล


nzb <- read.csv("nzbirds.csv")
sort(nzb$Egg.Length, na.last = TRUE)

# do some stat operations

summary(nzb$Egg.Length)
mean(nzb$Egg.Length, na.rm = TRUE)
range(nzb$Egg.Length, na.rm = TRUE)
sort(nzb$Egg.Length)
table(nzb$Egg.Length)
sort(table(nzb$Egg.Length))
boxplot(nzb$Egg.Length)
hist(nzb$Egg.Length)

# remove outliers
?quantile   # get help about using "quantile" function

quantile(nzb$Egg.Length,probs = c(.25,0.75), na.rm = TRUE) -> qvalues
qvalues

q1 <- qvalues[1]
q1 <- unname(q1)  # remove names but keep numbers
q3 <- qvalues[2]
q3 <- unname(q3)
iqr <- IQR(nzb$Egg.Length,na.rm = TRUE)

# find fence values (lower and upper)

f1 <- q1 - 1.5*iqr
f3 <- q3 + 1.5*iqr

egg.len <- na.omit(nzb$Egg.Length)
summary(egg.len)

# remove outliers, which are outside of the fence values
?which # get help about "which" function

which(egg.len < f1) # find at which indices having outliers
which(egg.len > f3)

new_egg.len <- egg.len[-which(egg.len < f1)] # use - to remove those indices
new_egg.len <- egg.len[-which(egg.len > f3)]

# see some stat summary
summary(new_egg.len)

# plot a histogram to see distribution
boxplot(new_egg.len)
hist(new_egg.len)

# To do:
# Use a "sample" function to randomly select NZ birds' egg lengths with two
# different sample sizes:

# i) n = 10
#    E.g., sp10 <- sample(new_egg.len, 10, replace = FALSE)
# ii) n = 30
#    E.g., sp30 <- sample(new_egg.len, 30, replace = FALSE)

# For each sample size, repeat the sampling 500 times (i.e, use for-looping).
# In each round, calculate mean and standard deviation (SD) for each sample.
# Keep all the 500 means and SD of each sample size.
# Create 4 histograms, think about them and compare.
# Provide some explanations about them.

# ----------- End ----------------------


#เก็บค่าMeanและ sd ของนกในการ random 10 Number จำนวน500 times
means <- numeric(500)
sds <- numeric(500)
for (i in (1:500)) { 
  sp <- sample(egg.len,10 , replace = FALSE)
  means[i] <- round(mean(sp),2)
  #print(mean(sp))
  sds[i] <- round(sd(sp),2)
  #print(sd(sp))
}

hist(means , main = 'Mean to randomly select NZ birds (N = 10)')
hist(sds , , main = 'SD to randomly select NZ birds (N = 10)')

#เก็บค่าMeanและ sd ของนกในการ random 30 Number จำนวน500 time
means_2 <- numeric(500)
sds_2 <- numeric(500)
for (i in (1:500)) { 
  sp_2 <- sample(egg.len,30 , replace = FALSE)
  means_2[i] <- round(mean(sp_2),2)
  print(mean(sp_2))
  sds_2[i] <- round(sd(sp_2),2)
  print(sd(sp_2))
}

hist(means_2 , main = 'Mean to randomly select NZ birds (N = 30)')
hist(sds_2 , , main = 'SD to randomly select NZ birds (N = 30)')

