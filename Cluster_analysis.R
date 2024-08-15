getwd()
Suicidal_Behaviours  <- read.csv("GHSH_Pooled_Data1.csv")

#1
seredn <- mean(Suicidal_Behaviours$Have_Understanding_Parents)
seredn
median(Suicidal_Behaviours$Have_Understanding_Parents)


#2
var(Suicidal_Behaviours$Have_Understanding_Parents) #дисперсія
stand_vidh <- sd(Suicidal_Behaviours$Have_Understanding_Parents) #стандартне відхилення
stand_vidh
cv <- stand_vidh / seredn * 100 #коефіцієнт варіації
cv
min_ <- min(Suicidal_Behaviours$Have_Understanding_Parents)
max_ <- max(Suicidal_Behaviours$Have_Understanding_Parents)
max_ - min_
IQR(Suicidal_Behaviours$Have_Understanding_Parents) #інтерквальний розмах


#3
boxplot(Suicidal_Behaviours$Have_Understanding_Parents,
        main="Box-and-whiskers plot:Adolescents with Suicidal Behaviours
        and Understanding with their parents   ",ylab="Percents(%)", col="coral2")

#4
quantile(Suicidal_Behaviours$Have_Understanding_Parents)

#5
quantile(Suicidal_Behaviours$Have_Understanding_Parents, prob = c(0.1, 0.9)) 

#6
library(psych)
describe(Suicidal_Behaviours$Have_Understanding_Parents)[11] #коефіцієнт асиметрії
describe(Suicidal_Behaviours$Have_Understanding_Parents)[12] #коефіцієнт ексцесу

#7
hist(Suicidal_Behaviours$Have_Understanding_Parents,
     freq=F, main="Sturges rule", col="darkorchid1")
hist(Suicidal_Behaviours$Have_Understanding_Parents, breaks = "Scott",
     freq=F, main="Scott rule", col = "lightpink1", 
     xlab = "Understanding with parents", ylab = "relative frequency")
hist(Suicidal_Behaviours$Have_Understanding_Parents, breaks = "FD", freq=F, main="FD rule",
     col = "darkseagreen3", xlab = "Understanding with parents", ylab = "relative frequency")
lines(density(Suicidal_Behaviours$Have_Understanding_Parents), col='chocolate2', lwd=5)
plot(density(Suicidal_Behaviours$Have_Understanding_Parents), col='cornflowerblue', lwd=5) #графік щільності

#8
library(car)
qqnorm(Suicidal_Behaviours$Have_Understanding_Parents, pch = 3, frame = FALSE, col="lightblue4")
qqline(Suicidal_Behaviours$Have_Understanding_Parents, col = "lightcoral", lwd = 3)

#9
plot(pnorm(sort(x)), (1:length(x))/length(x),
     xlab="Теоретична функція розподілу", ylab = "Емпірична функція
розподілу", asp = 1)
abline(0, 1, col="red")

plot(pnorm(sort((Suicidal_Behaviours$Have_Understanding_Parents  - mean(Suicidal_Behaviours$Have_Understanding_Parents)) / sd(Suicidal_Behaviours$Have_Understanding_Parents))),
     (1:length(Suicidal_Behaviours$Have_Understanding_Parents)) / length(Suicidal_Behaviours$Have_Understanding_Parents),xlab="Теоретична функція розподілу",ylab="Імпірична функція розподілу",asp=1)
abline(0, 1, col="green2") 

#10
shapiro.test(Suicidal_Behaviours$Have_Understanding_Parents)  
#критерій згоди Колмогорова-Смірнова для неперервних розподілів та критерій нормальності Шапіро-Уілка.