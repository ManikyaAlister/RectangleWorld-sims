source("functions/pedegogical-teacher.R")
borders = makeBorders(1:10)

# small true rectangle

rect1 = c(2,2,4,4)

test = pedTeacher(rect1,borders = borders,nPoints = 5)
plotPedTeacher(test, rect1)

# large true rectangle

rect2 = c(2,2,8,8)

test2 = pedTeacher(rect2, borders, nPoints = 5)
plotPedTeacher(test2, rect2)


testA0 = pedTeacher(rect1,borders = borders,nPoints = 5, alpha = 0)
plotPedTeacher(testA0, rect1)

evidence = c()

for (i in 1:length(test[,1])) {
  evidence[i] = isInRectangle(c(test[i,1], test[i,2]),rectangle)
}




