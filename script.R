#setwd('C:/Users/marty/Desktop/GITHUB/proj_optymal')
setwd('/Users/mateuszpindyk/Documents/GitHub/set_covering_problem')
n_rows <- sample(1:50, 1)
n_cols <- sample(1:50, 1)

n_elements <- sample(1:50, 1)

matrix1 <- matrix(sample(c(0, 1), n_rows*n_cols, replace = TRUE), ncol = n_cols)
#matrix1

col_names <- c()
for (i in LETTERS) {
  for (j in LETTERS){
    col_names <- append(col_names, paste(i,j))
    if (length(col_names) == n_cols){
      break
    }
  }
  if (length(col_names) == n_cols){
    break
  }
}

colnames(matrix1) <- col_names[1:n_cols]
rownames(matrix1) <- 1:n_rows


ilosc_kolumn<-dim(matrix1)[2]
koszty <- c()
for (i in c(1:ilosc_kolumn)) {
  koszty <- append(koszty, sample(10:50,1))
}

matrix2 <- rbind(matrix1, koszty)
dane <- matrix1
dane <- as.data.frame(dane)
#View(dane)
dane_temp<-dane

wysokosc<-as.numeric(length(t(dane))/length(dane))

############################################
numery_pokojow_z_kamera<-c()
nazwy_kamer<-c()
min_koszt<-c()
koszty_suma<-c()

while (length(numery_pokojow_z_kamera) != wysokosc) {
  koszt_na_kamere <- c()
  for (i in c(1:length(dane)))
  {
    koszt_na_kamere <- append(koszt_na_kamere,as.numeric(matrix2[wysokosc+1,i]))
  }
  for (i in c(1:length(dane)))
  {
    if (sum(dane_temp[,i]>0)) {
      koszt_na_kamere[i] <- koszt_na_kamere[i]/sum(dane_temp[,i])
    }
    else {
      koszt_na_kamere[i] <- 9999999
    }
  }
  min_koszt <- append(min_koszt, min(koszt_na_kamere))
  min_koszt_index <- as.numeric(match(min(koszt_na_kamere),koszt_na_kamere))
  koszty_suma <- append(koszty_suma,koszty[min_koszt_index])
  maks_kol_with_name <- names(dane)[min_koszt_index]
  
  for (i in c(1:wysokosc)) {
    
    if (dane_temp [i,min_koszt_index] == 1){
      numery_pokojow_z_kamera<-c(numery_pokojow_z_kamera,rownames(dane_temp[i,]))
      
      dane_temp[i,]<-0
      
      if(!(paste(maks_kol_with_name) %in% nazwy_kamer)){
        nazwy_kamer<-c(nazwy_kamer,maks_kol_with_name)
      }
    }
  }
}


nazwy_kamer
numery_pokojow_z_kamera
sum(koszty_suma)



#solver
