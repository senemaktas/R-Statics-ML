#öncelikle veri setimiz projeye ekleniyor.cesitli sekillerde eklemeler yapılabilir.
verisetim<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv")
                    ,sep = ";",dec = ".")

colnames(verisetim)<-c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","Quality")
#Aynı sonuclari almak icin random değeri belirlemek icin bir sayi belirliyoruz.
set.seed(1234)
#belirtilen oranlara göre veriseti iki parcaya ayrılıyor.
#sample fonksiyonu belirtilen boyutta bir örneğini alır.
SampleID<-sample(2,nrow(verisetim),replace=TRUE,prob=c(0.6,0.4))
#iki kısıma bölünen verinin 1. kısmı trainData adlı degiskene aktarılıyor.
#, den sonraki bosluk bütün sütunları alması gerktiğini söyler.
trainData<-verisetim[SampleID==1,]
#2. kısım veri test verisi olarak degiskene atanıyor
testData<-verisetim[SampleID==2,]
#karar agacını gercekleştirebilmek icin "party" paketini indiriyoruz.
summary(testData)
summary(trainData)
install.packages("party")
#paketi kullanmak icin dahil ediyoruz.
library(party)
#ctree fonksiyonu(classification tree) ,Ouality kategorik deger ve ~dan sonra kullanmak istedigimiz
#sutunları yazıyoruz.veri olarak egitim için ayırdıklarımızı kullanıyoruz. 
agac<-ctree(Quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + 
              free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,data = trainData )
plot(agac)
#tablo seklinde cıktı verir.3 ten 8 e kadar olan deger(ilk satır) Quality degerini gösteriyor.
#ilk sutun ise agac modelinden tahminleri belirtir.Diagonal olarak dogru sınıflandırmayı verir.
table(predict(agac),trainData$Quality)
#miss clasification hata oranını verir.
tab<-table(predict(agac),trainData$Quality)
1-sum(diag(tab))/sum(tab)
#olusturulan tree mizi plot fonksiyonu ile görsel cıktı alıyoruz.
plot(agac,type="simple")
#yukarıda egitim verisi ile ögrettikten sonra test verimiz ile
#(missclassification error in validation dataset)
#doğrulama veri kümesinde yanlış sınıflandırma hatasını buluyoruz.
testPred<-predict(agac,newdata=testData)
table(testPred,testData$Quality)
#missclassification hata oranını bulmak icin
tab<-table(testPred,testData$Quality)
1-sum(diag(tab))/sum(tab)
#confusion matrix elde etmek için bu paketi indirmemiz gerekiyor.
installed.packages("caret")
library(caret)
confusionMatrix(testPred,testData$Quality)
