library(tidyverse)
library(ggrepel)
library(GGally)       
library(fpc)          
library(DataExplorer) 
library(plotly)       
library(FactoMineR)   
library(factoextra)
library(psych)
library(corrplot)
library(pander)
library(ggplot2)
library(viridis)
library(ggplot2)
library(cluster)

proj <- read.csv2("./data/PROJEKT.csv")
str(proj)

summary(proj)

cor(proj[, 2:9])
corrplot(cor(proj[, 2:9]), order = "hclust", tl.cex = 0.7, col = COL1('Purples'), tl.col = 'black')
cortest.bartlett(cor(proj[, 2:9]), n = nrow(proj))
KMO(cor(proj[, 2:9]))

# P-value jest bardzo małe, więc można odrzucić hipotezę, że macierz korelacji jest macierzą jednostkową.

# Powyższe mocne korelacje oznaczają dużą szansę na powodzenie PCA.

# KMO > 0.5, więc PCA jest jak najbardziej dopuszczalna.


library(factoextra)

proj <- proj %>% select(-Kraj)
pr2 <- PCA(proj, graph = FALSE) 
fviz_screeplot(pr2, addlabels = TRUE, barfill = "turquoise4",
               barcolor="goldenrod", xlab = "", ylab = "")+theme_bw() 

fviz_pca_biplot(pr2, repel = TRUE, labelsize = 3,col.var = "goldenrod") # mniejsze etykiety

#Wybieramy 2 składowe, ponieważ łącznie wyjaśniają ponad 80% zmienności, 
#można to łatwo odczytać z wykresu słupkowego – dwie pierwsze składowe wyjaśniają ponad 80% (66,7% + 13,5% = 80,2%).


#Nazywamy wymiary w następujący sposób:
  
#PC1 = zarobki i ogólne zasoby kraju
#PC2 = kapitał na rozwój i stan służby zdrowia

#Definicje naszych wymiarów są poglądowe, zostały stworzone aby ułatwić analizę.
proj <- read.csv2("./data/PROJEKT.csv")
pr1 <- principal(proj[, 2:9], nfactors = 2, rotate = "none")

#Dodajemy kolumnę, żeby łatwiej zwizualizować dane.

kraje.pca <- pr1$scores %>% 
  as.data.frame() %>% 
  mutate(Kraj = proj$Kraj)

kraje.pca %>% 
  ggplot(aes(x=PC1, PC2)) +
  geom_point(aes(color=Kraj)) +
  geom_text_repel(aes(label = Kraj), size = 2.5) + 
  xlab("Zarobki i ogólne bogactwo kraju")+
  ylab("Kapitał na rozwój i stan służby zdrowia")+ 
  theme_bw()+
  theme(legend.position ="null")
  

#Największy budżet na rozwój oraz badania posiada Szwajcaria i w tej kwestii znacząco się wyróżnia.

#Najniższe PKB w badanej grupie ma Bułgaria. 

#Irlandia wyróżnia się prawdopodobnie tym, że produkuje dużą ilość energii 
#i ma wydajnych pracownikóW.

#Na Islandii budżet na rozwój również jest spory i dobrze się zarabia, 
#podobnie jest w Holandii.

#Kraje biedniejsze, mniej rozwinięte usytuowały się 
#po lewej stronie wykresu.

#Kraje bogatsze, lepiej rozwinięte znajdują się po prawej.

#Zwolnienia ze szpitala wliczają w sobie również śmierci, 
#dlatego odwrotnie korelują z PKB.

#Tam gdzie opieka zdrowotna jest na wyższym poziomie pacjenci 
#kardiologiczni częściej przeżywają np.zawał lub otrzymują stałą opiekę medyczną.

#Francja również może się pochwalić swoją produkcją energii.

#Portugalia z kolei nie słynie z posiadania sporego budżetu na rozwój.

#Polska nie wyróżnia sie specjalnie od innych krajów słowiańskich/wschodnio-europejskich.

#Zastanawiamy się teraz jaką metodę grupowania zastosować.

proj.scaled <- scale(proj[, 2:9]) %>% as.data.frame()
# hclust
clusterboot(proj.scaled, B = 500,
            clustermethod = hclustCBI, method = "ward.D2", k = 2)

clusterboot(proj.scaled, B = 500,
            clustermethod = hclustCBI, method = "ward.D2", k = 4)

# kmeans
clusterboot(proj.scaled, B = 500,
            clustermethod = kmeansCBI, krange = 2)
clusterboot(proj.scaled, B = 500,
            clustermethod = kmeansCBI, krange = 4)

# clara
clusterboot(proj.scaled, B = 500,
            clustermethod = claraCBI, k = 2)
clusterboot(proj.scaled, B = 500,
            clustermethod = claraCBI, k = 4)

#Najbardziej stabilna okazuje się clara, niewiele gorsza jest jednak metoda hclust, zatem przy niej zostaliśmy.
#Poniżej sprawdzimy na ile grup powinniśmy podzielić 

fviz_nbclust(proj.scaled, clara, method = "wss") #2
fviz_nbclust(proj.scaled, clara, method = "silhouette") #2
fviz_nbclust(proj.scaled, clara, method = "gap_stat") #1
#Podsumowując najlepiej wybrać dwie grupy
clara1 <- clara(proj.scaled, k = 2)
fviz_cluster(clara1, data = proj.scaled, repel = TRUE, labelsize = 8, ggtheme= theme_bw())
#Według grupowania algorytmem clara pierwszy wymiar przyjmuje wartości ujemne dla wysoko rozwiniętych krajów, a dodatnie dla mniej rozwiniętych.
#Inna metoda sprawdzenia 
#Hierarchiczna
#metoda Warda

summary(proj.scaled) 
rownames(proj.scaled) <- proj$Kraj
p <- dist(proj.scaled, method = "euclidean")
hc1 <- hclust(p, method = "ward.D2")

#Wyświetlenie dendrogramu - plot na obiekcie hclust.

fviz_dend(hc1, cex = 0.7, lwd = 0.7, main = "Dendrogram Państw", xlab = "Państwa", k = 2, k_colors = 
            c("darkorchid","mediumaquamarine"), ggtheme = theme_bw(), rect = TRUE)

#Najbliżej siebie są Czechy i Estonia.

#Odległość na wykresie tych dwóch zmiennych od siebie jest najmniejsza.

#Odczytujemy to na podstawie dendrogramu i najniżej położonej kreski łączącej pary.

#Dendrogram wyraźnie wyznacza dwie grupy. Odczytujemy go metodą "bottom up", czyli od dołu.

#Dzielimy nasz zbiór na dwie grupy i odczytujemy, która grupa ma przewagę dla poszczególnych zmiennych.

cutree(hc1, k = 2)
proj$cluster.w <- cutree(hc1, k = 2) %>% as.factor()
plot_boxplot(proj[, 2:10], by = "cluster.w", 
             geom_boxplot_args = list(fill = "turquoise4", "outlier.color" = "goldenrod"), ggtheme = theme_bw())

#W 1 grupie znajdują się kraje, które cechuje duża liczba zwolnień ze szpitala pacjentów z oddziałów kardiologicznych (w tym śmierci),
#oraz z niskim wskaźnikiem konsumpcji i mało wydajnymi pracownikami.

#W 2 grupie przeważają państwa wysoko rozwinięte, z dużą ilością użytkowników internetowych 
#oraz dużym budżetem na rozwój.


#Wizualizacja Państw i ich umiejscowienie na biplocie  

pr <- proj %>% 
  select(-Kraj)
rownames(pr) <- proj$Kraj
pc1 <- PCA(pr, quali.sup = 9, graph = FALSE)
fviz_pca_biplot(pc1, habillage = 9, legend = "bottom", col.var = "turquoise4",labelsize = 3)
fviz_pca_ind(pc1, habillage = 9, legend = "bottom",labelsize = 3,addEllipses =TRUE)

#Zgodnie z metodą Warda, odwrotnie niż algorytmem Clara, grupowanie przypisało dodatnie wartości wymiaru 1 Państwom lepiej rozwniętym.
#Zarysowane grupy są jednak do siebie bardzo zbliżone.
#Większe odchylenia można zauważyć w grupie 1, w grupie 2 większość Państw ma zbliżone do siebie wyniki.


