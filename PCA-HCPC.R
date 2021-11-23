#continue working dir from previous code (reg and boxplot)

#do basic PCA
all58.pca<-PCA(allrhino58traits[,4:61], scale.unit = TRUE, ncp = 5, graph =TRUE)

#basic
external.pca<-PCA(allrhino58traits[,37:43], scale.unit = TRUE, ncp = 3, graph =TRUE)

#get eigenvalues
external.eig<-external.pca$eig
external.eig

#contrib to PC1
externalcontribPC1<-fviz_contrib(external.pca, choice = "var", axes = 1, top = 7)
externalcontribPC1
#contrib to PC2
externalcontribPC2<-fviz_contrib(external.pca, choice = "var", axes = 2, top = 7)
externalcontribPC2

#playing here
ext.var = get_pca_var(external.pca)
ext.var
#get loading
ext.var$coord
#get conttribution
ext.var$contrib


###############
#visualization
###############
#install libraries for color
install.packages("ggsci")
library(ggsci)
#better visualization
#Biplot-final-published-convex hull-external character
external.biplot.final<-fviz_pca_biplot(external.pca, geom.ind = "point",
                col.ind = allrhino58traits$Species, fill.ind = allrhino58traits$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
 ggpubr::ggpar(external.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, external 7 characters",
              caption = NULL,
              xlab = "PC1(62.26%)", ylab = "PC2(24.84%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("external.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-external characters_sp_id
pdf("externalPCA.pdf")
external_sp_id.biplot.final<-fviz_pca_biplot(external.pca,  geom.ind = "point",
                col.ind = allrhino58traits$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(external_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, External 7 characters",
              caption = NULL,
              xlab = "PC1(62.26%)", ylab = "PC2(24.84%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("external_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

#PCA for noseleaf
#basic
noseleaf.pca<-PCA(allrhino58traits[,4:18], scale.unit = TRUE, ncp = 2, graph =TRUE)
#get eigenvalues
noseleaf.eig<-noseleaf.pca$eig
noseleaf.eig

#contrib to PC1
noseleafcontribPC1<-fviz_contrib(noseleaf.pca, choice = "var", axes = 1, top = 15)
noseleafcontribPC1
#contrib to PC2
noseleafcontribPC2<-fviz_contrib(noseleaf.pca, choice = "var", axes = 2, top = 15)
noseleafcontribPC2

#playing here do the stats
noseleaf.var = get_pca_var(noseleaf.pca)
noseleaf.var
#get loading
noseleaf.var$coord
#get conttribution
noseleaf.var$contrib

#install libraries for color
install.packages("ggsci")
library(ggsci)
#better visualization
#Biplot-final-published-convex hull-external character
noseleaf.biplot.final<-fviz_pca_biplot(noseleaf.pca, geom.ind = "point",
                col.ind = allrhino58traits$Species, fill.ind = allrhino58traits$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(noseleaf.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, noseleaf characters",
              caption = NULL,
              xlab = "PC1(47.16%)", ylab = "PC2(36.25%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("noseleaf.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-external characters_sp_id
pdf("noseleafPCA.pdf")
noseleaf_sp_id.biplot.final<-fviz_pca_biplot(noseleaf.pca,  geom.ind = "point",
                col.ind = allrhino58traits$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(noseleaf_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, noseleaf characters",
              caption = NULL,
              xlab = "PC1(47.16%)", ylab = "PC2(36.25%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("noseleaf_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')


#PCA for sella
#basic
sella.pca<-PCA(allrhino58traits[,19:27], scale.unit = TRUE, ncp = 3, graph =TRUE)

#get eigenvalues
sella.eig<-sella.pca$eig
sella.eig

#contrib to PC1
sellacontribPC1<-fviz_contrib(sella.pca, choice = "var", axes = 1, top = 9)
sellacontribPC1
#contrib to PC2
sellacontribPC2<-fviz_contrib(sella.pca, choice = "var", axes = 2, top = 9)
sellacontribPC2

#playing here do stats
sella.var = get_pca_var(sella.pca)
sella.var
#get loading
sella.var$coord
#get conttribution
sella.var$contrib

##############################################################################################33
#better visualization
#Biplot-final-published-convex hull-sella character
sella.biplot.final<-fviz_pca_biplot(sella.pca, geom.ind = "point",
                col.ind = allrhino58traits$Species, fill.ind = allrhino58traits$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(sella.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, sella characters",
              caption = NULL,
              xlab = "PC1(65.38%)", ylab = "PC2(16.64%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("sella.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-sella characters_sp_id
pdf("sellaPCA.pdf")
sella_sp_id.biplot.final<-fviz_pca_biplot(sella.pca,  geom.ind = "point",
                col.ind = allrhino58traits$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(sella_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, sella characters",
              caption = NULL,
              xlab = "PC1(65.38%)", ylab = "PC2(16.64%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("sella_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

#######
#load data
wing.aja = read.csv(file = "wing.csv", header = T, sep = ",")
str(wing.aja)
head(wing.aja)
#basic
wing.pca<-PCA(wing.aja[,4:7], scale.unit = TRUE, ncp = 3, graph =TRUE)

#get eigenvalues
wing.eig<-wing.pca$eig
wing.eig

#contrib to PC1
wingcontribPC1<-fviz_contrib(wing.pca, choice = "var", axes = 1, top = 4)
wingcontribPC1
#contrib to PC2
wingcontribPC2<-fviz_contrib(wing.pca, choice = "var", axes = 2, top = 4)
wingcontribPC2

#playing here do stats
wing.var = get_pca_var(wing.pca)
wing.var
#get loading
wing.var$coord
#get conttribution
wing.var$contrib

######################################################################################################################################################33

#PCA for WING

#better visualization
#Biplot-final-published-convex hull-wing character
wing.biplot.final<-fviz_pca_biplot(wing.pca, geom.ind = "point",
                col.ind = wing.aja$Species, fill.ind = wing.aja$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(wing.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, wing characters",
              caption = NULL,
              xlab = "PC1(46.28%)", ylab = "PC2(30.67%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("wing.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-wing characters_sp_id
pdf("wingPCA.pdf")
wing_sp_id.biplot.final<-fviz_pca_biplot(wing.pca,  geom.ind = "point",
                col.ind = wing.aja$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(wing_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, wing characters",
              caption = NULL,
              xlab = "PC1(46.28%)", ylab = "PC2(30.67%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("wing_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

#####
#PCA fpr acoustic

acoustic.pca<-PCA(allrhino58traits[,56:61], scale.unit = TRUE, ncp = 3, graph =TRUE)

#get eigenvalues
acoustic.eig<-acoustic.pca$eig
acoustic.eig

#contrib to PC1
acousticcontribPC1<-fviz_contrib(acoustic.pca, choice = "var", axes = 1, top = 6)
acousticcontribPC1
#contrib to PC2
acousticcontribPC2<-fviz_contrib(acoustic.pca, choice = "var", axes = 2, top = 6)
acousticcontribPC2

#playing here do stats
acoustic.var = get_pca_var(acoustic.pca)
acoustic.var
#get loading
acoustic.var$coord
#get conttribution
acoustic.var$contrib

###############################################################################################################################################3
#better visualization
#Biplot-final-published-convex hull-acoustic character
acoustic.biplot.final<-fviz_pca_biplot(acoustic.pca, geom.ind = "point",
                col.ind = allrhino58traits$Species, fill.ind = allrhino58traits$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(acoustic.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, acoustic characters",
              caption = NULL,
              xlab = "PC1(57.47%)", ylab = "PC2(23.87%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("acoustic.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-acoustic characters_sp_id
pdf("acoustic.pdf")
acoustic_sp_id.biplot.final<-fviz_pca_biplot(acoustic.pca,  geom.ind = "point",
                col.ind = allrhino58traits$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(acoustic_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, acoustic characters",
              caption = NULL,
              xlab = "PC1(57.47%)", ylab = "PC2(23.87%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("acoustic_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

###########################################################################################################
#do pca for acoustic again but removed ipi 


#load the data
acoustic = read.csv(file = "acoustic.csv", header = T, sep = ",")
str(acoustic)
head(acoustic)

#do pca
acousticnoIPI.pca<-PCA(acoustic[,4:8], scale.unit = TRUE, ncp = 3, graph =TRUE)

#better visualization
#Biplot-final-published-convex hull-acoustic character
acousticnoIPI.biplot.final<-fviz_pca_biplot(acousticnoIPI.pca, geom.ind = "point",
                col.ind = acoustic$Species, fill.ind = acoustic$Species,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(acousticnoIPI.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, acoustic characters",
              caption = NULL,
              xlab = "PC1(68.84%)", ylab = "PC2(18.84%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("acousticnoIPI.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')
#Biplot-final-published-convex hull-acoustic characters_sp_id
acousticnoIPI_sp_id.biplot.final<-fviz_pca_biplot(acousticnoIPI.pca,  geom.ind = "point",
                col.ind = acoustic$sp_id, 
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(acousticnoIPI_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, acoustic characters",
              caption = NULL,
               xlab = "PC1(68.84%)", ylab = "PC2(18.84%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
ggsave("acousticnoIPI_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

###DONE HERE FOR PCA

###NOT DONE YET LOOOOOOLLL TAKICUAH EE
DO THIS FOR COMBINATION ALL TRAITS (WHEN ALL TRAITS WERE COMBINED AND SCALED PRIOR ANALYSIS

########################
#########################
#do it again
#########################
#########################
rhinoselected = read.csv(file = "rhinoallSelectedtraitsforHCPC.csv", header = T, sep = ",")
str(rhinoselected)
rhinoselected$Id_SpId
length(unique(rhinoselected$Id_SpId)) #deleted two repeated samples
rownames(rhinoselected) = rhinoselected$Id_SpId

#try to scale the dataset since it has different unit (g, mm etc)
rhinoselected.scaled<-as.data.frame(scale(rhinoselected[,4:37]))
summary(rhinoselected.scaled)
str(rhinoselected.scaled)
head(rhinoselected.scaled)


#compute PCA with ncp=2
scaledselected.pca<-PCA(rhinoselected.scaled[,2:34], ncp=2, graph= T)

#get eigenvalues
scaledselected.eig<-scaledselected.pca$eig
scaledselected.eig

#contrib to PC1
scaledselectedcontribPC1<-fviz_contrib(scaledselected.pca, choice = "var", axes = 1, top = 32)
scaledselectedcontribPC1
scaledselectedcontribPC2<-fviz_contrib(scaledselected.pca, choice = "var", axes = 2, top = 32)
scaledselectedcontribPC2

#playing here do stats
scaledselected.var = get_pca_var(scaledselected.pca)
scaledselected.var
#get loading
scaledselected.var$coord
#get conttribution
scaledselected.var$contrib

###########################################3
#better visualization
#Biplot-final-published-convex hull-acoustic character
#Biplot-final-published-convex hull-acoustic characters_sp_id
pdf("all characters.pdf")
scaledselected_sp_id.biplot.final<-fviz_pca_biplot(scaledselected.pca,  geom.ind = "point",
                col.ind = rhinoselected$sp_id,
                palette = c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#42B54099", "#AD002A99", "#1B191999", "#800000FF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#725663FF"),
                addEllipses = TRUE, ellipse.type = "convex", ellipse.level = 0.95, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") + theme_classic() + scale_shape_manual(values = c (0,1,2,3,4,5,6,7,8,9,11,12,14,15,16,17))
ggpubr::ggpar(scaledselected_sp_id.biplot.final,
              title = "Principal Component Analysis",
              subtitle = "Rhinolophus, all characters",
              caption = NULL,
              xlab = "PC1(46.61%)", ylab = "PC2(20.20%)",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_classic()
              )
dev.off()
ggsave("scaledselected_sp_id.biplot.final.tiff", units="in", width=10, height = 7, dpi = 300, compression='lzw')

#compute PCA with ncp=2
all58hcpc.pca<-PCA(rhino[,4:61], ncp=2, graph= FALSE)
#compute hierarchical clustering on principal component
all58.hcpc<-HCPC(all58hcpc.pca, consol = T, iter.max = 1000, 
                    nb.clust = -1, min = 12, max = NULL,
                    metric = "euclidian",
                    method = "ward",
                    order = TRUE,
                    proba = 0.05, #see ?catdes for details
                    graph = T)

library(factoextra)
all58hcpc.factoextra<-fviz_dend(all58.hcpc, 
          cex = 0.5,                     # Label size
          paletter = "npg",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "npg",           # Rectangle color
          labels_track_height = 1     # Augment the room for labels
          )
all58hcpc.factoextra
ggpubr::ggpar(all58hcpc.factoextra,
              title = "Cluster Dendogram",
              subtitle = "Rhinolophidae. Morphology and Echolocation traits",
              caption = NULL,
              ggtheme = theme_classic()
              )
ggsave("all58hcpc.factoextra.tiff", units = "in", width = 18, height = 7, dpi = 300, compression = 'lzw')


#load data
rhino58forhcpc = read.csv(file = "rhinoall58traitsforHCPC.csv", header = T, sep = ",")
str(rhino58forhcpc$Id)
rhino58forhcpc$Id_SpId
length(unique(rhino58forhcpc$Id_SpId)) #deleted two repeated samples
rownames(rhino58forhcpc) = rhino58forhcpc$Id_SpId

rhino58.pca<-PCA(rhino58forhcpc[,4:61], ncp=2, graph= T)

#use basic hclust
rhino58.dist<-dist(rhino58forhcpc, method = 'euclidian')
rhino58.hclust_ward<-hclust(rhino58forhcpc.dist, method ="ward")

# Open a pdf file
pdf("rhino58.hclustplot.pdf") 
plot(rhino58.hclust_ward)
dev.off() 
# 2. Create a plot
#plot(x = my_data$wt, y = my_data$mpg,
     #pch = 16, frame = FALSE,
     #xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# Close the pdf file

####################
#scaled it
####################
#try to scale the dataset since it has different unit (g, mm etc)
rhino58forhcpc.scaled<-as.data.frame(scale(rhino58forhcpc[,4:61]))
summary(rhino58forhcpc.scaled)
str(rhino58forhcpc.scaled)
head(rhino58forhcpc.scaled)

#do simple hcpc
#compute PCA with ncp=2
scaled58.pca<-PCA(rhino58forhcpc.scaled[,2:58], ncp=2, graph= T)

#use basic hclust
scaled58.dist<-dist(rhino58forhcpc.scaled, method = 'euclidian')
scaled58.hclust_ward<-hclust(rhino58forhcpc.dist, method ="ward")
plot(scaled58.hclust_ward)

#########################
#########################
#do it again
#########################
#########################
rhinoselected = read.csv(file = "rhinoallSelectedtraitsforHCPC.csv", header = T, sep = ",")
str(rhinoselected)
rhinoselected$Id_SpId
length(unique(rhinoselected$Id_SpId)) #deleted two repeated samples
rownames(rhinoselected) = rhinoselected$Id_SpId

#try to scale the dataset since it has different unit (g, mm etc)
rhinoselected.scaled<-as.data.frame(scale(rhinoselected[,4:37]))
summary(rhinoselected.scaled)
str(rhinoselected.scaled)
head(rhinoselected.scaled)

#do simple hcpc
#compute PCA with ncp=2
scaledselected.pca<-PCA(rhinoselected.scaled[,2:34], ncp=5, graph= T)

#use basic hclust
scaledselected.dist<-dist(rhinoselected.scaled, method = 'euclidian')
scaledselected.hclust_ward<-hclust(scaledselected.dist, method ="ward")
plot(scaledselected.hclust_ward)


##################
#use external morpho and acoustic only
##################

#do simple hcpc
#compute PCA with ncp=2
morphocall.pca<-PCA(rhinoselected.scaled[,16:34], ncp=2, graph= T)

#use basic hclust
morphocall.dist<-dist(rhinoselected.scaled[,16:34], method = 'euclidian')
morphocall.hclust_ward<-hclust(morphocall.dist, method ="ward")
plot(morphocall.hclust_ward)
##############################
##do ward.d2

#load data
rhino58forhcpc1 = read.csv(file = "rhinoall58traitsforHCPC.csv", header = T, sep = ",")
str(rhino58forhcpc1$Id)
rhino58forhcpc1$Id_SpId
length(unique(rhino58forhcpc1$Id_SpId)) #deleted two repeated samples
rownames(rhino58forhcpc1) = rhino58forhcpc1$Id_SpId

rhino58.pca<-PCA(rhino58forhcpc1[,4:61], ncp=2, graph= T)

#use basic hclust
rhino58.dist1<-dist(rhino58forhcpc1, method = 'euclidian')
rhino58.hclust_ward.D<-hclust(rhino58forhcpc.dist, method ="ward.D2")

# Open a pdf file
pdf("rhino58.hclustplotward2.pdf") 
plot(rhino58.hclust_ward.D)
dev.off() 
# 2. Create a plot
#plot(x = my_data$wt, y = my_data$mpg,
     #pch = 16, frame = FALSE,
     #xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# Close the pdf file


