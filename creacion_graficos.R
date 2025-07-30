####Gráficos######



g_inseguro <- ggplot(proporciones_combinadas, aes(x = trimestre, y = proporcion, group = nom_cdmx1, color = nom_cdmx1)) +
  geom_line(data = proporciones_combinadas %>% filter(nom_cdmx1 == "CDMX"), 
            size = 1.8, show.legend = FALSE) +  # Línea para CDMX
  geom_line(data = proporciones_combinadas %>% filter(nom_cdmx1 != "CDMX"), 
            size = 0.8, show.legend = FALSE, alpha = 0.5) +  # Línea para las otras demarcaciones
  geom_point(size = 2) +  # Puntos en cada trimestre
  # Etiquetas para CDMX mostrando el porcentaje
  geom_text_repel(data = proporciones_combinadas %>% filter(nom_cdmx1 == "CDMX"), 
                  aes(label = scales::percent(proporcion, accuracy = 0.1)), 
                  vjust = -0.5, size = 3.5, fontface = "bold", color = "black", 
                  max.overlaps = Inf) +  # Permitir más superposiciones
  # Etiquetas para cada demarcación en el último trimestre
  geom_text_repel(data = proporciones_combinadas %>% filter(trimestre == levels(proporciones_combinadas$trimestre)[length(levels(proporciones_combinadas$trimestre))]), 
                  aes(label = nom_cdmx1), 
                  vjust = -0.5, size = 3, color = "black") +
  # Eliminar la etiqueta duplicada para CDMX en el último trimestre
  labs(x = "Trimestre", y = "", color = "") +
  ggtitle("Proporción de percepción de inseguridad en la Ciudad de México por Trimestre y Demarcación",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025") +
  theme_minimal() +
  theme(legend.position = "none") +  # Eliminar leyenda
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption = "Incluye la no respuesta para el cálculo de los porcentajes")

# Mostrar el gráfico
print(g_inseguro)


#Efectividad gobierno demarcación para resolver problemáticas

g_hist_desemp_gub_demar_cdmx<-ggsurvey(subset(dm_CB_agr,  NOM_ENT== "CIUDAD DE MEXICO")) +  
  aes(x = trimestre, fill= desemp_gub1) +geom_bar(position = "fill",) + 
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.75 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Ciudad de México: Efectividad del gobierno de la demarcación para resolver problemáticas",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana ENSU, Junio 2025")+
  scale_y_continuous(labels = scales::percent_format())+facet_wrap(~nom_cdmx1)

g_hist_desemp_gub_demar_cdmx


#Incivilidades CDMX

g_inci_vanda_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" & 
                                          BP1_4_1 != "No aplica"& BP1_4_1 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_1)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Vandalismo")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_vanda_CDMX_hist

g_inci_alch_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                       NOM_ENT == "CIUDAD DE MEXICO" & 
                                         
                                         BP1_4_2 != "No aplica"& BP1_4_2 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_2)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Consumo de alcohol")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_alch_CDMX_hist

g_inci_rob_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                      NOM_ENT == "CIUDAD DE MEXICO" & 
                                        BP1_4_3 != "No aplica"& BP1_4_3 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_3)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Robos o asaltos")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_rob_CDMX_hist

g_inci_band_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                       NOM_ENT == "CIUDAD DE MEXICO" & 
                                         BP1_4_4 != "No aplica"& BP1_4_4 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_4)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Bandas/pandillas")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_band_CDMX_hist

g_inci_drog_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                       NOM_ENT == "CIUDAD DE MEXICO" & 
                                         
                                         BP1_4_5 != "No aplica"& BP1_4_5 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_5)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Venta/consumo drogas")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_drog_CDMX_hist

g_inci_disp_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                       NOM_ENT == "CIUDAD DE MEXICO" & 
                                         
                                         BP1_4_6 != "No aplica"& BP1_4_6 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_6)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Disparos")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_disp_CDMX_hist

g_inci_huach_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" & 
                                          
                                          BP1_4_7 != "No aplica"& BP1_4_7 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_7)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Huachicol")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_huach_CDMX_hist

g_inci_elect_CDMX_hist<-ggsurvey(subset(dm_CB_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" & 
                                          BP1_4_8 != "No aplica"& BP1_4_8 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_4_8)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Tomas eléctricas irre.")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_inci_elect_CDMX_hist

plot_inci_CDMX_hist<- ggarrange(g_inci_vanda_CDMX_hist, g_inci_alch_CDMX_hist, g_inci_rob_CDMX_hist,
                                g_inci_band_CDMX_hist, g_inci_drog_CDMX_hist, g_inci_disp_CDMX_hist, 
                                g_inci_huach_CDMX_hist, g_inci_elect_CDMX_hist, common.legend = TRUE,
                                legend = "bottom", ncol = 4, nrow = 2) %>%
  annotate_figure(top = text_grob(expression(atop(bold("CDMX: Incivilidades de las que tuvo referencia o presenció cerca de su domicilio"), 
                                                  scriptstyle("INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")))))%>%
  annotate_figure(bottom = text_grob(expression(scriptstyle("No se tomó en cuenta las no respuesta para el cálculo de los porcentajes "))))

plot_inci_CDMX_hist
##Victimización
g_vict_hist_cdmx<- ggsurvey(subset(dm_CB_VIV_agr,  NOM_ENT == "CIUDAD DE MEXICO" & grepl("^(T2|T4)",trimestre) & R_DEF == "01")) +  
  aes(x = trimestre, fill= victi) +geom_bar(position = "fill", width = 0.5) + 
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 4 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ ggtitle("Victimización en el hogar en CDMX", subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana ENSU, Junio 2025")+
  scale_y_continuous(labels = scales::percent_format())

g_vict_hist_cdmx

######inseguridad por lugar


gcasa<-ggsurvey(subset(dm_CB_agr,  NOM_ENT == "CIUDAD DE MEXICO" & BP1_2_01 != "No aplica" & BP1_2_01 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_01)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Casa")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gcasa

gtrabajo<-ggsurvey(subset(dm_CB_agr,  NOM_ENT == "CIUDAD DE MEXICO" & BP1_2_02 != "No aplica" & BP1_2_02 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_02) + geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Trabajo")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gtrabajo

gCALLES<-ggsurvey(subset(dm_CB_agr,  NOM_ENT == "CIUDAD DE MEXICO" & BP1_2_03 != "No aplica" & BP1_2_03 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_03)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Calles")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gCALLES

gESCUELA<-ggsurvey(subset(dm_CB_agr,  NOM_ENT == "CIUDAD DE MEXICO" & BP1_2_04 != "No aplica" & BP1_2_04 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_04)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Escuela")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gESCUELA


gmercado<-ggsurvey(subset(dm_CB_agr,  NOM_ENT == "CIUDAD DE MEXICO" & BP1_2_05 != "No aplica" & BP1_2_05 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_05)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Mercado")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gmercado


gcentrocomercial<-ggsurvey(subset(dm_CB_agr,  
                                  NOM_ENT == "CIUDAD DE MEXICO" & 
                                    BP1_2_06 != "No aplica" & BP1_2_06 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_06)+  geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Centro de salud")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())
gcentrocomercial


gbanco<-ggsurvey(subset(dm_CB_agr,  
                        NOM_ENT == "CIUDAD DE MEXICO" & 
                          BP1_2_07 != "No aplica"& BP1_2_07 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_07)+  geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Banco")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gbanco


gcajero<-ggsurvey(subset(dm_CB_agr,  
                         NOM_ENT == "CIUDAD DE MEXICO" & 
                           BP1_2_08 != "No aplica" & BP1_2_08 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_08)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Cajero automático")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gcajero


gtransp<-ggsurvey(subset(dm_CB_agr,  
                         NOM_ENT == "CIUDAD DE MEXICO" & 
                           BP1_2_09 != "No aplica" & BP1_2_09 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_09) + geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Transporte público")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gtransp


gauto<-ggsurvey(subset(dm_CB_agr,  
                       NOM_ENT == "CIUDAD DE MEXICO" & 
                         BP1_2_10 != "No aplica" & BP1_2_10 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_10)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Automóvil")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gauto



gcarret<-ggsurvey(subset(dm_CB_agr,  
                         NOM_ENT == "CIUDAD DE MEXICO" & 
                           BP1_2_11 != "No aplica"& BP1_2_11 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_11)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Carretera")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

gcarret


grecre<-ggsurvey(subset(dm_CB_agr,  
                        NOM_ENT == "CIUDAD DE MEXICO" & 
                        BP1_2_12 != "No aplica" & BP1_2_12 != "NS/NR")) +  
  aes(x = trimestre, fill= BP1_2_12)+ geom_bar(position = "fill") +
  labs( y = "", x = "", fill = "")+
  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Parques y \n centros recreativos")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

grecre


g_inseg_lugar <- ggarrange(gcasa, gtrabajo, gCALLES, gESCUELA,
                    gmercado, gcentrocomercial, gbanco, gcajero,
                    gtransp, gauto,gcarret, grecre,
                    common.legend = TRUE,
                    legend = "bottom") %>%
  annotate_figure(top = text_grob(expression(atop(bold("CDMX: Percepción de inseguridad por lugar"), 
                                                  scriptstyle("INEGI, Encuesta Nacional de Seguridad Urbana ENSU, Junio 2025")))))
g_inseg_lugar

# 2. Construir el gráfico con facetas

#g_victimizacion_facetas <- dm_CB_VIV_agr_long |>
#  subset(nom_cdmx1 == "IZTP" &grepl("^(T2|T4)", trimestre))|>
#  ggsurvey(aes(x = trimestre, fill = estado_victimizacion)) +
#  geom_bar(position = "fill", width = 0.7) +
#  geom_text(aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
#                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3.2 )+
#  labs(
#    y = "Proporción de Hogares",
#    x = "Trimestre",
#    fill = "Estado de Victimización"
#  ) +
#  scale_y_continuous(labels = scales::percent_format()) +
#  theme_minimal() +
#  theme(
#    legend.position = "bottom",
#    # INICIO DE LA MODIFICACIÓN
#    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, colour = "black"), # Ajustes para visibilidad
#    strip.text = element_text(size = 9, face = "bold", colour = "black") # Ajuste de color para los títulos de faceta
    # FIN DE LA MODIFICACIÓN
#  ) +
#  ggtitle(
#    "Proporción de Victimización por Tipo de Delito y Trimestre en CDMX",
#    subtitle = "Cada faceta muestra la proporción de 'Sí' y 'No' por trimestre para un tipo de delito"
#  ) +
#  facet_wrap(~tipo_delito_facet, scales = "free_y", ncol = 3)

#g_victimizacion_facetas


#Opción dos, construir el gráfico igual al de incivilidades

g_vict_robo_veh_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" &
                                        grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_robo_veh)+ geom_bar(position = "fill", width = .5) +
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_robo_veh)),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Robo de vehículo")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_robo_veh_hist

g_vict_robo_acc_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                      NOM_ENT == "CIUDAD DE MEXICO" &
                                        grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_robo_acc)+ geom_bar(position = "fill", width = .5) +
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_robo_acc)),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Robo parcial vehículo")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_robo_acc_hist

g_vict_allana_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                      NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_allana)+ geom_bar(position = "fill", width = .5)+
  scale_fill_discrete(limits = c("Allanamiento morada", "No víctima"))+
  geom_text(data = . %>% filter(!is.na(vict_allana )),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Allanamiento morada")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_allana_hist

g_vict_robo_asalto_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_robo_asalto)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_robo_asalto)),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Robo o asalto en \n vía pública/transporte")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_robo_asalto_hist

g_vict_robo_otr_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                         NOM_ENT == "CIUDAD DE MEXICO" &
                                        grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_robo_otro)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_robo_otro)),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Robo otra forma")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_robo_otr_hist

g_vict_extor_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                      NOM_ENT == "CIUDAD DE MEXICO" &
                                     grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_amenaza_ext)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_amenaza_ext)),
    aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Extorsión o amenazas")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_extor_hist


g_vict_fraud_banc_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                   NOM_ENT == "CIUDAD DE MEXICO" &
                                     grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_fraude_banc)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_fraude_banc)), 
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Fraude bancario")+
  theme(plot.title = element_text(hjust = .5))+ scale_y_continuous(labels = scales::percent_format())

g_vict_fraud_banc_hist


g_vict_estafa_hist<-ggsurvey(subset(dm_CB_VIV_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" & 
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= vict_estafa)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(vict_estafa)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Estafa")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())

g_vict_estafa_hist



plot_vict_CDMX_hist<- ggarrange(g_vict_extor_hist, g_vict_robo_acc_hist, g_vict_robo_asalto_hist,
                                g_vict_fraud_banc_hist, g_vict_estafa_hist, g_vict_robo_veh_hist,
                                g_vict_allana_hist, g_vict_robo_otr_hist,
                                common.legend = TRUE,
                                legend = "bottom", ncol = 4, nrow = 2) %>%
  annotate_figure(top = text_grob(expression(atop(bold("CDMX: Victimización de algún miembro del hogar según tipo de delito"), 
                                                  scriptstyle("INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")))))%>%
  annotate_figure(bottom = text_grob(expression(scriptstyle("Se toma en cuenta la no respuesta para el cálculo del porcentaje."))))

plot_vict_CDMX_hist


g_aco_piropos_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_piropos)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_piropos)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Acoso sexual \n con piropos")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_piropos_hist


g_aco_intento_vio_hist<-ggsurvey(subset(dm_CB_agr,  
                                        NOM_ENT == "CIUDAD DE MEXICO" &
                                          grepl("^(T2|T4)", trimestre) )) +  
  aes(x = trimestre, fill= aco_intento_vio)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_intento_vio)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Intento de violación")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_intento_vio_hist


g_aco_intercam_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_intercam)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_intercam)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Oferta de \n intercambio sexual")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_intercam_hist

g_aco_redes_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                    grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_redes)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_redes)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Acoso sexual por \n llamada, correo o redes sociales")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_redes_hist

g_aco_viola_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" & 
                                    grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_viola)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_viola)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Violación")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_viola_hist

g_aco_exhi_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" & 
                                   grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_exhi)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_exhi)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Exhibicionismo")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_exhi_hist

g_aco_toca_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                   grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_toca)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_toca)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Contacto físico no consentido")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_toca_hist

g_aco_pic_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                  grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_pic)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_pic)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Contenido sexual \n no solicitado")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_pic_hist

g_aco_escenas_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= aco_escenas)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(aco_escenas)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 1.5 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("Ver escenas sexuales \n o pornografía")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_aco_escenas_hist


plot_aco_CDMX_hist <- ggarrange(
  g_aco_piropos_hist, 
  g_aco_intento_vio_hist,
  g_aco_intercam_hist,
  g_aco_redes_hist,
  g_aco_viola_hist,
  g_aco_exhi_hist,
  g_aco_toca_hist,
  g_aco_pic_hist,
  g_aco_escenas_hist,
  common.legend = TRUE,
  legend = "bottom", 
  ncol = 3, 
  nrow = 3
) %>%
  annotate_figure(
    top = text_grob(
      expression(
        atop(
          bold("CDMX: Violencia sexual y acoso según tipo de incidente por sexo"), 
          scriptstyle("INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")
        )
      )
    ),
    bottom = text_grob(
      expression(
        scriptstyle("Se toma en cuenta la no respuesta para el cálculo del porcentaje.")
      )
    )
  )

plot_aco_CDMX_hist


g_acos_intimi_hist<-ggsurvey(subset(dm_CB_agr,  
                                    NOM_ENT == "CIUDAD DE MEXICO" &
                                      grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= acos_intimi)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(acos_intimi)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("CDMX: Intimidación sexual",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_acos_intimi_hist


g_acos_abuso_hist<-ggsurvey(subset(dm_CB_agr,  
                                   NOM_ENT == "CIUDAD DE MEXICO" &
                                   grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= acos_abuso)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(acos_abuso)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("CDMX: Abuso sexual",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_acos_abuso_hist


g_acos_acoso_hist<-ggsurvey(subset(dm_CB_agr,  
                                   NOM_ENT == "CIUDAD DE MEXICO" &
                                   grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill=acos_acoso)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(acos_acoso)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("CDMX: Acoso sexual",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_acos_acoso_hist



g_acos_viola_hist<-ggsurvey(subset(dm_CB_agr,  
                                   NOM_ENT == "CIUDAD DE MEXICO" &
                                   grepl("^(T2|T4)", trimestre))) +  
  aes(x = trimestre, fill= acos_viola)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(acos_viola)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+ 
  ggtitle("CDMX: Violación o intento de violación",
          subtitle = "INEGI, Encuesta Nacional de Seguridad Urbana, Junio 2025")+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~SEXO)

g_acos_viola_hist


g_conf_pres<-ggsurvey(subset(dm_CB_agr,  
                                   NOM_ENT == "CIUDAD DE MEXICO" &
                                     grepl("^(T1|T3)", trimestre))) +  
  aes(x = trimestre, fill= conf_pres)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(conf_pres)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "",
    fill = "",
    title = "CDMX: Confianza en el gobierno federal",
    subtitle = "Encuesta Nacional de Seguridad Urbana ENSU, Marzo 2025",
    caption = "Nota: Para los Trimestres I-2023 a I-2024 se recodifica la escala de 0 a 10 a las categorías usadas del trimestre III-2024 en adelante."
  )+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())

g_conf_pres


g_conf_gob<-ggsurvey(subset(dm_CB_agr,  
                             NOM_ENT == "CIUDAD DE MEXICO" &
                               grepl("^(T1|T3)", trimestre))) +  
  aes(x = trimestre, fill= conf_gob)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(conf_gob)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "",
    fill = "",
    title = "CDMX: Confianza en el Gobierno de la Ciudad",
    subtitle = "Encuesta Nacional de Seguridad Urbana ENSU, Marzo 2025",
    caption = "Nota: Para los Trimestres I-2023 a I-2024 se recodifica la escala de 0 a 10 a las categorías usadas del trimestre III-2024 en adelante."
  )+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())

g_conf_gob

g_conf_alc<-ggsurvey(subset(dm_CB_agr,  
                            NOM_ENT == "CIUDAD DE MEXICO" &
                              grepl("^(T1|T3)", trimestre))) +  
  aes(x = trimestre, fill= conf_alc)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(conf_alc)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "",
    fill = "",
    title = "CDMX: Confianza en la Alcaldía de su demarcación",
    subtitle = "Encuesta Nacional de Seguridad Urbana ENSU, Marzo 2025",
    caption = "Nota: Para los Trimestres I-2023 a I-2024 se recodifica la escala de 0 a 10 a las categorías usadas del trimestre III-2024 en adelante."
  )+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())

g_conf_alc


#Confianza en fuerzas de seguridad
g_conf_ssc<-ggsurvey(subset(dm_CB_agr,  
                            NOM_ENT == "CIUDAD DE MEXICO" & !is.na(BP1_9_2))) +  
  aes(x = trimestre, fill= BP1_9_2)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(BP1_9_2)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "",
    fill = "",
    title = "CDMX: Confianza en la Policía de la Ciudad (estatal)",
    subtitle = "Encuesta Nacional de Seguridad Urbana ENSU, Junio 2025"
    )+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())

g_conf_ssc

g_conf_gn<-ggsurvey(subset(dm_CB_agr,  
                            NOM_ENT == "CIUDAD DE MEXICO" & !is.na(BP1_9_3))) +  
  aes(x = trimestre, fill= BP1_9_3)+ geom_bar(position = "fill", width = .5)+
  labs( y = "", x = "", fill = "")+
  geom_text(data = . %>% filter(!is.na(BP1_9_3)),
            aes(label = unlist(tapply(..count.., list(..x.., ..PANEL..),function(a) paste(round(100*a/sum(a), 1), '%'))),
                y = ..count.. ), stat = "count", position = position_fill(vjust = .5), size = 3 )+ 
  theme_minimal() + theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "",
    fill = "",
    title = "CDMX: Confianza en la Guardia Nacional",
    subtitle = "Encuesta Nacional de Seguridad Urbana ENSU, Junio 2025"
    )+
  theme(plot.title = element_text(hjust = 0))+
  scale_y_continuous(labels = scales::percent_format())

g_conf_gn



