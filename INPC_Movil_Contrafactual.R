  
  library(readr)
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(ggplot2)
  library(estimatr)
    
  # Limpieza INPC móvil quincenal -------------------------------------------
  
  datos <- read_csv("C:/Users/ivang/Downloads/Data/Data/INPC_MOVIL.csv")
  # Primero verificar que tengamos las columnas de ANIO, MES y VALOR de la
  # variable a graficar
  names(datos)
  datos$Periodos
  datos$`Precios e Inflación (NUEVO) > Índice nacional de precios al consumidor. Base segunda quincena de Julio 2018 (quincenal) > Clasificación del consumo individual por finalidades (CCIF) quincenal > Nacional > 08 Comunicaciones > 08.3 Servicios telefónicos y de facsímile > 08.3.0 Servicios telefónicos y de facsímile > 237 Servicio de telefonía móvil  /f1`
  
  # Selección de columnas y filtrado de la base
  
  # Verificar que los datos esten en una misma periodicidad.
  # Esto es que no haya datos anuales, trimestrales o mensuales mezclados
  
  datos$FECHA <- as.Date(datos$Periodos, format = "%d/%m/%Y")
  datos$FECHA
  
  datos$ANIO <- as.numeric(format(datos$FECHA, "%Y"))
  datos$MES <- as.numeric(format(datos$FECHA, "%m"))
  datos$QUINCENA <- as.numeric(format(datos$FECHA, "%d"))
  datos$VALOR <- datos$`Precios e Inflación (NUEVO) > Índice nacional de precios al consumidor. Base segunda quincena de Julio 2018 (quincenal) > Clasificación del consumo individual por finalidades (CCIF) quincenal > Nacional > 08 Comunicaciones > 08.3 Servicios telefónicos y de facsímile > 08.3.0 Servicios telefónicos y de facsímile > 237 Servicio de telefonía móvil  /f1`
  datos <- datos %>% mutate(DIA = case_when(QUINCENA== 2 ~ 15,
                                            T ~ QUINCENA))
  # check <- datos %>% filter((ANIO == 2012 & MES>10)|(ANIO == 2013 & MES == 1))# 6 observaciones de las 4 se interpolaran
  
  x <- c(1,8)
  y <- c(datos$VALOR[datos$ANIO == 2012 & datos$MES == 10 & datos$QUINCENA == 2],
         datos$VALOR[datos$ANIO == 2013 & datos$MES == 2 & datos$QUINCENA == 1])
  
  datos <- datos %>% mutate(VALOR = case_when((ANIO == 2012 & MES == 11 & QUINCENA == 1) ~ approx(x, y, xout = 2)$y,
                                              (ANIO == 2012 & MES == 11 & QUINCENA == 2) ~ approx(x, y, xout = 3)$y,
                                              (ANIO == 2012 & MES == 12 & QUINCENA == 1) ~ approx(x, y, xout = 4)$y,
                                              (ANIO == 2012 & MES == 12 & QUINCENA == 2) ~ approx(x, y, xout = 5)$y,
                                              (ANIO == 2013 & MES == 1 & QUINCENA == 1) ~ approx(x, y, xout = 6)$y,
                                              (ANIO == 2013 & MES == 1 & QUINCENA == 2) ~ approx(x, y, xout = 7)$y,
                                              T ~ VALOR
  ))
  
  
  
  
  datos_filtrados <- datos %>% select(ANIO, MES, DIA, VALOR)
  # 
  
  
  # fechas de contrafactuales ----------------------------------------
  
  
  anio_inicio <-  2011 # Información dispobile desde el 2011
  mes_inicio <- 1
  anio_fin <- 2024
  mes_fin <- 12
  # Fecha de la Reforma de telecomunicaciones
  anio_evento <- 2013
  mes_evento <- 7
  
  anio_inicio2 <- anio_evento
  mes_inicio2 <- mes_evento
  anio_fin2 <- 2024
  mes_fin2 <- 12
  # Fecha de entrada de AT&T
  anio_evento2 <- 2015
  mes_evento2 <- 10
  
  
  # Calculo de las variables de rezago --------------------------------------------
  
  datos_filtrados <- datos_filtrados %>% arrange(ANIO,MES)
  datos_filtrados$VALOR_LAG_1 <-  lag(datos_filtrados$VALOR,1)
  
  # Partición de datos en preevento y postevento ----------------------------
  
  dato_preevento <- datos_filtrados %>% 
    filter((ANIO >= anio_inicio & ANIO < anio_evento) | 
             (ANIO == anio_evento & MES <= mes_evento))
  
  dato_postevento <- datos_filtrados %>% 
    filter((ANIO <= anio_fin & ANIO > anio_evento) | 
             (ANIO == anio_evento & MES > mes_evento))
  
  dato_preevento2 <- datos_filtrados %>% 
    filter((ANIO >= anio_inicio2 & MES >= mes_evento& ANIO < anio_evento2) | 
             (ANIO == anio_evento2 & MES <= mes_evento2))
  
  dato_postevento2 <- datos_filtrados %>% 
    filter((ANIO <= anio_fin2 & ANIO > anio_evento2) | 
             (ANIO == anio_evento2 & MES > mes_evento2))
  
  dato_postevento <- dato_postevento %>% arrange(ANIO,MES)
  dato_preevento <- dato_preevento %>% arrange(ANIO,MES)
  
  dato_postevento2 <- dato_postevento2 %>% arrange(ANIO,MES)
  dato_preevento2 <- dato_preevento2 %>% arrange(ANIO,MES)
  
  # modelo preevento --------------------------------------------------------
  
  modelo_preevento <- lm(formula = VALOR ~ VALOR_LAG_1,
                         data = dato_preevento)
  
  summary(modelo_preevento)
  coef(modelo_preevento)[1]
  predict(modelo_preevento)
  
  coef(modelo_preevento)[1]+coef(modelo_preevento)[2]*dato_preevento$VALOR_LAG_1[20]
  
  dato_preevento$PREDICHO <- c(predict(modelo_preevento))# lineas cada 100
  #dato_preevento$PREDICHO <- c(NA, predict(modelo_preevento))#Ebitda
  # dato_preevento$PREDICHO <- c(predict(modelo_preevento), NA)
  
  # bptest(modelo_preevento)
  # 
  # dwtest(modelo_preevento)
  # library(estimatr)
  
  plot(modelo_preevento$residuals)
  
  modelo_preevento2 <- lm(formula = VALOR ~ VALOR_LAG_1,
                          data = dato_preevento2)
  
  summary(modelo_preevento2)
  modelo_preevento_errores_robustos2 <- coeftest(modelo_preevento2, 
                                                 vcov = vcovHC(modelo_preevento2, 
                                                               type="HC1"))
  
  dato_preevento2$PREDICHO <- predict(modelo_preevento2)
  modelo_preevento_errores_robustos2
  
  # modelo postevento -------------------------------------------------------
  
  modelo_postevento <- lm(formula = VALOR ~ VALOR_LAG_1,
                          data = dato_postevento)
  
   modelo_postr <- lm_robust(formula = VALOR ~ VALOR_LAG_1,
                             se_type = "HC3", 
                             data = dato_postevento)
  
  summary(modelo_postevento)
  
  modelo_postevento$residuals
  residuos_postr <- dato_postevento$VALOR - modelo_postr$fitted.values
  
  
  
  modelo_postevento_errores_robustos <- coeftest(modelo_postevento, 
                                                 vcov = vcovHC(modelo_postevento, 
                                                               type="HC1"))
  modelo_postevento_errores_robustos
  
  modelo_postevento2 <- lm(formula = VALOR ~ VALOR_LAG_1,
                           data = dato_postevento2)
  
  summary(modelo_postevento2)
  modelo_postevento_errores_robustos2 <- coeftest(modelo_postevento2, 
                                                  vcov = vcovHC(modelo_postevento2, 
                                                                type="HC1"))
  modelo_postevento_errores_robustos2
  
  
  # Errores y coeficientes --------------------------------------------------
  
  # extraemos la información para generar el contrafactual
  
  errores_preevento <- modelo_preevento$residuals
  errores_postevento <- modelo_postevento$residuals
  beta_cero_pre <- coef(modelo_preevento)[1]
  beta_uno_pre <- coef(modelo_preevento)[2]
  
  errores_postevento2 <- modelo_postevento2$residuals
  beta_cero_pre2 <- coef(modelo_preevento2)[1]
  beta_uno_pre2 <- coef(modelo_preevento2)[2]
  
  # dato_postevento$errores_postevento <- c(NA,errores_postevento)
  
  # Construcción del contrafactual ------------------------------------------
  
  contrafactual <- c()
  contrafactual2 <- c()
  
  # contrafactual[1] = beta_cero_pre + beta_uno_pre * dato_postevento$VALOR_LAG_1[2] + errores_postevento[2]
  
  for (i in 1:nrow(dato_postevento)) {
    if (i == 1){
      contrafactual[i] = beta_cero_pre + beta_uno_pre * dato_postevento$VALOR_LAG_1[i] + errores_postevento[i]
    }
    else{
      contrafactual[i] = beta_cero_pre + beta_uno_pre * contrafactual[i-1] + errores_postevento[i]
    }
  }
  
  for (i in 1:nrow(dato_postevento2)) {
    if (i == 1){
      contrafactual2[i] = beta_cero_pre2 + beta_uno_pre2 * dato_postevento2$VALOR_LAG_1[i] + errores_postevento2[i]
    }
    else{
      contrafactual2[i] = beta_cero_pre2 + beta_uno_pre2 * contrafactual2[i-1] + errores_postevento2[i]
    }
  }
  
  # dato_postevento <- dato_postevento %>% 
  #   mutate(contrafactual = beta_cero_pre + beta_uno_pre * VALOR_LAG_1 + errores_postevento)
  
  dato_postevento$contrafactual <- contrafactual
  dato_postevento2$contrafactual <- contrafactual2
  
  dato_postevento <- dato_postevento %>% filter(!is.na(contrafactual))
  dato_postevento <- bind_rows(dato_preevento, dato_postevento)
  
  dato_postevento2 <- dato_postevento2 %>% filter(!is.na(contrafactual2))
  dato_postevento2 <- bind_rows(dato_preevento2, dato_postevento2)
  
  fecha_evento <- as.Date(paste(anio_evento, mes_evento, "01", sep = "-"))
  fecha_evento2 <- as.Date(paste(anio_evento2, mes_evento2, "01", sep = "-"))
  
  nombre_evento <- "Reforma Telecom"
  nombre_evento2 <- "Entrada AT&T"
  
  
  # INPC Móvil ---------------------------------------------------------------
  
  
  ggplot() +
    geom_line(data = dato_postevento, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")), 
                                          y = contrafactual, 
                                          color = "Contrafactual Reforma"), 
              linetype = "dashed",
              size = 1) +
    geom_line(data = dato_postevento2, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")),
                                           y = contrafactual,
                                           color = "Contrafactual AT&T"),
              linetype = "dashed",
              size = 1) +
    geom_line(data = dato_postevento, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")), 
                                          y = VALOR, color = "Observado"), 
              size = 1) +
    geom_line(data = dato_postevento, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")),
                                          y = PREDICHO,
                                          color = "Ajuste modelo AR(1) \n pre Reforma \n"),
              linetype = "dashed",
              size = 1) +
    geom_line(data = dato_postevento2, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")),
                                           y = PREDICHO,
                                           color = "Ajuste modelo AR(1) \n pre AT&T\n"),
              linetype = "dashed",
              size = 1) +
    labs(
      # title = paste(var_interes,"comparación de conjuntos:\n Observado vs Contrafactual",sep = " "),
      x = "\nAño", y = "INPC móvil \n") +
    # scale_y_continuous(
    #   limits = c(0, 10000),
    #   breaks = seq(0, 10000, by = 1000)
    # ) +
    scale_x_date(
      date_breaks = "year",
      date_labels = "%Y",
      # breaks = as.Date(paste0(unique(dato_postevento$ANIO), "-01-01")),
      # labels = function(x) ifelse(format(x, "%m") == "03",format(x, "%Y"),"")
    )+
    # scale_x_date(date_breaks = "3 months", 
    #              date_labels = "%Y-Q%q") +
    geom_vline(xintercept = as.numeric(fecha_evento), 
               linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = fecha_evento, y = Inf, label = nombre_evento, 
             angle = 90, vjust = -0.5, hjust = 1, size = 5) +
    geom_vline(xintercept = as.numeric(fecha_evento2), 
               linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = fecha_evento2, y = Inf, label = nombre_evento2, 
             angle = 90, vjust = -0.5, hjust = 1, size = 5) +
    # scale_color_manual(values = c("Observado" = "blue", "Contrafactual" = "red")) +
    scale_color_manual(
      name = "Series",  # <-- Aquí cambias el título de la leyenda
      values = c("Observado" = "blue", "Contrafactual Reforma" = "red", 
                 "Contrafactual AT&T" = "green", 
                 "Ajuste modelo AR(1) \n pre Reforma \n" = "orange", 
                 "Ajuste modelo AR(1) \n pre AT&T\n" = "cyan")
    )+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = c(0.95, 1),  # Esquina superior derecha
          legend.justification = c(1, 1),  # Justifica a la esquina superior derecha
          legend.background = element_rect(fill = NA, color = NA, size = 0.5),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9))
  
  ggplot() +
    geom_line(data = dato_postevento, aes(x = as.Date(paste(ANIO, MES, "01", sep = "-")), 
                                          y = VALOR, color = "Nacional"), 
              size = 1) +
    labs(
      # title = paste(var_interes,"comparación de conjuntos:\n Observado vs Contrafactual",sep = " "),
      x = "\nAño", y = "INPC móvil \n") +
    # scale_y_continuous(
    #   limits = c(0, 10000),
    #   breaks = seq(0, 10000, by = 1000)
    # ) +
    scale_x_date(
      date_breaks = "year",
      date_labels = "%Y",
      # breaks = as.Date(paste0(unique(dato_postevento$ANIO), "-01-01")),
      # labels = function(x) ifelse(format(x, "%m") == "03",format(x, "%Y"),"")
    )+
    # scale_x_date(date_breaks = "3 months", 
    #              date_labels = "%Y-Q%q") +
    geom_vline(xintercept = as.numeric(fecha_evento), 
               linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = fecha_evento, y = Inf, label = nombre_evento, 
             angle = 90, vjust = -0.5, hjust = 1, size = 5) +
    geom_vline(xintercept = as.numeric(fecha_evento2), 
               linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = fecha_evento2, y = Inf, label = nombre_evento2, 
             angle = 90, vjust = -0.5, hjust = 1, size = 5) +
    # scale_color_manual(values = c("Observado" = "blue", "Contrafactual" = "red")) +
    scale_color_manual(
      name = "Serie",  # <-- Aquí cambias el título de la leyenda
      values = c("Nacional" = "blue", "Contrafactual Reforma" = "red", 
                 "Contrafactual AT&T" = "green", 
                 "Ajuste modelo AR(1) \n pre Reforma \n" = "orange", 
                 "Ajuste modelo AR(1) \n pre AT&T\n" = "cyan")
    )+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = c(0.95, 1),  # Esquina superior derecha
          legend.justification = c(1, 1),  # Justifica a la esquina superior derecha
          legend.background = element_rect(fill = NA, color = NA, size = 0.5),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9))