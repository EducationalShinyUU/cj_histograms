server <- function(input, output, session) {
  
  table_bins <- function(bins){
    out_tab <- rep(0, 10)
    tab_bins <- table(bins)
    out_tab[as.numeric(names(tab_bins))] <- tab_bins
    out_tab
  }
  
  update_data <- function(data, n_men, n_women, mean_men, mean_women, sd_men, sd_women, resample = FALSE){
    if(resample){
      data <- data.frame(y = c(rnorm(n_men,mean_men, sd_men), 
                               rnorm(n_women,mean_women, sd_women)),
                         Sex = c(rep("Men", n_men), rep("Women", n_women)))
      
    } else {
      data$y[data$Sex == "Men"] <- ((scale(data$y[data$Sex == "Men"]) * sd_men) + mean_men)
      data$y[data$Sex == "Women"] <- ((scale(data$y[data$Sex == "Women"]) * sd_women) + mean_women)
      if(n_men > sum(data$Sex == "Men")){
        data <- rbind(data, data.frame(y = rnorm((n_men-sum(data$Sex == "Men")),mean_men, sd_men),
                           Sex = rep("Men", (n_men-sum(data$Sex == "Men")))))
        data <- data[order(data$Sex), ]
      }
      if(n_women > sum(data$Sex == "Women")){
        data <- rbind(data, data.frame(y = rnorm((n_women-sum(data$Sex == "Women")),mean_women, sd_women),
                                       Sex = rep("Women", (n_women-sum(data$Sex == "Women")))))
        data <- data[order(data$Sex), ]
      }
      if(n_men < sum(data$Sex == "Men")){
        data <- data[-sample.int(sum(data$Sex == "Men"), (sum(data$Sex == "Men")-n_men)), ]
      }
      if(n_women < sum(data$Sex == "Women")){
        data <- data[-(sum(data$Sex == "Men") + sample.int(sum(data$Sex == "Women"), (sum(data$Sex == "Women")-n_women))), ]
      }
    }
    data$y[which(data$y < 0)] <- 0
    data$y[which(data$y > 10)] <- 10
    return(data)
  }
  
  plot_xrange = c(0, 10)
  n_men = 10
  n_women = 10
  mean_men = 5
  sd_men = 1
  mean_women = 5
  sd_women = 1
  data <- update_data(data = NULL, 
                      n_men = n_men,
                      n_women = n_women,
                      mean_men = mean_men,
                      mean_women = mean_women,
                      sd_men = sd_men,
                      sd_women = sd_women,
                      resample = TRUE)
  
  reactive_data <- reactiveValues(plot_xrange = plot_xrange,
                                  n_men = n_men,
                                  n_women = n_women,
                                  mean_men = mean_men,
                                  mean_women = mean_women,
                                  sd_men = sd_men,
                                  sd_women = sd_women, 
                                  data = data,
                                  #data = data.frame(y = rep(0:10, 2), Sex = rep(c("Men", "Women"), each = 11)),
                                  output_text = ""
  )
  
  observeEvent(c(input$scale_dat, input$mean1, input$mean2, input$sd1, input$sd2, input$N1, input$N2, input$diff, input$sd_overall, input$N_overall), {
    if(input$full_ui){
      reactive_data$data <- update_data(data = reactive_data$data, 
                                        n_men = input$N1,
                                        n_women = input$N2,
                                        mean_men = input$mean1,
                                        mean_women = input$mean2,
                                        sd_men = input$sd1,
                                        sd_women = input$sd2,
                                        resample = FALSE)  
    } else {
      
      reactive_data$data <- update_data(data = reactive_data$data, 
                                        n_men = ceiling(.5 * input$N_overall),
                                        n_women = floor(.5 * input$N_overall),
                                        mean_men = (5 - .5 * input$diff),
                                        mean_women = (5 + .5 * input$diff),
                                        sd_men = input$sd_overall,
                                        sd_women = input$sd_overall,
                                        resample = FALSE)
    }

  })
  
  observeEvent(input$resample_dat, {
    reactive_data$data <- update_data(data = reactive_data$data, 
                                      n_men = input$N1,
                                      n_women = input$N2,
                                      mean_men = input$mean1,
                                      mean_women = input$mean2,
                                      sd_men = input$sd1,
                                      sd_women = input$sd2,
                                      resample = TRUE)
  })
  observeEvent(input$resample_dat2, {
    reactive_data$data <- update_data(data = reactive_data$data, 
                                      n_men = ceiling(.5 * input$N_overall),
                                      n_women = floor(.5 * input$N_overall),
                                      mean_men = (5 - .5 * input$diff),
                                      mean_women = (5 + .5 * input$diff),
                                      sd_men = input$sd_overall,
                                      sd_women = input$sd_overall,
                                      resample = TRUE)
  })
  
  
  
  output$anova_results <- renderTable(
    data.frame(
    x = rep(seq(1, 10, 1), 2),
    y = c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Men"], seq(0, 10, length.out = 11), labels = FALSE)),
          table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Women"], seq(0, 10, length.out = 11), labels = FALSE))
    ),
    Sex = c(rep("Men", 10), rep("Women", 10)))
  )
  output$anova_plot <- renderPlot({
    p <- ggplot(data.frame(
      x = factor(rep(seq(1, 10, 1), 2)),
      y = c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Men"], seq(0, 10, length.out = 11), labels = FALSE)),
            table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Women"], seq(0, 10, length.out = 11), labels = FALSE))
      ),
      Sex = c(rep("Men", 10), rep("Women", 10))
    ), aes(x, y, fill = Sex)) + 
      geom_bar(stat = "identity",
                     colour = "black", alpha = .2, width = 1) + 

      theme_minimal() + 
      theme(strip.text = element_blank(),
            axis.title.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color="black")) +
      scale_x_discrete(expand = c(0, 0)) + #, limits = reactive_data$plot_xrange, breaks = seq(0, 10, length.out = 6)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Men"], seq(0, 10, length.out = 11), labels = FALSE)),
                                                               table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Women"], seq(0, 10, length.out = 11), labels = FALSE))))
                                                      )) +
      scale_fill_manual(name = NULL, values = c("blue", "red"))+
      facet_grid(Sex ~ .) + 
      ylab("Frequency")
    pb <- ggplot_build(p)
    pg <- ggplot_gtable(pb)
    hist_means <- scales::rescale(c(pb$layout$panel_ranges[[1L]][["x.range"]], 
                                    mean(reactive_data$data$y[reactive_data$data$Sex == "Men"]),
                                    mean(reactive_data$data$y[reactive_data$data$Sex == "Women"])
                                    ), c(0,1))[-c(1,2)]
    pg <- gtable_add_grob(pg, 
      segmentsGrob(x0=hist_means, x1=hist_means, y0=c(0,0), y1=c(1,1), gp=gpar(col = c("blue", "red"))),
      t=8, b=6, l=4)
    grid.newpage()
    grid.draw(pg)
  })
  
    
  
  
  output$stats <- renderUI({
    withMathJax(
      helpText(paste(c(
        "\\(M_1 - M_2 =\\)", 
        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Men"]), digits = 2, format = "f"), 
        "-", 
        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Women"]), digits = 2, format = "f"),
        "\\(=\\)",
        formatC((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"])), digits = 2, format = "f")), 
        collapse = " ")),
      helpText(paste(c(
        "\\(SD_{pooled} =\\)",
        formatC(sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])+var(reactive_data$data$y[reactive_data$data$Sex == "Women"]))/2), digits = 2, format = "f")
        ), 
        collapse = " ")),
      helpText(paste(c(
        "\\(SE = \\sqrt{\\frac{",
        formatC(sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])+var(reactive_data$data$y[reactive_data$data$Sex == "Women"]))/2), digits = 2, format = "f"),
        "^2}{",
        formatC(reactive_data$n_men + reactive_data$n_women, digits = 0, format = "f"),
        "} + \\frac{",
        formatC(sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])+var(reactive_data$data$y[reactive_data$data$Sex == "Women"]))/2), digits = 2, format = "f"),
        "^2}{",
        formatC(reactive_data$n_men + reactive_data$n_women, digits = 0, format = "f"),
        "}} =\\)",
        " ",
        formatC(
          sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])/reactive_data$n_men)+
              (var(reactive_data$data$y[reactive_data$data$Sex == "Women"])/reactive_data$n_women))
          , digits = 2, format = "f")
        ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(t = \\frac{M_1 - M_2}{SE} = \\frac{",
        formatC((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"])), digits = 2, format = "f"),
        "}{",
        formatC(
          sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])/reactive_data$n_men)+
                 (var(reactive_data$data$y[reactive_data$data$Sex == "Women"])/reactive_data$n_women))
          , digits = 2, format = "f"),
        "} = \\)",
        " ",
        formatC(((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"]))) /
                  sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])/reactive_data$n_men)+
                 (var(reactive_data$data$y[reactive_data$data$Sex == "Women"])/reactive_data$n_women))
          , digits = 2, format = "f")
        ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(p =\\)",
        " ",
        formatC(
          2*pt(abs(((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"]))) /
               sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])/reactive_data$n_men)+
                      (var(reactive_data$data$y[reactive_data$data$Sex == "Women"])/reactive_data$n_women))), 
             (reactive_data$n_men+reactive_data$n_women-2), lower.tail = FALSE)
                , digits = 2, format = "f")
      ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(d = \\frac{M_1 - M_2}{SD_{pooled}} = \\frac{",
        formatC((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"])), digits = 2, format = "f"),
        "}{",
        formatC(sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])+var(reactive_data$data$y[reactive_data$data$Sex == "Women"]))/2), digits = 2, format = "f"),
        "} = \\)",
        " ",
        formatC((mean(reactive_data$data$y[reactive_data$data$Sex == "Men"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Women"])) / 
                  sqrt((var(reactive_data$data$y[reactive_data$data$Sex == "Men"])+var(reactive_data$data$y[reactive_data$data$Sex == "Women"]))/2), digits = 2, format = "f")
      ), 
      collapse = "")
      )
    )
    
    
    
    
  })
   # renderText({
  #  cat("$$M_1 - M_2 = $$", 
  #        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Men"]), digits = 2, format = "f"), 
  #        "-", 
  #        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Women"]), digits = 2, format = "f"))
    
    #reactive_data$output_text
  #})
  
  
}

