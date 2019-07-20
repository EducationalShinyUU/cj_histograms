server <- function(input, output, session) {
  
  table_bins <- function(bins){
    out_tab <- rep(0, 10)
    tab_bins <- table(bins)
    out_tab[as.numeric(names(tab_bins))] <- tab_bins
    out_tab
  }
  
  update_data <- function(data, n_gr1, n_gr2, mean_gr1, mean_gr2, sd_gr1, sd_gr2, resample = FALSE){
    if(resample){
      data <- data.frame(y = c(rnorm(n_gr1,mean_gr1, sd_gr1), 
                               rnorm(n_gr2,mean_gr2, sd_gr2)),
                         Sex = c(rep("Group 1", n_gr1), rep("Group 2", n_gr2)))
      
    } else {
      data$y[data$Sex == "Group 1"] <- ((scale(data$y[data$Sex == "Group 1"]) * sd_gr1) + mean_gr1)
      data$y[data$Sex == "Group 2"] <- ((scale(data$y[data$Sex == "Group 2"]) * sd_gr2) + mean_gr2)
      if(n_gr1 > sum(data$Sex == "Group 1")){
        data <- rbind(data, data.frame(y = rnorm((n_gr1-sum(data$Sex == "Group 1")),mean_gr1, sd_gr1),
                           Sex = rep("Group 1", (n_gr1-sum(data$Sex == "Group 1")))))
        data <- data[order(data$Sex), ]
      }
      if(n_gr2 > sum(data$Sex == "Group 2")){
        data <- rbind(data, data.frame(y = rnorm((n_gr2-sum(data$Sex == "Group 2")),mean_gr2, sd_gr2),
                                       Sex = rep("Group 2", (n_gr2-sum(data$Sex == "Group 2")))))
        data <- data[order(data$Sex), ]
      }
      if(n_gr1 < sum(data$Sex == "Group 1")){
        data <- data[-sample.int(sum(data$Sex == "Group 1"), (sum(data$Sex == "Group 1")-n_gr1)), ]
      }
      if(n_gr2 < sum(data$Sex == "Group 2")){
        data <- data[-(sum(data$Sex == "Group 1") + sample.int(sum(data$Sex == "Group 2"), (sum(data$Sex == "Group 2")-n_gr2))), ]
      }
    }
    data$y[which(data$y < 0)] <- 0
    data$y[which(data$y > 10)] <- 10
    return(data)
  }
  
  plot_xrange = c(0, 10)
  n_gr1 = 10
  n_gr2 = 10
  mean_gr1 = 5
  sd_gr1 = 1
  mean_gr2 = 5
  sd_gr2 = 1
  data <- update_data(data = NULL, 
                      n_gr1 = n_gr1,
                      n_gr2 = n_gr2,
                      mean_gr1 = mean_gr1,
                      mean_gr2 = mean_gr2,
                      sd_gr1 = sd_gr1,
                      sd_gr2 = sd_gr2,
                      resample = TRUE)
  
  reactive_data <- reactiveValues(plot_xrange = plot_xrange,
                                  n_gr1 = n_gr1,
                                  n_gr2 = n_gr2,
                                  mean_gr1 = mean_gr1,
                                  mean_gr2 = mean_gr2,
                                  sd_gr1 = sd_gr1,
                                  sd_gr2 = sd_gr2, 
                                  data = data,
                                  output_text = ""
  )
  
  observeEvent(c(input$scale_dat, input$mean1, input$mean2, input$sd1, input$sd2, input$N1, input$N2, input$diff, input$sd_overall, input$N_overall), {
    if(input$full_ui){
      reactive_data$data <- update_data(data = reactive_data$data, 
                                        n_gr1 = input$N1,
                                        n_gr2 = input$N2,
                                        mean_gr1 = input$mean1,
                                        mean_gr2 = input$mean2,
                                        sd_gr1 = input$sd1,
                                        sd_gr2 = input$sd2,
                                        resample = FALSE)
      reactive_data$n_gr1 <- sum(reactive_data$data$Sex == "Group 1")
      reactive_data$n_gr2 <- sum(reactive_data$data$Sex == "Group 2")
      reactive_data$mean_gr1 <- mean(reactive_data$data[reactive_data$data$Sex == "Group 1", ]$y)
      reactive_data$mean_gr2 <- mean(reactive_data$data[reactive_data$data$Sex == "Group 2", ]$y)
      reactive_data$sd_gr1 <- sd(reactive_data$data[reactive_data$data$Sex == "Group 1", ]$y)
      reactive_data$sd_gr2 <- sd(reactive_data$data[reactive_data$data$Sex == "Group 2", ]$y)
      
    } else {
      
      reactive_data$data <- update_data(data = reactive_data$data, 
                                        n_gr1 = ceiling(.5 * input$N_overall),
                                        n_gr2 = floor(.5 * input$N_overall),
                                        mean_gr1 = (5 + .5 * input$diff),
                                        mean_gr2 = (5 - .5 * input$diff),
                                        sd_gr1 = input$sd_overall,
                                        sd_gr2 = input$sd_overall,
                                        resample = FALSE)
      reactive_data$n_gr1 <- sum(reactive_data$data$Sex == "Group 1")
      reactive_data$n_gr2 <- sum(reactive_data$data$Sex == "Group 2")
      reactive_data$mean_gr1 <- mean(reactive_data$data[reactive_data$data$Sex == "Group 1", ]$y)
      reactive_data$mean_gr2 <- mean(reactive_data$data[reactive_data$data$Sex == "Group 2", ]$y)
      reactive_data$sd_gr1 <- sd(reactive_data$data[reactive_data$data$Sex == "Group 1", ]$y)
      reactive_data$sd_gr2 <- sd(reactive_data$data[reactive_data$data$Sex == "Group 2", ]$y)
      
    }

  })
  
  observeEvent(input$resample_dat, {
    reactive_data$data <- update_data(data = reactive_data$data, 
                                      n_gr1 = input$N1,
                                      n_gr2 = input$N2,
                                      mean_gr1 = input$mean1,
                                      mean_gr2 = input$mean2,
                                      sd_gr1 = input$sd1,
                                      sd_gr2 = input$sd2,
                                      resample = TRUE)
  })
  observeEvent(input$resample_dat2, {
    reactive_data$data <- update_data(data = reactive_data$data, 
                                      n_gr1 = ceiling(.5 * input$N_overall),
                                      n_gr2 = floor(.5 * input$N_overall),
                                      mean_gr1 = (5 + .5 * input$diff),
                                      mean_gr2 = (5 - .5 * input$diff),
                                      sd_gr1 = input$sd_overall,
                                      sd_gr2 = input$sd_overall,
                                      resample = TRUE)
  })
  
  
  
  output$anova_results <- renderTable(
    data.frame(
    x = rep(seq(1, 10, 1), 2),
    y = c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 1"], seq(0, 10, length.out = 11), labels = FALSE)),
          table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 2"], seq(0, 10, length.out = 11), labels = FALSE))
    ),
    Sex = c(rep("Group 1", 10), rep("Group 2", 10)))
  )
  output$anova_plot <- renderPlot({
    p <- ggplot(data.frame(
      x = factor(rep(seq(1, 10, 1), 2)),
      y = c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 1"], seq(0, 10, length.out = 11), labels = FALSE)),
            table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 2"], seq(0, 10, length.out = 11), labels = FALSE))
      ),
      Sex = c(rep("Group 1", 10), rep("Group 2", 10))
    ), aes(x = x, y = y, fill = Sex)) + 
      geom_bar(stat = "identity",
                     colour = "black", alpha = .2, width = 1) + 

      theme_minimal() + 
      theme(strip.text = element_blank(),
            axis.title.x = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color="black")) +
      scale_x_discrete(expand = c(0, 0)) + #, limits = reactive_data$plot_xrange, breaks = seq(0, 10, length.out = 6)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(c(table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 1"], seq(0, 10, length.out = 11), labels = FALSE)),
                                                               table_bins(cut(reactive_data$data$y[reactive_data$data$Sex == "Group 2"], seq(0, 10, length.out = 11), labels = FALSE))))
                                                      )) +
      scale_fill_manual(name = NULL, values = c("blue", "red"), labels = c("Group 1", "Group 2"))+
      facet_grid(Sex ~ .) + 
      ylab("Frequency")
    pb <- ggplot_build(p)
    pg <- ggplot_gtable(pb)

    hist_means <- scales::rescale(c(pb$layout$panel_params[[1L]][["x.range"]],
                                    mean(reactive_data$data$y[reactive_data$data$Sex == "Group 1"]),
                                    mean(reactive_data$data$y[reactive_data$data$Sex == "Group 2"])
                                    ), c(0,1))[-c(1,2)]

    pg <- gtable_add_grob(pg,
      segmentsGrob(x0=hist_means, x1=hist_means, y0=c(0,0), y1=c(1,1), gp=gpar(col = c("blue", "red"))),
      t=9, b=2, l=5)
    grid.newpage()
    grid.draw(pg)
  })
  
    
  
  
  output$stats <- renderUI({
    mean_diff <- (mean(reactive_data$data$y[reactive_data$data$Sex == "Group 1"])-mean(reactive_data$data$y[reactive_data$data$Sex == "Group 2"]))
    sd_pooled <- sqrt(
      ((reactive_data$n_gr1-1)*var(reactive_data$data$y[reactive_data$data$Sex == "Group 1"])+
         (reactive_data$n_gr2-1)*var(reactive_data$data$y[reactive_data$data$Sex == "Group 2"]))/
        (reactive_data$n_gr1+reactive_data$n_gr2-2)
    )
    se_pooled <- sqrt((sd_pooled^2/reactive_data$n_gr1)+(sd_pooled^2/reactive_data$n_gr2))
    
    withMathJax(
      helpText(paste(c(
        "\\(M_1 - M_2 =\\)", 
        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Group 1"]), digits = 2, format = "f"), 
        "-", 
        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Group 2"]), digits = 2, format = "f"),
        "\\(=\\)",
        formatC(mean_diff, digits = 2, format = "f")), 
        collapse = " ")),
      helpText(paste(c(
        #"\\(SD_{pooled} = \\sqrt{\\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}} = \\)",
        "\\(SD_{pooled} = \\)",
        formatC(sd_pooled, digits = 2, format = "f")
        ), 
        collapse = " ")),
      helpText(paste(c(
        "\\(SE = \\sqrt{\\frac{",
        formatC(sd_pooled, digits = 2, format = "f"),
        "^2}{",
        formatC(reactive_data$n_gr1, digits = 0, format = "f"),
        "} + \\frac{",
        formatC(sd_pooled, digits = 2, format = "f"),
        "^2}{",
        formatC(reactive_data$n_gr2, digits = 0, format = "f"),
        "}} =\\)",
        " ",
        formatC(
          se_pooled, digits = 2, format = "f")
        ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(t = \\frac{M_1 - M_2}{SE} = \\frac{",
        formatC(mean_diff, digits = 2, format = "f"),
        "}{",
        formatC(se_pooled, digits = 2, format = "f"),
        "} = \\)",
        " ",
        formatC((mean_diff / se_pooled), digits = 2, format = "f")
        ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(p =\\)",
        " ",
        formatC(
          2*pt(abs(mean_diff/se_pooled), (reactive_data$n_gr1+reactive_data$n_gr2-2), lower.tail = FALSE), digits = 3, format = "f")
      ), 
      collapse = "")
      ),
      helpText(paste(c(
        "\\(d = \\frac{M_1 - M_2}{SD_{pooled}} = \\frac{",
        formatC(mean_diff, digits = 2, format = "f"),
        "}{",
        formatC(sd_pooled, digits = 2, format = "f"),
        "} = \\)",
        " ",
        formatC(mean_diff/sd_pooled, digits = 2, format = "f")
      ), 
      collapse = "")
      )
    )
    
    
    
    
  })
   # renderText({
  #  cat("$$M_1 - M_2 = $$", 
  #        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Group 1"]), digits = 2, format = "f"), 
  #        "-", 
  #        formatC(mean(reactive_data$data$y[reactive_data$data$Sex == "Group 2"]), digits = 2, format = "f"))
    
    #reactive_data$output_text
  #})
  
  
}

