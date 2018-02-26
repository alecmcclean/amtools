#' Data Explorer
#'
#' This function allows you to investigate a new dataset with simple visualizations. .
#' @param dataset Dataset to investigate
#' @keywords shiny, data, visualization
#' @export
#' @examples
#' DataExplorer()
DataExplorer <- function(dataset) {

  ### Check if data frame given
  stopifnot(is.data.frame(dataset))

  ### Load require packages
  require(shiny)
  require(tidyverse)
  require(magrittr)


  ##############################3
  ### Useful Helpder Functions
  ##############################3

  CountNum <- function(x, na.rm) {
    ### Count function with same syntzx as mean, max, min.
    as.integer(sum(!is.na(x)))
  }


  MakePrettyNumber <- function(x) {
    ### Choose number of decimal places for
    ### numbers in Pivot Table

    size <- max(abs(x), na.rm = TRUE)

    if (is.integer(x)) {
      dec <- 0

    } else {
      dec <- 0 +
        4 * (size <  1) +
        3 * (size >= 1   & size < 10) +
        2 * (size >= 10  & size < 100) +
        1 * (size >= 100 & size < 1000)

    }

    sprintf(paste0("%.", dec, "f"), x)
  }

  IsDiscrete <- function(vector) {
    ### Tell us whether vector is discrete.
    if ((length(unique(vector)) < 10) | any((!class(vector) %in% c("numeric", "integer")))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }



  #######################################################
  ### Clean Dataset for Input into Shiny
  #######################################################

  ### Clean column names
  colnames(dataset) <-
    paste0(toupper(substr(colnames(dataset), 1, 1)),
           substr(colnames(dataset), 2, nchar(colnames(dataset))))

  ################################################
  ### Categorize variables as discrete / cont
  ################################################

  drop_down_list <-
    dataset %>%
    map(IsDiscrete)

  discrete_vars <-
    drop_down_list %>%
    unlist %>%
    which(TRUE) %>%
    names

  discrete_array <- paste0('["', paste(discrete_vars, collapse = '","'), '"]')


  ##################################################
  ### Create Drop Down List of variables for App
  ##################################################

  drop_down_list <-
    drop_down_list %>%
    lapply(function(x) as.logical(sum(x))) %>%
    lapply(function(x) ifelse(x, " (Discrete)", " (Continuous)"))

  for(i in seq(drop_down_list)) {
    drop_down_list[[i]] <-
      paste0(names(drop_down_list)[[i]],
             drop_down_list[[i]])
  }

  drop_down_list %<>% unname()
  list           <- colnames(dataset)
  names(list)    <- drop_down_list
  drop_down_list <- list

  ### Clean Data Space
  rm(list, i)
  gc()


  ### Discrete Variables
  discrete_drop_down_list <- drop_down_list[grepl("Discrete", names(drop_down_list))]
  discrete_drop_down_list <- c(`No Wrap` = "No Wrap", discrete_drop_down_list)

  ### Continuous Variables
  cont_drop_down_list <- drop_down_list[!grepl("Discrete", names(drop_down_list))]

  dataset %<>% mutate_at(discrete_vars, as.factor)



  ########################################################################
  #### Conditional Statements to Evaluate User Choices
  ########################################################################

  #############################################
  ### Number of Variables Shown
  #############################################

  one_var_condition   <- "(input.num_var == 'one')"
  two_var_condition   <- "(input.num_var == 'two')"

  #############################################
  ### Condition for Main Histogram
  #############################################

  ### One Var, Continuous
  ### Two Vars, Equal and Continuous
  main_histogram_condition <-
    paste0(
      "($.inArray(input.x_var, ", discrete_array, ") == -1) &&",
      "((input.num_var == 'one') || (input.num_var == 'two' && (input.x_var == input.y_var)))"
    )


  #################################
  ### Simple Summary Stats
  #################################

  ### One Var, Discrete
  simple_summary_condition <-
    paste0(
      "input.num_var == 'one' && ($.inArray(input.x_var, ", discrete_array, ") != -1)"
    )

  #############################################
  ### Pivot Table if both x & y discrete
  #############################################

  ### Two Var, Both Discrete
  table_condition <-
    paste0(
      two_var_condition,
      "&&",
      "($.inArray(input.x_var, ", discrete_array, ") != -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") != -1)"
    )

  #################################################
  ### Drop down Histogram if Click in Pivot Table
  #################################################

  sub_histogram_condition <-
    paste0(table_condition,
           "&&",
           "($.inArray(input.fill_var, ", discrete_array, ") == -1)")



  ######################
  ### Box Plot
  ######################

  ### Two Vars
  ### X is Cont & Y is Disc or Vice Versa
  boxplot_condition <-
    paste0(
      two_var_condition, "&&",
      "((($.inArray(input.x_var, ", discrete_array, ") != -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") == -1))",
      "||",
      "(($.inArray(input.x_var, ", discrete_array, ") == -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") != -1)))"
    )

  ####################################
  ### Add Facet Wrap to Box Plot
  ####################################

  facet_boxplot_condition <-
    paste0(
      two_var_condition, "&&",
      "((($.inArray(input.x_var, ", discrete_array, ") != -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") == -1))",
      "||",
      "(($.inArray(input.x_var, ", discrete_array, ") == -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") != -1)))",
      "&&",
      # And Facet Wrap is defined
      "(input.facet_var != 'No Wrap')"
    )


  ####################
  ### Scatterplot
  ####################

  ### Two Vars
  ### X != Y and Both Continuous
  ### Facet Wrap is not defined

  scatterplot_condition <-
    paste0(
      two_var_condition, "&&",
      "(input.x_var != input.y_var)",
      "&&",
      "(($.inArray(input.x_var, ", discrete_array, ") == -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") == -1))",
      "&&",
      "(input.facet_var == 'No Wrap')"
    )

  #################################################
  ### Add Facet Wrap to Scatter Plot
  #################################################

  ### Two Variables, Both Continuous
  ### X != Y
  ### Facet Wrap is Defined

  facet_scatterplot_condition <-
    paste0(
      two_var_condition, "&&",
      "(input.x_var != input.y_var)",
      "&&",
      "(($.inArray(input.x_var, ", discrete_array, ") == -1)",
      "&&",
      "($.inArray(input.y_var, ", discrete_array, ") == -1))",
      "&&",
      "(input.facet_var != 'No Wrap')"
    )

  #################################################
  ### Show Drop Down menu for Wrap whenever
  ### boxplot or scatterplot is defined.
  #################################################

  ### Three Variables
  drop_down_condition <-
    paste0("(", boxplot_condition, ")",
           "||",
           "(", facet_boxplot_condition, ")",
           "||",
           "(", scatterplot_condition, ")",
           "||",
           "(", facet_scatterplot_condition, ")"
           )



  ##############################
  ### Create Shiny App
  ##############################

  shinyApp(

    #################################################
    ### Define UI
    #################################################

    ui <-
      fluidPage(
        sidebarLayout(

          ###############################
          ### Drop Down Menu
          ###############################

          sidebarPanel(

            ### Number of Variables to Compare
            selectInput(inputId = "num_var",
                        label   = h4("Number of Variables to Examine"),
                        choices = c("One Variable"    = "one",
                                    "Two Variables"   = "two")
                        ),

            ### Show one variable
            conditionalPanel(
              condition = paste0(one_var_condition, "||", two_var_condition),
              selectInput(inputId = "x_var",
                          label   = h4("Choose Variable 1:"),
                          choices = drop_down_list
                          )
            ),


            ### Y-Axis / Row Variable
            conditionalPanel(
              condition = "input.num_var == 'two'",
              selectInput(inputId = "y_var",
                          label   = h4("Choose Variable 2:"),
                          choices = drop_down_list
                          )
            ),

            ### Variable for Facet Wrap - must be Discrete
            conditionalPanel(
              condition = drop_down_condition,
              selectInput(inputId = "facet_var",
                          label   = h4("Choose Discrete Variable to Subset and Display Multiple Plots"),
                          choice  = discrete_drop_down_list
                          )
              ),

            ### Variable to Populate Cells - must be Continuous
            conditionalPanel(
              condition = table_condition,
              selectInput(inputId = "fill_var",
                          label   = h4("Choose Continuous Variable to Fill Table"),
                          choice  = cont_drop_down_list
                          )
              ),

            ### Function on Variable in Pivot Table
            conditionalPanel(
              condition = table_condition,
              selectInput(inputId = "func",
                          label = h4("Function on Variable in Cells"),
                          choice = c("Count"   = "CountNum",
                                     "Mean"    = "mean",
                                     "Median"  = "median",
                                     "Minimum" = "min",
                                     "Maximum" = "max",
                                     "Standard Deviation" = "sd")
                          )
              ),
          ),

          ###############################
          ### Main Data Display
          ###############################

          mainPanel(

            verbatimTextOutput("choice"),

            ##########################
            ### One Variable
            ##########################

            ### One Variable, Continuous
            ### Two Variables, Equal and Continuous
            ### Plot Histogram and Summary Stats
            conditionalPanel(
              condition = main_histogram_condition,
              plotOutput("plot_main_histogram"),
              tableOutput("summary_stats")
            ),

            ### One Variable, Discrete
            ### Show Summary Table
            conditionalPanel(
              condition = simple_summary_condition,
              tableOutput("summary_stats_discrete_var")
            ),


            ##########################
            ### Two Variables
            ##########################

            ### Two Variables, Both Discrete, x == y | x != y
            ### Pivot Table
            conditionalPanel(
              condition = table_condition,
              DT::dataTableOutput("myDatatable")
              ),

            ### Two Variables, Both Continuous, x != y
            ### Scatterplot
            conditionalPanel(
              condition = scatterplot_condition,
              plotOutput("plot_scatterplot")
              ),

            ### Two Variables, x / y is Discrete and other is Continuous
            ### Boxplot
            conditionalPanel(
              condition = boxplot_condition,
              plotOutput("plot_boxplot")
            ),

            ##########################
            ### Three Variables
            ##########################

            ### Scatterplot above Satisfied
            ### Third Variable is Discrete
            ### Facet Wrap Scatter Plot
            conditionalPanel(
              condition = facet_scatterplot_condition,
              plotOutput("plot_facet_scatterplot")
              ),

            ### Boxplot above is Satisfied
            ### Third Variable is Discrete
            ### Facet Boxplot
            conditionalPanel(
              condition = facet_boxplot_condition,
              plotOutput("plot_facet_boxplot")
              ),


            ###############################
            ### Secondary Data Display
            ###############################

            ### Two Variable Pivot Table Satisfied
            ### User Clicks in Table
            ### Display Histogram Below
            conditionalPanel(
              condidition = sub_histogram_condition,
              textOutput("Selected_Cells"),
              plotOutput("plot_sub_histogram")
            )
            )
          )
        ),


    #################################################
    ### Define server
    #################################################

    server <- function(input, output) {

      ### User Choice
      output$x_var <- renderText({input$x_var})

      ### Brattle logo
      output$brattle_logo <-
        renderImage({
          list(src = paste0("N:/General/80010_Corporate_Marketing_and_BD_Materials/",
                            "Images and Photos/Logo and Print Details/Standard Logo/",
                            "RGB/Brattle RGB Medium Logo.png"),
               height = 30,
               width = 180,
               deleteFile = FALSE)
        })


      ###################################################
      ### Histogram if user examines just one variable
      ###################################################

      output$plot_main_histogram <-
        renderPlot(
          ggplot(dataset) +
            geom_histogram(aes_string(input$x_var)) +
            ylab("Count") +
            theme_bw()
        )


      ######################################################
      ### Summary Stats if user examines just one variable
      ######################################################

      output$summary_stats <-
        renderTable({
          datalist <-
            list("Minimum"       = min(dataset[[input$x_var]],               na.rm = T),
                 "1st Quartile"  = quantile(dataset[[input$x_var]], c(0.25), na.rm = T),
                 "Median"        = median(dataset[[input$x_var]],            na.rm = T),
                 "Mean"          = mean(dataset[[input$x_var]],              na.rm = T),
                 "3rd Quartile"  = quantile(dataset[[input$x_var]], c(0.75), na.rm = T),
                 "Maximum"       = max(dataset[[input$x_var]],               na.rm = T),
                 "Number of NAs" = sum(is.na(dataset[[input$x_var]])))

          return(data.frame("Stat" = names(datalist), "Value" = as.vector(unlist(datalist))))
          })

      output$summary_stats_discrete_var <-
        renderTable({
          tab <-
            table(dataset[[input$x_var]]) %>%
            as.data.frame()

          colnames(tab)[[1]] <- input$x_var
          return(tab)
        })


      #############################################
      ### Pivot Table if both x & y discrete
      #############################################

      ### Create Dataset for Pivot Table
      data_table <-
        reactive({
          dataset %>%
            group_by_(input$x_var, input$y_var) %>%
            summarise_(variable = interp(~ f(x, na.rm = T),
                                         f = match.fun(input$func),
                                         x = as.name(input$fill_var))) %>%
            ungroup() %>%
            mutate(variable = MakePrettyNumber(variable)) %>%
            spread_(key_col = input$x_var, value = "variable")
        })

      ### Plot pivot Table
      output$myDatatable <-
        DT::renderDataTable(
          data_table(),
          server = TRUE,
          selection = list(mode = 'single', target = "cell"),
          rownames = FALSE
        )


      #################################################
      ### Drop down Histogram if Click in Pivot Table
      #################################################

      ### Histogram Dataset:
      hist_dataset <-
        reactive({
          req(input$myDatatable_cells_selected)

          data <- data_table()

          ### Find correct index
          col_index <- input$myDatatable_cells_selected[1,2] + 1
          col_val   <- colnames(data[,col_index])
          row_index <- input$myDatatable_cells_selected[1,1]
          row_val   <- as.character(data[row_index,1][[1]])

          ### Filter
          if (input$x_var != input$y_var) {
            hist_data <-
              dataset %>%
              filter(UQ(rlang::sym(input$x_var)) == col_val,
                     UQ(rlang::sym(input$y_var)) == as.character(row_val))

          } else {
            hist_data <-
              dataset %>%
              filter(UQ(rlang::sym(input$x_var)) == col_val)

          }

          return(hist_data)
        })


      ### Plot Histogram Output:
      output$plot_sub_histogram <-
        renderPlot({

          ### Require that x and y discrete otherwise do not plot
          req(grepl("Discrete", names(drop_down_list[drop_down_list == input$x_var])))
          req(grepl("Discrete", names(drop_down_list[drop_down_list == input$y_var])))

          ### Require that user has clicked cell otherwise do not plot
          req(input$myDatatable_cells_selected)
          req(input$myDatatable_cells_selected[1,2] != 0)

          ### Create and plot dataset
          df <- hist_dataset()

          x_min <- min(dataset[[input$fill_var]])
          x_max <- max(dataset[[input$fill_var]])

          plot <-
            ggplot(data = df) +
            geom_histogram(aes_string(x = input$fill_var)) +
            xlim(x_min, x_max) +
            ylab("Count") +
            theme_bw()

          return(plot)
        })

      output$Selected_Cells <- renderPrint("Histogram Condition Satisfied")


      ###########################################################
      ### Box Plot if x continuous & y discrete or vice versa
      ###########################################################

      ### Boxplot template
      boxplot <-
        reactive({
          plot <- ggplot(data = dataset) +
            theme_bw()

          if (grepl("Discrete", names(drop_down_list[drop_down_list == input$x_var]))) {
            plot <-
              plot +
              geom_boxplot(aes_string(x = input$x_var, y = input$y_var))

          } else {
            plot <-
              plot +
              geom_boxplot(aes_string(x = input$y_var, y = input$x_var))
          }

          return(plot)
        })

      output$plot_boxplot <-
        renderPlot({
          req(input$facet_var == "No Wrap")
          boxplot()
        })

      ###########################################################
      ### Facet Wrap Boxplot
      ###########################################################

      output$plot_facet_boxplot <-
        renderPlot({
          req(input$facet_var != "No Wrap")

          if ((input$facet_var == input$x_var) | (input$facet_var == input$y_var)) {
            return(boxplot())

          } else {
            plot <-
              boxplot() +
              facet_wrap(as.formula(paste0("~", input$facet_var))) +
              theme(axis.text.x = element_text(angle = 60, hjust = 1))
            return(plot)
          }
        })


      #################################################
      ### Scatter plot if x != y and both continuous
      #################################################

      ### Plot regular scatter plot
      output$plot_scatterplot <-
        renderPlot({
          req(input$facet_var == "No Wrap")
          ggplot(data = dataset,
                 aes_string(x = input$x_var, y = input$y_var)) +
            geom_point() +
            xlab(input$x_var) +
            ylab(input$y_var) +
            theme_bw()
        })


      #################################################
      ### Add Facet Wrap to Scatter Plot
      #################################################

      ### Create Plot with Facet Wrap
      output$plot_facet_scatterplot <-
        renderPlot({
          req(input$facet_var != "No Wrap")
          ggplot(data = dataset,
                 aes_string(x = input$x_var, y = input$y_var)) +
            geom_point() +
            xlab(input$x_var) +
            ylab(input$y_var) +
            facet_wrap(as.formula(paste0("~", input$facet_var)),
                       scales = "free") +
            theme_bw(axis.text.x = element_text(angle = 60, hjust = 1))
        })

  	}
  )
}

