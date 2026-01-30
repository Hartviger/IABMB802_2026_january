###############
# Lipid Heatmap
###############

tabPanel("Lipid Heatmap",
         useShinyjs(),
         tabsetPanel(
           
           tabPanel("Group selection",
                    fluidRow(
                      column(6, box(
                        width = NULL,
                        title = "User guide",
                        tags$ul(
                          tags$li("Select the data frame to use."),
                          tags$li("Click 'Run Data Processing' to start the analysis."),
                          tags$li("After data processing select the numerator and denominator groups."),
                          tags$li("Go to the 'Lipid Visualization' tab set thresholds and see the results"),
                          tags$li("Have fun!"),
                          br(),
                          actionButton("df_explain", "What is the differnt data frames?"),
                          br(),
                          conditionalPanel(
                            condition = "input.run_process > 0", # Only display the following if data processing has been triggered.
                            br(),
                            tableOutput("groups_table"), # Shows grouped data of samples
                          )
                        )
                      )),
                      column(6, box(
                        width = NULL, 
                        title = "Data Frame and group selection:", 
                        radioButtons("selected_dataset", "Data frames",
                                     choices = c("Original Data" = "original", "Merged Data" = "merged"),
                                     selected = "original", width = "50%"),
                        
                        column(6, 
                               actionButton("run_process", "Run Data Processing"),
                               uiOutput("select_group_ui_heatmap"),
                               tableOutput("numerator_table"),
                               tableOutput("denominator_table"),
                               conditionalPanel(
                                 condition = "input.run_process > 0", 
                                 actionButton("show_lipid_info", label = "Lipid Summary", icon = icon("info-circle")),
                                 actionButton("show_lipid_cal", label = "Calculation Summary", icon = icon("calculator")),
                                 actionButton("show_lipid_remove", label = "Filtered Summary", icon = icon("filter")),
                                 actionButton("lipid_contact", label = "Any questions?", icon = icon("question-circle"))
                                 
                                 
                                 
                                 
                               )
                        )
                      ))
                    ),
                    
                    conditionalPanel(
                      condition = "input.run_process > 0", 
                      column(6, box(
                        title = "Numerator Group Table",
                        width = NULL,
                        DT::dataTableOutput("numerator_group_table")  
                      )),
                      column(6, box(
                        title = "Denominator Group Table",
                        width = NULL,
                        DT::dataTableOutput("denominator_group_table")  
                      ))
                    )
           ),
           
           ####  VISUALIZATION TAB 
           
           tabPanel(
             "Lipid Visualization",
             conditionalPanel(
               condition = "input.run_process > 0",
               # First row with Lipid Selection and Plot Settings side by side
               fluidRow(
                 # Column for Lipid Selection and Plot Settings
                 column(
                   width = 6,
                   box(
                     width = NULL,
                     title = "Plot Settings", solidHeader = TRUE,
                     uiOutput("select_lipid_ui"),
                     tags$li("Lipids will show up if they are within the threshold of p-value and logFC."),
                     tags$li("Default settings: lipids displayed: all, logFC: 0, p-value: 1, p-adj: 1, min. lipids: 1 or 2."),
                     
                     br(),
                     br(),
                     uiOutput("logFC_input_ui"),
                     uiOutput("p_value_max_ui"),
                     uiOutput("p_value_adj"),
                     uiOutput("min_lipids_per_class_ui"),
                     checkboxInput("split_screen", "Show Heatmap and Table side by side", value = FALSE),
                     checkboxInput("show_grid", "Display Grid Lines", value = TRUE),
                     
                     br(),
                     # Add the action button to open the modal dialog
                     actionButton("download_heatmap_btn", "Download Heatmap Image"),
                     br(),
                     # Add the UI output for the warning message
                     uiOutput("filteredDataWarning")
                   )
                 ),
                 
                 # Column for Color Settings
                 column(
                   width = 6,
                   box(
                     width = NULL,
                     title = "Settings", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                     tabBox(
                       width = NULL,
                       tabPanel("Colors",
                                colourInput("low_color", "Negative logFC color", value = "#4575b4"),
                                colourInput("mid_color", "Mid logFC color", value = "white"),
                                colourInput("high_color", "Positive logFC color", value = "#d73027"),
                                colourInput("panel_bg_color", "Panel Background Color", value = "#D3D3D3"),
                                colourInput("strip_bg_color", "Strip Background Color", value = "#3483d1"),
                                colourInput("strip_text_color", "Strip Text Color", value = "black")
                       ),
                       tabPanel("Text Sizes",
                                numericInput("strip_text_size", "Strip Text Size", value = 16),
                                numericInput("axis_text_x_size", "X-axis Numbers Size", value = 12),
                                numericInput("axis_text_y_size", "Y-axis Numbers Size", value = 12),
                                numericInput("axis_title_size", "Axis Title Size", value = 20),
                                numericInput("legend_title_size", "Legend Title Size", value = 18),
                                numericInput("legend_text_size", "Legend Numbers Size", value = 14),
                                numericInput("axis_title_x_size", "X-axis Title Size", value = 20),
                                numericInput("axis_title_y_size", "Y-axis Title Size", value = 20),
                                numericInput("plot_title_size", "Plot Title Size", value = 22)
                       ),
                       tabPanel("Axis Settings",
                                numericInput("axis_text_x_angle", "X-axis Text Angle", value = 90, min = 0, max = 360),
                       ),
                       tabPanel("Legend Settings",
                                numericInput("barwidth", "LogFC Bar Width", value = 15),
                                numericInput("barheight", "LogFC Bar Height", value = 1)
                       ),
                       tabPanel("Labels",
                                textInput("x_axis_label", "X-axis Label", value = "Number of fatty-acid carbon atoms"),
                                textInput("y_axis_label", "Y-axis Label", value = "Number of fatty-acid double bonds"),
                                textInput("main_title", "Main Title", value = "")
                       )
                     )
                   )
                 ),
                 
                 column(
                   width = 6,
                   box(
                     width = NULL,
                     radioButtons(
                       inputId = "selected_logfc_sclae_bar",
                       label = "Select logFC scale bar:",
                       choices = c("Manual" = "Manual", "Dynamic Range" = "dynamic"),
                       selected = "Manual"
                     ),
                     conditionalPanel(
                       condition = "input.selected_logfc_sclae_bar == 'Manual'",
                       numericInput(
                         inputId = "logFC_scale_manual",
                         label = "Set Manual logFC Scale:",
                         value = 2.5,  
                         step = 0.1,
                       ),
                       tags$li(
                         "Lipids in the heatmap with a '+' sign exceed the upper logFC scale, 
                                   while those with a '-' sign fall below the lower logFC scale. 
                                   Adjust the 'logFC scale bar input' to change this."
                       )
                     ),
                     uiOutput("selected_groups_text"),
                   )
                 )
                 
                 
               )
             ),
             
             # Output for the visualization
             uiOutput("visualization_ui"),
             tags$div(
               class = "alert-warning",
               uiOutput("table_message_2")
             )
           ),
           
           ####  HEATMAP TABLE TAB 
           
           tabPanel(
             "Table of Heatmap",
             column(width = 12,
                    dataTableOutput("pValueTable")
             ),
             tags$div(
               class = "alert-warning",
               uiOutput("table_message_1")
             )
           ),
           
           ####  BUBBLE PLOT TAB
           
           #tabPanel(
           #"Bubble plot of data",
           
           #uiOutput("bubble_plot_ui"),
           #tags$div(
           #class = "alert-warning", # You can change this to 'alert-success', 'alert-warning', etc.
           #uiOutput("table_message")
           #)
           #)
           
         )
), #### End of Lipid Heatmap ####


