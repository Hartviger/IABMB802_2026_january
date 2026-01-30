###############
# Lipid Heatmap
###############

tabPanel(
  "Lipid Heatmap",
  useShinyjs(),
  
  ## ---- CSS: make Plot settings '+' big & visible ----
  tags$head(
    tags$style(HTML("
      /* Bigger and clearer '+' button for the Plot settings box */
      #plot_settings_box .box-tools .btn-box-tool {
        font-size: 26px;
        padding: 6px 16px;
        color: #337ab7;
        opacity: 1;               /* make sure it is not faded */
      }
      #plot_settings_box .box-tools .btn-box-tool i {
        font-size: 26px;
      }
      #plot_settings_box .box-tools .btn-box-tool:hover {
        background-color: #e6eef7;
        border-radius: 16px;
      }
    "))
  ),
  
  tabsetPanel(
    
    #### ---------------- GROUP SELECTION TAB ---------------- ####
    tabPanel(
      "Group selection",
      fluidRow(
        column(
          6,
          box(
            width = NULL,
            title = "User guide",
            tags$ul(
              tags$li("Select the data frame to use."),
              tags$li("Click 'Run Data Processing' to start the analysis."),
              tags$li("After data processing select the Group of Interest and Reference Groups."),
              tags$li("Go to the 'Lipid Visualization' tab set thresholds and see the results"),
              tags$li("Have fun!")
            ),
            br(),
            actionButton("df_explain", "What is the differnt data frames?"),
            br(),
            conditionalPanel(
              condition = "input.run_process > 0",
              br(),
              tableOutput("groups_table") # Shows grouped data of samples
            )
          )
        ),
        column(
          6,
          box(
            width = NULL,
            title = "Data Frame and group selection:",
            radioButtons(
              "selected_dataset", "Data frames",
              choices = c("Original Data" = "original", "Merged Data" = "merged"),
              selected = "original", width = "50%"
            ),
            column(
              6,
              uiOutput("heatmap_name_col_ui"),
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
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.run_process > 0",
        column(
          6,
          box(
            title = "Group of Interest Table",
            width = NULL,
            DT::dataTableOutput("numerator_group_table")
          )
        ),
        column(
          6,
          box(
            title = "Reference Group Table",
            width = NULL,
            DT::dataTableOutput("denominator_group_table")
          )
        )
      )
    ),
    
    #### ---------------- LIPID VISUALIZATION TAB ---------------- ####
    tabPanel(
      "Lipid Visualization",
      
      ## first row: thresholds + logFC / sig settings (only after processing)
      conditionalPanel(
        condition = "input.run_process > 0",
        fluidRow(
          
          # Left column: thresholds
          column(
            width = 6,
            box(
              width = NULL,
              title = "Set thresholds for Lipid Heatmap", solidHeader = TRUE,
              uiOutput("select_lipid_ui"),
              tags$li("Lipids will show up if they are within the threshold of p-value and logFC."),
              tags$li("Default settings: lipids displayed: all, logFC: 0, p-value: 1, p-adj: 1, min. lipids: 1 or 2."),
              br(), br(),
              uiOutput("logFC_input_ui"),
              uiOutput("p_value_max_ui"),
              uiOutput("p_value_adj"),
              
              uiOutput("carbon_range_ui"),
              uiOutput("double_range_ui"),
              
              uiOutput("min_lipids_per_class_ui"),
              br(),
              actionButton("download_heatmap_btn", "Download Heatmap Image"),
              br(),
              uiOutput("filteredDataWarning")
              
            )
          ),
          
          # Right column: logFC scale + significant markers
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
                  step = 0.1
                ),
                tags$li(
                  "Lipids in the heatmap with a '+' sign exceed the upper logFC scale, 
                   while those with a '-' sign fall below the lower logFC scale. 
                   Adjust the 'logFC scale bar input' to change this."
                )
              ),
              uiOutput("selected_groups_heatmap_text"),
              hr(),
              checkboxInput("split_screen", "Show Heatmap and Table side by side", value = FALSE),
              checkboxInput("outline_sig", "Draw squares around significant lipids", value = FALSE),
              
              conditionalPanel(
                condition = "input.outline_sig == true",
                radioButtons(
                  "sig_shape", "Marker shape:",
                  choices  = c("Squares" = "square", "Circles (size by significance)" = "circle"),
                  selected = "square",
                  inline   = TRUE
                ),
                radioButtons(
                  "sig_metric", "Use:",
                  choices = c("p-value" = "p", "p-adj (BH)" = "padj"),
                  inline  = TRUE
                ),
                numericInput(
                  "sig_threshold", "Significance threshold (≤)",
                  value = 0.05, min = 0, max = 1, step = 0.001
                ),
                colourInput("sig_outline_color", "Outline color", value = "#0f3a64"),
                numericInput("sig_outline_size", "Outline size", value = 0.7, min = 0.1, step = 0.1)
              )
            )
          )
        )
      ),
      
      ## Plot settings box – only visible after run_process > 0
      conditionalPanel(
        condition = "input.run_process > 0",
        column(
          width = 12,
          box(
            width = NULL,
            id = "plot_settings_box",
            title = tagList(
              icon("sliders"),
              span("Plot settings"),
              span("  (click '+' on the right to expand)",
                   style = "font-size:11px; color:#777; margin-left:5px;")
            ),
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            tabBox(
              width = NULL,
              
              tabPanel(
                "Colors",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_colors", "Reset Colors", icon = icon("undo"))
                ),
                
                # ---- NEW: palette presets ----
                selectInput(
                  "palette_preset",
                  "Color palette preset",
                  choices = c(
                    "Custom / manual (use colors below)" = "manual",
                    "Default: Blue–White–Red"            = "default",
                    "CB-friendly: Blue–White–Vermillion (Okabe–Ito)" = "blue_orange",
                    "CB-friendly: Purple–White–Green (Okabe–Ito)"    = "purple_green",
                    "Viridis-like (sequential)"          = "viridis_like"
                  ),
                  selected = "manual"
                ),
                helpText("Preset changes the logFC gradient colors; you can still fine-tune them below."),
                tags$hr(),
                
                # existing color inputs
                colourInput("low_color",  "Negative logFC color", value = "#4575b4"),
                colourInput("mid_color",  "Mid logFC color",      value = "white"),
                colourInput("high_color", "Positive logFC color", value = "#d73027"),
                colourInput("panel_bg_color", "Panel Background Color", value = "#D3D3D3"),
                colourInput("strip_bg_color", "Strip Background Color", value = "#3483d1"),
                colourInput("strip_text_color", "Strip Text Color", value = "black"),
                hr(),
                checkboxInput("tile_border_show", "Show tile borders", value = TRUE),
                colourInput("tile_border_color", "Tile border color", value = "white"),
                numericInput("tile_border_size", "Tile border size", value = 0.2,
                             min = 0, max = 3, step = 0.1),
                hr(),
                checkboxInput("show_grid", "Display Grid Lines", value = TRUE),
                
                # NEW: choose grid color (only visible if grid is on)
                conditionalPanel(
                  condition = "input.show_grid == true",
                  colourInput("grid_color", "Grid line color", value = "#B0B0B0")
                )
                
                
              ),
              
              
              tabPanel(
                "Text Sizes",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_text_sizes", "Reset Text Sizes", icon = icon("undo"))
                ),
                numericInput("strip_text_size", "Strip Text Size", value = 16),
                numericInput("axis_text_x_size", "X-axis Numbers Size", value = 12),
                numericInput("axis_text_y_size", "Y-axis Numbers Size", value = 12),
                numericInput("axis_title_size", "Axis Title Size", value = 20),
                numericInput("legend_title_size", "Legend Title Size", value = 18),
                numericInput("legend_text_size", "Legend Numbers Size", value = 14),
                numericInput("axis_title_x_size", "X-axis Title Size", value = 20),
                numericInput("axis_title_y_size", "Y-axis Title Size", value = 20),
                numericInput("plot_title_size", "Main Title size", value = 22),
                numericInput("plot_subtitle_size", "Subtitle size", value = 18),
                checkboxInput("plot_subtitle_bold", "Subtitle bold", value = FALSE),
                
                # ---- font selector (only once!) ----
                selectInput(
                  "font_family",
                  "Base font family",
                  choices = c(
                    "Default (from R theme)" = "Default",
                    "Helvetica"       = "Helvetica",
                    "Arial"           = "Arial",
                    "Verdana"         = "Verdana",
                    "Tahoma"          = "Tahoma",
                    "Calibri"         = "Calibri",
                    "Times"           = "Times",
                    "Times New Roman" = "Times New Roman",
                    "Georgia"         = "Georgia",
                    "Palatino"        = "Palatino",
                    "Garamond"        = "Garamond",
                    "Courier"         = "Courier",
                    "Courier New"     = "Courier New",
                    "Consolas"        = "Consolas",
                    "Comic Sans MS"   = "Comic Sans MS",
                    "Trebuchet MS"    = "Trebuchet MS"
                  ),
                  selected = "Default"
                )
              ),
              
              tabPanel(
                "Axis Settings",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_axis", "Reset Axis Settings", icon = icon("undo"))
                ),
                numericInput("axis_text_x_angle", "X-axis Text Angle", value = 90, min = 0, max = 360)
              ),
              
              tabPanel(
                "Legend Settings",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_legend", "Reset Legend Settings", icon = icon("undo"))
                ),
                numericInput("barwidth", "LogFC Bar Width", value = 15),
                numericInput("barheight", "LogFC Bar Height", value = 1),
                selectInput(
                  "legend_side", "LogFC bar position",
                  choices = c(
                    "Top"    = "top",
                    "Bottom" = "bottom",
                    "Left"   = "left",
                    "Right"  = "right",
                    "Hide"   = "none"
                  ),
                  selected = "top"
                )
              ),
              
              
              tabPanel(
                "Titles & Labels",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_labels", "Reset Labels", icon = icon("undo"))
                ),
                textInput("x_axis_label", "X-axis Label",
                          value = "Number of fatty-acid carbon atoms"),
                textInput("y_axis_label", "Y-axis Label",
                          value = "Number of fatty-acid double bonds"),
                textInput("main_title", "Main Title",
                          value = "Lipid Fold Changes by Class"),
                textInput("sub_title", "Subtitle",
                          value = "Groups being compared is"),
                textInput("group_join", "Text between groups", value = "vs"),
                checkboxInput(
                  "auto_subtitle_groups",
                  "Append '<numerator> vs <denominator>' to subtitle",
                  value = TRUE
                ),
                sliderInput(
                  "title_hjust", "Title horizontal position",
                  min = 0, max = 1, value = 0, step = 0.05
                ),
                sliderInput(
                  "subtitle_hjust", "Subtitle horizontal position",
                  min = 0, max = 1, value = 0, step = 0.05
                )
                
              ),
              
              tabPanel(
                "Layout",
                div(
                  style = "text-align:right; margin-bottom:8px;",
                  actionButton("reset_layout", "Reset Layout", icon = icon("undo"))
                ),
                
                # --- IMPROVEMENT: Clearer Mode Selection ---
                # Using Radio Buttons makes the choice obvious compared to a small checkbox
                radioButtons(
                  "layout_mode",
                  label = "Plot Dimensions Mode:",
                  choices = c(
                    "Auto-Scale" = "auto", 
                    "Custom (Manual)" = "custom"
                  ),
                  selected = "auto",
                  inline = TRUE # Makes them sit side-by-side
                ),
                
                helpText("Auto-Scale calculates height based on how many lipid classes are in your data."),
                tags$hr(),
                
                # Only show these inputs when custom layout is selected
                conditionalPanel(
                  condition = "input.layout_mode == 'custom'",
                  wellPanel( # Adds a grey background to make this section pop out
                    h4("Custom Dimensions"),
                    numericInput(
                      "facet_cols",
                      "Number of facet columns",
                      value = 3, min = 1, max = 10, step = 1
                    ),
                    numericInput(
                      "plot_width_px",
                      "Plot width (px)",
                      value = 1100, min = 400, max = 6000, step = 50
                    ),
                    numericInput(
                      "plot_height_px",
                      "Plot height (px)",
                      value = 1000, min = 400, max = 6000, step = 50
                    )
                  )
                ),
                
                # ---- panel spacing & plot margins (always visible) ----
                numericInput(
                  "panel_spacing_mm",
                  "Panel spacing (mm)",
                  value = 2, min = 0, max = 20, step = 0.5
                ),
                
                fluidRow(
                  column(3, numericInput("margin_top_mm",    "Top margin (mm)",    value = 5, min = 0, max = 50, step = 0.5)),
                  column(3, numericInput("margin_right_mm",  "Right margin (mm)",  value = 5, min = 0, max = 50, step = 0.5)),
                  column(3, numericInput("margin_bottom_mm", "Bottom margin (mm)", value = 5, min = 0, max = 50, step = 0.5)),
                  column(3, numericInput("margin_left_mm",   "Left margin (mm)",   value = 5, min = 0, max = 50, step = 0.5))
                )
              ),
              
              tabPanel(
                "Export and import of settings",
                tags$hr(),
                h4("Export / Import heatmap settings"),
                helpText("Save all current heatmap style & layout settings to a file, or load them again later."),
                
                fluidRow(
                  column(
                    6,
                    downloadButton("download_heatmap_settings",
                                   "Export settings (.rds)")
                  ),
                  column(
                    6,
                    fileInput("upload_heatmap_settings",
                              "Import settings (.rds)",
                              accept = ".rds")
                  )
                )
              )
              
            )
            
          )
        )
      ),
      
      # Heatmap / table output
      uiOutput("visualization_ui"),
      tags$div(
        class = "alert-warning",
        uiOutput("table_message_2")
      )
    ),
    
    #### ---------------- HEATMAP TABLE TAB ---------------- ####
    tabPanel(
      "Table of Heatmap",
      column(
        width = 12,
        div(
          style = "margin-bottom: 10px;",
          downloadButton("download_pvalues_csv",
                         "Download Supplementary Table (CSV)"),
          downloadButton("download_pvalues_xlsx",
                         "Download Supplementary Table (Excel)")
        ),
        dataTableOutput("pValueTable")
      ),
      tags$div(
        class = "alert-warning",
        uiOutput("table_message_1")
      )
    )
    
    
  )
),