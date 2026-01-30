
# ------------------------------------------------------------------------------
# UI builder for Absolute Quantification modal
# ------------------------------------------------------------------------------

absq_modal_body <- function() {
  tagList(
    div(
      class = "absq-modal-body",
      
      # 1. Settings -------------------------------------------------------------
      div(
        class = "absq-section",
        h4("1. Settings"),
        fluidRow(
          column(
            6,
            radioButtons(
              "absq_matrix_type", "Sample Matrix",
              choices  = c("Tissue (mg)", "Biofluid (µL)"),
              selected = "Tissue (mg)",
              width    = "100%"
            )
          ),
          column(
            6,
            checkboxInput(
              "absq_include_qc",
              "Include QC samples in checks",
              value = TRUE,
              width = "100%"
            )
          )
        )
      ),
      
      # 2. Readiness checks ----------------------------------------------------
      div(
        class = "absq-section",
        h4("2. Readiness Checks"),
        uiOutput("absq_readiness_ui")
      ),
      
      # 3. Internal standard spikes -------------------------------------------
      div(
        class = "absq-section",
        h4("3. Internal Standard Spikes"),
        
        fluidRow(
          column(
            6,
            selectInput(
              "absq_spike_unit",
              "Input Unit",
              choices  = c("pmol", "nmol"),
              selected = "pmol",
              width    = "100%"
            )
          ),
          column(
            6,
            div(
              class = "absq-help-text",
              "Double-click cells in the table to edit individual amounts."
            )
          )
        ),
        
        br(),
        
        # Auto-fill panel
        wellPanel(
          style = "padding: 10px; background-color: #fff; border: 1px solid #ddd;",
          p(
            style = "font-size: 0.9em; color: #666; margin-bottom: 5px;",
            tags$i(class = "fa fa-info-circle"),
            " Psst, if you have the same value for all of 'Spiked Amount', just auto fill here:"
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "absq_autofill_val",
                "Auto-fill Amount:",
                value = 0,
                step  = 0.1,
                width = "100%"
              ),
              p(
                style = "font-size: 0.9em; color: #666; margin-bottom: 5px;",
                tags$i(class = "fa fa-info-circle"),
                "Double click in table to edit, and write in 'Spiked Amount'"
              ),
            ),
            column(
              6,
              style = "margin-top: 25px;",
              actionButton(
                "absq_autofill_btn",
                "Apply to All Rows",
                icon  = icon("arrow-down"),
                class = "btn-warning",
                width = "50%"
              )
            )
          )
        ),
        
        DT::DTOutput("absq_spike_table")
      ),
      
      # 4. Compute & results ---------------------------------------------------
      div(
        class = "absq-section",
        h4("4. Compute & Results"),
        
        selectInput(
          "absq_is_method",
          "IS Matching Strategy",
          choices  = c("Nearest RT", "Same lipid structure"),
          selected = "Nearest RT",
          width    = "50%"
        ),
        
        column(6,
               
               style = "margin-top: 25px; transform: scale(1.2); transform-origin: left;",
               checkboxInput(
                 "absq_remove_is_rows", 
                 "Remove IS rows from results", 
                 value = FALSE, 
                 width = "100%"
               )
        ),
        
        bsButton(
          "absq_compute",
          "Compute Amounts",
          style = "primary",
          width = "100%"
        ),
        
        tags$hr(),
        DT::DTOutput("absq_result_table_modal"),
        
        
        
        tags$hr(),
        checkboxInput(
          "absq_save_as_new",
          "Save as new dataset",
          value = TRUE,
          width = "100%"
        )
      )
    )
  )
}


# ==============================================================================
# Absolute Quantification (AQ) – SERVER BLOCK
# ==============================================================================

# ---------------------------------
# Helper functions
# ---------------------------------

# Bullet with green check / red cross for readiness UI
absq_readiness_bullet <- function(ok, txt) {
  color <- if (ok) "#2e7d32" else "#c62828"
  icon  <- if (ok) shiny::icon(if (ok) "check" else "times")
  
  shiny::tags$li(
    style = paste0("color:", color, "; font-weight:600;"),
    icon,
    shiny::span(style = "color:black; font-weight:normal; margin-left:4px;", txt)
  )
}

# Find an 'amount' column in sequence table (exact match preferred, then fuzzy)
absq_find_amount_col <- function(seq_tbl) {
  # exact case-insensitive match
  exact <- which(tolower(colnames(seq_tbl)) == "amount")
  if (length(exact) >= 1) return(exact[1])
  
  # fuzzy match
  fuzzy <- grep("amount", colnames(seq_tbl), ignore.case = TRUE)
  if (length(fuzzy) >= 1) return(fuzzy[1])
  
  NA_integer_
}

# Find RT column index, either via sequence labels or data column names
absq_find_rt_col <- function(seq_tbl, d) {
  rt_from_seq <- which(seq_tbl[, 1] == "RT")
  if (length(rt_from_seq)) return(rt_from_seq[1])
  
  rt_from_data <- which(colnames(d) == "RT")
  if (length(rt_from_data)) return(rt_from_data[1])
  
  NA_integer_
}

# Choose IS index for each feature (Nearest RT / Same lipid structure)
absq_choose_is_index <- function(d, seq_tbl, is_rows,
                                 method = c("Nearest RT", "Same lipid structure")) {
  method <- match.arg(method)
  
  rt_col_idx   <- absq_find_rt_col(seq_tbl, d)
  name_rows    <- which(seq_tbl[, 1] == "Name")
  name_col_idx <- if (length(name_rows)) name_rows[1] else 1
  
  n_feat <- nrow(d)
  
  # Base mapping: nearest RT
  if (!is.na(rt_col_idx) && length(is_rows) > 0) {
    feat_rts <- suppressWarnings(as.numeric(d[, rt_col_idx]))
    is_rts   <- suppressWarnings(as.numeric(d[is_rows, rt_col_idx]))
    
    nearest <- vapply(
      feat_rts,
      function(y) {
        if (is.na(y)) return(1L)
        which.min(abs(is_rts - y))
      },
      integer(1)
    )
  } else {
    nearest <- rep(1L, n_feat) # fallback
  }
  
  if (method == "Nearest RT") return(nearest)
  
  # Same lipid structure: match by lipid class prefix
  feat_names <- as.character(d[, name_col_idx])
  is_names   <- as.character(d[is_rows, name_col_idx])
  
  feat_class <- sub("^([A-Za-z]+).*", "\\1", feat_names)
  is_class   <- sub("^([A-Za-z]+).*", "\\1", is_names)
  
  chosen <- nearest
  
  if (!is.na(rt_col_idx) && length(is_rows) > 0) {
    is_rts <- suppressWarnings(as.numeric(d[is_rows, rt_col_idx]))
    feat_rts <- suppressWarnings(as.numeric(d[, rt_col_idx]))
  }
  
  for (i in seq_len(n_feat)) {
    matches <- which(is_class == feat_class[i])
    if (length(matches) > 0) {
      if (!is.na(rt_col_idx)) {
        # if multiple candidates, pick nearest RT
        current_rt <- feat_rts[i]
        if (!is.na(current_rt)) {
          local_rts <- is_rts[matches]
          best_idx  <- which.min(abs(local_rts - current_rt))
          chosen[i] <- matches[best_idx]
        } else {
          chosen[i] <- matches[1]
        }
      } else {
        chosen[i] <- matches[1]
      }
    }
  }
  chosen
}


sticky_footer <- function(...) {
  shiny::tagList(
    tags$div(
      # This is where the buttons passed via '...' will be placed
      ..., 
      # Add necessary CSS to make the footer stick at the bottom
      style = paste0(
        "position: fixed;",
        "bottom: 0;",
        "left: 0;",
        "width: 100%;",
        "padding: 10px 15px;",
        "border-top: 1px solid #e5e5e5;",
        "background-color: #f5f5f5;",
        "z-index: 1050;"
      )
    )
  )
}


# ==============================================================================
# Absolute Quantification – SERVER LOGIC
# ==============================================================================

# State: separate spike tables per matrix type
# rv$absq_spikes[["Tissue (mg)"]]   -> df(IS_row, IS_name, spike_pmol)
# rv$absq_spikes[["Biofluid (µL)"]] -> df(IS_row, IS_name, spike_pmol)
rv$absq_spikes       <- list()
rv$absq_result       <- NULL
rv$absq_amounts_used <- NULL

# ------------------------------------------------------------------------------
# Open modal
# ------------------------------------------------------------------------------
observeEvent(input$absq_open_modal, {
  rv$absq_result <- NULL # clear previous results
  showModal(
    modalDialog(
      title = "Absolute Quantification",
      size = "l",
      # easyClose = FALSE, # Keep as is if you want to force button use
      footer = sticky_footer( # <-- USE THE NEW STICKY FOOTER
        modalButton("Close window"),
        bsButton("absq_save", "Save & Apply",
                 style = "success", icon = icon("save"))
      ),
      absq_modal_body()
    )
  )
  
  # Disable save until something has been computed
  shinyjs::disable("absq_save")
})

# ------------------------------------------------------------------------------
# Basic data access
# ------------------------------------------------------------------------------
absq_data <- reactive({
  req(rv$activeFile)
  rv$data[[rv$activeFile]]
})

absq_seq <- reactive({
  req(rv$activeFile)
  rv$sequence[[rv$activeFile]]
})

# Which sample columns are “in play”
absq_selected_sample_mask <- reactive({
  req(absq_seq(), !is.null(input$absq_include_qc))
  sel_labels <- if (isTRUE(input$absq_include_qc)) c("Sample", "QC") else "Sample"
  absq_seq()[, 1] %in% sel_labels
})

# Internal standards (rows + names)
absq_is_parsed <- reactive({
  d <- absq_data()
  is_idx <- grep("\\(IS\\)", d[, 1], ignore.case = TRUE)
  if (length(is_idx) == 0) return(NULL)
  
  data.frame(
    IS_row  = is_idx,
    IS_name = d[is_idx, 1],
    stringsAsFactors = FALSE
  )
})

# ------------------------------------------------------------------------------
# Readiness checks (depends on matrix type)
# ------------------------------------------------------------------------------
output$absq_readiness_ui <- renderUI({
  req(input$absq_matrix_type, absq_seq(), absq_data())
  
  seq <- absq_seq()
  d   <- absq_data()
  is_info <- absq_is_parsed()
  
  has_amt <- "amount" %in% colnames(seq)
  amt_ok  <- has_amt && any(!is.na(suppressWarnings(as.numeric(seq[, "amount"]))))
  has_is  <- !is.null(is_info) && nrow(is_info) > 0
  n_is    <- if (is.null(is_info)) 0 else nrow(is_info)
  
  has_rt  <- "RT" %in% seq[, 1] || "RT" %in% colnames(d)
  
  is_bio  <- grepl("Biofluid", input$absq_matrix_type, ignore.case = TRUE)
  unit_txt <- if (is_bio) "volume in µL" else "mass in mg"
  
  tags$ul(
    absq_readiness_bullet(has_amt,
                          paste0("Sequence contains 'amount' column (", unit_txt, ")")),
    absq_readiness_bullet(amt_ok,
                          "Amount column contains numeric values"),
    absq_readiness_bullet(has_is,
                          paste0("Internal Standards detected (", n_is, " found)")),
    absq_readiness_bullet(has_rt,
                          "Retention Time (RT) available")
  )
})

# ------------------------------------------------------------------------------
# Initialise / maintain spike tables per matrix
# ------------------------------------------------------------------------------

# Helper: ensure a spike table exists for current matrix type
absq_ensure_spike_table <- function() {
  info <- absq_is_parsed()
  key  <- isolate(input$absq_matrix_type)
  if (is.null(info) || is.null(key)) return()
  
  if (is.null(rv$absq_spikes[[key]])) {
    # base table
    base_df <- data.frame(
      IS_row     = info$IS_row,
      IS_name    = info$IS_name,
      spike_pmol = 0,
      stringsAsFactors = FALSE
    )
    
    # try to copy from the "other" matrix type if it exists
    other_keys <- setdiff(c("Tissue (mg)", "Biofluid (µL)"), key)
    donor <- NULL
    if (length(other_keys)) {
      for (ok in other_keys) {
        if (!is.null(rv$absq_spikes[[ok]])) {
          donor <- rv$absq_spikes[[ok]]
          break
        }
      }
    }
    if (!is.null(donor)) {
      merged <- merge(base_df, donor[, c("IS_row", "spike_pmol")],
                      by = "IS_row", all.x = TRUE, suffixes = c("", ".donor"))
      base_df$spike_pmol <- ifelse(
        is.na(merged$spike_pmol.donor),
        base_df$spike_pmol,
        merged$spike_pmol.donor
      )
    }
    
    base_df <- base_df[order(base_df$IS_row), ]
    rv$absq_spikes[[key]] <- base_df
  }
}

# Rebuild/ensure table when IS set OR matrix type changes
observeEvent(list(absq_is_parsed(), input$absq_matrix_type), {
  req(absq_is_parsed(), input$absq_matrix_type)
  absq_ensure_spike_table()
})

# ------------------------------------------------------------------------------
# Spike table display (per matrix type + per unit)
# ------------------------------------------------------------------------------
absq_spike_display <- reactive({
  req(absq_seq(), absq_data(), absq_is_parsed(), input$absq_matrix_type)
  
  key <- input$absq_matrix_type
  absq_ensure_spike_table()
  df <- rv$absq_spikes[[key]]
  req(df)
  
  seq_tbl <- absq_seq()
  d       <- absq_data()
  
  # RT column
  rt_col_idx <- which(seq_tbl[, 1] == "RT")[1]
  if (is.na(rt_col_idx)) rt_col_idx <- which(colnames(d) == "RT")[1]
  
  rt_vals <- if (!is.na(rt_col_idx)) {
    round(as.numeric(d[df$IS_row, rt_col_idx]), 2)
  } else {
    rep(NA_real_, nrow(df))
  }
  
  # pmol <-> nmol display scaling
  unit_scale <- if (identical(input$absq_spike_unit, "nmol")) 1/1000 else 1
  col_name   <- paste0("Spiked Amount (", input$absq_spike_unit, ")")
  
  out <- data.frame(
    `IS row`  = df$IS_row,
    `IS name` = df$IS_name,
    `RT`      = rt_vals,
    check.names = FALSE
  )
  out[[col_name]] <- round(df$spike_pmol * unit_scale, 6)
  out
})

# When matrix type or spike unit changes, clear results + disable save
observeEvent(list(input$absq_matrix_type, input$absq_spike_unit), {
  rv$absq_result <- NULL
  shinyjs::disable("absq_save")
})


# ------------------------------------------------------------------------------
# Auto-switch Input Unit based on Matrix Type
# ------------------------------------------------------------------------------
observeEvent(input$absq_matrix_type, {
  req(input$absq_matrix_type)
  
  # Define your logic here:
  # Example: If Biofluid is selected, switch to "pmol". 
  #          If Tissue is selected, switch to "nmol".
  
  if (grepl("Biofluid", input$absq_matrix_type)) {
    updateSelectInput(session, "absq_spike_unit", selected = "pmol")
  } else {
    # Assuming Tissue usually works better with nmol, or user preference
    updateSelectInput(session, "absq_spike_unit", selected = "nmol")
  }
})

# ------------------------------------------------------------------------------
# Auto-fill spike amounts (per matrix type)
# ------------------------------------------------------------------------------
observeEvent(input$absq_autofill_btn, {
  req(input$absq_matrix_type)
  key <- input$absq_matrix_type
  absq_ensure_spike_table()
  req(rv$absq_spikes[[key]])
  
  val <- input$absq_autofill_val
  if (is.na(val)) {
    showNotification("Please enter a valid numeric amount.", type = "warning")
    return()
  }
  
  unit_scale <- if (identical(input$absq_spike_unit, "nmol")) 1000 else 1
  fill_pmol  <- val * unit_scale
  
  rv$absq_spikes[[key]]$spike_pmol <- fill_pmol
  showNotification("All spike amounts updated for this matrix type.", type = "message")
})

# ------------------------------------------------------------------------------
# Render spike table + handle edits (per matrix type)
# ------------------------------------------------------------------------------
output$absq_spike_table <- DT::renderDT({
  req(absq_spike_display())
  DT::datatable(
    absq_spike_display(),
    editable = list(
      target  = "cell",
      disable = list(columns = 0:2)   # only last column editable
    ),
    options   = list(pageLength = 10, dom = "t", scrollX = FALSE),
    selection = "none",
    rownames  = FALSE
  )
})

observeEvent(input$absq_spike_table_cell_edit, {
  info <- input$absq_spike_table_cell_edit
  req(info, input$absq_matrix_type)
  
  key <- input$absq_matrix_type
  absq_ensure_spike_table()
  req(rv$absq_spikes[[key]])
  
  val <- suppressWarnings(as.numeric(info$value))
  if (is.na(val)) return()
  
  unit_scale <- if (identical(input$absq_spike_unit, "nmol")) 1000 else 1
  
  disp     <- absq_spike_display()
  is_row   <- disp[info$row, "IS row"]
  idx_spk  <- match(is_row, rv$absq_spikes[[key]]$IS_row)
  if (is.na(idx_spk)) return()
  
  rv$absq_spikes[[key]]$spike_pmol[idx_spk] <- val * unit_scale
})

# ------------------------------------------------------------------------------
# Compute absolute amounts
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Compute absolute amounts (UPDATED)
# ------------------------------------------------------------------------------
observeEvent(input$absq_compute, {
  req(input$absq_matrix_type, absq_seq(), absq_data(), absq_is_parsed())
  
  key <- input$absq_matrix_type
  absq_ensure_spike_table()
  spikes <- rv$absq_spikes[[key]]
  req(spikes)
  
  d <- absq_data()
  seq <- absq_seq()
  
  # 1) Sample columns
  mask <- absq_selected_sample_mask()
  sample_cols <- which(mask)
  req(length(sample_cols) > 0)
  sample_ids <- colnames(d)[sample_cols]
  
  # 2) Process Sample Amounts
  if (!"amount" %in% colnames(seq)) {
    showNotification("Missing 'amount' column in sequence file.", type = "error")
    return()
  }
  
  raw_amt <- suppressWarnings(as.numeric(seq[sample_ids, "amount"]))
  if (any(is.na(raw_amt))) {
    raw_amt[is.na(raw_amt)] <- median(raw_amt, na.rm = TRUE)
    showNotification("Missing sample amounts replaced with median.", type = "warning")
  }
  
  # --- LOGIC UPDATE START ---
  
  # Determine denominator (mg vs uL)
  is_bio <- grepl("Biofluid", key, ignore.case = TRUE)
  unit_den <- if (is_bio) "\u00B5L" else "mg"
  
  # Determine numerator (pmol vs nmol) from input
  unit_num <- input$absq_spike_unit 
  
  # Create the label, e.g., "nmol/mg" or "pmol/uL"
  unit_lbl <- paste0(unit_num, "/", unit_den)
  
  # 3) Match IS rows
  is_rows <- spikes$IS_row
  is_map <- absq_choose_is_index(d, seq, is_rows, method = input$absq_is_method)
  
  # 4) Math: Raw / IS * Spike / SampleAmt
  raw_int <- d[, sample_cols, drop = FALSE]
  is_int <- d[is_rows[is_map], sample_cols, drop = FALSE]
  
  # Retrieve stored spike amounts (These are always stored in pmol base)
  spike_amt_pmol <- spikes$spike_pmol[is_map]
  
  # Convert spike amount to the target unit selected by user
  # If User wants nmol, we divide pmol by 1000. If pmol, factor is 1.
  conversion_factor <- if (unit_num == "nmol") 1/1000 else 1
  spike_amt_target <- spike_amt_pmol * conversion_factor
  
  eps <- 1e-12
  # Calculate using the converted spike amount
  res <- (raw_int / pmax(is_int, eps)) * spike_amt_target
  res_final <- sweep(res, 2, raw_amt, "/") # divide by mass/volume
  
  # --- LOGIC UPDATE END ---
  
  # 5) Build preview table
  name_row_idx <- which(seq[, 1] == "Name")[1]
  if (is.na(name_row_idx)) name_row_idx <- 1
  
  compounds <- d[, name_row_idx]
  is_used_nm <- d[is_rows[is_map], name_row_idx]
  
  preview_df <- data.frame(
    Compound = compounds,
    `IS used` = is_used_nm,
    Units = unit_lbl, # This now reflects the correct dynamic unit
    check.names = FALSE
  )
  
  preview_df <- cbind(preview_df, as.data.frame(res_final))
  
  # --- NEW FILTERING LOGIC ---
  if (isTRUE(input$absq_remove_is_rows)) {
    # 'is_rows' holds the indices of the IS in the original data
    if (length(is_rows) > 0) {
      preview_df <- preview_df[-is_rows, , drop = FALSE]
    }
  }    
  
  rv$absq_result <- preview_df
  
  # Update reference table for visibility
  rv$absq_amounts_used <- data.frame(
    Sample = sample_ids,
    Amount = raw_amt,
    Unit = unit_den,
    stringsAsFactors = FALSE
  )
  
  showNotification(paste("Absolute amounts calculated in", unit_lbl), type = "message")
  shinyjs::enable("absq_save")
})

# ------------------------------------------------------------------------------
# Render result tables in modal
# ------------------------------------------------------------------------------
output$absq_result_table_modal <- DT::renderDT({
  req(rv$absq_result)
  DT::datatable(
    rv$absq_result,
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
})

# ------------------------------------------------------------------------------
# Save back to dataset
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Save back to dataset (UPDATED)
# ------------------------------------------------------------------------------
observeEvent(input$absq_save, {
  req(rv$absq_result, absq_data(), absq_seq(), input$absq_matrix_type)
  
  d <- absq_data()
  res <- rv$absq_result
  
  # Prepare destination data
  d_out <- d
  
  # --- NEW ALIGNMENT LOGIC ---
  # If results have fewer rows than original data, we assume IS rows were removed.
  # We must apply the same removal to 'd_out' so they match.
  if (nrow(res) != nrow(d_out)) {
    is_rows_save <- grep("\\(IS\\)", d[, 1], ignore.case = TRUE)
    if (length(is_rows_save) > 0) {
      d_out <- d_out[-is_rows_save, , drop = FALSE]
    }
    
    # Safety check
    if (nrow(res) != nrow(d_out)) {
      showNotification("Error: Row count mismatch. Could not align results.", type = "error")
      return()
    }
  }
  # ---------------------------
  
  meta_cols <- c("Compound", "IS used", "Units")
  sample_cols_res <- setdiff(colnames(res), meta_cols)
  common <- intersect(colnames(d), sample_cols_res)
  
  if (!length(common)) {
    showNotification("No matching sample columns between data and AQ result.", type = "error")
    return()
  }
  
  idx_d <- match(common, colnames(d_out)) # Match against the (potentially filtered) d_out
  idx_res <- match(common, colnames(res))
  
  d_out[, idx_d] <- as.matrix(res[, idx_res, drop = FALSE])
  
  # Construct Suffix
  is_bio <- grepl("Biofluid", input$absq_matrix_type, ignore.case = TRUE)
  unit_den <- if (is_bio) "uL" else "mg"
  unit_num <- input$absq_spike_unit
  suffix <- paste0("_AbsQ_", unit_num, "_", unit_den)
  
  info <- paste0("Absolute Quant (", input$absq_is_method, ", ", unit_num, "/", unit_den, ")")
  
  rv$tmpData <- d_out
  # Note: The sequence file usually doesn't change rows, but if your system relies on 1:1 mapping 
  # between sequence and data rows, you might need to filter 'rv$tmpSequence' here too.
  # Assuming standard Lipidomics workflow where Sequence is metadata per sample (columns), 
  # this is usually fine.
  rv$tmpSequence <- absq_seq() 
  
  updateDataAndSequence(
    "Absolute Quantification Saved",
    isTRUE(input$absq_save_as_new),
    suffix,
    info
  )
  removeModal()
})







# ==============================================================================
### for the UI.R document

# --- Absolute Quantification trigger in sidebar ---

tags$head(
  tags$style(HTML("
    .absq-modal-body {
      padding: 5px 5px 15px 5px;
    }

    .absq-section {
      background-color: #f9fafb;
      border-radius: 8px;
      padding: 15px 20px;
      margin-bottom: 15px;
      border: 1px solid #e5e7eb;
    }

    .absq-section h4 {
      margin-top: 0;
      margin-bottom: 10px;
      font-weight: 600;
    }

    .absq-help-text {
      margin-top: 25px;
      color: #6b7280;
      font-size: 0.9em;
    }

    /* Make buttons look more prominent inside the modal */
    #absq_compute, #absq_save {
      font-weight: 600;
    }
  "))
),


tags$style(HTML("
  /* Spike table: push everything to the left and align text */
  #absq_spike_table_wrapper {
    margin-left: 0;
    padding-left: 0;
  }

  #absq_spike_table table.dataTable {
    margin-left: 0 !important;
  }

  #absq_spike_table table.dataTable th,
  #absq_spike_table table.dataTable td {
    text-align: left;
  }
")),



conditionalPanel(
  condition = "input.normalizeIS > 0",
  tags$hr(),
  h4("Absolute Quantification Calculation"),
  bsButton("absq_open_modal", "Open Absolute Quantification window", width = "90%"),
  
)

