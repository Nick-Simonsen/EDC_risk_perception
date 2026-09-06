# Canonical factor name mapping (keys use the Q3_ column prefix from the raw data)
# For functions that work with short keys (e.g. "1Vl"), we use FACTOR_NAME_MAPPING_SHORT
FACTOR_NAME_MAPPING <- c(
    "Q3_1Vl" = "Voluntariness",
    "Q3_2Im" = "Immediacy",
    "Q3_3KP" = "Known to exposed",
    "Q3_4KS" = "Known to science",
    "Q3_5Av" = "Controllability",
    "Q3_6Fa" = "Newness",
    "Q3_7Ch" = "Chronic-Catastrophic",
    "Q3_8Dr" = "Common-Dread",
    "Q3_9Fa" = "Severity of consequences"
)
FACTOR_NAME_MAPPING_SHORT <- setNames(FACTOR_NAME_MAPPING, sub("^Q3_", "", names(FACTOR_NAME_MAPPING)))

writeLatexSnippet <- function(txt, path) {
  cat(txt, "%\n", file = path, sep = "") # % prevents the final newline from becoming a space in TeX
}

formatDemographicsToLaTeX <- function(demographics, output_dir, country) {

    for (i in seq_along(demographics)) {
        value <- demographics[[i]]
        
        if (i == 1) { # Age
            latex_content <- sprintf("\\textit{Mean}$_{\\mathrm{age}}$ = %.2f, \\textit{SD} = %.2f", value[[1]], value[[2]])
            filename <- paste0("age.tex")
        } else if (i == 2) { # No_age
            latex_content <- as.character(value)
            filename <- paste0("no_age.tex")
        } else if (i == 3) { # Bio_sex
            latex_content <- as.character(value[[1]])
            writeLatexSnippet(latex_content, file.path(output_dir, paste0("total_women.tex")))
            latex_content <- as.character(value[[2]])
            writeLatexSnippet(latex_content, file.path(output_dir, paste0("total_men.tex")))
            latex_content <- as.character((value[[3]]))
            writeLatexSnippet(latex_content, file.path(output_dir, paste0("total_non_binary.tex")))
            latex_content <- as.character(value[[4]])
            filename <- paste0("no_bio_sex.tex")
        } else { # In case I accidentally add items without giving them proper formatting... 
            latex_content <- as.character(value)
            filename <- paste0("item_", i, "_", country, ".tex")
        }
        
        writeLatexSnippet(latex_content, file.path(output_dir, filename))
    }
}


formatKnowledgeFamiliarityToLaTeX <- function(kf, output_dir) {
    knowledge_content   <- sprintf("\\textit{Mean} = %.2f, \\textit{SD} = %.2f",
                                   kf$knowledge[[1]], kf$knowledge[[2]])
    familiarity_content <- sprintf("\\textit{Mean} = %.2f, \\textit{SD} = %.2f",
                                   kf$familiarity[[1]], kf$familiarity[[2]])

    writeLatexSnippet(knowledge_content,   file.path(output_dir, "knowledge.tex"))
    writeLatexSnippet(familiarity_content, file.path(output_dir, "familiarity.tex"))
}


formatLikertDescriptivesToLaTeX <- function(descriptives, output_dir, country) {
    require(dplyr)
    require(tidyr)
    require(kableExtra)
    require(stringr)

    name_mapping <- FACTOR_NAME_MAPPING_SHORT

    item_labels <- c(
        "1" = "Drinking water containing BPA",
        "2" = "Eating food with phthalates",
        "3" = "Taking pharma products containing parabens",
        "4" = "Eating food containing PFAS",
        "5" = "Consuming fish contaminated with BFRs",
        "6" = "Eating fruit/veggies with pesticides",
        "7" = "Apply body lotion containing BPA",
        "8" = "Use deodorant containing phthalates",
        "9" = "Use sunscreen containing parabens",
        "10" = "Apply face creams containing PFAS",
        "11" = "Sleep on mattresses with BFRs",
        "12" = "Using insect repellent with pesticides",
        "13" = "Breathing BPA in dust",
        "14" = "Breathing air containing phthalates from air fresheners",
        "15" = "Breathing parabens in dust",
        "16" = "Breathing PFAS while spending time in carpeted room",
        "17" = "Breathing BFRs released from electronics",
        "18" = "Breathing air containing pesticides from landfills"
    )

    # Helper to split long headers into two lines
    sh <- function(...) sprintf("\\shortstack[c]{%s}", paste(c(...), collapse="\\\\"))


    formatted_data <- descriptives %>%
        pivot_wider(names_from = Statistic, values_from = value) %>%
        mutate(Mean_SD_Mdn = sprintf("%.2f (%.2f) [%.2f]", Mean, SD, Median)) %>%
        dplyr::select(Factor, Item, Mean_SD_Mdn) %>%
        mutate(Factor = as.character(Factor), Item = as.character(Item)) %>%
        mutate(
            Factor = dplyr::if_else(Factor %in% names(name_mapping), name_mapping[Factor], Factor),
            Item   = dplyr::if_else(Item %in% names(item_labels),  item_labels[Item],    Item)
    )

    final_table <- formatted_data %>% pivot_wider(names_from = Factor, values_from = Mean_SD_Mdn)

    overall_means <- sapply(final_table[-1], function(x) {
        vals  <- stringr::str_match(x, "([0-9.]+) \\(([0-9.]+)\\)")
        means <- as.numeric(vals[,2])
        sprintf("%.2f", mean(means, na.rm = TRUE))
    })

    overall_sds <- sapply(final_table[-1], function(x) {
        vals <- stringr::str_match(x, "([0-9.]+) \\(([0-9.]+)\\)")
        sds  <- as.numeric(vals[,3])
        sprintf("%.2f", mean(sds, na.rm = TRUE))
    })

    overall_medians <- sapply(final_table[-1], function(x) {
        vals <- stringr::str_match(x, "([0-9.]+) \\(([0-9.]+)\\) \\[([0-9.]+)\\]")
        mdns <- as.numeric(vals[,4])
        sprintf("%.2f", mean(mdns, na.rm = TRUE))
    })

    final_table <- rbind(
        final_table,
        c(Item = "Overall mean", overall_means),
        c(Item = "Overall SD", overall_sds),
        c(Item = "Overall median", overall_medians)
    )

    headers <- names(final_table)
    headers[-1] <- unname(FACTOR_NAME_MAPPING_SHORT)

    anchor_note <- paste0(
        "\\vspace{0.3em}\n",
        "\\item \\textit{Scale anchors}: ",
        "\\textit{Voluntariness}: 1 = voluntary, 7 = involuntary; ",
        "\\textit{Immediacy}: 1 = immediate, 7 = delayed; ",
        "\\textit{Known to exposed}: 1 = known precisely, 7 = not known; ",
        "\\textit{Known to science}: 1 = known precisely, 7 = not known; ",
        "\\textit{Controllability}: 1 = uncontrollable, 7 = controllable; ",
        "\\textit{Newness}: 1 = new, 7 = old; ",
        "\\textit{Chronic-Catastrophic}: 1 = chronic, 7 = catastrophic; ",
        "\\textit{Common-Dread}: 1 = common, 7 = dread; ",
        "\\textit{Severity of consequences}: 1 = certain not to be fatal, 7 = certain to be fatal."
    )

    latex_table <- kableExtra::kable(final_table,
                                     format     = "latex",
                                     booktabs   = TRUE,
                                     caption    = sprintf("Mean (SD) [Median] Likert-scale Ratings for the 18 Exposure Scenarios Across Nine Risk Attributes (%s)", country),
                                     col.names  = headers,
                                     align      = c("l", rep("r", ncol(final_table)-1)),
                                     escape     = FALSE) %>%
                    kableExtra::kable_styling(
                        latex_options   = c("hold_position", "scale_down"),
                        full_width      = FALSE,
                        font_size       = 8) %>%
                    kableExtra::row_spec(nrow(final_table)-3, hline_after = TRUE) %>%
                    kableExtra::column_spec(1, width = "5.0cm", italic = TRUE, latex_valign = "m") %>%
                    kableExtra::column_spec(2:ncol(final_table), width = "1.75cm", latex_valign = "m") %>%
                    kableExtra::footnote(general = "All items were rated on 7-point Likert scales. Values reported as Mean (SD) [Median].",
                                        threeparttable = TRUE, escape = FALSE) %>%
                    { sub("\\end{tablenotes}", paste0(anchor_note, "\n\\end{tablenotes}"), ., fixed = TRUE) } %>%
                    { sub("\\\\caption\\{(.+?)\\}",
                        sprintf("\\\\caption{\\1}\n\\\\label{table:likert-descriptives_%s}", country), .) } %>%
                    { paste0("\\begin{landscape}\n\\begin{adjustbox}\n", .,
                        "\n\\end{adjustbox}\n\\end{landscape}") }

    writeLines(latex_table, file.path(output_dir, paste0("likertTable_", country, ".tex")))
}

formatCombinedLikertDescriptivesToLaTeX <- function(descriptives_by_country, output_dir) {
    require(dplyr)
    require(tidyr)
    require(kableExtra)
    require(stringr)

    name_mapping <- FACTOR_NAME_MAPPING_SHORT

    # Items 1-6: consumption; 7-12: dermal application; 13-18: inhalation
    item_to_category <- c(
        setNames(rep("Consumption", 6), as.character(1:6)),
        setNames(rep("Dermal Application", 6), as.character(7:12)),
        setNames(rep("Inhalation", 6), as.character(13:18))
    )
    category_levels <- c("Consumption", "Inhalation", "Dermal Application")
    factor_levels <- unname(name_mapping)
    countries <- names(descriptives_by_country)

    combined <- dplyr::bind_rows(
        lapply(countries, function(cn) {
            dplyr::mutate(descriptives_by_country[[cn]], Country = cn)
        })
    )

    summarised <- combined %>%
        dplyr::mutate(
            Factor   = dplyr::if_else(Factor %in% names(name_mapping),
                                      name_mapping[Factor], Factor),
            Category = item_to_category[as.character(Item)]
        ) %>%
        dplyr::filter(!is.na(Category)) %>%
        tidyr::pivot_wider(names_from = Statistic, values_from = value) %>%
        dplyr::group_by(Country, Factor, Category) %>%
        dplyr::summarise(
            Mean    = mean(Mean, na.rm = TRUE),
            SD      = mean(SD, na.rm = TRUE),
            Median  = mean(Median, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            cell        = sprintf("%.2f (%.2f) [%.2f]", Mean, SD, Median),
            Factor      = factor(Factor,   levels = factor_levels),
            Category    = factor(Category, levels = category_levels),
            Country     = factor(Country,  levels = countries)
        )

    wide <- summarised %>%
        dplyr::select(Factor, Country, Category, cell) %>%
        tidyr::unite("col", Country, Category, sep = "__") %>%
        tidyr::pivot_wider(names_from = col, values_from = cell)

    col_order <- unlist(lapply(countries,
        function(c) paste(c, category_levels, sep = "__"))) # Ensure columns are ordered by country, then category
    wide <- wide[, c("Factor", col_order)]
    wide <- wide %>% dplyr::arrange(Factor)

    header_cols <- c("", rep(category_levels, length(countries)))

    top_header <- setNames(
        c(1, rep(length(category_levels), length(countries))),
        c(" ", countries)
    )

    anchor_note <- paste0(
        "\\vspace{0.3em}\n",
        "\\item \\textit{Scale anchors}: ",
        "\\textit{Voluntariness}: 1 = voluntary, 7 = involuntary; ",
        "\\textit{Immediacy}: 1 = immediate, 7 = delayed; ",
        "\\textit{Known to exposed}: 1 = known precisely, 7 = not known; ",
        "\\textit{Known to science}: 1 = known precisely, 7 = not known; ",
        "\\textit{Controllability}: 1 = uncontrollable, 7 = controllable; ",
        "\\textit{Newness}: 1 = new, 7 = old; ",
        "\\textit{Chronic-Catastrophic}: 1 = chronic, 7 = catastrophic; ",
        "\\textit{Common-Dread}: 1 = common, 7 = dread; ",
        "\\textit{Severity of consequences}: 1 = certain not to be fatal, 7 = certain to be fatal."
    )

    latex_table <- kableExtra::kable(
            wide,
            format      = "latex",
            booktabs    = TRUE,
            linesep     = "",
            caption     = "Mean (SD) [Median] Psychometric Ratings by Exposure Pathway and Country",
            col.names   = header_cols,
            align       = c("l", rep("r", length(col_order))),
            escape      = FALSE) %>%
        kableExtra::add_header_above(top_header, bold = TRUE) %>%
        kableExtra::kable_styling(
            latex_options   = c("hold_position", "scale_down"),
            full_width      = FALSE,
            font_size       = 8) %>%
        kableExtra::column_spec(1, width = "4.5cm", italic = TRUE, latex_valign = "m") %>%
        kableExtra::column_spec(2:ncol(wide), width = "2.4cm", latex_valign = "m") %>%
        kableExtra::footnote(
            general         = "All items were rated on 7-point Likert scales. Per-item Mean/SD/Median were computed within each country, then averaged across the items belonging to each exposure category.",
            threeparttable  = TRUE, escape = FALSE) %>%
        { sub("\\end{tablenotes}", paste0(anchor_note, "\n\\end{tablenotes}"), ., fixed = TRUE) } %>%
        { sub("\\\\caption\\{(.+?)\\}",
              "\\\\caption{\\1}\n\\\\label{table:likert-descriptives_combined}", .) } %>%
        { paste0("\\begin{landscape}\n\\begin{adjustbox}\n", .,
                 "\n\\end{adjustbox}\n\\end{landscape}") }

    writeLines(latex_table, file.path(output_dir, "likertTable_combined.tex"))
}


formatHealthDescriptivesToLaTeX <- function(df,
                                            name = "health_descriptives",
                                            main_caption = "Mean (SD) [Median] Ratings by Category and Country",
                                            output_dir = "./Results") {
  require(dplyr)
  require(tidyr)
  require(knitr)
  require(kableExtra)

  # If the input is wide (no Category/Rating), infer DV columns and pivot to long.
  if (!all(c("Category", "Rating") %in% names(df))) {
    grouping <- intersect(names(df), c("Subject", "Country"))
    dv_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], grouping)
    if (length(dv_cols) == 0)
      stop("Could not infer DV columns. Ensure numeric DVs are present (e.g., Severity/Susceptibility/Overall risk).")
    df <- df %>%
      dplyr::select(dplyr::any_of(c("Subject", "Country")), dplyr::all_of(dv_cols)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(dv_cols),
                          names_to = "Category",
                          values_to = "Rating")
    df$Category <- factor(df$Category, levels = dv_cols)
  }

  # Normalize factor levels for ordering in the table.
  df$Country  <- if (is.factor(df$Country)) df$Country else factor(df$Country)
  df$Category <- if (is.factor(df$Category)) df$Category else factor(df$Category)

  categories <- levels(df$Category)
  countries  <- levels(df$Country)

  # Mean (SD) per Category x Country; complete empty cells with "--".
  summ <- df %>%
    dplyr::group_by(Category, Country) %>%
    dplyr::summarise(
      mean = mean(Rating, na.rm = TRUE),
      sd   = stats::sd(Rating, na.rm = TRUE),
      mdn  = stats::median(Rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(cell = sprintf("$%.2f\\ (%.2f)\\ [%.2f]$", mean, sd, mdn)) %>%
    tidyr::complete(
      Category = factor(categories, levels = categories),
      Country  = factor(countries, levels = countries)
    ) %>%
    dplyr::mutate(cell = ifelse(is.na(cell), "--", cell))

  wide <- summ %>%
    dplyr::select(Category, Country, cell) %>%
    tidyr::pivot_wider(names_from = Country, values_from = cell) %>%
    dplyr::arrange(factor(Category, levels = categories))

  # Header: blank first col, escape country names.
  colnames(wide) <- c("", tex_escape(colnames(wide)[-1]))

  label_core <- slug(name)
  tex <- knitr::kable(
    wide,
    format   = "latex",
    booktabs = TRUE,
    caption  = main_caption,
    align    = c("l", rep("r", length(countries))),
    escape   = FALSE
  ) %>%
    { sub("\\\\caption\\{(.+?)\\}",
          sprintf("\\\\caption{\\1}\n\\\\label{table:%s}", label_core), .) } %>%
    kableExtra::kable_styling(latex_options = c("HOLD_position"),
                              full_width = FALSE) %>%
    kableExtra::column_spec(1, italic = TRUE) %>%
    kableExtra::footnote(general = "All items were rated on 7-point Likert scales. Values reported as Mean (SD) [Median].",
                         threeparttable = TRUE, escape = FALSE)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_path <- file.path(output_dir, paste0(slug(name), ".tex"))
  writeLines(tex, output_path)
  invisible(output_path)
}


format_and_bold <- function(x) {
    x <- as.matrix(x)
    abs_x <- abs(x)
    max_abs <- apply(abs_x, 1, max) # Find the maximum absolute value for each row
    formatted <- sprintf("%.2f", x)
    formatted <- gsub("-", "\\\\mbox{-}", formatted) # Non-breaking hyphen
    bolded <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
    for (i in 1:nrow(x)) {
        bolded[i, ] <- abs_x[i, ] == max_abs[i] # Identify values to bold for each row
    }
    ifelse(bolded, paste0("\\textbf{", formatted, "}"), formatted)
}


inline_math <- function(x) sprintf("\\(%s\\)", x)


format_p_value <- function(p, inline = FALSE) {
  if (is.character(p) && grepl("<", p)) out <- "\\textit{p} < .001"
  else {
    p <- as.numeric(p)
    out <- dplyr::case_when(
      p < .001 ~ "\\textit{p} < .001",
      p < .01 ~ "\\textit{p} < .01",
      p < .05 ~ "\\textit{p} < .05",
      p >= 0.9995 ~ "\\textit{p} = 1",
      p < 1 ~ paste0("\\textit{p} = .", sub("^0\\.", "", sprintf('%.3f', p))),
      TRUE ~ "\\textit{p} = 1",
    )
  }
  if (inline) out else sprintf("$%s$", out)
}


write_latex_value <- function(content, output_dir, filename) {
    content <- as.character(content) # Ensure content is character; otherwise, writeLines will fail
    file_path <- file.path(output_dir, paste0(filename, ".tex"))
    writeLatexSnippet(content, file_path)
}


slug <- function(x) {
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("[^[:alnum:]_\\-]+", "", x)
    substr(x, 1, 120)
}


tex_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\", x)
    x <- gsub("([#%&_{}$])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
}


createLoadingsTable <- function(pca_results) {
    require(knitr)
    require(kableExtra)
    require(dplyr)

    name_mapping <- FACTOR_NAME_MAPPING

    rownames(pca_results$pca$loadings) <- name_mapping[rownames(pca_results$pca$loadings)]
    loadings_matrix <- as.data.frame(unclass(pca_results$pca$loadings))

    # Apply the function to the loadings matrix
    loadings_matrix[] <- format_and_bold(loadings_matrix)
    rownames(loadings_matrix) <- gsub("_", " ", rownames(pca_results$pca$loadings))

    eigenvalues <- pca_results$eigenvalues
    ss_loadings   <- pca_results$ss_loadings
    prop_variance <- pca_results$prop_variance

    loadings_matrix <- rbind(
        loadings_matrix,
        `Sums of Squares` = sprintf("%.2f", ss_loadings[1:ncol(loadings_matrix)]),
        `Eigenvalue` = sprintf("%.2f", eigenvalues[1:ncol(loadings_matrix)]),
        `Proportion of Variance` = sprintf("%.2f", prop_variance[1:ncol(loadings_matrix)])
    )

    loadings_table <- knitr::kable(loadings_matrix, 
                            format      = "latex",
                            booktabs    = TRUE,
                            caption     = "PCA Loadings with Varimax Rotation, Eigenvalues, and Variance Explained",
                            align       = c("l", rep("r", ncol(loadings_matrix))),
                            col.names   = paste0("RC", 1:ncol(loadings_matrix)),
                            escape      = FALSE) %>% # Allow LaTeX commands
        kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE) %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1, italic = TRUE) %>%
        column_spec(2:ncol(loadings_matrix), width = "1.8cm") %>%
        row_spec(nrow(loadings_matrix) - 3, hline_after = TRUE)

    invisible(loadings_table)
}


formatPCAResultsToLaTeX <- function(pca_results, output_dir, country) {
    require(knitr)
    require(kableExtra)
    require(dplyr)

    kmo_value <- as.character(round(pca_results$kmo$MSA, 2))
    bartlett_value <- sprintf("$\\chi^2(%d, N = %d) = %.2f$, %s", 
                              pca_results$bartlett$df, 
                              nrow(pca_results$pca$scores), 
                              pca_results$bartlett$chisq, 
                              format_p_value(pca_results$bartlett$p.value))
    components_text <- as.character(pca_results$num_components)
    communality_text <- as.character(round(pca_results$pca_communality, 2))
    loadings_table <- createLoadingsTable(pca_results)
    
    write_files <- list(
        "kmo_test.tex" = kmo_value,
        "bartlett_test.tex" = bartlett_value,
        "number_components.tex" = components_text,
        "pca_communality.tex" = communality_text,
        "pca_loadings.tex" = as.character(loadings_table)
    )
    
    mapply(function(content, filename) {
        writeLatexSnippet(content, file.path(output_dir, filename))
    }, write_files, names(write_files))
    
}


createCombinedTable <- function(table_list,
                                country,
                                output_dir,
                                main_caption = "Varimax-rotated Principal Component Loadings for Perceived EDC Risk Attributes Across Countries") {
    if (length(table_list) != length(country)) {
        stop("The length of table_list and country must be the same.")
    }
    
    # Rename loadings columns with country suffixes before merging
    loadings_list <- lapply(seq_along(table_list), function(i) {
        df <- data.frame(unclass(table_list[[i]]$pca$loadings))
        country_suffix <- country[i]
        colnames(df) <- paste0(colnames(df), "_", country_suffix)
        df$Row.names <- rownames(table_list[[i]]$pca$loadings)
        df
    })
    
    # Merge data while keeping proper country suffixes
    merged_df <- Reduce(function(x, y) merge(x, y, by = "Row.names", all = TRUE), loadings_list)

    name_mapping <- FACTOR_NAME_MAPPING
    merged_df$Row.names <- name_mapping[merged_df$Row.names]

    col_names <- colnames(merged_df)
    loadings_cols <- col_names[-1] # Exclude Row.names
    
    # Bold highest absolute values per country group
    for (cntry in unique(country)) {
        country_cols <- grep(paste0("_", cntry, "$"), col_names, value = TRUE)
        country_data <- as.matrix(merged_df[, country_cols])
        merged_df[, country_cols] <- format_and_bold(country_data) # Call format_and_bold helper function
    }
    
    # Prepare display names and group headers
    clean_cols <- sapply(strsplit(loadings_cols, "_"), `[`, 1)
    header <- c(" " = 1)
    for (cntry in unique(country)) {
        n_cols <- sum(grepl(paste0("_", cntry, "$"), loadings_cols))
        header <- c(header, setNames(n_cols, cntry))
    }

    ss_loadings    <- unlist(lapply(table_list, `[[`, "ss_loadings"))
    eigenvalues <- unlist(lapply(table_list, `[[`, "eigenvalues"))
    prop_var    <- unlist(lapply(table_list, `[[`, "prop_variance"))

    ss_loadings_row <- setNames(data.frame(t(c("Sum of Squares", sprintf("%.2f", ss_loadings)))),
                                c("Row.names", loadings_cols))
    eigenvalues_row <- setNames(data.frame(t(c("Eigenvalue", sprintf("%.2f", eigenvalues)))),
                                c("Row.names", loadings_cols))
    proportion_variance_row <- setNames(data.frame(t(c("Proportion of Variance", sprintf("%.2f", prop_var)))),
                                        c("Row.names", loadings_cols))

    merged_df <- rbind(merged_df, ss_loadings_row, eigenvalues_row, proportion_variance_row)
        
    loadings_table <- knitr::kable(merged_df,
                            format      = "latex",
                            booktabs    = TRUE,
                            caption     = main_caption,
                            align       = c("l", rep("r", length(loadings_cols))),
                            col.names   = c("", clean_cols),
                            escape      = FALSE) %>%
    { sub("\\\\caption\\{(.+?)\\}",
          "\\\\caption{\\1}\n\\\\label{table:pca_loadings_table}", .) } %>%
    kable_styling(
        latex_options = c("hold_position", "scale_down"),
        full_width = FALSE) %>%
    add_header_above(header) %>%
    row_spec(nrow(merged_df)-3, hline_after = TRUE) %>%
    row_spec(nrow(merged_df)-2, hline_after = FALSE) %>%
    column_spec(1, italic = TRUE) %>%
    column_spec(2:ncol(merged_df), width = "1.6cm")
    
    writeLines(loadings_table, file.path(output_dir, "combined_PCA_table.tex"))
}


formatANOVAResultsToLaTeX <- function(anova_results, output_dir) {
    require(dplyr)
    require(purrr)

    extract_values <- function(result, keys) { # Iterate over keys vector, extract values and set names of keys
        map(keys, ~ result[[.x]]) %>% set_names(keys)
    }

    write_test_results <- function(statistic, df1, df2, p_value, prefix) {
        if (is.null(statistic) || is.null(df1) || is.null(p_value)) return(NULL)
        p_formatted <- format_p_value(p_value, inline = TRUE)
        tex_sym <- ifelse(anova_results$omnibus_label == "eta_p2",
                            "\\eta_p^2", "\\epsilon^2")

        if (is.null(df2)) {
            full_result <- paste0("$\\chi^2(", df1, ") = ", sprintf("%.2f", statistic), ", ", p_formatted, sprintf(", %s = %.2f$", tex_sym, anova_results$omnibus_es))
        } else {
            full_result <- paste0("$\\textit{F}(", df1, ", ", df2, ") = ", sprintf("%.2f", statistic), ", ", p_formatted, sprintf(", %s = %.2f$", tex_sym, anova_results$omnibus_es))
        }
        write_latex_value(full_result, output_dir, paste0(prefix, "_full_result"))
    }

    test_type <- if (grepl("Kruskal", anova_results$main_test_message)) "kruskal" else "anova"
    write_latex_value(test_type, output_dir, "test_type")
    
    if (!is.null(anova_results$main_test_result)) {
        if (test_type == "kruskal") {
            kw <- anova_results$main_test_result
            write_test_results(kw$statistic, kw$parameter, NULL, kw$p.value, "ANOVA")
        } else {
            aovtab <- anova_results$main_test_result
            write_test_results(aovtab$`F value`[1], aovtab$Df[1], aovtab$Df[2], aovtab$`Pr(>F)`[1], "ANOVA")
        }
    }

    if (!is.null(anova_results$levene_test_result)) {
        levene_values <- extract_values(anova_results$levene_test_result, c("F value", "Df", "Pr(>F)"))
        write_test_results(levene_values$`F value`[1], levene_values$Df[1], levene_values$Df[2], levene_values$`Pr(>F)`[1], "levene")
    }

    process_pairwise <- function(p_values, test_type) {
        # Effect-size lookup keyed by sorted "A_vs_B"
        eff_label <- ifelse(test_type == "anova", "d", "r")
        eff_lookup <- if (is.null(anova_results$effect_sizes) || !nrow(anova_results$effect_sizes)) {
            setNames(numeric(0), character(0))
        } else {
            es <- anova_results$effect_sizes
            keys <- paste0(pmin(es$group1, es$group2), "_vs_", pmax(es$group1, es$group2))
            setNames(es$effsize, keys)
        }

        write_pair <- function(a, b, pval) {
            groups <- sort(trimws(c(a, b)))
            pair_name <- paste0(groups[1], "_vs_", groups[2])

            p_txt <- format_p_value(as.numeric(pval))
            eff <- eff_lookup[[pair_name]]
            eff_txt <- if (!is.null(eff) && is.finite(eff)) sprintf(", $%s = %.2f$", eff_label, eff) else ""

            line <- paste0(p_txt, eff_txt)
            write_latex_value(line, output_dir, pair_name)
        }

        if (test_type == "kruskal") {
            if (!is.null(p_values) && nrow(p_values) > 0) {
            rn <- rownames(p_values); cn <- colnames(p_values)
            for (i in seq_along(rn)) for (j in seq_along(cn)) {
                if (!is.na(p_values[i, j])) write_pair(rn[i], cn[j], p_values[i, j])
                }
            }
        } else {
            tukey_results <- anova_results$pairwise_result$ActivityType
            if (!is.null(tukey_results) && nrow(tukey_results) > 0) {
            pcol <- if ("p adj" %in% names(tukey_results)) "p adj" else "p.value"
            purrr::map(seq_len(nrow(tukey_results)), ~{
                pair <- strsplit(rownames(tukey_results)[.x], "-")[[1]]
                write_pair(pair[1], pair[2], tukey_results[.x, pcol])
                })
            }
        }
    }

    if (!is.null(anova_results$pairwise_result)) {
        process_pairwise(anova_results$pairwise_result$p.value, test_type)
    }
}


formatCombinedANOVADescriptivesToLaTeX <- function(anova_results_by_country,
                                                   output_dir = "./Results",
                                                   file_name = "oneway_anova_pathway_descriptives",
                                                   main_caption = "Mean (SD) [Median] Perceived Risk Rankings by Exposure Pathway") {
    require(dplyr)
    require(tidyr)
    require(knitr)
    require(kableExtra)
    require(purrr)

    pathway_levels <- c("consumption", "inhalation", "application")
    pathway_labels <- c(
        "consumption" = "Consumption",
        "inhalation" = "Inhalation",
        "application" = "Dermal Application"
    )
    country_levels <- c("UK", "DK", "US")
    country_labels <- c("UK" = "UK", "DK" = "Denmark", "US" = "US")

    extract_country_rows <- function(anova_res, ctry) {
        vm <- anova_res$variable_means
        if (is.null(vm) || !length(vm)) return(NULL)

        rows <- lapply(vm, function(x) {
            list(
                Pathway   = as.character(x[[1]]),
                Mean    = as.numeric(x[[2]]),
                SD      = as.numeric(x[[3]]),
                Median  = as.numeric(x[[4]])
            )
        })

        bind_rows(rows) %>% mutate(Country = as.character(ctry))
    }

    long_df <- purrr::imap_dfr(anova_results_by_country, extract_country_rows)
    if (!nrow(long_df)) return(invisible(NULL))

    long_df <- long_df %>%
        mutate(
            Pathway   = factor(Pathway, levels = pathway_levels),
            Country = factor(Country, levels = country_levels),
            cell    = ifelse(
                is.na(SD) | is.na(Median),
                sprintf("%.2f", Mean),
                sprintf("%.2f (%.2f) [%.2f]", Mean, SD, Median)
            )
        )

    wide_df <- long_df %>%
        select(Pathway, Country, cell) %>%
        tidyr::pivot_wider(names_from = Country, values_from = cell) %>%
        arrange(Pathway)

    wide_df$Pathway <- pathway_labels[as.character(wide_df$Pathway)]
    colnames(wide_df) <- c("Exposure pathway", country_labels[colnames(wide_df)[-1]])

    latex_tbl <- knitr::kable(
        wide_df,
        format      = "latex",
        booktabs    = TRUE,
        caption     = main_caption,
        align       = c("l", rep("r", ncol(wide_df) - 1)),
        escape      = FALSE
    ) %>%
        { sub("\\\\caption\\{(.+?)\\}",
              sprintf("\\\\caption{\\1}\n\\\\label{table:%s}", "oneway-anova-pathway-descriptives"), .) } %>%
        kableExtra::kable_styling(
            latex_options   = c("HOLD_position", "scale_down"),
            full_width      = FALSE,
            font_size       = 9
        ) %>%
        kableExtra::column_spec(1, italic = TRUE, width = "4.6cm") %>%
        kableExtra::footnote(
            general             = "Lower mean ranks indicate greater perceived health risk, because participants ranked individual exposure scenarios from highest to lowest perceived risk.",
            general_title       = "Note:",
            threeparttable      = TRUE,
            escape              = FALSE,
            footnote_as_chunk   = TRUE
        )

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    writeLines(latex_tbl, file.path(output_dir, paste0(file_name, ".tex")))
    invisible(TRUE)
}


renameRegressionVars <- function(var_name) {
    if (identical(var_name, "(Intercept)")) return("Intercept")
    
    general_vars <- c(
        "social_trust" = "Institutional Trust",
        "Q9_2Age" = "Age",
        "Q8_2Kn_1" = "Knowledge",
        "Q8_1Fa_1" = "Familiarity",
        "genderMale" = "Male",
        "genderNon-binary" = "Non-binary",
        "educational_levelPrimary school" = "Primary School",
        "educational_levelMaster's degree" = "Master's Degree",
        "educational_levelPh.D." = "Ph.D.",
        "educational_levelSecondary school" = "Secondary School",
        "educational_levelVocational or similar" = "Vocational or Similar",
        "perceived_healthVery bad" = "Perceived Health: Very bad",
        "perceived_healthBad" = "Perceived Health: Bad",
        "perceived_healthModerate " = "Perceived Health: Moderate",
        "perceived_healthModerate" = "Perceived Health: Moderate",
        "perceived_healthVery good" = "Perceived Health: Very Good"
    )

    if (var_name %in% names(general_vars)) {
        renamed <- general_vars[[var_name]]
    } else if (grepl("^geographical_residence", var_name)) {
        region <- sub("^geographical_residence", "", var_name)
        renamed <- paste("Geographical Residence:", region)
    } else if (grepl("^annual_household_income", var_name)) {
        income <- sub("^annual_household_income", "", var_name)
        renamed <- paste("Household Income:", income)
    } else if (grepl("^number_children", var_name)) {
        children <- sub("^number_children", "", var_name)
        renamed <- paste("Has Children")
    } else if (grepl("^employment_status", var_name)) {
        status <- sub("^employment_status", "", var_name)
        renamed <- paste("Employment Status:", status)
    } else {
        renamed <- var_name
    }

    renamed <- gsub("\\$", "\\\\$", renamed) # Escape dollar sign (US data only)

    return(renamed)
}


parse_income_order <- function(label) {
    x <- gsub(",", "", label)
    nums <- regmatches(x, gregexpr("\\d+", x))[[1]]

    lower <- as.numeric(nums[1])

    if (grepl("\\bLess than\\b", x, ignore.case = TRUE)) return(lower - 0.5)
    if (grepl("\\bMore than\\b", x, ignore.case = TRUE)) return(lower + 0.5)

    return(lower)
}


parse_health_order <- function(x) {
    x <- tolower(x)
    if (grepl("very bad", x, fixed = TRUE)) return(1)
    if (grepl("bad", x, fixed = TRUE)) return(2)
    if (grepl("moderate", x, fixed = TRUE)) return(3)
    if (grepl("very good", x, fixed = TRUE)) return(5)
    if (grepl("good", x, fixed = TRUE)) return(4)
    return(99) # Catch exceptions (shouldn't be possible)
}


formatRegressionResultsToLaTeX <- function(regression_results,
                                           output_dir,
                                           country,
                                           model_labels = c("Severity", "Susceptibility", "Overall risk")) {
    require(dplyr)
    require(purrr)
    require(kableExtra)

    n_models <- length(regression_results)
    models_list <- lapply(regression_results, `[[`, "original_model")
    robust_indicators <- vapply(regression_results, function(x) isTRUE(x$robust_se_used), logical(1))

    # Union of coefficient names across models
    term_order <- unique(unlist(lapply(models_list, function(m) names(stats::coef(m)))))

    star_sym <- function(p) ifelse(p < .001, "***",
                            ifelse(p < .01,  "**",
                            ifelse(p < .05,  "*", "")))


    per_model <- lapply(seq_len(n_models), function(j) {
        snips <- as.data.frame(regression_results[[j]]$coefficients_for_snippets)

        coef_names <- names(stats::coef(models_list[[j]]))
        snips <- snips[coef_names, , drop = FALSE]

        p_col <- if ("Pr(>|t|)" %in% colnames(snips)) "Pr(>|t|)" else "Pr(>t)"

        tibble(
            term    = coef_names,
            est     = as.numeric(snips[["Estimate"]]),
            se      = as.numeric(snips[["Std. Error"]]),
            p       = as.numeric(snips[[p_col]])
        )
    })

    renamed_predictors <- setNames(sapply(term_order, function(v) tex_escape(renameRegressionVars(v))), term_order)

    # Sort income terms numerically, health terms conceptually
    intercept_term <- "(Intercept)"
    income_term_names <- names(renamed_predictors)[grepl("Household Income", renamed_predictors)]
    health_term_names <- names(renamed_predictors)[grepl("Perceived Health", renamed_predictors)]
    other_term_names <- setdiff(names(renamed_predictors), c(intercept_term, income_term_names, health_term_names))

    ordered_income_names <- income_term_names[order(sapply(renamed_predictors[income_term_names], parse_income_order))]
    ordered_health_names <- health_term_names[order(sapply(renamed_predictors[health_term_names], parse_health_order))]

    final_term_order <- c(intercept_term, other_term_names, ordered_income_names, ordered_health_names)
    final_order <- renamed_predictors[final_term_order]

    display <- tibble(Predictor = final_order)
    for (j in seq_len(n_models)) {
        # Create ordered dataframe with all terms in final_term_order
        ordered_data <- tibble(term = final_term_order) %>%
            left_join(per_model[[j]], by = "term")
        
        # Extract formatted cells
        dfj <- ordered_data %>%
            transmute(cell = ifelse(is.na(est), "", sprintf("%.2f%s (%.2f)", est, star_sym(p), se))) %>%
            pull(cell)
        display[[paste0("m", j)]] <- dfj
    }

    # Add F-statistics 
    F_cells <- sapply(seq_len(n_models), function(j) {
        f_info <- regression_results[[j]]$f_statistic_info
        sprintf("$\\mathit{F}(%d,\\;%d)=%.2f$",
                as.integer(f_info[2]), as.integer(f_info[3]), as.numeric(f_info[1]))
    })
    display <- bind_rows(
        display,
        tibble(Predictor = "$F$-statistic",
               !!!setNames(as.list(F_cells), paste0("m", seq_len(n_models))))
    )

    # Add overall model p-value
    p_cells <- sapply(seq_len(n_models), function(j) {
        f_info <- regression_results[[j]]$f_statistic_info
        pval <- pf(as.numeric(f_info[1]), as.integer(f_info[2]), as.integer(f_info[3]), lower.tail = FALSE)
        format_p_value(pval, inline = TRUE)
    })
    display <- bind_rows(
        display,
        tibble(Predictor = "Model $p$-value",
               !!!setNames(as.list(p_cells), paste0("m", seq_len(n_models))))
    )

    # Add R^2
    r2_cells <- sapply(regression_results, function(res) sprintf("%.2f", res$r_squared))
        display <- bind_rows(
        display,
        tibble(Predictor = "$R^2$",
               !!!setNames(as.list(r2_cells), paste0("m", seq_len(n_models))))
    )

    # Add adjusted R^2
    adjr2_cells <- sapply(models_list, function(m) sprintf("%.2f", summary(m)$adj.r.squared))
        display <- bind_rows(
        display,
        tibble(Predictor = "Adj. $R^2$",
               !!!setNames(as.list(adjr2_cells), paste0("m", seq_len(n_models))))
    )

    # Add Cohen's f^2 (NA-safe fallback)
    fx <- sapply(regression_results, function(x) if (!is.null(x$effect_size)) x$effect_size else NA_real_)
    display <- bind_rows(
        display,
        tibble(Predictor = "Cohen's \\textit{f}\\textsuperscript{2}",
               !!!setNames(as.list(sprintf("%.2f", fx)), paste0("m", seq_len(n_models))))
    )

    # Headers and note
    colnames(display) <- c("Predictor", model_labels)

    note_text <- if (any(robust_indicators)) {
        paste0("Parentheses represent standard errors (SEs). SEs are robust (HC1) for model(s): ", paste(which(robust_indicators), collapse = ", "),
                "; standard SEs otherwise. Signif.: * \\(p < .05\\), ** \\(p < .01\\), *** \\(p < .001\\).")
    } else {
        "Parentheses represent standard errors (SEs). SEs are standard (homoskedastic). Signif.: * \\(p < .05\\), ** \\(p < .01\\), *** \\(p < .001\\)."
    }

    ref_specific <- switch(
        country,
        "UK" = "Country-specific reference categories for income and geographical residence were \\£60,000--\\£79,999 and London.",
        "US" = "Country-specific reference categories for income and geographical residence were \\\\$45,000--\\\\$74,999 and South.",
        "Danish" = "Country-specific reference categories for income and geographical residence were 565,000--749,999 DKK and the Region of Zealand.",
        ""
    )

    ref_note <- paste0(
        "The reference category was female participants with a Bachelor's degree, working full-time, without children, and reporting good health. ",
        ref_specific
    )

    note_text <- paste0(note_text, "\\\\newline ", ref_note) # We use 'newline' because \n doesn't work for some reason...

    latex_tbl <- knitr::kable(
        display,
        format      = "latex",
        booktabs    = TRUE,
        caption     = sprintf("Regression Results for Perceived Severity, Perceived Susceptibility, and Perceived Overall Risk in the %s Sample", country),
        align       = c("l", rep("r", ncol(display) - 1)),
        escape      = FALSE
    ) %>%
        kable_styling(latex_options = c("HOLD_position", "scale_down"),
                      full_width = FALSE, font_size = 7) %>%
        column_spec(1, width = "7.6cm", italic = TRUE) %>%
        column_spec(2:ncol(display), width = "2.30cm") %>%
        { sub("\\\\caption\\{(.+?)\\}",
            sprintf("\\\\captionsetup{font=small}\n\\\\caption{\\1}\n\\\\label{table:regression_results_%s}", country), .) } %>%
        kableExtra::footnote(general = note_text, threeparttable = TRUE, escape = FALSE)

    latex_tbl <- paste0("{\\renewcommand{\\arraystretch}{0.85}\n", latex_tbl, "\n}")

    writeLines(latex_tbl, file.path(output_dir, "combined_regression_summaries.tex"))

    qq_data <- do.call(rbind, lapply(seq_along(regression_results), function(i) {
        data.frame(
            residuals = regression_results[[i]]$qq_plot_residuals,
            model_label = factor(model_labels[i], levels = model_labels)
        )
    }))

    qq_plot <- ggplot(qq_data, aes(sample = residuals)) +
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~model_label, scales = "free") +
        labs(title = paste0("Q-Q Plots of Regression Residuals for the ", country, " Sample"),
            x = "Theoretical Quantiles",
            y = "Sample Quantiles") +
        theme_minimal()
    
    ggsave(
        filename = file.path(output_dir, "combined_qq_plot.pdf"),
        plot = qq_plot,
        width = 6,
        height = 4,
        units = "in"
    )
    
    for (i in seq_along(regression_results)) {
        results <- regression_results[[i]]
        label <- model_labels[i]

        # We base the summaries on the original lm model (even if heterogeneity is detected) because it's the standard approach & we care mostly about individual beta coefficient inference
        f_stat_info = results$f_statistic_info
        f_stat      = f_stat_info[1]                           # F-statistic value
        df1         = as.integer(f_stat_info[2])               # Numerator degrees of freedom
        df2         = as.integer(f_stat_info[3])               # Denominator degrees of freedom
        p_value     = pf(f_stat, df1, df2, lower.tail = FALSE) # Calculate p-value
        r_squared   = results$r_squared

        overall_model_sentence <- sprintf(
            "$\\mathit{F}(%d,\\;%d) = %.2f,\\; %s,\\; R^2 = %.2f$%s",
            df1, df2, f_stat, format_p_value(p_value), r_squared,
            ifelse(results$robust_se_used, " (robust SEs used for coefficients)", "") # Indicate if robust SEs were used
        )
        writeLatexSnippet(overall_model_sentence, file.path(output_dir, paste0(label, "_overall_model.tex")))

        coefficients <- results$coefficients_for_snippets
        significant_coefficients <- coefficients[coefficients[, 4] <= 0.05, , drop = FALSE]

        if (nrow(significant_coefficients) > 0) {
            apply(significant_coefficients, 1, function(row) {
                beta <- row[1]; se <- row[2]; t_stat <- row[3]; p_val <- row[4]
                predictor <- rownames(coefficients)[which(coefficients[, 1] == beta)]
                if (predictor != "(Intercept)") {
                snippet <- inline_math(
                    sprintf("\\beta = %.2f,\\; SE = %.2f,\\; t(%d) = %.2f,\\; %s",
                            beta, se, df2, t_stat,
                            format_p_value(p_val, inline = TRUE))
                )
                fname <- sprintf("%s_%s_significant_coefficient.tex",
                                label,
                                gsub('[^[:alnum:]]+', '_',
                                        renameRegressionVars(predictor)))
                writeLatexSnippet(snippet, file.path(output_dir, fname))
                }
            })
        }

        bp_test <- results$bp_test
        bp_test_formatted <- sprintf("BP = %.2f, df = %d, %s",
                                bp_test$statistic, bp_test$parameter, format_p_value(bp_test$p.value))
        writeLatexSnippet(bp_test_formatted, file.path(output_dir, paste0(label, "_bp_test.tex")))

        shapiro_test <- results$shapiro_test
        shapiro_test_formatted <- sprintf("W = %.2f, %s",
                                        shapiro_test$statistic, format_p_value(shapiro_test$p.value))
        writeLatexSnippet(shapiro_test_formatted, file.path(output_dir, paste0(label, "_shapiro_test.tex")))

        dw_test <- results$dw_test
        dw_test_formatted <- sprintf("DW = %.2f, %s",
                                    dw_test$statistic, format_p_value(dw_test$p.value))
        writeLatexSnippet(dw_test_formatted, file.path(output_dir, paste0(label, "_dw_test.tex")))

    }
}


createRIndexTable <- function(table_list,
                              output_dir,
                              main_caption = "Combined R-Index table", translate = TRUE) {
    require(tidyverse)
    require(knitr)
    require(kableExtra)

    long_tbl <- imap_dfr(
        table_list,
        function(tbl, ctry) {
            out <- tbl %>%
                dplyr::select(Attribute,
                       Group,
                       ChoiceProb = sum_choice_prob,
                       Rindex = Rank_by_Rindex) %>%
                mutate(Country = ctry,
                       Attribute_EN = Attribute)

            if (translate && ctry == "DK") {
                out <- out %>%
                    mutate(Attribute_EN = translateDKAttributes(Attribute))
            }
            out
        }
    )

    wide_tbl <- long_tbl %>%
        dplyr::select(Attribute_EN, Group, Country, ChoiceProb, Rindex) %>%
        pivot_wider(
            names_from = Country,
            values_from = c(ChoiceProb, Rindex),
            names_glue = "{.value}_{Country}"
        )

    country_codes <- names(table_list)
    ordered_cols <- c("Attribute_EN", "Group")

    for (ctry in country_codes) {
        ordered_cols <- c(ordered_cols,
                          paste0("ChoiceProb_", ctry),
                          paste0("Rindex_", ctry))
    }
    wide_tbl <- wide_tbl %>% dplyr::select(all_of(ordered_cols))

    first_ridx <- paste0("Rindex_", country_codes[1])
    wide_tbl <- wide_tbl %>%
        mutate(Attribute_EN = gsub("\\.+", " ", Attribute_EN),
               Group = factor(Group,
                              levels = c("consumption",
                                         "application",
                                         "inhalation"),
                               ordered = TRUE)) %>%
        arrange(Group, get(first_ridx))

    header_top <- c(" " = 2)
    for (ctry in country_codes) {header_top <- c(header_top, setNames(2, ctry))}

    col_sub <- rep(c("Choice Prob.", "R-index"), length(table_list))

    latex_tbl <- knitr::kable(
        wide_tbl,
        format      = "latex",
        booktabs    = TRUE,
        caption     = main_caption,
        align       = c("l", "l", rep("r", ncol(wide_tbl) - 2)),
        col.names   = c("Attribute", "Group", col_sub),
        escape      = TRUE ) %>%
        kableExtra::kable_styling(
                latex_options = c("HOLD_position", "scale_down"),
                full_width = FALSE
        ) %>%
        { sub("\\\\caption\\{(.+?)\\}",
              "\\\\caption{\\1}\n\\\\label{table:rindex_combined}", .) } %>%
        kableExtra::add_header_above(header_top, bold = TRUE) %>%
        kableExtra::column_spec(1, italic = TRUE, width = "8.5cm") %>%
        kableExtra::column_spec(2, width = "3.2cm") %>%
        kableExtra::column_spec(3:ncol(wide_tbl), width = "1.6cm") %>%
        kableExtra::footnote(
            general = "An item with a high R-index score is more likely to be preferred over other items and can therefore be interpreted as having greater perceived importance. Summing across each row of the matrix yields an overall dominance score for each exposure item, which can be used to derive a stochastic rank order.",
            threeparttable = TRUE,
            escape = FALSE
        )

    writeLines(latex_tbl,
               file.path(output_dir, "RindexTable.tex"))

    invisible(wide_tbl)
}


formatMixedANOVAToLaTeX <- function(results, contrasts_as_tables, output_dir) {
    require(tidyverse)
    require(knitr)
    require(kableExtra)

    # afex::nice() encodes df as "d1, d2" string; parse for F(df1, df2)
    parse_df <- function(dfstr) {
        parts <- strsplit(as.character(dfstr), ", ")[[1]]
        parts <- trimws(parts)
        c(suppressWarnings(as.numeric(parts[1])), suppressWarnings(as.numeric(parts[2])))
    }

    # Report integer dfs as integers, else one decimal (vectorized)
    fmt_df <- function(x) {
        x <- as.numeric(x)
        ifelse(
            is.infinite(x), "\\infty",
            ifelse(abs(x - round(x)) < 1e-6,
                sprintf("%d", round(x)),
                sprintf("%.1f", x))
        )
    }

    F_line <- function(df1, df2, Fvalue, p) {
        paste0("$\\textit{F}(", fmt_df(df1), ",\\, ", fmt_df(df2), ") = ",
            sprintf("%.2f", as.numeric(Fvalue)), "$, ", format_p_value(p))
    }

    t_line <- function(df, tvalue, p) {
        paste0("$t(", fmt_df(df), ") = ", sprintf("%.2f", as.numeric(tvalue)),
               "$, ", format_p_value(p))
    }

    safe_col <- function(df, col, default = NA) {
        if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
    }

    headnum <- function(x) as.numeric(sub("([0-9.]+).*", "\\1", as.character(x))) # Extract leading number

    eta_for <- function(effect_label) {
        df <- as.data.frame(results$omnibus_eta)
        eff_col <- if ("Parameter" %in% names(df)) "Parameter" else if ("Effect" %in% names(df)) "Effect" else return(NA_real_)
        eta_col <- intersect(names(df), c("Eta2_partial", "pes"))
        hit <- df[df[[eff_col]] == effect_label, eta_col[1], drop = TRUE]
        as.numeric(hit[1])
    }
    
    createTable <- function(df, caption, label, footnote, output_name) {      

        tex_escape_percent <- function(x) gsub("%", "\\\\%", x, fixed = TRUE) # Escape percent signs for LaTeX without touching $...$ math

        latex_tbl <- knitr::kable(
            df,
            format      = "latex",
            booktabs    = TRUE,
            caption     = caption,
            align       = c("l", rep("r", ncol(df) - 1)),
            escape      = FALSE
        ) %>%
            kableExtra::kable_styling(
                latex_options = c("HOLD_position", "scale_down"),
                font_size = 18,
                full_width = FALSE
            ) %>%
            { sub("\\\\caption\\{(.+?)\\}",
                  sprintf("\\\\caption{\\1}\n\\\\label{table:%s}", label), .) } %>%
            kableExtra::column_spec(1, italic = TRUE, width = "8.5cm") %>%
            kableExtra::column_spec(2:ncol(df), width = "3.2cm") %>%
            kableExtra::footnote(
                general = tex_escape_percent(footnote),
                threeparttable = TRUE,
                escape = FALSE
            )

        writeLines(latex_tbl, file.path(output_dir, output_name))
    }

    # Use the test type flags as single source of truth for branching
    ctry_flag <- results$ctry_test_type
    cat_flag  <- results$cat_test_type
    inter_flag <- if(identical(cat_flag, "emmeans")) "emmeans_pairwise" else "yuen_trimmed"

    # Get Category (within-subjects) main effects
    aovTable <- results$main_anova_table

    if (any(aovTable$Effect == "Category")) {
        res <- aovTable[aovTable$Effect == "Category", ][1, ]
        dfs <- parse_df(res$df);
        eta <- eta_for("Category")
        line <- paste0(F_line(dfs[1], dfs[2], headnum(res$F), res$p.value),
                       if (is.na(eta)) "" else sprintf(", $\\eta_p^2 = %.2f$", eta))
        
        write_latex_value(line, output_dir, "category_main_effect")
    }

    # Get Country (between-subjects) main effect (standard mixed ANOVA or Welch)
    ctry <- results$country_main_effect

    if (identical(ctry_flag, "welch")) {
        eta <- eta_for("Country")
        line <- paste0("$\\textit{F}_\\text{Welch}(", sprintf("%.1f", as.numeric(ctry$df1[1])), ",\\, ",
                       sprintf("%.1f", as.numeric(ctry$df2[1])), ") = ",
                       sprintf("%.2f", as.numeric(ctry$F[1])), "$, ",
                       format_p_value(ctry$p[1]),
                       sprintf("; $\\eta_p^2 = %.2f$", eta))

        write_latex_value(line, output_dir, "country_main_effect")
    } else { # Standard ANOVA main effects
        res <- ctry[1, ]
        dfs <- parse_df(res$df)
        eta <- eta_for("Country")
        line <- paste0("Country main effect: ",
                        F_line(dfs[1], dfs[2], res$F, res$p.value),
                        sprintf(", $\\eta_p^2 = %.2f$", eta))

        write_latex_value(line, output_dir, "country_main_effect")
    }

    # Get Country x Category interaction effects
    if (any(aovTable$Effect %in% c("Country:Category", "Category:Country"))) {
        res <- aovTable[aovTable$Effect %in% c("Country:Category", "Category:Country"), ][1, ]
        dfs <- parse_df(res$df)
        eta <- eta_for("Country:Category")
        line <- paste0(F_line(dfs[1], dfs[2], headnum(res$F), res$p.value),
                       sprintf(", $\\eta_p^2 = %.2f$", eta))

        write_latex_value(line, output_dir, "interaction_main_effect")
    }

    # Simple effects / contrasts
    bycat <- tibble::as_tibble(results$by_category)
    byctry <- tibble::as_tibble(results$by_country)
    ipw <- tibble::as_tibble(results$interaction_pw)

    # Contrasts as tables
    if (identical(cat_flag, "games_howell")) {
        have_g <- "Cohen" %in% names(bycat) || "Hedges_g" %in% names(bycat)
        g_col <- if("Cohen" %in% names(bycat)) "Cohens_d" else "Hedges_g"
        g_lo <- if("d_CI_low" %in% names(bycat)) "d_CI_low" else if ("g_CI_low" %in% names(bycat)) "g_CI_low" else NA_character_
        g_hi <- if("d_CI_high" %in% names(bycat)) "d_CI_high" else if ("g_CI_high" %in% names(bycat)) "g_CI_high" else NA_character_

        bycat_fmt <- bycat %>%
            transmute(
                Category = tex_escape(Category),
                Contrast = tex_escape(contrast),
                `\\(\\Delta M\\)` = sprintf("$%.2f$", safe_col(., "estimate")),
                `$p$` = vapply(safe_col(., "p.value", safe_col(., "p.adj")), format_p_value, character(1)),
                `Effect size` = if (have_g) paste0("$g = ", sprintf("%.2f", .data[[g_col]]), "$") else "--",
                `g\\;95\\%\\;CI` = if (!is.na(g_lo)) paste0("$[", sprintf("%.2f", .data[[g_lo]]), ", ",
                                                        sprintf("%.2f", .data[[g_hi]]), "]$") else "--",
                `Diff\\;95\\%\\;CI` = paste0("$[", sprintf("%.2f", CI_low), ", ", sprintf("%.2f", CI_high), "]$")
            )
        note_bycat <- "Between-country contrasts within each Category using Games-Howell (heteroscedastic). Mean-difference CI shown under “Diff 95\\% CI”. Effect sizes reported as Hedges' g with CI."
    } else {
        bycat_fmt <- bycat %>%
            transmute(
                Category = tex_escape(Category),
                Contrast = tex_escape(contrast),
                `\\(\\Delta M\\)` = sprintf("$%.2f$", estimate),
                `$df$` = sprintf("$%s$", fmt_df(as.numeric(df))),
                `$t$` = sprintf("$%.2f$", t.ratio),
                `$p$` = vapply(p.value, format_p_value, character(1)),
                `Effect size` = paste0("$d = ", sprintf("%.2f", Cohens_d), "$"),
                `95\\%\\;CI` = paste0("$[", sprintf("%.2f", CI_low), ", ", sprintf("%.2f", CI_high), "]$")
            )
        note_bycat <- "Between-country contrasts within each Category using emmeans (Holm-adjusted)."
    }

    if (identical(cat_flag, "games_howell")) {
        byctry_fmt <- byctry %>%
            transmute(
                Country = tex_escape(Country),
                Contrast = tex_escape(contrast),
                `\\(\\Delta M\\)` = sprintf("$%.2f$", estimate),
                `$p$` = vapply(p.value, format_p_value, character(1)),
                `$p_{crit}$` = if ("p.crit" %in% names(.)) sprintf("$%.3f$", as.numeric(p.crit)) else "--",
                `Effect size` = paste0("$d_z = ", sprintf("%.2f$", Cohens_d)),
                `95\\%\\;CI` = paste0("$[", sprintf("%.2f$", CI_low), ", ", sprintf("%.2f$", CI_high), "]$")
            )
        note_byctry <- "Within-country (paired) Category contrasts (20% trimmed means; FWER controlled). Effect sizes reported as paired $d_z$ with 95\\% CI."
    } else {
        byctry_fmt <- byctry %>%
            transmute(
                Country = tex_escape(Country),
                Contrast = tex_escape(contrast),
                `\\(\\Delta M\\)` = sprintf("$%.2f$", estimate),
                `$df$` = sprintf("$%s$", fmt_df(as.numeric(df))),
                `$t$`  = sprintf("$%.2f$", t.ratio),
                `$p$`  = vapply(p.value, format_p_value, character(1)),
                `Effect size` = paste0("$d_z = ", sprintf("%.2f$", Cohens_d)),
                `95\\%\\;CI` = paste0("$[", sprintf("%.2f$", CI_low), ", ", sprintf("%.2f$", CI_high), "]$")
            )
        note_byctry <- "Within-country (paired) Category contrasts (Holm-adjusted). Effect sizes reported as paired $d_z$ with 95\\% CI."
    }

    ipw_fmt <- ipw %>%
        transmute(
            Contrast = tex_escape(contrast),
            `\\(\\Delta M\\)` = sprintf("$%.2f$", estimate),
            `$df$` = sprintf("$%s$", fmt_df(as.numeric(df))),
            `$t$`  = sprintf("$%.2f$", t.ratio),
            `$p$`  = vapply(p.value, format_p_value, character(1)),
            `Effect size` = paste0("$g = ", sprintf("%.2f$", Cohens_d)),
            `95\\%\\;CI` = paste0("$[", sprintf("%.2f$", CI_low), ", ", sprintf("%.2f$", CI_high), "]$")
        )
        
    if (identical(inter_flag, "yuen_trimmed")) {
        note_ipw <- "Interaction contrasts computed as robust difference-of-differences via Yuen's trimmed-means test (20\\% trimming) on subject-level difference scores; $p$-values Holm-adjusted across the nine interaction contrasts. Standardized effect sizes reported as Hedges' $g$ on raw difference scores, with 95\\% CIs adjusted per comparison to match its Holm-corrected significance threshold."
    } else {
        note_ipw <- "Interaction pairwise contrasts (Holm-adjusted)."
    }
    
    # If the Welch-ADF omnibus test is available
    if (!is.null(results$wj_omnibus)) {
        wj <- as_tibble(results$wj_omnibus)
        wj_fmt <- wj %>%
            mutate(
                Effect  = tex_escape(Effect),
                WJ      = sprintf("$%.2f$", stat),
                df1     = sprintf("$%s$", fmt_df(df1)),
                df2     = sprintf("$%s$", fmt_df(df2)),
                `$p$`   = vapply(p, format_p_value, character(1))
            ) %>%
            dplyr::select(Effect, WJ, df1, df2, `$p$`)
        note_wj <- "Welch-ADF Omnibus Test Results (Trimmed Means, Heteroscedastic-robust)."
    }

    createTable(bycat_fmt, "Post-hoc Category Contrasts Between Countries", "bycat_table", note_bycat, "bycat_table.tex")
    createTable(byctry_fmt, "Within-country (Paired) Category Contrasts", "byctry_table", note_byctry, "byctry_table.tex")
    createTable(ipw_fmt, "Interaction Pairwise Contrasts", "ipw_table", note_ipw, "ipw_table.tex")
    createTable(wj_fmt, "Welch-ADF Omnibus Test Results", "wj_table", note_wj, "wj_table.tex")

    invisible(TRUE)
}


formatConsistencyHealthToLaTeX <- function(results,
                                           output_dir,
                                           country,
                                           base_name = "consistency_health_") {

    pc <- results[results$method == "Polychoric", , drop = FALSE]
    sp <- results[results$method == "Spearman",  , drop = FALSE]

    # Polychoric correlation
    assoc_line <- paste0(
        "Polychoric $\\hat{\\rho}=", sprintf("%.2f", as.numeric(pc[["estimate"]])),
        "$ \\([", sprintf("%.2f", as.numeric(pc$CI_low)), ", ",
                    sprintf("%.2f", as.numeric(pc$CI_high)), "]\\), ",
        format_p_value(pc$p, inline = TRUE), "; ",
        "Spearman $\\rho=", sprintf("%.2f", as.numeric(sp[["estimate"]])),
        "$ \\([", sprintf("%.2f", as.numeric(sp$CI_low)), ", ",
                    sprintf("%.2f", as.numeric(sp$CI_high)), "]\\), ",
        format_p_value(sp$p, inline = TRUE)
    )

    write_latex_value(assoc_line, output_dir, paste0(slug(base_name), country, "_association"))

    # Wilcoxon robustness check
    agree_line <- paste0(
        "susceptibility $-$ severity: mean diff $=", sprintf("%.2f", as.numeric(results$mean_diff[1])), "$, ",
        "Wilcoxon $V=", format(as.numeric(results$wilcoxon_V[1]), scientific = FALSE), "$, ",
        format_p_value(results$wilcoxon_p[1], inline = TRUE), "; ",
        "$d_z=", sprintf("%.2f", as.numeric(results$cohens_dz[1])),
        "$, $r=", sprintf("%.2f", as.numeric(results$r_effectsize[1])),
        "$; within $\\pm 1$: ", sprintf("%.2f\\%%", 100*as.numeric(results$prop_within1[1]))
    )

    write_latex_value(agree_line, output_dir, paste0(slug(base_name), country, "_agreement"))
}


formatExposureCLMToLaTeX <- function(results,
                                     output_dir,
                                     country,
                                     base_name = "exposure_clm_") {

    pathway_map <- c(S_cons = "Consumption", S_app = "Application", S_inh = "Inhalation")
    corr <- tibble::as_tibble(results$correlations)
    pcol <- if ("p_adj" %in% names(corr)) "p_adj" else "p"
    has_ci <- all(c("CI_low", "CI_high") %in% names(corr))

    corr$pathway <- pathway_map[match(corr$pathway, names(pathway_map))]
    pieces <- apply(corr, 1, function(r) {
        rho  <- as.numeric(r[["rho"]])
        pval <- as.numeric(r[[pcol]])
        seg <- paste0("Spearman$_{", r[["pathway"]], "}$ $\\rho = ", sprintf("%.2f", rho), "$")

        if (has_ci) {
            seg <- paste0(seg, " \\([", sprintf("%.2f", as.numeric(r[["CI_low"]])), ", ",
                                sprintf("%.2f", as.numeric(r[["CI_high"]])), "]\\)")
        }

        paste0(seg, ", ", format_p_value(pval, inline = TRUE))
    })
    corr_line <- paste(pieces, collapse = "; ")
    write_latex_value(corr_line, output_dir, paste0(slug(base_name), country, "_spearman"))

    # Omnibus LR vs null + McFadden R^2
    lr <- tibble::as_tibble(results$lr_test)
    lr_line <- paste0(
        "$\\chi^2(", lr$df[1], ") = ",
        sprintf("%.2f", as.numeric(lr$LR[1])), ", ",
        format_p_value(lr$p[1], inline = TRUE),
        ", R^2_{\\mathrm{McF}}=", sprintf("%.2f", as.numeric(lr$R2_McF[1])), "$"
    )
    write_latex_value(lr_line, output_dir, paste0(slug(base_name), country, "_omnibus"))

    # Location (parallel) effects
    mt <- tibble::as_tibble(results$model_table)

    if (nrow(mt) > 0) {
        mt <- mt[is.finite(mt$OR), , drop = FALSE]
        if (nrow(mt) > 0) {
            effs <- apply(mt, 1, function(r) {
                sprintf("%s: OR $=%.2f$ [%.2f, %.2f], $z=%.2f$, %s",
                        r[["predictor"]],
                        as.numeric(r[["OR"]]),
                        as.numeric(r[["OR_CI_low"]]),
                        as.numeric(r[["OR_CI_high"]]),
                        as.numeric(r[["z"]]),
                        format_p_value(as.numeric(r[["p"]]), inline = TRUE))
                })
            loc_line <- paste(effs, collapse = "; ")
            write_latex_value(loc_line, output_dir, paste0(slug(base_name), country, "_location_effects"))
        }
    }

    # PO status
    flags <- results$flags
    map_names <- function(x) sub("^C_cons_inh$", "Consumption vs. Inhalation",
                             sub("^C_app_inh$", "Application vs. Inhalation", x))

    po_line <- if (isTRUE(flags$partial_po)) {
        paste0("PO check: partial PO; non-parallel for ",
            paste(map_names(flags$partial_predictors), collapse = " and "),
            "; parallel for ",
            paste(map_names(flags$location_predictors), collapse = " and "))
    } else {
        paste0("PO check: proportional-odds (parallel slopes) supported for both contrasts.")
    }
    write_latex_value(po_line, output_dir, paste0(slug(base_name), country, "_po_status"))
}


formatFriedmanResultsToLaTeX <- function(friedman_results, output_dir) {
    require(dplyr)
    require(purrr)
    require(tibble)

    fr <- friedman_results$main_test_result
    chisq <- unname(fr$statistic)
    df1 <- unname(fr$parameter)
    pval <- unname(fr$p.value)
    p_txt <- format_p_value(pval, inline = TRUE)

    # Kendall's W
    wdf <- as.data.frame(friedman_results$omnibus_es)
    w_col <- names(wdf)[grepl("kendalls?_w", tolower(names(wdf)))] # Pull Kendall's W
    W <- suppressWarnings(as.numeric(wdf[[w_col[1]]][1])) # Extract the acual W
    w_part <- sprintf(", \\textit{W} = %.2f", W)

    full_result <- paste0("$\\chi^2(", df1, ") = ", sprintf("%.2f", chisq), ", ", p_txt, w_part, "$")
    write_latex_value(full_result, output_dir, "Friedman_full_result")

    # Pairwise results
    pw <- friedman_results$pairwise_result
    es <- friedman_results$effect_sizes

    # Effect-size lookup keyed by sorted "group1_vs_group2"
    esdf <- as.data.frame(es)
    cand <- names(esdf)[grepl("r_rank_biserial|rank.?biserial|r_rb", tolower(names(esdf)))] # Pull rank-biserial column used by effectsize
    keys <- paste0(esdf$group1, "_vs_", esdf$group2)
    vals <- suppressWarnings(as.numeric(esdf[[cand[1]]]))
    eff_lookup <- setNames(vals, keys)

    for (i in seq_len(nrow(pw))) {
        g1 <- as.character(pw$group1[i])
        g2 <- as.character(pw$group2[i])
        pair_name <- paste0(g1, "_vs_", g2)

        p_txt <- format_p_value(as.numeric(pw$p_adj[i]))
        rrb <- eff_lookup[[pair_name]]
        eff_txt <- sprintf(", $r_{rb} = %.2f$", rrb)

        write_latex_value(paste0(p_txt, eff_txt), output_dir, pair_name)
    }
}


formatChemicalLabelToLaTeX <- function(results, output_dir) {
    require(dplyr)
    require(knitr)
    require(kableExtra)

    pathway_labels <- c(
        "consumption" = "Consumption",
        "application" = "Dermal Application",
        "inhalation" = "Inhalation"
    )

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    for (ctry in names(results$by_country)) {
        fit <- results$by_country[[ctry]]

        # Omnibus model snippet
        ms <- summary(fit$model)
        fst <- ms$fstatistic
        fval <- as.numeric(fst[1])
        df1 <- as.integer(fst[2])
        df2 <- as.integer(fst[3])
        pval <- pf(fval, df1, df2, lower.tail = FALSE)
        r2 <- ms$r.squared

        omnibus_line <- sprintf(
            "$\\mathit{F}(%d,\\;%d) = %.2f$, %s, $R^2 = %.2f$",
            df1, df2, fval, format_p_value(pval), r2
        )
        write_latex_value(omnibus_line, output_dir, paste0("chemical_label_omnibus_", ctry))

        # Contrasts table: each chemical vs. Pesticides, by pathway (Holm-adjusted)
        ctr_df <- as.data.frame(summary(fit$contrasts_by_pathway))

        ctr_fmt <- ctr_df %>%
            dplyr::mutate(
                Pathway       = dplyr::recode(as.character(Pathway), !!!pathway_labels),
                contrast    = sub(" - Pesticides", "", contrast, fixed = TRUE),
                Estimate    = sprintf("$%.2f$", estimate),
                `$SE$`      = sprintf("$%.2f$", SE),
                `$df$`      = sprintf("$%s$", ifelse(
                    abs(df - round(df)) < 1e-6,
                    as.character(round(df)),
                    sprintf("%.1f", df)
                )),
                `$t$`                   = sprintf("$%.2f$", t.ratio),
                `$p_{\\mathrm{adj}}$`   = vapply(p.value, format_p_value, character(1))
            ) %>%
            dplyr::select(
                Pathway, `Chemical label` = contrast,
                Estimate, `$SE$`, `$df$`, `$t$`, `$p_{\\mathrm{adj}}$`
            )

        ctry_tbl <- knitr::kable(
            ctr_fmt,
            format      = "latex",
            booktabs    = TRUE,
            caption     = sprintf("Chemical Label Contrasts relative to Pesticides within each Exposure Pathway, %s Sample (Holm-adjusted)", ctry),
            align       = c("l", "l", rep("r", ncol(ctr_fmt) - 2)),
            escape      = FALSE
        ) %>%
            { sub("\\\\caption\\{(.+?)\\}",
                  sprintf("\\\\caption{\\1}\n\\\\label{table:chemical_label_contrasts_%s}", ctry), .) } %>%
            kableExtra::kable_styling(
                latex_options   = c("HOLD_position", "scale_down"),
                full_width      = FALSE,
                font_size       = 9
            ) %>%
            kableExtra::column_spec(1, italic = TRUE, width = "3.5cm") %>%
            kableExtra::column_spec(2, width = "3.0cm") %>%
            kableExtra::column_spec(3:ncol(ctr_fmt), width = "1.8cm") %>%
            kableExtra::footnote(
                general         = "Borda scores (higher values indicate greater perceived risk) were modeled using OLS with cluster-robust standard errors clustered by participant. Pesticides served as the reference chemical; $p$-values Holm-adjusted within each pathway.",
                threeparttable  = TRUE,
                escape          = FALSE
            )

        writeLines(ctry_tbl, file.path(output_dir, paste0("chemical_label_contrasts_", ctry, ".tex")))
    }
}
