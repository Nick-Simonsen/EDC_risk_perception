renameIndex <- function(df) {
    index_labels <- c(
        "1" = "Drinking (BPA)",
        "2" = "Eating (phthalates)",
        "3" = "Pharma (parabens)",
        "4" = "Eating (PFAS)",
        "5" = "Eating (BFRs)",
        "6" = "Eating (pesticides)",
        "7" = "Body lotion (BPA)",
        "8" = "Deodorant (phthalates)",
        "9" = "Sunscreen (parabens)",
        "10" = "Face creams (PFAS)",
        "11" = "Sleeping on mattresses with BFRs",
        "12" = "Insect repellent (pesticides)",
        "13" = "Breathing (BPA)",
        "14" = "Breathing (phthalates)",
        "15" = "Breathing (parabens)",
        "16" = "Breathing (PFAS)",
        "17" = "Breathing (BFRs)",
        "18" = "Breathing (pesticides)"
    )
    
    rownames(df) <- seq_len(nrow(df))
    rownames(df) <- index_labels[rownames(df)]
    return(df)
}


.biplotPrep <- function(pca_scores, pca_loadings, country) {

    anchor_pairs <- list(
        Q3_1Vl = list(low = "VOLUNTARY", high = "INVOLUNTARY", scale = c(1, 7)),
        Q3_2Im = list(low = "IMMEDIATE", high = "DELAYED", scale = c(1, 7)),
        Q3_3KP = list(low = "KNOWN", high = "UNKNOWN", scale = c(1, 7)),
        Q3_4KS = list(low = "KNOWN TO SCIENCE", high = "UNKNOWN TO SCIENCE", scale = c(1, 7)),
        Q3_5Av = list(low = "UNCONTROLLABLE", high = "CONTROLLABLE", scale = c(1, 7)),
        Q3_6Fa = list(low = "NEW", high = "OLD", scale = c(1, 7)),
        Q3_7Ch = list(low = "CHRONIC", high = "CATASTROPHIC", scale = c(1, 7)),
        Q3_8Dr = list(low = "COMMON", high = "DREAD", scale = c(1, 7)),
        Q3_9Fa = list(low = "NONFATAL", high = "FATAL", scale = c(1, 7))
    )
    
    # Determine label placements based on loadings
    determine_label_positions <- function(loadings, anchor_pairs) {
        label_positions <- data.frame(
            variable = rownames(loadings),
            PC1_loading = loadings[, 1],
            PC2_loading = loadings[, 2],
            stringsAsFactors = FALSE
        )
        
        label_positions$primary_axis <- ifelse(abs(label_positions$PC1_loading) > abs(label_positions$PC2_loading), "PC1", "PC2")
        
        pc1_pos <- character(); pc1_neg <- character()
        pc2_pos <- character(); pc2_neg <- character()

        for (i in seq_len(nrow(label_positions))) {
            var <- label_positions$variable[i]
            if (!var %in% names(anchor_pairs)) next
                
            low <- anchor_pairs[[var]]$low
            high <- anchor_pairs[[var]]$high
            
            if(label_positions$primary_axis[i] == "PC1") { # PC1
                if (label_positions$PC1_loading[i] >= 0) { pc1_neg <- c(pc1_neg, low); pc1_pos <- c(pc1_pos, high) }
                else { pc1_neg <- c(pc1_neg, high); pc1_pos <- c(pc1_pos, low) }
            } else { # PC2
                if (label_positions$PC2_loading[i] >= 0) { pc2_neg <- c(pc2_neg, low); pc2_pos <- c(pc2_pos, high) } 
                else { pc2_neg <- c(pc2_neg, high); pc2_pos <- c(pc2_pos, low) }
            }
        }
        
        return(list(
            PC1_neg = paste(unique(pc1_neg), collapse = ",\n"),
            PC1_pos = paste(unique(pc1_pos), collapse = ",\n"),
            PC2_neg = paste(unique(pc2_neg), collapse = ",\n"),
            PC2_pos = paste(unique(pc2_pos), collapse = ",\n")
        ))
    }

    limit <- 3.0 # Outer bounds of the plot
    arrow_end <- 2.4 # Where arrows stop

    scores_df <- as.data.frame(renameIndex(pca_scores)[, 1:2])
    colnames(scores_df) <- c("PC1", "PC2")
    scores_df$label <- rownames(scores_df)
    scores_df$is_ghost <- FALSE

    # Stationary anchors
    axis_labels <- determine_label_positions(pca_loadings, anchor_pairs)
    anchors_df <- data.frame(
        PC1 = c(0, 0, arrow_end + 0.3, -(arrow_end + 0.3)),
        PC2 = c(arrow_end + 0.3, -(arrow_end + 0.3), 0, 0),
        label = c(axis_labels$PC2_pos, axis_labels$PC2_neg, axis_labels$PC1_pos, axis_labels$PC1_neg),
        country = country,
        hjust = c(0.5, 0.5, 0, 1),
        vjust = c(0, 1, 0.5, 0.5)
    )

    # Ghost points: Invisible points along arrows and anchors to repel labels
    line_steps <- seq(-arrow_end, arrow_end, length.out = 50)
    ghost_lines <- data.frame(
        PC1 = c(line_steps, rep(0, 50)),
        PC2 = c(rep(0, 50), line_steps),
        label = NA,
        is_ghost = TRUE
    )

    ghost_anchors <- data.frame(
        PC1 = c(rep(0, 5), rep(0, 5), seq(arrow_end, limit, 0.1), seq(-limit, -arrow_end, 0.1)),
        PC2 = c(seq(arrow_end, limit, 0.1), seq(-limit, -arrow_end, 0.1), rep(0, 5), rep(0, 5)),
        label = NA,
        is_ghost = TRUE
    )

    repel_df <- rbind(scores_df, ghost_lines, ghost_anchors)
    repel_df$country <- country

    return(list(
        repel_df    = repel_df,
        anchors     = anchors_df,
        limit       = limit,
        arrow_end   = arrow_end,
        country     = country
    ))

}


.biplotPlot <- function(prepped_list, facet = FALSE, facet_nrow = 1,
                        country_order = NULL, title = NULL) {
    require(ggplot2)
    require(grid)
    require(ggrepel)

    bind <- function(field) do.call(rbind, lapply(prepped_list, `[[`, field))

    df <- bind("repel_df")
    anchors <- bind("anchors")
    limit <- prepped_list[[1]]$limit
    arrow_end <- prepped_list[[1]]$arrow_end

    if (is.null(country_order)) {
        df$country <- factor(df$country, levels = country_order)
        anchors$country <- factor(anchors$country, levels = country_order)
    }

    ggplot(df, aes(x = PC1, y = PC2)) +
        # Stationary arrows
        geom_segment(aes(x = -arrow_end, y = 0, xend = arrow_end, yend = 0),
                     arrow = arrow(length = unit(0.2, "cm"), ends = "both"), linewidth = 1) +
        geom_segment(aes(x = 0, y = -arrow_end, xend = 0, yend = arrow_end), 
                     arrow = arrow(length = unit(0.2, "cm"), ends = "both"), linewidth = 1) +
        
        geom_point(data = subset(df, is_ghost), alpha = 0) + # Ghost points
        geom_point(data = subset(df, !is_ghost), size = 2, color = "black") + # Real data points

        geom_text(data = anchors, aes(label = label, hjust = hjust, vjust = vjust), fontface = "bold", size = 2.8, lineheight = 0.85) +
        
        geom_text_repel(
            data = subset(df, !is.na(label)),
            aes(label = label),
            size = 3,
            force = 15, # Strong push away from all points
            point.padding = 0.6, # Ensure space between text & axis line points
            box.padding = 0.8, # Ensure space between labels
            min.segment.length = 0, # Always draw a line to point
            segment.color = "grey50",
            segment.alpha = 0.5,
            max.overlaps = Inf
        ) +

    facet_wrap(~country, nrow = facet_nrow) +
    scale_x_continuous(limits = c(-limit * 1.3, limit * 1.3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-limit * 1.3, limit * 1.3), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = title, x = "Principal Component 1", y = "Principal Component 2") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.spacing = unit(2, "lines")
    )
}


.buildBiplot <- function(pca_scores, pca_loadings, country) {
    prep <- .biplotPrep(pca_scores, pca_loadings, country)
    .biplotPlot(list(prep), facet = FALSE, country_order = country, title = paste0("PCA Biplot -", country)) # Make sure that "prep" is a list (otherwise plot function will break)
}


createBiplot <- function(pca_scores, pca_loadings, country, output_dir,
                         width = 32.5, height = 12.5, dpi = 300) {
    biplot <- .buildBiplot(pca_scores, pca_loadings, country)

    ggsave(
        file.path(output_dir, paste0(country, "_PCA_Biplot.pdf")),
        plot    = biplot,
        width   = width,
        height  = height,
        units   = "cm",
        dpi     = dpi
    )

    invisible(biplot)
}


createBiplotPanel <- function(pca_results_list, countries, output_dir, file_extension = ".pdf",
                              facet_nrow = 1, width = 65, height = 25, dpi = 300) {

    prepped <- Map(function(res, ctry) {
        .biplotPrep(res$pca$scores, res$pca$loadings, ctry)
    }, pca_results_list, countries)

    panel <- .biplotPlot(
        prepped_list    = prepped,
        facet           = TRUE,
        facet_nrow      = facet_nrow,
        country_order   = countries,
        title           = "PCA biplots"
    )

    ggsave(
        file.path(output_dir, paste0("PCA_Biplots_Panel", file_extension)),
        plot    = panel,
        width   = width,
        height  = height,
        units   = "cm",
        dpi     = dpi
    )

    invisible(panel)
}
