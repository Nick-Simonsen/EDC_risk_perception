##### Init #####
library(tidyverse)
library(car)
library(caret)
library(rstatix)
library(emmeans)
library(effsize)
library(effectsize)
library(reshape2)
library(stringr)
library(purrr)
library(dplyr) # Re-attach after caret (which loads plyr and masks dplyr functions)

source("convertToLaTeX.R")
source("plots.R")
source("helperFuncs.R")

options(warn = -1) # Suppress warnings

##### Descriptive stats functions #####
demographicsDescriptives <- function(df, country, output_dir = './Results') {
    current_year <- 2025 # Year this data was collected
    ages <- current_year - df$Q9_2Age

    mean_age <- mean(ages, na.rm = TRUE)
    sd_age <- sd(ages, na.rm = TRUE)
    no_age <- sum(is.na(ages))

    total_women <- sum(df$Q9_1Ge == "Female", na.rm = TRUE)
    total_men <- sum(df$Q9_1Ge == "Male", na.rm = TRUE)
    total_non_binary <- sum(df$Q9_1Ge == "Non-binary", na.rm = TRUE)
    no_bio_sex <- sum(df$Q9_1Ge == "")

    demographics_lst <- list(
        age <- list(mean_age, sd_age),
        No_age <- no_age,
        bio_sex <- list(total_women, total_men, total_non_binary, no_bio_sex)
    )

    demographics_output_dir <- file.path(output_dir, paste0("Demographics_Results_", country))
    if (!dir.exists(demographics_output_dir)) {
        dir.create(demographics_output_dir, recursive = TRUE)
    }

    formatDemographicsToLaTeX(demographics_lst, demographics_output_dir, country)

    return(demographics_lst)
}

knowledgeFamiliarityDescriptives <- function(df, country, output_dir = './Results') {
    knowledge   <- df %>% select(contains("Q8_2")) %>% mutate(across(everything(), as.numeric)) %>% unlist()
    familiarity <- df %>% select(contains("Q8_1")) %>% mutate(across(everything(), as.numeric)) %>% unlist()

    mean_knowledge <- mean(knowledge, na.rm = TRUE)
    sd_knowledge <- sd(knowledge, na.rm = TRUE)
    mean_familiarity <- mean(familiarity, na.rm = TRUE)
    sd_familiarity <- sd(familiarity, na.rm = TRUE)
    
    kf_lst <- list(
        knowledge   = list(mean_knowledge, sd_knowledge),
        familiarity = list(mean_familiarity, sd_familiarity)
    )
    
    kf_output_dir <- file.path(output_dir, paste0("Demographics_Results_", country))
    if (!dir.exists(kf_output_dir)) {
        dir.create(kf_output_dir, recursive = TRUE)
    }

    formatKnowledgeFamiliarityToLaTeX(kf_lst, kf_output_dir)

    return(kf_lst)
}

likertScaleDescriptives <- function(df, country, output_dir = './Results') {
    likert_data <- df %>% select(contains("Q3")) %>% mutate(across(everything(), as.numeric))
    
    likert_descriptives <- likert_data %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) %>%
        pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

    # Extract meaningful components from variable names and rename them
    likert_descriptives <- likert_descriptives %>%
        mutate(
            Factor      = str_extract(variable, "(?<=Q3_)[0-9A-Za-z]+"), # Regex BS to extracts factor (e.g., 1Vl, 9Fa)
            Item        = str_extract(variable, "(?<=_)[0-9]{1,2}(?=_mean|_sd|_median)"), # Regex BS to extracts item number (1-18)
            Statistic   = case_when(
                str_detect(variable, "_mean$") ~ "Mean",
                str_detect(variable, "_sd$") ~ "SD",
                str_detect(variable, "_median$") ~ "Median",
                TRUE ~ NA_character_
            )
        ) %>%
        select(Factor, Item, Statistic, value)

    descriptives_output_dir <- file.path(output_dir, "Likert_Descriptives")
    if (!dir.exists(descriptives_output_dir)) {
        dir.create(descriptives_output_dir, recursive = TRUE)
    }

    formatLikertDescriptivesToLaTeX(likert_descriptives, descriptives_output_dir, country)

    return(likert_descriptives)
}

##### PCA #####
pcaAnalysis <- function(df, country, output_dir = './Results') {
    require(psych)
    
    splitSecondUnderscore <- function(x) {
        parts <- strsplit(x, "_")[[1]]
        list(Dimensions = paste(parts[1:2], collapse = "_"), Question = paste(parts[-(1:2)], collapse = "_"))
    }

    split_data <- df %>%
        pivot_longer(cols = -ResponseId, names_to = "name", values_to = "Value") %>%
        mutate(split = map(name, splitSecondUnderscore)) %>%
        unnest_wider(split) %>%
        select(-name) %>%
        distinct()

    split_data <- split_data %>% filter(str_detect(Dimensions, "Q3"))
    avg_data <- split_data %>%
        dplyr::group_by(Question, Dimensions) %>%
        dplyr::summarise(avg = mean(Value), .groups = "drop") %>%
        as.data.frame()
    avg_data$Question <- as.integer(avg_data$Question) # Convert to integers to avoid lexicographic character ordering (which will fuck up the subsequent plots!)
    avg_data <- reshape2::dcast(avg_data, Question ~ Dimensions, value.var = "avg")
    avg_data <- avg_data[order(avg_data$Question), ]

    rownames(avg_data) <- as.character(avg_data$Question)
    avg_data <- avg_data[, setdiff(names(avg_data), "Question")]
    
    cor_matrix <- cor(avg_data)
    duplicates <- caret::findCorrelation(cor_matrix, cutoff = 0.99) # ~1 for exact duplicates
    if (length(duplicates) > 0) { avg_data <- avg_data[, -duplicates] }

    cor_matrix <- cor(avg_data) # Recalculate correlation matrix after removing duplicates

    kmo <- KMO(cor_matrix)
    bartlett <- cortest.bartlett(cor_matrix, n = nrow(avg_data))

    # Determine number of components
    eigenvalues <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
    num_components <- sum(eigenvalues > 1L)
    if (num_components == 0) { stop("No components with eigenvalue > 1. PCA cannot be performed.") }

    # Perform PCA with varimax rotation
    pca <- suppressWarnings(psych::principal(avg_data, nfactors = num_components, rotate = "varimax", scores = TRUE))
    pca_communality <- mean(pca$communality)

    characteristics <- rownames(pca$loadings) %>% 
        gsub("_", " ", .) # Replace underscores with spaces

    pca_results <- list(
        pca                 = pca,
        kmo                 = kmo,
        bartlett            = bartlett,
        num_components      = num_components,
        pca_communality     = pca_communality,
        duplicates          = duplicates,
        characteristics     = characteristics,
        eigenvalues         = eigenvalues[1:num_components],
        ss_loadings         = as.numeric(pca$Vaccounted["SS loadings", ]),
        prop_variance       = as.numeric(pca$Vaccounted["Proportion Var", ])
    )

    pca_output_dir <- file.path(output_dir, paste0("PCA_Results_", country))
    if (!dir.exists(pca_output_dir)) {
        dir.create(pca_output_dir, recursive = TRUE)
    }

    formatPCAResultsToLaTeX(pca_results, pca_output_dir, country)
    
    createBiplot(
        pca_scores      = pca$scores,
        pca_loadings    = pca$loadings,
        country         = country,
        output_dir      = pca_output_dir
    )

    return(pca_results)
}

##### One-way ANOVA #####
oneWayANOVA <- function(df, country, output_dir = './Results') {
    bindColumns <- function(start_col, end_col) {
        purrr::map_dfc(list(r1, r2, r3), ~ .x %>% select(start_col:end_col)) # Bind specific columns from each rating set using purrr::map_dfc and lambda function (with starting and ending column indices)
    }
    
    r1 <- df %>% select(contains("R1_")) %>% mutate(across(everything(), as.numeric))
    r2 <- df %>% select(contains("R2_")) %>% mutate(across(everything(), as.numeric)) + 6 # Adding 6 because of temporal distances (i.e., these items were rated second)
    r3 <- df %>% select(contains("R3_")) %>% mutate(across(everything(), as.numeric)) + 12 # Adding 12 because of temporal distances (i.e., these items were rated third)
    
    consumption <- bindColumns(1, 6)
    application <- bindColumns(7, 12)
    inhalation <- bindColumns(13, 18)
    
    combined_long <- bind_rows(
        consumption %>% pivot_longer(cols = everything(), names_to = 'variable', values_to = 'value') %>% na.omit() %>% mutate(ActivityType = "consumption"),
        application %>% pivot_longer(cols = everything(), names_to = 'variable', values_to = 'value') %>% na.omit() %>% mutate(ActivityType = "application"),
        inhalation %>% pivot_longer(cols = everything(), names_to = 'variable', values_to = 'value') %>% na.omit() %>% mutate(ActivityType = "inhalation")
    )

    pathway_stats <- combined_long %>%
        dplyr::group_by(ActivityType) %>%
        dplyr::summarise(
            mean = mean(value, na.rm = TRUE),
            sd = stats::sd(value, na.rm = TRUE),
            median = stats::median(value, na.rm = TRUE),
            .groups = "drop"
        )

    get_pathway_stats <- function(pathway_name) {
        row <- pathway_stats %>% dplyr::filter(ActivityType == pathway_name)
        list(pathway_name, row$mean[[1]], row$sd[[1]], row$median[[1]])
    }

    performPostHocTests <- function(data, test_type, model = NULL) {
        if (test_type == "kruskal") {
            pairwise_result <- suppressWarnings(pairwise.wilcox.test(data$value, data$ActivityType, p.adjust.method = "bonferroni"))
            effect_sizes <- suppressWarnings(data %>% wilcox_effsize(value ~ ActivityType, paired = FALSE))
        } else if (test_type == "anova") {
            tukey_result <- TukeyHSD(model)
            emmeans_result <- suppressWarnings(emmeans(model, pairwise ~ ActivityType))
            effect_sizes <- suppressWarnings(eff_size(emmeans_result, sigma = sigma(model), edf = df.residual(model), method = "d"))
            pairwise_result <- tukey_result
        }
        list(pairwise_result = pairwise_result, effect_sizes = effect_sizes)
    }

    performMainTest <- function(data, test_type, model = NULL) {
        if (test_type == "kruskal") kruskal.test(value ~ ActivityType, data = data)
        else car::Anova(model)
    }

    levene_test_result <- car::leveneTest(value ~ ActivityType, data = combined_long)
    test_type <- ifelse(levene_test_result$`Pr(>F)`[1] < 0.05, "kruskal", "anova")
    anova_model <- if (test_type == "anova") aov(value ~ ActivityType, data = combined_long) else NULL
    main_test_result <- performMainTest(combined_long, test_type, model = anova_model)

    p_val <- if (test_type == "kruskal") main_test_result$p.value else main_test_result$`Pr(>F)`[1]
    if (p_val < 0.05) {
        post_hoc_message <- "Significant difference between groups."
        post_hoc_results <- performPostHocTests(combined_long, test_type, model = anova_model)
    } else {
        post_hoc_message <- "No significant difference between groups."
        post_hoc_results <- list(pairwise_result = NULL, effect_sizes = NULL)
    }

    omnibus_es  <- if (test_type == "anova") {
                        effectsize::eta_squared(
                            anova_model,
                            partial = TRUE
                        )$Eta2_partial[1] # Partial-eta squared
                    } else { # Kruskal-Willis test
                        H <- main_test_result$statistic # Kruskal H
                        k <- length(unique(combined_long$ActivityType))
                        n <- nrow(combined_long)
                        (H - k + 1) / (n - k) # Epsilon squared
                    }
    es_label <- ifelse(test_type == "anova", "eta_p2", "epsilon2")

    anova_results <- list(
        main_test_message   = ifelse(test_type == "kruskal", "Kruskal-Wallis test", "One-way ANOVA"),
        post_hoc_message    = post_hoc_message,
        levene_test_result  = levene_test_result,
        main_test_result    = main_test_result,
        pairwise_result     = post_hoc_results$pairwise_result,
        effect_sizes        = post_hoc_results$effect_sizes,
        variable_means      = list(get_pathway_stats("consumption"),
                                   get_pathway_stats("application"),
                                   get_pathway_stats("inhalation")),
        omnibus_es          = omnibus_es,
        omnibus_label       = es_label
    )

    anova_output_dir <- file.path(output_dir, paste0("Anova_Results_", country))
    if (!dir.exists(anova_output_dir)) {
        dir.create(anova_output_dir, recursive = TRUE)
    }

    formatANOVAResultsToLaTeX(anova_results, anova_output_dir)
    
    return(anova_results)
}

##### Multiple Regression #####
multipleRegression <- function(df, demographics_df, country, output_dir = './Results') {
    require(lmtest)

    # Outcome variables
    perceived_severity <- df %>% select(contains("Q6_1ED_1")) %>% mutate(across(everything(), as.numeric)) %>% unlist()
    perceived_susceptibility <- df %>% select(contains("Q6_1ED_2")) %>% mutate(across(everything(), as.numeric)) %>% unlist()
    perceived_exposure <- df %>% select(contains("Q6_1ED_3")) %>% mutate(across(everything(), as.numeric)) %>% unlist()

    # Predictor variables
    social_trust <- df %>% select(contains("Q7_1")) %>% rowMeans(na.rm = TRUE)
    familiarity_level <- demographics_df %>% select(contains("Q8_1")) %>% mutate(across(everything(), as.numeric))
    knowledge_level <- demographics_df %>% select(contains("Q8_2")) %>% mutate(across(everything(), as.numeric))
    age <- demographics_df %>% select(contains("Q9_2"))

    number_children <- demographics_df %>% # Convert number of children to to binary (chilren/no children)
        select(contains("Q9_5")) %>%
        pull() %>%
        as.character() %>%
        (\(x) ifelse(x == "0", "0", "1"))() %>% # Use a lambda function because otherwise ifelse statement throws a hissy-fit
        as.factor() %>%
        relevel("0")

    # Use helper function to relevel data for baseline regression
    gender <- relevelVars(demographics_df, "Q9_1", c(""), "Female")
    educational_level <- relevelVars(demographics_df, "Q9_6", c("Prefer not to say"), "Bachelor’s degree")
    employment_status <- relevelVars(demographics_df, "Q9_3", c("Other"), "Working full-time")
    perceived_health <- relevelVars(demographics_df, "Q9_8", c("Prefer not to say"), "Good")

    if (country == "UK") {
        geographical_residence <- relevelVars(demographics_df, "region", c(""), "London")
        annual_household_income <- relevelVars(demographics_df, "Q9_7", c("Prefer not to say"), "£60,000 - £79,999 per year")
    }

    if (country == "US") {
        geographical_residence <- relevelVars(demographics_df, "region", c(""), "South")
        annual_household_income <- relevelVars(demographics_df, "Q9_7", c("Prefer not to say"), "$45,000 - $74,999 per year")
    }

    if (country == "DK" | country == "Danish") {
        geographical_residence <- relevelVars(demographics_df, "region", c(""), "Region of Zealand")
        annual_household_income <- relevelVars(demographics_df, "Q9_7", c("Prefer not to say"), "565,000 - 749,999 DKK per year")
    }

    getDiagnostics <- function(model) {
        list(
            reset_test       = resettest(model), # Ramsey RESET test for non-linearity
            vif              = car::vif(model), # VIF values do not depend on robust standard errors, so we just calculate it using the standard model
            bp_test          = bptest(model), # Breusch-Pagan test for homoscedasticity
            shapiro_test     = shapiro.test(model$residuals), # Shapiro-Wilk test for normality
            dw_test          = dwtest(model), # Durbin-Watson test for autocorrelation
            r_squared        = summary(model)$r.squared, # R-squared is not available for robust models and is based on proportion of variance explained (i.e., depends on fitted values and residuals; not standard errors), so we simply use the basic regression here 
            f_statistic_info = summary(model)$fstatistic # F-statistic details
        )
    }
    
    fitModel <- function(outcome, predictor) {
        data <- cbind(outcome, predictor)
        regression_model <- lm(outcome ~ ., data = data, na.action = na.exclude)
        diagnostics <- getDiagnostics(regression_model)

        # Set default values to variables related to model diagnostics
        log_transform_applied <- FALSE
        robust_ses_calculated <- FALSE
        robust_se_values <- c(NULL)
        model_coefficients_summary <- summary(regression_model)$coefficients # Default to standard summary

        if (diagnostics$reset_test$p.value < 0.05) {
            if (any(outcome <= 0)) {
                shift <- abs(min(outcome)) + 1 # If the outcome contains non-positive values, shift them.
                outcome_shifted <- outcome + shift
            } else {
                outcome_shifted <- outcome
            }
            
            outcome_transformed <- log(outcome_shifted)
            
            data_trans <- as.data.frame(cbind(outcome_transformed, predictor))
            regression_model <- lm(outcome_transformed ~ ., data = data_trans, na.action = na.exclude)
            log_transform_applied <- TRUE
            diagnostics <- getDiagnostics(regression_model)
        }

        if (!is.na(diagnostics$bp_test$p.value) && diagnostics$bp_test$p.value < 0.05) {
            require(sandwich)

            cov_matrix <- vcovHC(regression_model, type = "HC1")
            robust_coeftest_output <- coeftest(regression_model, vcov = cov_matrix)
            robust_se_values <- sqrt(diag(cov_matrix))
            model_coefficients_summary <- robust_coeftest_output[,] # Update to be robust model coefficient summary
            robust_ses_calculated <- TRUE
        }

        r_squared <- diagnostics$r_squared
        f2 <- r_squared / (1 - r_squared)

        results <- list(
            original_model            = regression_model, # Always store the original lm object
            coefficients_for_snippets = model_coefficients_summary, # Use this for individual tex files (might be robust)
            log_transform_applied     = log_transform_applied,
            robust_se_values          = robust_se_values, # Store robust SEs if calculated, otherwise NULL
            reset_test                = diagnostics$reset_test,
            vif                       = diagnostics$vif,
            r_squared                 = r_squared,
            f_statistic_info          = diagnostics$f_statistic_info, # Store F-stat details [value, df1, df2]
            bp_test                   = diagnostics$bp_test,
            shapiro_test              = diagnostics$shapiro_test,
            dw_test                   = diagnostics$dw_test,
            effect_size               = f2,
            robust_se_used            = robust_ses_calculated,
            qq_plot_residuals         = residuals(regression_model)
        )

        return(results)
    }
    
    severity_results <- fitModel(perceived_severity, cbind(social_trust, knowledge_level, familiarity_level, age, gender, educational_level, employment_status, geographical_residence, annual_household_income, number_children, perceived_health))
    susceptibility_results <- fitModel(perceived_susceptibility, cbind(social_trust, knowledge_level, familiarity_level, age, gender, educational_level, employment_status, geographical_residence, annual_household_income, number_children, perceived_health))
    exposure_results <- fitModel(perceived_exposure, cbind(social_trust, knowledge_level, familiarity_level, age, gender, educational_level, employment_status, geographical_residence, annual_household_income, number_children, perceived_health))

    regression_results <- list(
        severity_results = severity_results,
        susceptibility_results = susceptibility_results,
        exposure_results = exposure_results
    )

    regression_output_dir <- file.path(output_dir, paste0("Regression_Results_", country))
    if (!dir.exists(regression_output_dir)) {
        dir.create(regression_output_dir, recursive = TRUE)
    }

    formatRegressionResultsToLaTeX(regression_results, regression_output_dir, country)
    
    return(regression_results)
}

##### Exploratory intra- & inter-country perceived EDC health risk perceptions #####
exploratoryMixedRMANOVA <- function(data_list, country_names, contrasts_as_tables = TRUE, output_dir = "./Results") {
    require(afex)
    afex_options(es_aov = "pes") # To get partial eta^2 rather than GES
    require(welchADF)
    require(WRS2)

    addD <- function(emm_contrast, paired = FALSE) {
        tbl <- as_tibble(emm_contrast)
        es <- effectsize::t_to_d(t = tbl$t.ratio, df = tbl$df, paired = paired, ci = 0.95)
        bind_cols(tbl, tibble(Cohens_d = es$d, CI_low = es$CI_low, CI_high = es$CI_high))
    }

    # Paired Cohen's d_z (mean(diff)/sd(diff)); simple effect sizes for within pairs
    paired_dz <- function(df_long, id, within_factor, dv, a, b) {
        wide <- df_long %>%
        dplyr::filter(.data[[within_factor]] %in% c(a, b)) %>%
            tidyr::pivot_wider(id_cols = {{ id }}, names_from = {{ within_factor }}, values_from = {{ dv }}) %>%
            dplyr::mutate(diff = .data[[a]] - .data[[b]]) # a - b, to match WRS2::rmmcp's psihat convention
        m <- mean(wide$diff, na.rm = TRUE)
        sd <- stats::sd(wide$diff, na.rm = TRUE)
        d <- m / sd
        tibble::tibble(contrast = paste0(a, " - ", b), Cohens_dz = d)
    }

    performPostHocRM <- function(mixed_aov, combined_data, test_type) {

        if (test_type == "emmeans") {
            grid <- emmeans(mixed_aov, ~ Category * Country)
            interaction_tbl <- addD(emmeans::contrast(grid, interaction = "pairwise", adjust = "holm"))

            interaction_pw <- interaction_tbl %>%
                        dplyr::mutate(
                        contrast = paste0(.data$Category_pairwise, " | ", .data$Country_pairwise)) %>%
                        select(contrast, estimate, SE, df, t.ratio, p.value, Cohens_d, CI_low, CI_high)
            
            by_cat_pairs <- emmeans::contrast(emmeans(mixed_aov, pairwise ~ Country | Category, adjust = "holm"))
            by_cat_posthoc <- addD(by_cat_pairs) %>%
                select(Category, contrast, estimate, SE, df, t.ratio, p.value, Cohens_d, CI_low, CI_high)

            by_ctry_pairs <- emmeans::contrast(emmeans(mixed_aov, ~ Category | Country), method = "pairwise", adjust = "holm")
            by_ctry_post_hoc <- addD(by_ctry_pairs, paired = TRUE) %>%
                select(Country, contrast, estimate, SE, df, t.ratio, p.value, Cohens_d, CI_low, CI_high)

            return(list(
                interaction_pw  = interaction_pw,
                by_category     = by_cat_posthoc,
                by_country      = by_ctry_post_hoc
            ))
        }

        # If not 'emmeans' it means we have heteroscedasticity (thus, we do a robust non-parametric post-hocs)
        # Between-country simple effects within Category: Games-Howell
        gh_by_cat <- combined_data %>%
            group_by(Category) %>%
            group_modify(~ rstatix::games_howell_test(.x, Rating ~ Country, detailed = TRUE)) %>%
            ungroup() %>%
            mutate(contrast = paste(group2, '-', group1)) %>%
            dplyr::select(Category, contrast,
                   estimate, conf.low, conf.high, statistic, df, p.adj) %>%
            dplyr::rename(CI_low = conf.low, CI_high = conf.high, t.ratio = statistic, p.value = p.adj)

        # Hedges g for GH results
        g_stats <- effectsize::t_to_d(t = gh_by_cat$t.ratio, df = gh_by_cat$df, ci = 0.95, hedges.correction = TRUE)
        by_category <- bind_cols(
            gh_by_cat %>% tibble(Hedges_g = g_stats$d, g_CI_low = g_stats$CI_low, g_CI_high = g_stats$CI_high)
        ) %>%
        dplyr::select(Category, contrast, estimate, df, t.ratio, p.value, Hedges_g, g_CI_low, g_CI_high, CI_low, CI_high)

        # Within-country (repeated) simple effects of Category: robust RM (trimmed means; controls for FWER)
        rm_within_pairs <- combined_data %>%
            group_split(Country) %>%
            setNames(levels(combined_data$Country)) %>%
            lapply(function(x) {
                out <- suppressWarnings(WRS2::rmmcp(y = x$Rating, groups = x$Category, blocks = x$Subject)) # Applies default 20% trim
                comp <- as_tibble(out$comp) %>%
                    dplyr::rename(contrast = Group, estimate = psihat, CI_low = ci.lower, CI_high = ci.upper, p.value = p.value, p.crit = p.crit) %>%
                    mutate(Country = unique(x$Country),
                           contrast = as.character(contrast))
                lvl <- levels(x$Category)
                pairs <- as.data.frame(t(combn(lvl, 2)), stringsAsFactors = FALSE); colnames(pairs) <- c("a", "b"); labels <- paste0(pairs$a, " - ", pairs$b)
                comp$contrast <- labels
                dz <- purrr::pmap_dfr(pairs, ~ paired_dz(x, id = "Subject", within_factor = "Category", dv = "Rating", a = ..1, b = ..2))
                left_join(comp, dz, by = "contrast") %>% dplyr::rename(Cohens_d = Cohens_dz)
            })
        by_country <- bind_rows(rm_within_pairs) %>%
            dplyr::select(Country, contrast, estimate, p.value, p.crit, Cohens_d, CI_low, CI_high)

        # Interaction contrasts: Yuen trimmed-mean t on subject-level differences (Holm adjusted)
        cat_pairs <- as.data.frame(t(combn(levels(combined_data$Category), 2)), stringsAsFactors = FALSE)
        ctry_pairs <- as.data.frame(t(combn(levels(combined_data$Country), 2)), stringsAsFactors = FALSE)
        colnames(cat_pairs) <- c("Ci", "Cj")
        colnames(ctry_pairs) <- c("Gi", "Gj")

        wide <- combined_data %>%
            pivot_wider(id_cols = c(Subject, Country), names_from = Category, values_from = Rating)
        grid <- tidyr::crossing(cat_pairs, ctry_pairs)

        interaction_raw <- purrr::pmap(
            grid,
            function(Ci, Cj, Gi, Gj) {
                xi <- wide %>% filter(Country == Gi)
                xi_diff <- xi[[Cj]] - xi[[Ci]]
                xj <- wide %>% filter(Country == Gj)
                xj_diff <- xj[[Cj]] - xj[[Ci]]
                tmp <- tibble(
                    diff = c(xi_diff, xj_diff),
                    grp = factor(c(rep(Gi, length(xi_diff)), rep(Gj, length(xj_diff))), levels = c(Gi, Gj))
                )

                yt <- suppressWarnings(WRS2::yuen(diff ~ grp, data = tmp)) # Applies default 20% trim

                list(
                    contrast    = paste0("(", Cj, " - ", Ci, ") | ", Gi, " - ", Gj),
                    estimate    = unname(yt$diff),
                    df          = unname(yt$df),
                    t.ratio     = unname(yt$test),
                    p.raw       = unname(yt$p.value),
                    xi_diff     = xi_diff,
                    xj_diff     = xj_diff
                )
            })

        p_raw  <- vapply(interaction_raw, `[[`, numeric(1), "p.raw")
        p_holm <- p.adjust(p_raw, method = "holm")
        holm_alpha <- pmin(0.05 * p_raw / p_holm, 0.05) # Per-row alpha implied by Holm adjustment already applied to this row's p-value (p_holm = M_i * p_raw for an effective multiplier M_i >= 1

        interaction_tbl <- purrr::map_dfr(seq_along(interaction_raw), function(i) {
            row <- interaction_raw[[i]]

            es <- effectsize::cohens_d(row$xi_diff, row$xj_diff, hedges.correction = TRUE)
            g  <- unname(es$Cohens_d[1])

            # Reconstruct CI implied by Yuen's own t-statistic (diff, test, df) at row-specific Holm alpha, (then rescale it onto Hedges' g units)
            se_diff  <- row$estimate / row$t.ratio
            crit     <- qt(1 - holm_alpha[i] / 2, row$df)
            ci_lo    <- row$estimate - crit * se_diff
            ci_hi    <- row$estimate + crit * se_diff
            scale    <- g / row$estimate
            ci_g     <- sort(c(ci_lo, ci_hi) * scale)

            tibble(
                contrast = row$contrast,
                estimate = row$estimate,
                df       = row$df,
                t.ratio  = row$t.ratio,
                p.value  = p_holm[i],
                Cohens_d = g,
                CI_low   = ci_g[1],
                CI_high  = ci_g[2]
            )
        })

        interaction_pw <- interaction_tbl %>%
            dplyr::select(contrast, estimate, df, t.ratio, p.value, Cohens_d, CI_low, CI_high)

        list(
            interaction_pw  = interaction_pw,
            by_category     = by_category,
            by_country      = by_country
        )
    }

    countryMainEffect <- function(anova_table, subj_means_data, test_type) {
        if (test_type == "welch") {
            w <- oneway.test(Rating ~ Country,
                             data = subj_means_data,
                             var.equal = FALSE)
            tibble(
                test    = "Welch one-way (Country on subject means)",
                df1     = unname(w$parameter[1]),
                df2     = unname(w$parameter[2]),
                F       = unname(w$statistic),
                p       = unname(w$p.value)
            )
        } else {
            anova_table %>% filter(Effect == "Country")
        }
    }

    names(data_list) <- country_names
    country_vec <- names(data_list)

    combined_data <- lapply(seq_along(data_list), function(i) {
        df <- data_list[[i]]
        country <- country_vec[[i]]

        severity <- df %>% dplyr::select(contains("Q6_1ED_1")) %>% mutate(across(everything(), as.numeric))
        susceptibility <- df %>% dplyr::select(contains("Q6_1ED_2")) %>% mutate(across(everything(), as.numeric))
        exposure <- df %>% dplyr::select(contains("Q6_1ED_3")) %>% mutate(across(everything(), as.numeric))

        n <- nrow(severity) # This is arbitrary but since it's within subject ratings all vars will have same length
        subject_ids <- paste0(country, "_", seq_len(n))

        bind_rows(
            severity %>% mutate(Subject = subject_ids, Category = "Severity", Country = country) %>%
                pivot_longer(-c(Subject, Category, Country), values_to = "Rating") %>% dplyr::select(-name),
            susceptibility %>% mutate(Subject = subject_ids, Category = "Susceptibility", Country = country) %>%
                pivot_longer(-c(Subject, Category, Country), values_to = "Rating") %>% dplyr::select(-name),
            exposure %>% mutate(Subject = subject_ids, Category = "Overall risk", Country = country) %>%
                pivot_longer(-c(Subject, Category, Country), values_to = "Rating") %>% dplyr::select(-name)
        )
    }) %>% 
    bind_rows() %>% 
    mutate(
        Category = factor(Category, levels = c("Severity", "Susceptibility", "Overall risk")),
        Country = factor(Country, levels = c("DK", "UK", "US")),
        Subject = factor(Subject)
    )

    subj_means <- combined_data %>%
        group_by(Subject, Country) %>%
            dplyr::summarise(
                Rating = mean(Rating, na.rm = TRUE),
                Country = first(Country),
                .groups = "drop"
            ) %>%
        mutate(Country = factor(Country))

    levene <- car::leveneTest(Rating ~ Country, data = subj_means)
    
    ctry_test_type <- ifelse(levene$`Pr(>F)`[1] < .05, "welch", "anova")
    cat_test_type <- ifelse(levene$`Pr(>F)`[1] < .05, "games_howell", "emmeans")

    mixed_aov <- afex::aov_ez(
        id      = "Subject",
        dv      = "Rating",
        within  = "Category",
        between = "Country",
        data    = combined_data,
        type    = 3
    )

    anova_table <- afex::nice(mixed_aov, correction = "GG", es = "pes") %>% as_tibble() # GG corrected
    country_main_effect <- countryMainEffect(anova_table, subj_means, ctry_test_type) # If levene not sig. get ANOVA outcome, else get Welch test for between-subjects Country level

    # Robust omnibus for both within and interaction (we will report these seperately in manuscript *IF* the levene is sig.)
    wj <- welchADF.test(
        formula     = Rating ~ Country * Category,
        data        = combined_data,
        contrast    = "omnibus",
        trimming    = TRUE # Trimmed means + Winsorized variances
    )

    # Construct a tibble from the wj output
    wj_txt <- capture.output(summary(wj))
    rows <- grep("^(Country|Category)", wj_txt, value = TRUE)
    mat <- do.call(rbind, strsplit(trimws(rows), "\\s+"))
    df <- as.data.frame(mat, stringsAsFactors = FALSE)[, 1:5]
    names(df) <- c("Effect", "stat", "df1", "df2", "p")
    df$stat <- as.numeric(df$stat)
    df$df1 <- as.numeric(df$df1)
    df$df2 <- as.numeric(df$df2)
    df$p <- as.numeric(df$p)

    wj_omnibus <- as_tibble(df)

    # Post-hoc tests
    posthoc <- performPostHocRM(mixed_aov, combined_data, cat_test_type)
    omnibus_es <- effectsize::eta_squared(mixed_aov, partial = TRUE)

    interaction_p <- suppressWarnings(anova_table %>% filter(Effect == "Country:Category") %>% pull(p.value))
    post_hoc_message <- if (!is.na(interaction_p) && interaction_p < .05)
        "Significant interaction (report simple effects)." else "No interaction (report main effects)."

    results <- list(
        main_anova_table    = anova_table,
        wj_omnibus          = wj_omnibus,
        country_main_effect = country_main_effect,
        interaction_pw      = posthoc$interaction_pw,
        by_category         = posthoc$by_category,
        by_country          = posthoc$by_country,
        ctry_test_type      = ctry_test_type,
        cat_test_type       = cat_test_type,
        omnibus_eta         = omnibus_es,
        levene              = levene,
        post_hoc_message    = post_hoc_message
    )

    mixed_anova_output_dir <- file.path(output_dir, paste0("Mixed_Anova_Results"))
    if (!dir.exists(mixed_anova_output_dir)) {
        dir.create(mixed_anova_output_dir, recursive = TRUE)
    }

    formatHealthDescriptivesToLaTeX(combined_data)
    formatMixedANOVAToLaTeX(results, contrasts_as_tables, mixed_anova_output_dir)

    return(results)
}

##### Exploratory R-index analysis #####
rindexAnalysis <- function(df) {

    collapseRounds <- function(dat,
                                shifts = c(R1_ = 0, R2_ = 6, R3_ = 12)) {
        
        parts <- map(names(shifts), function(pref) {
            dat %>%
                select(starts_with(pref)) %>%
                mutate(across(everything(), as.numeric)) %>%
                { . + shifts[[pref]] } %>%
                rename_with(~ str_remove(.x, paste0("^", pref)))
        })

        collapsed <- parts[[1]]
        for (col in names(collapsed)) {
            collapsed[[col]] <- dplyr::coalesce(
                parts[[1]][[col]],
                parts[[2]][[col]],
                parts[[3]][[col]]
            )
        }

        return(collapsed)
    }

    rindexPair <- function(fi, fj) {
        cumj <- cumsum(fj); totj <- cumj[18]
        cumi <- cumsum(fi); toti <- cumi[18]
        A <- sum(fi * (totj - cumj))
        C <- sum(fj * (toti - cumi))
        B <- 0.5 * sum(fi * fj)
        (A + B) / (A + (2 * B) + C) * 100
    }

    collapsed <- collapseRounds(df)
    attrs <- names(collapsed)

    r1_cols <- df %>% select(starts_with("R1_")) %>% names()
    base_order <- str_remove(r1_cols, "^R1_")
    group_map <- set_names(rep(c("consumption", "application", "inhalation"), each = 6),
                           base_order)
    groups <- group_map[attrs]

    makeFreqRow <- function(x) {
        vec <- tabulate(factor(x, levels = 1:18), nbins = 18)
        tibble::as_tibble(setNames(as.list(vec), paste0("Rank", 1:18)))
    }

    freq <- map_dfr(attrs,
                ~ makeFreqRow(collapsed[[.x]]),
                .id = "Attribute") %>%
        column_to_rownames("Attribute") %>%
        as.matrix()

    n_attr <- length(attrs)
    Rmat <- matrix(100, n_attr, n_attr, dimnames = list(attrs, attrs))
    for (i in seq_len(n_attr)) {
        for (j in seq_len(n_attr)) {
            if (i != j) {Rmat[i, j] <- rindexPair(freq[i, ], freq[j, ])}
        }
    }

    sum_choice <- rowSums(Rmat)
    rank_idx <- rank(-sum_choice, ties.method = "min")
    summary_tbl <- tibble(
        Attribute = attrs,
        Group = groups,
        sum_choice_prob = round(sum_choice, 1),
        Rank_by_Rindex = rank_idx
    ) %>%
        mutate(Group = factor(Group,
                              levels = c("consumption", "application", "inhalation"),
                              ordered = TRUE)) %>%
        arrange(Group, Rank_by_Rindex)

    return(summary_tbl)
}

##### Exploratory consistency checks (perceived susc vs seve; rankings vs. overall risk) #####
consistencyHealth <- function(df, country, output_dir = "./Results") {
    require(polycor)
    require(DescTools)

    sev  <- df %>% dplyr::select(dplyr::contains("Q6_1ED_1")) %>% mutate(across(everything(), as.numeric)) %>% unlist()
    susc <- df %>% dplyr::select(dplyr::contains("Q6_1ED_2")) %>% mutate(across(everything(), as.numeric)) %>% unlist()
    dat  <- tibble::tibble(Severity = sev, Susceptibility = susc) %>% tidyr::drop_na()

    sev_ord <- ordered(dat$Severity, levels = 1:7)
    susc_ord <- ordered(dat$Susceptibility, levels = 1:7)

    # Polychoric correlation
    pc_obj <- suppressWarnings(polycor::polychor(sev_ord, susc_ord, std.err = TRUE))
    pc_est <- pc_obj$rho
    cov_matrix <- vcov(pc_obj)
    pc_se <- sqrt(diag(cov_matrix))

    z <- stats::qnorm(1 - (1 - 0.95)/2)
    pc_CI <- c(pc_est - z * pc_se, pc_est + z * pc_se)

    # Spearman correlation
    sp <- suppressWarnings(stats::cor.test(as.numeric(sev_ord), as.numeric(susc_ord),
                          method = "spearman", exact = FALSE))

    sp_CI <- unname(sp$conf.int)
    pc_lo <- pc_CI[1]; pc_hi <- pc_CI[2]
    sp_lo <- sp_CI[1]; sp_hi <- sp_CI[2]

    # Agreement (paired differences)
    diff <- as.numeric(susc_ord) - as.numeric(sev_ord)
    wil <- suppressWarnings(stats::wilcox.test(diff, mu = 0, exact = FALSE, correct = TRUE))
    N <- length(diff)
    z_w <- stats::qnorm(wil$p.value / 2, lower.tail = FALSE) * sign(stats::median(diff, na.rm = TRUE))
    r_eff <- abs(z_w) / sqrt(N)
    dz <- mean(diff, na.rm = TRUE) / stats::sd(diff, na.rm = TRUE)
    prop_within1 <- mean(abs(diff) <= 1, na.rm = TRUE)

    # If Spearman CI is missing, we bootstrap it
    if (is.null(sp$conf.int)) {
        set.seed(123)

        n <- length(sev_ord)
        B <- 2000L
        s_boot <- replicate(B, {
            idx <- sample.int(n, replace = TRUE)
            cor(as.numeric(sev_ord[idx]), as.numeric(susc_ord[idx]), method = "spearman")
        })
        sp_CI <- stats::quantile(s_boot, probs = c(0.025, 0.975), na.rm = TRUE)
        sp_lo <- as.numeric(sp_CI[1]); sp_hi <- as.numeric(sp_CI[2])
    }

    pc_p <- 2 * pnorm(abs(pc_est / pc_se), lower.tail = FALSE)

    result <- tibble(
        method          = c("Polychoric", "Spearman"),
        estimate        = c(pc_est, unname(sp$estimate)),
        CI_low          = c(pc_lo, sp_lo),
        CI_high         = c(pc_hi, sp_hi),
        p               = c(pc_p, sp$p.value),
        mean_diff       = mean(diff, na.rm = TRUE),
        median_diff     = median(diff, na.rm = TRUE),
        prop_within1    = prop_within1,
        wilcoxon_V      = unname(wil$statistic),
        wilcoxon_p      = wil$p.value,
        cohens_dz       = dz,
        r_effectsize    = r_eff
    )

    consistency_health_output_dir <- file.path(output_dir, paste0("Consistency_Health_Results_", country))
    if (!dir.exists(consistency_health_output_dir)) {
        dir.create(consistency_health_output_dir, recursive = TRUE)
    }

    formatConsistencyHealthToLaTeX(result, consistency_health_output_dir, country)
}

.buildpathwayScores <- function(df, # Function used to turn items into Borda scores
                               pathways = list(consumption = 1:6,
                                             application = 7:12,
                                             inhalation  = 13:18)) {
    
    r1 <- df %>% dplyr::select(dplyr::contains("R1_")) %>% dplyr::mutate(dplyr::across(everything(), as.numeric))
    r2 <- df %>% dplyr::select(dplyr::contains("R2_")) %>% dplyr::mutate(dplyr::across(everything(), as.numeric)) + 6L
    r3 <- df %>% dplyr::select(dplyr::contains("R3_")) %>% dplyr::mutate(dplyr::across(everything(), as.numeric)) + 12L

    bindColumns <- function(start_col, end_col) {
        purrr::map_dfc(list(r1, r2, r3), ~ .x %>% dplyr::select(start_col:end_col))
    }

    consumption <- bindColumns(1, 6)
    application <- bindColumns(7, 12)
    inhalation  <- bindColumns(13, 18)

    # Convert global ranks (1..18) to Borda scores where larger = higher perceived risk
    to_borda <- function(m) 19L - as.matrix(m)

    S_cons <- rowMeans(to_borda(consumption), na.rm = TRUE)
    S_app  <- rowMeans(to_borda(application),  na.rm = TRUE)
    S_inh  <- rowMeans(to_borda(inhalation),   na.rm = TRUE)

    tibble(S_cons = S_cons, S_app = S_app, S_inh = S_inh)
}

exposureCLM <- function(df, df2, country, output_dir = "./Results") {
    require(ordinal)
    require(broom)

    pathway <- .buildpathwayScores(df)

    exp_raw <- df2 %>% select(contains("Q6_1ED_3")) %>% mutate(across(everything(), as.numeric))
    Exposure <- rowMeans(exp_raw, na.rm = TRUE)
    Exposure <- ordered(as.integer(pmin(pmax(round(Exposure), 1L), 7L)), levels = 1:7)

    dat <- bind_cols(pathway, tibble(Exposure = Exposure)) %>% drop_na()

    cor_rows <- lapply(c("S_cons", "S_app", "S_inh"), function(v) {
        x <- as.numeric(dat[[v]])
        y <- as.numeric(dat$Exposure)
        ct <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
        tibble(
            pathway = v,
            rho = unname(ct$estimate),
            p = ct$p.value
        )
    }) %>% bind_rows() %>% mutate(p_adj = p.adjust(p, method = "holm"))

    # Two independent contrasts to avoid perfect collinearity among S_cons/S_app/S_inh
    X <- dat %>%
        transmute(
            Exposure,
            C_cons_inh = scale(S_cons - S_inh)[, 1],
            C_app_inh  = scale(S_app - S_inh)[, 1]
        )

    fit_null <- ordinal::clm(Exposure ~ 1, data = X, link = "logit")
    fit_po <- ordinal::clm(Exposure ~ C_cons_inh + C_app_inh, data = X, link = "logit")

    # LR vs null + pseudo-R2
    ll0 <- as.numeric(logLik(fit_null)); 
    df0 <- attr(logLik(fit_null), "df")

    ll1 <- as.numeric(logLik(fit_po));
    df1 <- attr(logLik(fit_po),   "df")

    LR <- 2 * (ll1 - ll0);
    df_LR <- df1 - df0
    
    p_LR <- pchisq(LR, df = df_LR, lower.tail = FALSE)
    R2_McF <- 1 - ll1/ll0

    # Manual proportional-odds checks via LR against non-parallel models
    po_lrt <- function(pred) {
        m <- ordinal::clm(Exposure ~ C_cons_inh + C_app_inh,
                    nominal = as.formula(paste("~", pred)),
                    data = X, link = "logit")
        llm <- as.numeric(logLik(m)); dfm <- attr(logLik(m), "df")
        LRT <- 2 * (llm - ll1); ddf <- dfm - df1
        tibble(predictor = pred,
            LRT = LRT,
            df  = ddf,
            p   = pchisq(LRT, df = ddf, lower.tail = FALSE)
        )
    }
    viol_tbl <- bind_rows(po_lrt("C_cons_inh"), po_lrt("C_app_inh")) %>%
        mutate(violates = is.finite(p) & (p < 0.05))

    # Partial-PO refit; only if a predictor violates PO
    fit <- fit_po
    viol_pred <- viol_tbl %>% filter(violates) %>% pull(predictor)

    po_flag_partial <- FALSE
    po_flag_nominal <- character(0)
    po_flag_location <- c("C_cons_inh", "C_app_inh")

    # Containers
    nonparallel_rows <- tibble::tibble()
    pooled_rows <- tibble::tibble()

    if (length(viol_pred) > 0) {
        keep_pred <- setdiff(c("C_cons_inh","C_app_inh"), viol_pred)
        loc_rhs   <- if (length(keep_pred)) paste(keep_pred, collapse = " + ") else "1"
        loc_form  <- as.formula(paste("Exposure ~", loc_rhs))
        nom_form  <- as.formula(paste("~", paste(viol_pred, collapse = " + ")))

        fit <- ordinal::clm(loc_form, nominal = nom_form, data = X, link = "logit")

        zcrit <- qnorm(0.975)
        cn <- names(coef(fit))
        cov_matrix <- vcov(fit)

        # Thresholds end with (Intercept); nominal have '|' and are not thresholds
        is_threshold <- grepl("\\(Intercept\\)$", cn)
        is_nominal   <- grepl("\\|", cn) & !is_threshold
        nominal_names <- cn[is_nominal]

        # index to link each nominal coef to its predictor and cutpoint
        nominal_index <- tibble::tibble(
            nm        = nominal_names,
            cutpoint  = sub("^([^.:]+)[.:].*$", "\\1", nominal_names), # Extract before first '.' or ':'
            predictor = sub("^[^.:]+[.:]", "", nominal_names) # Remove leading + first '.' or ':'; keep rest
        )

        get_nominal <- function(pred) {
            idx <- which(nominal_index$predictor == pred)
            nm <- nominal_index$nm[idx]
            b  <- coef(fit)[nm]
            se <- sqrt(diag(cov_matrix))[nm]

            tibble::tibble(
                predictor  = pred,
                cutpoint   = nominal_index$cutpoint[idx],
                beta       = b,
                OR         = exp(b),
                OR_CI_low  = exp(b - zcrit * se),
                OR_CI_high = exp(b + zcrit * se)
            )
        }

        nonparallel_rows <- dplyr::bind_rows(lapply(viol_pred, get_nominal))

        # Calculate pooled estimates for violating predictors & z/p via robust GLS
        if (nrow(nonparallel_rows) > 0) {
            gls_pool <- function(pred) {
                nm <- nominal_index$nm[nominal_index$predictor == pred]
                b <- as.numeric(coef(fit)[nm])
                cov_matrix <- as.matrix(cov_matrix[nm, nm, drop = FALSE])
                k <- length(b)

                Vinv <- solve(cov_matrix) # Invert covariance matrix

                one <- rep(1, k)
                denom <- as.numeric(t(one) %*% Vinv %*% one)
                num   <- as.numeric(t(one) %*% Vinv %*% b)

                beta_hat <- num / denom
                se_hat   <- sqrt(1 / denom)

                # Wald z from pooled estimate
                z_wald <- beta_hat / se_hat
                p_wald <- 2 * pnorm(abs(z_wald), lower.tail = FALSE)

                tibble::tibble(
                    predictor  = pred,
                    beta       = beta_hat,
                    OR         = exp(beta_hat),
                    OR_CI_low  = exp(beta_hat - zcrit * se_hat),
                    OR_CI_high = exp(beta_hat + zcrit * se_hat),
                    z          = z_wald,
                    p          = p_wald
                )
            }

        pooled_rows <- dplyr::bind_rows(lapply(viol_pred, gls_pool))
    }

    po_flag_partial  <- TRUE
    po_flag_nominal  <- viol_pred
    po_flag_location <- keep_pred
}

    coefs_nms <- names(stats::coef(fit))
    coef_keep <- intersect(coefs_nms, c("C_cons_inh","C_app_inh"))

    zcrit <- qnorm(0.975)

    # Empty table helper
    empty_est_tbl <- function() tibble::tibble(
        predictor   = character(),
        beta        = double(),
        OR          = double(),
        OR_CI_low   = double(),
        OR_CI_high  = double(),
        z           = double(),
        p           = double()
    )

    loc_tbl <- broom::tidy(fit, conf.int = FALSE) %>%
        dplyr::filter(.data$term %in% coef_keep) %>%
        dplyr::transmute(
            predictor   = .data$term,
            beta        = .data$estimate,
            OR          = exp(.data$estimate),
            OR_CI_low   = exp(.data$estimate - zcrit*.data$std.error),
            OR_CI_high  = exp(.data$estimate + zcrit*.data$std.error),
            z           = .data$statistic,
            p           = .data$p.value
        )

    viol_tbl_rows <- if (nrow(pooled_rows)) {pooled_rows %>%
        dplyr::transmute(
            predictor,
            beta        = as.numeric(beta),
            OR          = as.numeric(OR),
            OR_CI_low   = as.numeric(OR_CI_low),
            OR_CI_high  = as.numeric(OR_CI_high),
            z           = as.numeric(z),
            p           = as.numeric(p)
        )
    } else {
        empty_est_tbl()
    }

    est_merged <- dplyr::rows_upsert(loc_tbl, viol_tbl_rows, by = "predictor")
    predictor_set <- tibble::tibble(predictor = c("C_cons_inh", "C_app_inh"))
    est_complete <- dplyr::right_join(est_merged, predictor_set, by = "predictor")

    est_complete <- est_complete %>%
        dplyr::mutate(
            beta        = as.numeric(.data$beta),
            OR          = as.numeric(.data$OR),
            OR_CI_low   = as.numeric(.data$OR_CI_low),
            OR_CI_high  = as.numeric(.data$OR_CI_high),
            z           = as.numeric(.data$z),
            p           = as.numeric(.data$p)
        )

    label_map <- c(
        C_cons_inh = "Consumption vs Inhalation",
        C_app_inh  = "Application vs Inhalation"
    )

    est <- est_complete %>%
        dplyr::transmute(
            predictor = dplyr::recode(.data$predictor, !!!label_map),
            beta, OR, OR_CI_low, OR_CI_high, z, p
        )
    
    result <- list(
            correlations    = cor_rows,
            lr_test         = tibble(LR = LR, df = df_LR, p = p_LR, R2_McF = R2_McF),
            po_test         = viol_tbl,
            model_table     = est,
            model_object    = fit,
            pathway_scores  = dplyr::select(dat, S_cons, S_app, S_inh, Exposure),
            contrasts       = dplyr::select(X, C_cons_inh, C_app_inh),
            flags           = list(
                partial_po          = po_flag_partial,
                partial_predictors  = po_flag_nominal,
                location_predictors = po_flag_location
            )
        )

    exposure_clm_output_dir <- file.path(output_dir, paste0("Exposure_CLM_Results_", country))
    if (!dir.exists(exposure_clm_output_dir)) {
        dir.create(exposure_clm_output_dir, recursive = TRUE)
    }

    formatExposureCLMToLaTeX(result, exposure_clm_output_dir, country)

    return(result)
}

##### Load data #####
if (!dir.exists("./Results")) {
    dir.create("./Results", recursive = TRUE)
}

root_dir <- "Processed Data/"

UK_likert_data <- read.csv(paste0(root_dir, "UK_likert_data.csv"))
UK_rankings_data <- read.csv(paste0(root_dir, "UK_rankings_data.csv"))
UK_demographics_data <- read.csv(paste0(root_dir, "UK_demographics.csv"))

DK_likert_data <- read.csv(paste0(root_dir, "DK_likert_data.csv"))
DK_likert_data <- rename_cols(DK_likert_data)
DK_rankings_data <- read.csv(paste0(root_dir, "DK_rankings_data.csv"))
DK_demographics_data <- read.csv(paste0(root_dir, "DK_demographics.csv")) 
colnames(DK_demographics_data) <- colnames(DK_demographics_data) %>% str_replace_all("Q9_4Ch", "Q9_5Ch") # To be consistent with other two datasets (minor Qualtrics labling error)
DK_demographics_data$region[DK_demographics_data$region == "Bornholms Regionskommune"] <- "Region Hovedstaden"
DK_demographics_data <- translateDKVars(DK_demographics_data) # Translate relevant variables to English

US_likert_data <- read.csv(paste0(root_dir, "US_likert_data.csv"))
US_rankings_data <- read.csv(paste0(root_dir, "US_rankings_data.csv"))
US_demographics_data <- read.csv(paste0(root_dir, "US_demographics.csv"))

##### Call planned stats functions #####
UK_demographics <- demographicsDescriptives(UK_demographics_data, "UK")
DK_demographics <- demographicsDescriptives(DK_demographics_data, "DK")
US_demographics <- demographicsDescriptives(US_demographics_data, "US")

UK_familiarityKnowledge <- knowledgeFamiliarityDescriptives(UK_demographics_data, "UK")
DK_familiarityKnowledge <- knowledgeFamiliarityDescriptives(DK_demographics_data, "DK")
US_familiarityKnowledge <- knowledgeFamiliarityDescriptives(US_demographics_data, "US")

UK_PCA <- pcaAnalysis(UK_likert_data, "UK")
DK_PCA <- pcaAnalysis(DK_likert_data, "DK")
US_PCA <- pcaAnalysis(US_likert_data, "US")

createBiplotPanel(
    pca_results_list = list(UK_PCA, DK_PCA, US_PCA),
    countries = c("UK", "DK", "US"),
    output_dir = "./Results",
    file_extension = ".svg"
)

UK_ANOVA <- oneWayANOVA(UK_rankings_data, "UK")
DK_ANOVA <- oneWayANOVA(DK_rankings_data, "DK")
US_ANOVA <- oneWayANOVA(US_rankings_data, "US")

formatCombinedANOVADescriptivesToLaTeX(
    anova_results_by_country = list(UK = UK_ANOVA, DK = DK_ANOVA, US = US_ANOVA),
    output_dir = "./Results"
)

UK_regression <- multipleRegression(UK_likert_data, UK_demographics_data, "UK")
DK_regression <- multipleRegression(DK_likert_data, DK_demographics_data, "Danish")
US_regression <- multipleRegression(US_likert_data, US_demographics_data, "US") # Severity model ends up getting log transformed

# Inspect regression models
# UK_regression$severity_results$log_transform_applied
# UK_regression$exposure_results$log_transform_applied
# UK_regression$susceptibility_results$log_transform_applied

# DK_regression$severity_results$log_transform_applied
# DK_regression$exposure_results$log_transform_applied
# DK_regression$susceptibility_results$log_transform_applied

# US_regression$severity_results$log_transform_applied # Only one that returns TRUE
# US_regression$exposure_results$log_transform_applied
# US_regression$susceptibility_results$log_transform_applied

createCombinedTable(table_list = list(UK_PCA, US_PCA, DK_PCA),
                    country = c("UK", "US", "DK"),
                    output_dir = './Results',
                    main_caption = "Varimax-rotated PCA loadings for perceived EDC risk attributes across countries")

DK_likert_desc <- likertScaleDescriptives(DK_likert_data, "DK")
UK_likert_desc <- likertScaleDescriptives(UK_likert_data, "UK")
US_likert_desc <- likertScaleDescriptives(US_likert_data, "US")

formatCombinedLikertDescriptivesToLaTeX(
    descriptives_by_country = list(US = US_likert_desc,
                                   UK = UK_likert_desc,
                                   DK = DK_likert_desc),
    output_dir = file.path('./Results', 'Likert_Descriptives')
)

##### Call exploratory mixed ANOVA function #####
mixedRMANOVA <- exploratoryMixedRMANOVA(data_list = list(DK_likert_data,
                                                         UK_likert_data,
                                                         US_likert_data),
                                        country_names = c("DK", "UK", "US"))

##### Call exploratory R-index analysis function #####
DK_summary <- rindexAnalysis(DK_rankings_data)
UK_summary <- rindexAnalysis(UK_rankings_data)
US_summary <- rindexAnalysis(US_rankings_data)

createRIndexTable( # Convert R-index summaries to LaTeX table
    table_list = list(DK = DK_summary,
                      UK = UK_summary,
                      US = US_summary),
    output_dir = "./Results",
    main_caption = "R-index Summary of Exposure Item Rankings Across Countries"
)

##### Call exploratory consistency analyses functions #####
UK_ratings_consistency <- consistencyHealth(UK_likert_data, "UK")
DK_ratings_consistency <- consistencyHealth(DK_likert_data, "DK")
US_ratings_consistency <- consistencyHealth(US_likert_data, "US")

UK_ranking_exposure_consistency <- exposureCLM(UK_rankings_data, UK_likert_data, "UK")
DK_ranking_exposure_consistency <- exposureCLM(DK_rankings_data, DK_likert_data, "DK") # This violates partial PO assumption
US_ranking_exposure_consistency <- exposureCLM(US_rankings_data, US_likert_data, "US")

##### One-way ANOVA Robustness Check #####
oneWayFriedman <- function(df, country, output_dir = "./Results", id_col = NULL) {
    bindColumns <- function(start_col, end_col) {
        purrr::map_dfc(list(r1, r2, r3), ~ .x %>% select(start_col:end_col)) # Bind specific columns from each rating set using purrr::map_dfc and lambda function (with starting and ending column indices)
    }
    
    r1 <- df %>% select(contains("R1_")) %>% mutate(across(everything(), as.numeric))
    r2 <- df %>% select(contains("R2_")) %>% mutate(across(everything(), as.numeric)) + 6 # Adding 6 because of temporal distances (i.e., these items were rated second)
    r3 <- df %>% select(contains("R3_")) %>% mutate(across(everything(), as.numeric)) + 12 # Adding 12 because of temporal distances (i.e., these items were rated third)
    
    consumption <- bindColumns(1, 6)
    application <- bindColumns(7, 12)
    inhalation <- bindColumns(13, 18)

    pid <- if (!is.null(id_col) && id_col %in% names(df)) df[[id_col]] else seq_len(nrow(df)) # Extract PID from df or if NULL use the column rows

    rowAgg <- function(x) { # Collapse to one value per participant, per condition
        v <- apply(as.matrix(x), 1, function(r) mean(r, na.rm = TRUE))
        as.numeric(v)
    }

    wide <- tibble::tibble(
        pid         = pid,
        consumption = rowAgg(consumption),
        application = rowAgg(application),
        inhalation  = rowAgg(inhalation)
    )

    long <- wide %>%
        tidyr::pivot_longer(
            cols = c(consumption, application, inhalation),
            names_to = "ActivityType",
            values_to = "value"
        ) %>%
        dplyr::mutate(
            ActivityType = factor(ActivityType, levels = c("consumption", "application", "inhalation"))
        )
        
    friedman_omnibus <- stats::friedman.test(value ~ ActivityType | pid, data = long)
    kendall_w <- effectsize::kendalls_w(as.matrix(wide[, c("consumption", "application", "inhalation")]))
    names(as.data.frame(kendall_w))

    pairwise_result <- NULL
    effect_sizes <- NULL

    if (friedman_omnibus$p.value < 0.05) {
        conds <- c("consumption", "application", "inhalation")
        pairs <- combn(conds, 2, simplify = FALSE)

        pairwise_result <- purrr::map_dfr(pairs, function(p) {
            a <- p[1]; b <- p[2]
            wt <- suppressWarnings(stats::wilcox.test(
                wide[[a]], wide[[b]],
                paired = TRUE,
                exact = FALSE
            ))
            tibble::tibble(
                group1 = a,
                group2 = b,
                statistic = unname(wt$statistic),
                p = wt$p.value
            )
        }) %>%
            dplyr::mutate(p_adj = stats::p.adjust(p, method = "bonferroni"))

        # Rank-biserial correlation for paired comparisons
        effect_sizes <- purrr::map_dfr(pairs, function(p) {
            a <- p[1]; b <- p[2]
            es <- effectsize::rank_biserial(
                wide[[a]], wide[[b]],
                paired = TRUE,
                ci = 0.95
            )
            tibble::as_tibble(as.data.frame(es)) %>%
                dplyr::mutate(group1 = a, group2 = b, .before = 1)
        })
    }

    friedman_results <- list(
        main_test_result    = friedman_omnibus,
        pairwise_result     = pairwise_result,
        effect_sizes        = effect_sizes,
        omnibus_es          = kendall_w,
        omnibus_label       = "KendallW"
    )

    friedman_output_dir <- file.path(output_dir, paste0("Friedman_Results_", country))
    if (!dir.exists(friedman_output_dir)) {
        dir.create(friedman_output_dir, recursive = TRUE)
    }

    formatFriedmanResultsToLaTeX(friedman_results, friedman_output_dir)

    return(friedman_results)
}

fried_DK <- oneWayFriedman(DK_rankings_data, country = "DK", id_col = "ResponseId")
fried_UK <- oneWayFriedman(UK_rankings_data, country = "UK", id_col = "ResponseId")
fried_US <- oneWayFriedman(US_rankings_data, country = "US", id_col = "ResponseId")

##### Do chemical names impact rank scores and pathway? (Addresses Associate Editor concern) #####
chemicalLabelEffectClean <- function(data_list, country_names, output_dir = "./Results") {
    require(emmeans)
    require(sandwich)
    require(lmtest)

    toScenarioLong <- function(df, country) { # Reshape rankings to long Borda-score format
        shifts <- c(R1_ = 0L, R2_ = 6L, R3_ = 12L)

        r1_cols <- names(df)[startsWith(names(df), "R1_")]
        scenario_names <- sub("^R1_", "", r1_cols)
        pathway_map <- data.frame(
            Scenario = scenario_names,
            Pathway = rep(c("consumption", "application", "inhalation"), each = 6L),
            stringsAsFactors = FALSE
        )

        parts <- lapply(names(shifts), function(pref) {
            sub_df <- df[, startsWith(names(df), pref), drop = FALSE]
            sub_df[] <- lapply(sub_df, as.numeric)
            sub_df <- sub_df + shifts[[pref]]
            names(sub_df) <- sub(paste0("^", pref), "", names(sub_df))
            sub_df
        })

        # Coalesce across rounds to get one column per scenario, with NA if not ranked in any round (should not be possible given task, but just in case)
        collapsed <- parts[[1]]
        for (col in names(collapsed)) {
            collapsed[[col]] <- dplyr::coalesce(
                parts[[1]][[col]], parts[[2]][[col]], parts[[3]][[col]]
            )
        }

        collapsed$Subject <- paste0(country, "_", df$ResponseId)

        tidyr::pivot_longer(
            collapsed, cols = -Subject, names_to = "Scenario", values_to = "Rank"
        ) %>%
        dplyr::filter(!is.na(Rank)) %>%
        dplyr::left_join(pathway_map, by = "Scenario") %>%
        dplyr::mutate(
            Borda = 19L - as.integer(Rank), # Higher = perceived riskier (because Borda scores)
            Chemical = dplyr::case_when( # Language agnostic chemical categorization based on scenario names
                grepl("BPA|Bisphenol", Scenario, ignore.case = TRUE) ~ "BPA",
                grepl("phthalate|ftalat", Scenario, ignore.case = TRUE) ~ "Phthalates",
                grepl("paraben", Scenario, ignore.case = TRUE) ~ "Parabens",
                grepl("PFAS|polyfluor", Scenario, ignore.case = TRUE) ~ "PFAS",
                grepl("BFR|brominated|bromerede|flammeh", Scenario, ignore.case = TRUE) ~ "BFRs",
                grepl("pesticid", Scenario, ignore.case = TRUE) ~ "Pesticides",
                TRUE ~ NA_character_
            ),
            Country  = country
        )
    }

    prep_long <- function(dat, country_label) {
        toScenarioLong(dat, country_label) %>%
            dplyr::mutate(
                Pathway = factor(Pathway, levels = c("consumption", "application", "inhalation")),
                Chemical = factor(Chemical, levels = c("Pesticides", "BPA", "Phthalates",
                                                    "Parabens", "PFAS", "BFRs")),
                Subject = factor(Subject)
            )
    }

    .fitChemModel <- function(dat) {
        model <- lm(Borda ~ Chemical * Pathway, data = dat)
        vcov_cl <- sandwich::vcovCL(model, cluster = ~ Subject, data = dat) # Cluster-robust standard errors at the subject level
        coeftest <- lmtest::coeftest(model, vcov = vcov_cl) # Cluster-robust coefficient tests

        # EMMs per chemical within each pathway; trt.vs.ctrl gives each chemical vs. Pesticides
        emm_by_pathway <- emmeans::emmeans(model, ~ Chemical | Pathway, vcov. = vcov_cl)
        contrasts_by_pathway <- emmeans::contrast(
            emm_by_pathway,
            method  = "trt.vs.ctrl",
            ref     = "Pesticides",
            adjust  = "holm"
        )

        list(
            model               = model,
            coeftest            = coeftest,
            emm_by_pathway        = emm_by_pathway,
            contrasts_by_pathway  = contrasts_by_pathway
        )
    }

    names(data_list) <- country_names

    combined <- purrr::imap_dfr(data_list, prep_long) %>%
        dplyr::mutate(Country = factor(Country, levels = country_names))
    by_country <- purrr::imap(data_list, prep_long)

    pooled_fit <- .fitChemModel(combined)
    by_country_fit <- purrr::map(by_country, .fitChemModel)

    results <- list(
        model                   = pooled_fit$model,
        coeftest                = pooled_fit$coeftest,
        emm_by_pathway          = pooled_fit$emm_by_pathway,
        contrasts_by_pathway    = pooled_fit$contrasts_by_pathway,
        combined_data           = combined,
        by_country              = by_country_fit
    )

    chemical_label_output_dir <- file.path(output_dir, paste0("Chemical_Label_Effect_Results"))
    if (!dir.exists(chemical_label_output_dir)) {
        dir.create(chemical_label_output_dir, recursive = TRUE)
    }

    formatChemicalLabelToLaTeX(results, chemical_label_output_dir)
    
    return(results)
}

chemical_label_clean <- chemicalLabelEffectClean(
    data_list = list(DK_rankings_data, UK_rankings_data, US_rankings_data),
    country_names = c("DK", "UK", "US")
)
