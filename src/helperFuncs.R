rename_cols <- function(df) {
    require(stringr)
    
    colnames(df) <- colnames(df) %>%
        str_replace_all("Q3_1VL", "Q3_1Vl") %>%
        str_replace_all("Q3_3KnP", "Q3_3KP") %>%
        str_replace_all("Q3_4KnS", "Q3_4KS")

    return(df)
}

translateDKVars <- function(df) {
    df %>%
        mutate(
            Q9_1Ge = case_when(
                Q9_1Ge == "Kvinde" ~ "Female",
                Q9_1Ge == "Mand" ~ "Male",
                Q9_1Ge == "Ikke binær" ~ "Non-binary",
                Q9_1Ge == "Nonbinær" ~ "Non-binary",
                TRUE ~ Q9_1Ge
            ),
            Q9_3Em = case_when(
                Q9_3Em == "Andet" ~ "Other",
                Q9_3Em == "Studerende" ~ "Student",
                Q9_3Em == "Arbejdsløs og på udkig efter arbejde" ~ "Unemployed and looking for work",
                Q9_3Em == "Hjemmegående" ~ "A homemaker or stay-at-home parent",
                Q9_3Em == "Deltidsarbejde" ~ "Working part-time",
                Q9_3Em == "Fuldtidsarbejde" ~ "Working full-time",
                Q9_3Em == "Pensioneret" ~ "Retired",
                Q9_3Em == "Andet " ~ "Other",
                TRUE ~ Q9_3Em
            ),
            Q9_6Ed = case_when(
                Q9_6Ed == "Folkeskole " ~ "Primary school",
                Q9_6Ed == "Gymnasial uddannelse" ~ "Secondary school",
                Q9_6Ed == "Erhvervsuddannelse eller lignende" ~ "Vocational or similar",
                Q9_6Ed == "Bachelorgrad" ~ "Bachelor’s degree",
                Q9_6Ed == "Kandidatgrad" ~ "Master's degree",
                Q9_6Ed == "PhD" ~ "Ph.D.",
                Q9_6Ed == "Foretrækker ikke at svare" ~ "Prefer not to say",
                TRUE ~ Q9_6Ed
            ),
            Q9_7In = gsub("\\.", ",", Q9_7In) %>%
                gsub("om året", "per year", .) %>%
                gsub("Mere end", "More than", .) %>%
                gsub("Mindre end", "Less than", .) %>%
                gsub("Foretrækker ikke at sige  ", "Prefer not to say", .), 
            Q9_8He = case_when(
                Q9_8He == "Meget dårligt" ~ "Very bad",
                Q9_8He == "Dårligt" ~ "Bad",
                Q9_8He == "Moderat " ~ "Moderate",
                Q9_8He == "Godt" ~ "Good",
                Q9_8He == "Meget godt" ~ "Very good",
                Q9_8He == "Foretrækker ikke at sige" ~ "Prefer not to say",
                TRUE ~ Q9_8He
            ),
            region = case_when(
                region == "Region Sjælland" ~ "Region of Zealand",
                region == "Region Syddanmark" ~ "Region of Southern Jutland",
                region == "Region Hovedstaden" ~ "Region of the Capital",
                region == "Region Midtjylland" ~ "Region of Central Jutland",
                region == "Region Nordjylland" ~ "Region of Northern Jutland",
                TRUE ~ region
            )
        )
}

relevelVars <- function(df, column_pattern, na_values, ref_level) {
    df %>%
        select(contains(column_pattern)) %>%
        pull() %>%
        replace(. %in% na_values, NA) %>%
        factor() %>%
        relevel(ref = ref_level)
}


translateDKAttributes <- function(x, keep_unknown = TRUE) {

    # Look-up table
    lut <- c(
        "At.spise.fødevarer..der.indeholder.per..og.polyfluoralkylstoffer..PFAS." = "Eating.foods.that.contain.per..and.polyfluoroalkyl.substances..PFAS.",
        "At.spise.fisk.fanget.i.vand.forurenet.med.bromerede.flammehæmmere..BFR.stoffer." = "Consuming.fish.caught.in.waters.contaminated.with.Brominated.Flame.Retardants..BFRs.",
        "At.spise.frugt.og.grøntsager..der.er.behandlet.med.pesticider" = "Eating.fruits.and.vegetables.treated.with.pesticides",
        "At.spise.mad.med.ftalater..der.overføres.fra.plastemballage" = "Eating.food.with.phthalates.transferred.from.plastic.packaging",
        "At.drikke.vand.fra.plastflasker..der.indeholder.Bisphenol.A..BPA." = "Drinking.water.from.plastic.bottles.that.contain.Bisphenol.A..BPA.",
        "Indtagelse.af.farmaceutiske.produkter..der.indeholder.parabener.som.konserveringsmidler" = "Taking.pharmaceutical.products.that.contain.parabens.used.as.preservatives",
        "Brug.af.insektmidler..der.indeholder.pesticider.direkte.på.huden" = "Using.insect.repellents.with.pesticides.on.the.skin",
        "Brug.af.ansigtscremer..der.indeholder.per..og.polyfluoralkylstoffer..PFAS..direkte.på.huden" = "Applying.face.creams.containing.per..and.polyfluoroalkyl.substances..PFAS..directly.to.the.skin",
        "Brug.af.bodylotioner..der.indeholder.Bisphenol.A..BPA..direkte.på.huden" = "Applying.body.lotions.containing.Bisphenol.A..BPA..directly.to.the.skin",
        "At.sove.på.en.madras..der.indeholder.bromerede.flammehæmmere..BFR.stoffer." = "Sleeping.on.a.mattress.that.contains.Brominated.Flame.Retardants..BFRs.",
        "Brug.af.solcremer..der.indeholder.parabener.som.konserveringsmiddel" = "Using.sunscreens.that.contain.parabens.as.preservatives",
        "Brug.af.deodoranter.med.parfume..der.indeholder.ftalater" = "Using.deodorants.with.phthalates.in.their.fragrance",
        "Indånding.af.luft..der.indeholder.pesticider..som.kan.have.bevæget.sig.fra.et.affaldsanlæg" = "Breathing.air.containing.pesticides.that.have.migrated.from.landfills",
        "Indånding.af.bromerede.flammehæmmere..BFR.stoffer...der.frigives.fra.elektroniske.apparater" = "Breathing.in.brominated.flame.retardants..BFRs..released.from.electronic.devices",
        "Indånding.af.per..og.polyfluoralkylstof..PFAS..partikler.ved.ophold.på.indendørsarealer.med.gulvtæppe" = "Breathing.in.per..and.polyfluoroalkyl.substances..PFAS..particles.while.spending.time.in.a.carpeted.indoor.space",
        "Indånding.af.Bisphenol.A..BPA..partikler.fra.indendørs.støv" = "Breathing.in.Bisphenol.A..BPA..particles.from.indoor.dust",
        "Indånding.af.luft..der.indeholder.ftalater.fra.indendørs.luftfriskere" = "Breathing.air.containing.phthalates.from.indoor.air.fresheners",
        "Indånding.af.parabenpartikler.fra.indendørs.støv" = "Breathing.in.paraben.particles.from.indoor.dust"
    )

    out <- lut[x]

    if (keep_unknown) {
        unknown <- is.na(out) & !is.na(x)
        out[unknown] <- gsub("\\.", "", x[unknown]) # Strip dots; keep text
    }

    return(unname(out))
}