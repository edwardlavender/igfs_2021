################################
################################
#### analyse_fish_biometrics

#### This code:
# 1) Analyses the length-frequency data from IGFS leg 1

#### Steps preceding this code
# 1) Processing of raw data via process_data_raw.R


################################
################################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(magrittr)
library(prettyGraphics)

#### Load data
biometrics <- readRDS("./data/fish/biometrics.rds")


################################
################################
#### Sampling levels

#### Table of sampling levels
# Examine the species with more than n obs
biometric_league_tbl <-
  biometrics %>%
  dplyr::group_by(.data$SppCode) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::filter(.data$n > 50L) %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::select(`Common name` = .data$CommName,
                `Scientific name` = .data$SciName,
                N = .data$n)
nrow(biometric_league_tbl)
write.table(biometric_league_tbl, "./fig/biometric_league_tbl.txt",
            na = "-",
            quote = FALSE, sep = ",", row.names = FALSE)


################################
################################
#### Length frequency charts

#### Length frequency charts across all species with > 50 obs
# Examine the species with more than n obs
biometric_ranks <-
  biometrics %>%
  dplyr::group_by(.data$SppCode) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(.data$n >= 50L) %>%
  dplyr::arrange(dplyr::desc(.data$n))
# Define species for plotting
biometrics_for_ranked <-
  biometrics[which(biometrics$SppCode %in% biometric_ranks$SppCode), ] %>%
  data.frame()
# Check species without taxonomy
unique(biometrics_for_ranked$SciName[is.na(biometrics_for_ranked$family)])
# Update required species
# "Trisopterus esmarki": "Actinopteri", "Gadiformes", "Gadidae"
# "Loligo forbesi": "Cephalopoda", "Myopsida", "Loliginidae"
# "Todaropsis eblanae": "Cephalopoda", "Oegopsida", "Ommastrephidae"
biometrics_for_ranked[
  which(biometrics_for_ranked$SciName == "Trisopterus esmarki"),
  c("class", "order", "family")
  ] <- c("Actinopteri", "Gadiformes", "Gadidae")
biometrics_for_ranked[
  which(biometrics_for_ranked$SciName == "Loligo forbesi"),
  c("class", "order", "family")
] <- c("Cephalopoda", "Myopsida", "Loliginidae")
biometrics_for_ranked[
  which(biometrics_for_ranked$SciName == "Todaropsis eblanae"),
  c("class", "order", "family")
] <- c("Cephalopoda", "Oegopsida", "Ommastrephidae")
# Arrange data by taxonomy
biometrics_for_ranked %>% dplyr::arrange(.data$class, .data$order, .data$family)
# Make plots
png("./fig/length-freq.png", height = 12, width = 12, units = "in", res = 600)
n_spp <- length(unique(biometrics_for_ranked))
pp <- par(mfrow = rev(par_mf(n_spp)), oma = c(3, 4, 3, 3), mar = c(2, 2, 2, 2))
purrr::walk(split(biometrics_for_ranked, biometrics_for_ranked$SppCode), function(bio){
  main <- bquote(paste(
    italic(.(bio$SciName[1])),
    " [n = ", .(nrow(bio)), "]"
    ))
  include_common_names <- FALSE
  if(include_common_names){
    if(!is.na(main)) {
      main <- bquote(paste(
        atop(.(main), paste("\n(", italic(.(bio$SciName[1])), ") [n = ", .(nrow(bio)), "]"))
      ))
    } else {
      main <- bquote(atop(
        italic(.(bio$SciName[1])),
        paste("[n = ", .(nrow(bio)), "]")
      ))
    }
  }
  pretty_plot(density(bio$LngtCm, from = 0),
              pretty_axis_args = list(control_digits = 3),
              xlim = c(0, NA),
              xlab = "", ylab = "",
              type = "l")
  rug(bio$LngtCm, pos = 0, lwd = 2)
  mtext(side = 3, text = main, line = 0, cex = 0.8)
})
mtext(side = 1, "Length (cm)", cex = 1.25, line = 1, outer = TRUE)
mtext(side = 2, "Density", cex = 1.25, line = 2, outer = TRUE)
par(pp)
dev.off()


#### End of code.
################################
################################
