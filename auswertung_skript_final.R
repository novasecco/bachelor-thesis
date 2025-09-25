setwd("C:/Uni/BA/FINAL")
dat <- readxl::read_xlsx("data_Secco_BA_2025-09-01_14-23.xlsx") # 250
dat <- dat[-1, ]

length(unique(dat$CASE))
# 251

## Zeitspalten finden
# Alles was mit TIME beginnt (egal ob TIME033, TIME_SUM, TIMEABC...)
time_drop <- grep("^TIME", names(dat), value = TRUE, ignore.case = TRUE)

## Randomisierungs-Spalten finden
rn_drop <- grep("^RN", names(dat), value = TRUE, ignore.case = TRUE)

## Zusammenfuehren
drop_cols <- unique(c(time_drop, rn_drop))

## Entfernen
dat <- dat[, !(names(dat) %in% drop_cols), drop = FALSE]

dat_alt <- dat

### DEMOGRAPHICS VOR EXCLUSIONS ###
# Geschlecht
table(dat_alt$SD01)

# 1   2   3   4 
# 158  78   4   7 
# haben angeklickt


# 1           2           3          4 
# 147         72          4          7  
# w           m           d          k.A.
# wurden bedingung zugewiesen

# Alter



range(dat_alt$SD02_01, na.rm = TRUE)

dat_alt$SD02_01 <- as.numeric(dat_alt$SD02_01)

str(dat_alt$SD02_01)
# num [1:230] 21 54 32 52 41 29 57 29 24 40 ...

mean(dat_alt$SD02_01, na.rm = TRUE)
# 30.75709
# vor entfernung

sd(dat$SD02_01, na.rm = TRUE)
# 13.3068

## Verteilung Bedingungen ALT
table(dat_alt$bedingung)

# FFATTR FFUNATTR   FHATTR FHUNATTR 
#     62       57       62       49 
##### Alterscheck
dat$SD02_01 <- as.numeric(dat$SD02_01)

nrow(dat[dat$SD02_01 >= 18, ])
# 248

dat[!is.na(dat$SD02_01) & dat$SD02_01 < 18, ]
# wer ist unter 18?, CASE 334-11, 575-17, 607-3

dat <- dat[dat$SD02_01 >= 18, ]
nrow(dat)
# 248

##### Unvollständige Antworten entfernen

## Fragespalten definieren
dat$US09_10 <- NULL # Frage, die eigentlich vorher entfernt werden sollte (vergessen)

us_cols <- grep("^US\\d{2}($|_)", names(dat), value = TRUE, ignore.case = TRUE)

### Datensatz sortieren
dat <- dat[!is.na(dat$bedingung), ] # Entfernung aller unzugewiesenen; 230

## 1) Prefix-Sets für die 4 Bedingungen
ffattr_pref   <- sprintf("US%02d", 1:4)    # US01–US04
fhattr_pref   <- sprintf("US%02d", 5:8)    # US05–US08
ffunattr_pref <- sprintf("US%02d", 9:12)   # US09–US12
fhunattr_pref <- sprintf("US%02d", 13:16)  # US13–US16

## 2) Spaltenfinder (nur USxx_02.._09; KEINE statUS etc.)
cols_for <- function(prefixes) {
  pattern <- paste0("^(", paste(prefixes, collapse="|"), ")_(0[2-9])$")
  grep(pattern, names(dat), value = TRUE, ignore.case = TRUE)
}

cols_FFATTR   <- cols_for(ffattr_pref)
cols_FHATTR   <- cols_for(fhattr_pref)
cols_FFUNATTR <- cols_for(ffunattr_pref)
cols_FHUNATTR <- cols_for(fhunattr_pref)

## sanity check: sollten jeweils 32 sein
sapply(list(FFATTR=cols_FFATTR, FHATTR=cols_FHATTR, FFUNATTR=cols_FFUNATTR, FHUNATTR=cols_FHUNATTR), length)


## 3) Leere Strings in diesen Spalten zu NA machen (vor dem Zählen!)
all_cols <- unique(c(cols_FFATTR, cols_FHATTR, cols_FFUNATTR, cols_FHUNATTR))
dat[all_cols] <- lapply(dat[all_cols], function(x) { 
  x <- as.character(x); 
  x[trimws(x) == ""] <- NA; 
  x 
})

## 4) Pro Person die passenden 32 zählen
counts <- rep(NA_integer_, nrow(dat))

idx <- dat$bedingung == "FFATTR";    if (any(idx)) counts[idx] <- rowSums(!is.na(dat[idx, cols_FFATTR,   drop=FALSE]))
idx <- dat$bedingung == "FHATTR";    if (any(idx)) counts[idx] <- rowSums(!is.na(dat[idx, cols_FHATTR,   drop=FALSE]))
idx <- dat$bedingung == "FFUNATTR";  if (any(idx)) counts[idx] <- rowSums(!is.na(dat[idx, cols_FFUNATTR, drop=FALSE]))
idx <- dat$bedingung == "FHUNATTR";  if (any(idx)) counts[idx] <- rowSums(!is.na(dat[idx, cols_FHUNATTR, drop=FALSE]))

table(counts, useNA = "ifany")       
# Übersicht
# counts
#  0   8  16  24  32 
# 34  16  11   6 161 

bad_ids <- dat$CASE[is.na(counts) | counts != 32]
bad_ids                                 # IDs mit unvollständigen Antworten, manuell gecheckt

## 5) Filtern (falls prereg so verlangt)
dat <- dat[counts == 32, ]
nrow(dat)       # 161, stimmt

#### DEMOGRAPHICS NACHHER ###
table(dat$SD01)

#   1   2   3   4 
# 106  48   4   3 

# Alter
dat$SD02_01 <- as.numeric(dat$SD02_01)
str(dat$SD02_01)
# num [1:161] 21 54 32 41 29 57 29 24 40 66 ...

range(dat$SD02_01)
# 18 84

mean(dat$SD02_01)
# [1] 32.01242

sd(dat$SD02_01)
# 14.24429



## Struktur Bedingungen
dat$bedingung <- factor(dat$bedingung)
str(dat$bedingung) 
table(dat$bedingung) 

# FFATTR FFUNATTR   FHATTR FHUNATTR 
# 41       42       40       38 



# "FHATTR" = fehlerhaft + attraktiv
# "FHUNATTR" = fehlerhaft + unattraktiv
# "FFATTR" = fehlerfrei + attraktiv
# "FFUNATTR" = fehlerfrei + unattraktiv

# Bedingungen in Kompetenz und Attraktivitaet aufgliedern
dat$competence <- ifelse(grepl("^FH", dat$bedingung), "fehlerhaft", "fehlerfrei")
dat$attractiveness   <- ifelse(grepl("UNATTR", dat$bedingung), "unattraktiv", "attraktiv")

dat$competence <- factor(dat$competence)
dat$attractiveness   <- factor(dat$attractiveness)

table(dat$competence, dat$attractiveness)

##           attraktiv unattraktiv
# fehlerfrei        41          42
# fehlerhaft        40          38
# stimmt alles noch

## Mittelwerte pro Person ####
us_cols <- all_cols
dat[us_cols] <- lapply(dat[us_cols], function(x) as.numeric(as.character(x)))
dat$usability <- rowMeans(dat[us_cols], na.rm = TRUE)

## Mittelwerte pro Bedingung
mffa <- mean(dat$usability[dat$bedingung == "FFATTR"])
mffa
# [1] 4.590774

mffua <- mean(dat$usability[dat$bedingung == "FFUNATTR"])
mffua
# [1] 4.594512 ~ 4.59

mfua <- mean(dat$usability[dat$bedingung == "FHUNATTR"])
mfua
# [1] 3.606086

mfa <- mean(dat$usability[dat$bedingung == "FHATTR"])
mfa
# [1] 3.648438

#### ANOVA ####
anv <- summary(aov(usability ~ attractiveness * competence, dat))
anv
# Df Sum Sq Mean Sq F value Pr(>F)    
# attractiveness              1   0.24    0.24   0.730  0.394    
# competence                  1  32.28   32.28  97.936 <2e-16 ***
#   attractiveness:competence   1   0.10    0.10   0.308  0.580    
# Residuals                 157  51.75    0.33                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Main Effect of Competence signifikant! Rest nicht

effectsize::eta_squared(aov(usability ~ attractiveness * competence, dat), ci = 0.95)
# Effect Size for ANOVA (Type I)

# Parameter                 | Eta2 (partial) |       95% CI
# ---------------------------------------------------------
# attractiveness            |       4.63e-03 | [0.00, 1.00]
# competence                |           0.38 | [0.29, 1.00]
# attractiveness:competence |        1.96e-03| [0.00, 1.00]
# 
# - One-sided CIs: upper bound fixed at [1.00].


## f nach cohens: cohens_f <- sqrt(eta2 / (1 - eta2))
sqrt(0.38/0.63)
# 0.7766432 ~ 0.78

#### PLOTS ####
barplot <- plot(dat$usability ~ dat$bedingung, xlab = "Condition", 
                ylab = "Mean Usability Rating", 
                main = "Mean Usability Rating Across the Four Conditions",
                type = "b",
                col = c("pink", "lightblue"))



plot(c(mffa, mffua, mfa, mfua), type="p", xlab = "Condition", 
                ylab = "Mean Usability Rating", 
                main = "Mean Usability Rating Across the Four Conditions",
                axes = FALSE)
axis(1, at = 1:4, labels = c("FFATTR", "FFUNATTR", "FATTR", "FUNATTR"))
axis(2, at = 3:5)



plot(1:2, c(mffa, mfa), type = "b", pch = 16, lty = 1,
          ylim = c(3.5,4.75),
     xaxt = "n", xlab = "Competence Dimension", ylab = "Mean Usability Rating")
axis(1, at = 1:2, labels = c("fault-free","faulty"))
title(main = "Mean Usability Rating Across the Four Conditions", line = 0.5)
# zweite Linie
lines(1:2, c(mffua, mfua), type ="b", pch = 17, lty = 2)

legend("topright", title = "Attractiveness Dimension", legend = c("attractive","unattractive"),
       pch = c(16,17), lty = c(1,2))

op <- par(mgp = c(1.5, 0.5, 0))  # Achsentitel näher, Labels dichter, Ticks Standard


###### KOMPETENZ AUF X-ACHSE ######

# Fehlerbalken berechnen
se <- function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x)))

se_ffa <- with(dat, se(usability[competence=="fehlerfrei" & attractiveness=="attraktiv"]))      # FFATTR
se_fha <- with(dat, se(usability[competence=="fehlerhaft"  & attractiveness=="attraktiv"]))     # FHATTR
se_ffu <- with(dat, se(usability[competence=="fehlerfrei" & attractiveness=="unattraktiv"]))    # FFUNATTR
se_fhu <- with(dat, se(usability[competence=="fehlerhaft"  & attractiveness=="unattraktiv"]))   # FHUNATTR


plot(1:2, c(mffa, mfa), type = "b", pch = 16, lty = 1,
     ylim = c(3.45,4.8),
     xaxt = "n", xlab = "Competence Dimension", ylab = "Mean Usability Rating",
     main = "")
title(main = "Mean Usability with ±SE (Attractiveness × Competence)", line = 0.5)

axis(1, at = 1:2, labels = c("fault-free","faulty"))

# zweite Linie
lines(1:2, c(mffua, mfua), type ="b", pch = 17, lty = 2)

### Fehlerbalken

# attractive-Linie: Punkte (1=mffa, 2=mfa)
arrows(1, mffa - se_ffa, 1, mffa + se_ffa, angle=90, code=3, length=0.05)
arrows(2, mfa  - se_fha, 2, mfa  + se_fha, angle=90, code=3, length=0.05)

# unattractive-Linie: Punkte (1=mffua, 2=mfua)
arrows(1, mffua - se_ffu, 1, mffua + se_ffu, angle=90, code=3, length=0.05)
arrows(2, mfua  - se_fhu, 2, mfua  + se_fhu, angle=90, code=3, length=0.05)

### Signifikanzstern

## Y-Position oberhalb der höchsten Linie festlegen
y_bracket <- max(c(mffa, mfa, mffua, mfua)) + 0.15

## Klammer fault-free vs. faulty
segments(1, y_bracket, 2, y_bracket)         # horizontale Linie
segments(1, y_bracket, 1, y_bracket - 0.05)  # linker Haken
segments(2, y_bracket, 2, y_bracket - 0.05)  # rechter Haken

## Signifikanzstern einzeichnen
text(1.5, y_bracket + 0.05, labels = "***", cex = 1.3)


legend("bottomleft",  xpd = TRUE,
       title = "Attractiveness Dimension",
       legend = c("attractive","unattractive"),
       pch = c(16,17), lty = c(1,2))

par(op)  # reset zu Standard




###### ATTRACTIVENESS AUF X-ACHSE #######
## Sicherstellen, dass die Faktornamen/Levels passen
dat$attractiveness <- droplevels(dat$attractiveness)
dat$competence     <- droplevels(dat$competence)

## Reihenfolge für die Achse/Legende (anpassen falls nötig)
lv_attr <- c("attraktiv","unattraktiv")      # deine Level im Datensatz
lv_comp <- c("fehlerfrei","fehlerhaft")

## Mittelwerte & SE pro Zelle
se <- function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x)))

m  <- with(dat, tapply(usability, list(attractiveness, competence), mean, na.rm=TRUE))
seM<- with(dat, tapply(usability, list(attractiveness, competence), se))

## Auf gewünschte Reihenfolge bringen
m   <- m[lv_attr, lv_comp, drop=FALSE]
seM <- seM[lv_attr, lv_comp, drop=FALSE]

## Plot vorbereiten
x <- 1:2
ymax <- max(m + seM, na.rm=TRUE)
op <- par(mgp=c(1.5,0.5,0))

plot(x, m[, "fehlerfrei"], type="b", pch=16, lty=1,
     xaxt="n",
     xlab="Attractiveness",
     ylab="Mean Usability Rating",
     ylim=c(3.2, ymax + 0.3),
     main="Mean Usability with ±SE (Attractiveness × Competence)")

axis(1, at=x, labels=c("attractive","unattractive"))



## Fehlerbalken (fault-free)
arrows(x, m[, "fehlerfrei"] - seM[, "fehlerfrei"],
       x, m[, "fehlerfrei"] + seM[, "fehlerfrei"],
       angle=90, code=3, length=0.05)

## Zweite Linie: faulty
lines(x, m[, "fehlerhaft"], type="b", pch=17, lty=2)

## Fehlerbalken (faulty)
arrows(x, m[, "fehlerhaft"] - seM[, "fehlerhaft"],
       x, m[, "fehlerhaft"] + seM[, "fehlerhaft"],
       angle=90, code=3, length=0.05)

legend("topright", title="Competence",
       legend=c("fault-free","faulty"),
       pch=c(16,17), lty=c(1,2))

par(op)



##### EXPLORATIVE ANALYSE #####
# manipulationscheck
# MA03 = wahrgenommene ATTR ; MA04 = wahrgenommene FEHL

ma_cols <- grep("^MA\\d{2}($|_)", names(dat), value = TRUE)
ma_cols 
has_ma <- rowSums(!is.na(dat[ma_cols])) > 0
dat_ma <- dat[has_ma, ]

nrow(dat)     # gesamte Stichprobe (z. B. 161 nach Exklusion)
nrow(dat_ma)  # Teilstichprobe mit Manipulationscheck
ma_cols <- grep("^MA", names(dat), value = TRUE)
all(rowSums(!is.na(dat[ma_cols])) > 0)
# Check, selbe Stichprobe alle haben Manipulationscheck ausgefüllt

## Mittelwerte MCATTR pro bedingung, wurde attraktivität wirklich so wahrgenommen?
MC_MA <- tapply(as.numeric(dat$MA03), dat$bedingung, mean, na.rm = TRUE)
MC_MA
#   FFATTR FFUNATTR   FHATTR FHUNATTR 
# 5.341463 3.238095 4.825000 3.131579 
MC_SDA <- tapply(as.numeric(dat$MA03), dat$bedingung, sd, na.rm = TRUE)
MC_SDA
#   FFATTR FFUNATTR   FHATTR FHUNATTR 
# 0.964618 1.321668 1.174243 1.094731 

## Mittelwerte MCF pro bedingung, wurde fehlerhaftigkeit wirklich so wahrgenommen?
MC_MF <- tapply(as.numeric(dat$MA04), dat$bedingung, mean, na.rm = TRUE)
#   FFATTR FFUNATTR   FHATTR FHUNATTR 
# 1.951220 2.261905 5.000000 5.263158

MC_SDF <- tapply(as.numeric(dat$MA04), dat$bedingung, sd, na.rm = TRUE)
MC_SDF
#    FFATTR  FFUNATTR    FHATTR  FHUNATTR 
# 0.8646161 1.1274734 1.2194997 1.0826447 



#### Korrelationsanalysen
## wahrgenommene Fehlerhaftigkeit und Usability
corPF <- cor(dat$usability, dat$MA04, use="pairwise.complete.obs")
# [1] -0.7448468
corPA <- cor(dat$usability, dat$MA03, use="pairwise.complete.obs")
# [1] 0.1776359

par(mfrow = c(1,2), mar = c(4.2,4.5,3,1), mgp = c(2.2,0.7,0))
## plotten
# fehlerhaftigkeit
plot(dat$MA04, dat$usability,
     xlab = "Perceived Faultiness",
     ylab = "Usability",
     main = "Correlation Usability and \n Perceived Faultiness",
     cex.main = 0.8,
     pch = 16, col = "grey")

abline(lm(usability ~ MA04, data = dat), col = "red", lwd = 2)

legend("bottomleft", legend = paste0("r = - 0.745 "),
       bty = "n")

# attractiveness
plot(dat$MA03, dat$usability,
     xlab = "Perceived Attractiveness",
     ylab = "Usability",
     main = "Correlation Usability and \n Perceived Attractiveness",
     cex.main = 0.8,
     pch = 16, col = "grey")

abline(lm(usability ~ MA03, data = dat), col = "blue", lwd = 2)

# Korrelation einblenden
legend("bottomleft", legend = paste0("r = 0.178"),
       bty = "n")

###### Erfahrung chatbot

dat$MA02 <- as.numeric(dat$MA02)
str(dat$MA02)
table(dat$MA02, useNA="ifany")
# 1  2  3  4  5 
# 66 47 35 11  2 

## gruppen bilden
dat$MA02_grp <- cut(dat$MA02,
                    breaks = c(0, 1, 3, 5),   # Grenzen: 0–1, 2–3, 4–5
                    labels = c("low","medium","high"),
                    right = TRUE)
table(dat$MA02_grp)
#  low medium   high 
#   66     82     13 

## mittelwerte pro gruppe in usability
tapply(dat$usability, dat$MA02_grp, mean, na.rm=TRUE)
#      low   medium     high 
# 4.013258 4.156631 4.045673 

## 2x2x3 anova
fit <- aov(usability ~ competence * attractiveness * as.factor(MA02_grp), data=dat)
summary(fit)
# Df Sum Sq Mean Sq F value Pr(>F)    
# competence                           1  32.17   32.17  96.738 <2e-16 ***
#   attractiveness                       1   0.36    0.36   1.073  0.302    
# MA02_grp                             2   0.67    0.33   1.002  0.370    
# competence:attractiveness            1   0.09    0.09   0.272  0.603    
# competence:MA02_grp                  2   0.95    0.48   1.429  0.243    
# attractiveness:MA02_grp              2   0.15    0.07   0.220  0.802    
# competence:attractiveness:MA02_grp   2   0.46    0.23   0.685  0.505    
# Residuals                          149  49.55    0.33                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
