library(readr)
library(glmnet)
library(ggplot2)
library(broom)
library(patchwork)
# Daten einlesen -----------------------------------------------------------

Fifa <- read_csv("FS4/Fifa.csv")


# Height & Weight in metrische Einheiten umrechnen -------------------------

Fifa$Height  <- round(as.numeric(sub("'.*", "", Fifa$Height)) * 30.48 +
                        as.numeric(sub(".*'", "", Fifa$Height)) * 2.54)

Fifa$Weight  <- round(as.numeric(sub("lbs", "", Fifa$Weight)) * 0.45359237)

# Aufteilung nach Positionen ----------------------------------------------

verteidigung <- c("RB", "LB", "RWB", "LWB", "CB", "RCB", "LCB")

mittelfeld <- c("CM", "RCM", "LCM",
                "CDM", "LDM", "RDM",
                "CAM", "LAM", "RAM",
                "RM", "LM")

sturm <- c("ST", "LS", "RS", "CF",
           "LW", "RW", "LF", "RF")

torwart <- "GK"

Fifa_Pos <- Fifa
Fifa_Pos$Group <- ifelse(Fifa_Pos$Position %in% verteidigung, "Def",
                         ifelse(Fifa_Pos$Position %in% mittelfeld, "Mid",
                                ifelse(Fifa_Pos$Position %in% sturm, "Att", NA)))

Fifa_Pos$...1 <- NULL

Fifa_Pos <- Fifa_Pos[, !(names(Fifa_Pos) %in% 
                           c("...1","X","ID","Name","Club","Nationality",
                             "Contract.Valid.Until","Position", "Jersey.Number",
                             "GKHandling","GKKicking",
                             "GKPositioning","GKReflexes", "GKDiving"))]

# Attribut-Klassen + Farben ------------------------------------------------

attribute_class <- c(
  "Age"                      = "Personal",
  "Nationality"              = "Personal",
  "Overall"                  = "General",
  "Potential"                = "General",
  "Club"                     = "Personal",
  "Preferred.Foot"           = "Personal",
  "International.Reputation" = "Personal",
  "Weak.Foot"                = "Personal",
  "Position"                 = "Personal",
  "Jersey.Number"            = "Personal",
  "Contract.Valid.Until"     = "Personal",
  "Height"                   = "Personal",
  "Weight"                   = "Personal",
  "Potential"                = "Personal",
  
  "Crossing"                 = "Technical",
  "Finishing"                = "Technical",
  "HeadingAccuracy"          = "Technical",
  "ShortPassing"             = "Technical",
  "LongPassing"              = "Technical",
  "BallControl"              = "Technical",
  "Volleys"                  = "Technical",
  "Dribbling"                = "Technical",
  "Curve"                    = "Technical",
  "FKAccuracy"               = "Technical",
  "LongShots"                = "Technical",
  "Penalties"                = "Technical",
  "ShotPower"                = "Technical",
  
  "Acceleration"             = "Physical",
  "SprintSpeed"              = "Physical",
  "Agility"                  = "Physical",
  "Reactions"                = "Physical",
  "Balance"                  = "Physical",
  "Jumping"                  = "Physical",
  "Stamina"                  = "Physical",
  "Strength"                 = "Physical",
  
  "Aggression"               = "Mental",
  "Interceptions"            = "Mental",
  "Positioning"              = "Mental",
  "Vision"                   = "Mental",
  "Composure"                = "Mental",
  
  "Marking"                  = "Defensive",
  "StandingTackle"           = "Defensive",
  "SlidingTackle"            = "Defensive",
  
  "GKDiving"                 = "Goalkeeper",
  "GKHandling"               = "Goalkeeper",
  "GKKicking"                = "Goalkeeper",
  "GKPositioning"            = "Goalkeeper",
  "GKReflexes"               = "Goalkeeper"
)

class_colors <- c(
  "Personal"    = "#1F77B4",  # Blau
  "General"     = "#2CA02C",  # Dunkelgrün
  "Technical"   = "#FF7F0E",  # Orange
  "Physical"    = "#D62728",  # Rot
  "Mental"      = "#9467BD",  # Lila
  "Defensive"   = "#17BECF",  # Türkis
  "Goalkeeper"  = "#7F7F7F"   # Grau
)

# LASSO-Hilfsfunktion ------------------------------------------------------

run_lasso <- function(df, label) {
  df <- na.omit(df)
  
  df_num <- df[ , sapply(df, is.numeric)]
  
  konstante <- sapply(df_num, function(x) length(unique(x)) <= 1)
  df_num <- df_num[ , !konstante]
  
  X <- model.matrix(Overall ~ ., df_num)[ , -1]
  y <- df_num$Overall
  
  cvfit <- cv.glmnet(X, y, alpha = 1)
  
  coefs <- coef(cvfit, s = "lambda.1se")
  
  df_coefs <- data.frame(
    variable = rownames(coefs),
    coef = as.numeric(coefs)
  )
  
  df_coefs <- df_coefs[df_coefs$variable != "(Intercept)", ]
  df_nonzero <- df_coefs[df_coefs$coef != 0, ]
  df_nonzero <- df_nonzero[order(abs(df_nonzero$coef), decreasing = TRUE), ]
  
  cat("\n=====================\n")
  cat("Top 10 Variablen für:", label, "\n")
  cat("=====================\n")
  
  return(df_nonzero)
}

# Datensplits --------------------------------------------------------------

Def <- subset(Fifa_Pos, Group == "Def")
Mid <- subset(Fifa_Pos, Group == "Mid")
Att <- subset(Fifa_Pos, Group == "Att")

# LASSO pro Gruppe ---------------------------------------------------------

coef_def <- run_lasso(Def, "Verteidigung")
coef_mid <- run_lasso(Mid, "Mittelfeld")
coef_att <- run_lasso(Att, "Sturm")

# Top 10 + Klassen anhängen ------------------------------------------------

top10_def <- head(coef_def, 10)
top10_mid <- head(coef_mid, 10)
top10_att <- head(coef_att, 10)

top10_def$Class <- attribute_class[top10_def$variable]
top10_mid$Class <- attribute_class[top10_mid$variable]
top10_att$Class <- attribute_class[top10_att$variable]

# Balkenplots mit Colourcoding --------------------------------------------

p_lasso_def <- ggplot(top10_def,
       aes(x = reorder(variable, abs(coef)),
           y = abs(coef),
           fill = Class)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 wichtige Variablen – Verteidigung",
    x = "Attribut",
    y = "|LASSO-Koeffizient|",
    fill = "Attribut-Kategorie"
  ) +
  scale_fill_manual(values = class_colors) +
  theme_minimal()

p_lasso_mid <- ggplot(top10_mid,
       aes(x = reorder(variable, abs(coef)),
           y = abs(coef),
           fill = Class)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 wichtige Variablen – Mittelfeld",
    x = "Attribut",
    y = "|LASSO-Koeffizient|",
    fill = "Attribut-Kategorie"
  ) +
  scale_fill_manual(values = class_colors) +
  theme_minimal()

p_lasso_att <- ggplot(top10_att,
       aes(x = reorder(variable, abs(coef)),
           y = abs(coef),
           fill = Class)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 wichtige Variablen – Angriff",
    x = "Attribut",
    y = "|LASSO-Koeffizient|",
    fill = "Attribut-Kategorie"
  ) +
  scale_fill_manual(values = class_colors) +
  theme_minimal()

# Lineare Modelle mit Top-10-Variablen -------------------------------------

reg_def <- lm(Overall ~ ., data = Def[, c("Overall", top10_def$variable)])
reg_mid <- lm(Overall ~ ., data = Mid[, c("Overall", top10_mid$variable)])
reg_att <- lm(Overall ~ ., data = Att[, c("Overall", top10_att$variable)])



summary(reg_def)
summary(reg_mid)
summary(reg_att)

# Konfidenzintervalle + Colourcoding ---------------------------------------

reg_def_df <- broom::tidy(reg_def, conf.int = TRUE)
reg_mid_df <- broom::tidy(reg_mid, conf.int = TRUE)
reg_att_df <- broom::tidy(reg_att, conf.int = TRUE)

reg_def_df <- subset(reg_def_df, term != "(Intercept)")
reg_mid_df <- subset(reg_mid_df, term != "(Intercept)")
reg_att_df <- subset(reg_att_df, term != "(Intercept)")

reg_def_df$Class <- attribute_class[reg_def_df$term]
reg_mid_df$Class <- attribute_class[reg_mid_df$term]
reg_att_df$Class <- attribute_class[reg_att_df$term]

p_ci_def <- ggplot(reg_def_df,
       aes(x = reorder(term, estimate),
           y = estimate,
           color = Class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Schätzer und 95%-Konfidenzintervalle",
    subtitle = "(Top 10 Variablen – Verteidigung)",
    x = "Variable",
    y = "Koeffizient",
    color = "Attribut-Kategorie"
  ) +
  scale_color_manual(values = class_colors, na.value = "black") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.3)
  )

p_ci_mid <- ggplot(reg_mid_df,
       aes(x = reorder(term, estimate),
           y = estimate,
           color = Class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Schätzer und 95%-Konfidenzintervalle",
    subtitle = "(Top 10 Variablen – Mittelfeld)",
    x = "Variable",
    y = "Koeffizient",
    color = "Attribut-Kategorie"
  ) +
  scale_color_manual(values = class_colors, na.value = "black") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.3)
  )

p_ci_att <- ggplot(reg_att_df,
       aes(x = reorder(term, estimate),
           y = estimate,
           color = Class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Schätzer und 95%-Konfidenzintervalle",
    subtitle = "(Top 10 Variablen – Angriff)",
    x = "Variable",
    y = "Koeffizient",
    color = "Attribut-Kategorie"
  ) +
  scale_color_manual(values = class_colors, na.value = "black") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.3)
  )

all <- gridExtra::grid.arrange(
  p_lasso_def, p_ci_def,
  p_lasso_mid, p_ci_mid,
  p_lasso_att, p_ci_att,
  ncol = 2
)

def <- gridExtra::grid.arrange(p_lasso_def, p_ci_def)

mid <- gridExtra::grid.arrange(p_lasso_mid, p_ci_mid)

att <- gridExtra::grid.arrange(p_lasso_att, p_ci_att)
