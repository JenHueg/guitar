# Notwendige Pakete laden
library(ordinal)   # Für CLMM
library(ggplot2)   # Für Visualisierung (optional)
library(dplyr)     # Für Datenmanipulation
library(readr)     # Für CSV-Einlesen

# Arbeitsverzeichnis prüfen
getwd()  # zeigt, wo R gerade nach der Datei sucht

# Optional: Arbeitsverzeichnis setzen, wenn nötig (Pfad anpassen!)
setwd("")

getwd()

# CSV-Datei einlesen – ACHTUNG: Dateiname und .csv müssen stimmen. Mit "delim" Trennzeichen definieren.
daten <- read_delim("Beispiel_Datensatz.csv", delim = ";")

# Gibt Spaltenspezifikation eines eingelesenen Datensatzes zurück (hängt mit package readr zusammen)
spec(daten)

# Struktur prüfen (hilfreich zur Kontrolle)
str(daten)
head(daten)
glimpse(daten)
table(daten$response, useNA = "always")

# Spalten korrekt definieren
# Namen müssen entsprechend nach Spaltennamen in CSV benannt werden
daten$response <- factor(daten$response, ordered = TRUE, levels = 1:5)
daten$condition <- factor(daten$condition)
daten$subject_id <- factor(daten$subject_id)
daten$item <- factor(daten$item)

# Referenzbedingung explizit setzen – z. B. Bedingung "1" als Referenz
# daten$Bedingung <- factor(daten$Bedingung, levels = c("1", "2", "3"))
# daten$condition <- factor(daten$condition, levels = c("od", "md", "mj"))

# Modell mit Versuchspersonen & Items als Random Effects
#modell <- clmm(response ~ condition + (1 | subject_id) + (1 | item), data = daten)

# Modellzusammenfassung anzeigen
#summary(modell)

# Nullmodell ohne den Haupteffekt "Diskurspartikel/condition"
#modell_null <- clmm(
#response ~ 1 + (1|subject_id) + (1|item),
 # data = daten
#)

# Vergleich: Modell mit vs. ohne "unabhängige Variable, hier Diskurspartikel"
#anova(modell_null, modell)


# OPTIONAL: Gruppe bilden – z.B. Bedingung 1 vs. 2+3
daten$Gruppe <- ifelse(daten$condition == "od", "od", "md_und_mj")
daten$Gruppe <- factor(daten$Gruppe, levels = c("od", "md_und_mj"))

# Optionales Modell mit dieser Gruppierung
modell_gruppe <- clmm(response ~ Gruppe + (1 | subject_id) + (1 | item), data = daten)
summary(modell_gruppe)

# OPTIONAL: Visualisierung der mittleren Ratings pro Bedingung
daten %>%
 group_by(condition) %>%
summarise(Mittelwert = mean(as.numeric(condition))) %>%
ggplot(aes(x = condition, y = Mittelwert, fill = condition)) +
geom_bar(stat = "identity") +
labs(title = "Mittleres Antwortverhalten pro Bedingung",
y = "Mittelwert (1–5)", x = "condition") +
theme_minimal()


