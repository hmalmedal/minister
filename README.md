# Tid i regjering

En Shiny-app som viser hvor lenge statsråder sitter i regjering før registrert
avskjed. Appen bruker data fra `regjering.csv` til å beregne Kaplan–Meier-kurver.
Du kan sammenligne regjeringer eller vise en samlet kurve for hele datasettet.

## Kom i gang

Du trenger R og følgende R-pakker. Kjør dette i R-konsollen én gang:

```r
install.packages(c(
  "tidyverse", "lubridate", "survival", "ggsurvfit", "shiny", "bslib"
))
```

Åpne `minister.Rproj` i RStudio, og kjør:

```r
shiny::runApp(".")
```

Du kan også åpne `app.R` og velge **Run App** i RStudio. Uten RStudio starter du
appen fra prosjektmappen i en terminal:

```sh
Rscript -e 'shiny::runApp(".")'
```

Åpne den lokale adressen som R skriver ut hvis nettleseren ikke åpnes automatisk.
Stopp appen med **Stop** i RStudio eller `Ctrl+C` i terminalen.

## Bruk appen

- **Ingen regjeringer valgt:** Alle observasjoner inngår i én samlet kurve med
  navnet «Alle regjeringer». Dette er også startvisningen.
- **Én regjering valgt:** Grafen viser kurven for denne regjeringen.
- **Flere regjeringer valgt:** Hver regjering får sin egen kurve.
- **Fjern alle valg:** Gå tilbake til den samlede kurven.

Velgeren **Velg regjeringer** ligger i sidepanelet, sammen med en lenke til
prosjektet på GitHub. Grafen oppdateres når utvalget endres.

## Slik tolkes grafen

Den vannrette aksen viser **År som statsråd**, målt fra hver observasjons
startdato. Den loddrette aksen viser **Estimert andel uten avskjed**.
Kurven beregnes med `survival::Surv()` og `ggsurvfit::survfit2()`, gruppert etter
regjering, og tegnes med `ggsurvfit()`.

`Avskjed = 1` regnes som en hendelse og gir et fall i kurven. `Avskjed = 0`
regnes som høyresensurering: Observasjonen bidrar fram til sluttdatoen, uten at
det registreres en avskjedshendelse. En slik rad kan ha en kjent sluttdato;
verdien 0 betyr derfor ikke nødvendigvis at personen fortsatt sitter i regjering.
Appen bruker kodingen i CSV-filen og utleder ikke avskjed fra datoene.

Kurven er et statistisk estimat, ikke den direkte andelen av de opprinnelige
statsrådene som fortsatt sitter på et gitt tidspunkt. Sammenligninger må ta
hensyn til ulik observasjonstid og sensurering. Appen viser ikke årsaker til
avskjed eller tester av forskjeller mellom regjeringene.

Varigheten beregnes som:

```r
decimal_date(Sluttdato) - decimal_date(Startdato)
```

Dette er forskjellen mellom datoene uttrykt som desimalår. Manglende sluttdato
erstattes i minnet med dagens dato i tidssonen `Europe/Oslo` når appen lastes.
CSV-filen endres ikke. Data og dagens dato lastes utenfor serverfunksjonen;
start appen på nytt etter dataendringer eller for å oppdatere datoen ved en
langvarig kjøring.

## Dataformat og vedlikehold

`regjering.csv` er en kommaseparert UTF-8-fil med disse kolonnene:

| Kolonne | Format | Betydning |
| --- | --- | --- |
| `Navn` | Tekst | Navn på statsråden. Brukes også av sorteringsskriptet. |
| `Startdato` | `ÅÅÅÅ-MM-DD` | Startdato for observasjonen. |
| `Sluttdato` | `ÅÅÅÅ-MM-DD` eller `NA` | Siste observasjonsdato. `NA` erstattes med dagens dato ved lasting. |
| `Avskjed` | `0` eller `1` | `1` for registrert avskjed, `0` for sensurert observasjon. |
| `Regjering` | Tekst | Regjeringsnavn som brukes i velgeren og til gruppering av kurvene. |

Hver rad behandles som en egen observasjon. Appen slår ikke sammen rader for
samme person. Nye regjeringsnavn blir automatisk tilgjengelige i velgeren når
appen lastes på nytt, i den rekkefølgen de først forekommer i filen.

Ved oppdatering:

1. Legg til eller endre rader i `regjering.csv`, og behold kolonnenavnene.
2. Kontroller datoene, at sluttdato ikke er før startdato, og at `Avskjed` er
   kodet riktig. Bruk `NA` for ukjent sluttdato og `0` for en pågående observasjon.
3. Sorter eventuelt filen ved å kjøre `source("regjeringssortering.R")` fra
   prosjektmappen. **Skriptet overskriver `regjering.csv`.**
4. Start appen på nytt og kontroller visningen.

Sorteringsskriptet plasserer manglende sluttdatoer først, deretter sluttdato og
startdato i synkende rekkefølge. Ved like datoer sorteres navn i stigende norsk
rekkefølge ved hjelp av `factor_no()`.

Appen har ingen egen datavalidering eller automatisk innhenting av data.
CSV-filen inneholder heller ingen kildekolonne eller forklaring av de enkelte
avskjedskodingene; slike opplysninger må avklares ved vedlikehold av datasettet.

## Filer og oppbygning

| Fil | Innhold |
| --- | --- |
| `app.R` | Laster data, beregner varighet og definerer Shiny-grensesnitt og server. |
| `regjering.csv` | Observasjonene som appen leser. |
| `regjeringssortering.R` | Valgfritt vedlikeholdsskript som sorterer og skriver datafilen. |
| `factor_no.R` | Hjelpefunksjon for faktorer med norsk sorteringsrekkefølge. |
| `minister.Rproj` | RStudio-prosjekt med UTF-8 og to mellomrom som innrykk. |

I `app.R` velger den reaktive funksjonen `regjeringsdata()` observasjonene som
skal vises. `regjering_survfit()` beregner kurvene, og `output$p` tegner grafen.
Grensesnittet bruker `bslib` med Bootstrap 5 og temaet Flatly.

## Feilsøking

- **En pakke mangler:** Kjør installasjonskommandoen over i den R-installasjonen
  du bruker til å starte appen.
- **`regjering.csv` blir ikke funnet:** Start fra prosjektmappen. Sjekk
  arbeidsmappen med `getwd()`, eller åpne `minister.Rproj` på nytt.
- **Dataendringer vises ikke:** Stopp og start appen på nytt; filen leses ved
  lasting, ikke ved hvert valg i grensesnittet.
- **Feil ved lesing eller beregning:** Kontroller kolonnenavn, datoformat og
  hendelseskoding. `readr::problems(readr::read_csv("regjering.csv"))` kan vise
  problemer med innlesingen, men erstatter ikke kontroll av datainnholdet.
