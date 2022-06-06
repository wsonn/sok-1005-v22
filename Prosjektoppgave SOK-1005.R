library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(zoo)


# Oppgave 1

# importerer de 6 datasettene
AwichSA <- read_csv("AppWichStoreAttributes.csv")
county_crime <- read_csv("county_crime.csv")
county_demographic <- read_csv("county_demographic.csv")
county_employment <- read_csv("county_employment.csv")
weekly_sales <- read_csv("weekly_sales_10stores.csv")
weekly_weather <- read_csv("weekly_weather.csv")

# slår sammen datasettene som viser statistikk for hver 'county'
county_stats <- list(county_crime, county_demographic, county_employment) %>% 
  reduce(full_join, by = "County_Name")

# slår sammen datasettene som viser statistikk for hver butikk
store_stats <- merge(x = AwichSA, y = weekly_sales, by.x = "Store_Num",
                      by.y = "Store_num")

# legger til kolonne for uke i 'store_stats'
## starter med å lage ny kolonne for dato med type: "Date" og ikke "Chr"
store_stats$Date <- as.Date(store_stats$Date, format = "%m/%d/%Y")

store_stats <- store_stats %>% 
  mutate(Week = week(Date))

## endrer kolonnenavn i 'weekly weather'
weekly_weather <- weekly_weather %>% 
  rename(Week = Weather_Week,
         Store_Weather_Station = Weather_Station)


# legger til datasettet for værstatistikken i datasettet for butikkstatistikken
stats <- full_join(store_stats, weekly_weather, by = c("Week", "Store_Weather_Station"))

# legger til datasettet for countystats
county_stats <- county_stats %>% 
  mutate(Store_County = County_Name)

stats <- full_join(stats, county_stats, by = "Store_County")

# Oppgave 2

# Her skal jeg velge et utsalgssted, for å lage en kortsiktig individuell
# salgsrapport. Jeg ønsker å se på om været
# har noe å si når det kommer til hva som selger, og hvor mye som selger. Å
# ha oversikt over hva som selger avhengig av vær (og dermed årstid) kan føre 
# til mer profitabel drift ved at man kan forberede seg og kjøpe inn mer av
# visse råvarer og så videre. Samtidig kan man også unngå å overbemanne
# ved dårlige tider, og å underbemanne i perioder med varme og fint vær (ferier).
# Videre kan jeg se på hvilke sandwicher som utgjør størst andel av salget på 
# en ukentlig basis, og om disse er de samme som har høyest profitt-margin.

# Jeg ønsker å få oversikt over bedriften før jeg begår valget om hvilket
# utsalgssted jeg skal se nærmere på.
# Derfor lager jeg et kakediagram for å få oversikt over fylkenes profitt og 
# arbeidsstyrke - som jeg ser på som potensielle kunder.

q <- stats %>% 
  group_by(Store_County) %>% 
  mutate(Total_Profit = sum(Profit)) %>% 
  select(Store_County, Total_Profit, County_Labor_Force, 
         County_Unemployment_Rate) %>% 
  distinct(Store_County, .keep_all = TRUE)

library(cowplot)

plot_grid(ggplot(q, aes(x="", y=Total_Profit, fill=Store_County)) +
            labs(title = "Total profitt per county") +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +theme(legend.position="none") +
            theme_void() +
            scale_fill_brewer(palette="Paired"),
          ggplot(q, aes(x="", y=County_Labor_Force, fill=Store_County)) +
            labs(title = "Total arbeidsstyrke per county") +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +theme(legend.position="none") +
            theme_void() +
            scale_fill_brewer(palette="Paired"),
          ncol = 2,
          labels = "AUTO")

q %>% 
  arrange(-County_Unemployment_Rate) %>% 
  head()


# Fylket med størst forskjell mellom arbeidsstyrke og inntekt viser seg å være 
# Appleton, som kun har en butikk. Dette synes jeg er interessant, så derfor
# velger jeg å se nørmere på dette utsalget.
# Det viser seg også at Appleton er det fylket med størst arbeidsledighet.

Oppgave2 <- stats %>% 
  select(Store_County, Store_Num, Description, Sold, Sales, Price, Profit, Margin,
         County_Labor_Force, County_Unemployment_Rate, Date, Week, Year,
         Weather_Bad_Weather_days) %>% 
  filter(Store_County == 'Appleton County') %>%
  group_by(Week) %>% 
  mutate(YearWeek = paste(Year, Week, sep = " / "),
         Total_Sales_Week = sum(Sold),
         Total_Profit_Week = sum(Profit))

Oppgave2 <- Oppgave2 %>% 
  mutate(Total_Profit_Week = sum(Profit))

Oppgave2 %>%
  ggplot(aes(x=Date, y=Total_Sales_Week)) +
  geom_point(color = "red", size = 3.5) +
  geom_line(aes(x=Date, y=Total_Sales_Week),color = "dodgerblue",size = 0.1) +
  labs(title = "Totalt antall salg per uke - Appleton",
       subtitle = "Gjelder alle varer",
       x = "",
       y = "Antall varer") +
  scale_x_date(date_breaks = "2 weeks") +
  theme(axis.text.x = element_text(angle = -60))

Oppgave2 %>% 
  ggplot(aes(x=Date, y=Total_Profit_Week)) +
  geom_point(color = "red", size = 3.5) +
  geom_line(aes(x=Date, y=Total_Profit_Week),color = "dodgerblue",size = 0.1) +
  labs(title = "Total profitt per uke - Appleton",
       subtitle = "Gjelder alle varer",
       x = "",
       y = "Ukentlig profitt ($)") +
  scale_x_date(date_breaks = "2 weeks") +
  theme(axis.text.x = element_text(angle = -60))

# Disse figurene viser bedriftens ukentlige salg og profitt i Appleton. Her er
# det spesielt to uker som skiller seg ut negativt. Imidlertid kan man se på datoene
# at dette er ved ulike høytider, enten det er thanksgiving eller jul. Mitt ønske
# er derimot å se på en "normal" uke. Derfor velger jeg vilkårlig en uke i
# "sommer-halvåret". Jeg velger uke 25 i 2012, som er i juni. I tillegg 
# har bedriften såpass mange varer at jeg velger å begrense ned datasettet.
# Ettersom dette skal være en amerikansk hurtigmat-kjede som spesifiserer
# seg på Sandwicher er det disse jeg ønsker å se på. Derfor velger jeg å 
# avgrense søket til varer som kun inneholder ordet "REGULAR", siden dette
# tilsier at varen er en sandwich/sub med normal størrelse.

u25 <- stats %>%
  filter(Store_County == "Appleton County",
         Week == 25) %>% 
  filter(str_detect(Description, "REGULAR"))

u25.2 <- u25 %>% 
  select(Week, INV_NUMBER, Description, Price, Sold, Sales, Profit, Tot_Sls, Margin,
         Weather_Bad_Weather_Week, Weather_Bad_Weather_days)

u25.2 %>% 
  select(Week, Weather_Bad_Weather_days) %>%
  distinct(Week, .keep_all = TRUE) %>% 
  head()

u25.2 %>% 
  select(Description, Sold, Price, Sales) %>% 
  arrange(-Sold) %>% 
  head()

# Her har jeg funnet at dette var en uke med stort sett fint vær, ettersom det
# kun var en dag med "dårlig vær".
# Ved å sortere etter den sandwichen som det har blitt solgt flest av i kvantum finner jeg ut
# at det har blitt solgt flest "månedens sub" denne uka. Det er derimot ikke nødvendigvis
# slik at det er denne som utgjør størst andel av salget (i dollar).

u25.2 %>% 
  arrange(-Tot_Sls) %>% 
  select(Description, Sold, Price, Sales, Tot_Sls, Margin, Profit) %>% 
  head()

u25.2 %>% 
  summarise(sum(Sold))

u25.2 %>% 
  summarise(sum(Profit))

# Det er månedens sub som utgjør den største andelen av salget denne uken
# i dollar også. Det ble solgt 1207 sandwicher denne uka, og total profitt for 
# uka endte opp med å bli 4688,93$.

u25.2 %>%
  select(Description, Tot_Sls) %>% 
  arrange(-Tot_Sls) %>% 
  slice(1:5) %>% 
  mutate(D2 = str_remove_all(Description, "REGULAR ")) %>% 
  ggplot(aes(x=D2, y=Tot_Sls, fill=D2)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Andel av ukentlig Sandwich-salg (i $)",
       subtitle = "Appleton County, uke 25",
       x = "Vare",
       y = "Andel av salg (i $)") +
  guides(fill=guide_legend(title="Vare")) + 
  theme_light()

u25.2 %>% 
  select(Description, Margin) %>% 
  arrange(-Margin) %>% 
  head()

# Her kan man se hvilke subs som utgjorde størst andel av butikkens salg denne
# uken. Månedens sub utgjorde altså 8% av salget. Samtidig kan man se at flere 
# av disse 'bestselgerne' også er de sandwichene med høyest profitt-margin.
# Videre velger jeg å sammenligne med en uke med minst 6 "Bad weather days".

stats %>% 
  select(Week, Weather_Bad_Weather_days, Store_County) %>%
  arrange(Week) %>% 
  filter(Store_County == "Appleton County", 
         Weather_Bad_Weather_days >= 6) %>% 
  distinct(Week, Weather_Bad_Weather_days)

# 6 uker med veldig dårlig vær. Velger å se nærmere uke 3 (2013), som er en vanlig uke
# i januar, helt uten høytider o.l. som kan påvirke utsalget.

u3 <- stats %>%
  filter(Store_County == "Appleton County",
         Week == 3) %>% 
  filter(str_detect(Description, "REGULAR"))

u3.2 <- u3 %>% 
  select(INV_NUMBER, Description, Price, Sold, Sales, Profit, Margin, Tot_Sls,
       Weather_Bad_Weather_Week, Weather_Bad_Weather_days)

u3.2 %>% 
  select(Description, Sold, Price, Sales) %>% 
  arrange(-Sold) %>% 
  head()

# I denne uka var det fortsatt 'månedens sub' som var den varen det ble solgt
# flest av i kvantum.

u3.2 %>% 
  arrange(-Tot_Sls) %>% 
  select(Description, Sold, Price, Sales, Tot_Sls) %>% 
  head()

u3.2 %>% 
  summarise(sum(Sold))

u3.2 %>% 
  summarise(sum(Profit))

# Da er det ikke noe sjokk at det var 'månedens sub' som utgjorde den største
# andelen av salget også denne uka. Det ble til sammen solgt 831 sandwicher,
# noe som endte med en profitt på 3449.44$.

u3.2 %>%
  select(Description, Tot_Sls) %>% 
  arrange(-Tot_Sls) %>% 
  slice(1:5) %>% 
  mutate(D2 = str_remove_all(Description, "REGULAR ")) %>% 
  ggplot(aes(x=D2, y=Tot_Sls, fill=D2)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Andel av ukentlig Sandwich-salg (i $)",
       subtitle = "Appleton County, uke 3",
       x = "Vare",
       y = "Andel av salg (i $)") +
  guides(fill=guide_legend(title="Vare")) + 
  theme_light()

u3.2 %>% 
  select(Description, Margin) %>% 
  arrange(-Margin) %>% 
  head()

# Det er altså fortsatt månedens sub som gjelder. Veldig liten forskjell på "bestselgerne",
# selv om ukene varierer stort - både værmessig og når på året det er. Denne
# uka var det 'Roasted Chicken' som hadde høyest profittmargin. Resten av 
# 'bestselgerne' er også blant de med høyest profittmargin, noe som er positivt
# for bedriften, som selger mest av det de tjener mest per enhet på. Det eneste
# som har forbedringspotensiale her er å øke profittmarginen på den mestselgende
# sandwichen, som er 'månedens sub'. Dette trenger imidlertid ikke nødvendigvis
# å være nødvendig, ettersom månedens sandwich skifter hver måned, noe som betyr
# at produksjonskostnad, og derav profittmargin også endres månedlig.
# Den største forskjellen ligger ved at det kun ble solgt 831 sandwicher denne
# uka, 376 færre enn i uke 25. Dette ga en profitt på 3449.44$, noe som er 
# 1239$ lavere enn profitten på subs fra uke 25. Hvorfor?

uthev25 <- Oppgave2 %>% 
  filter(Week == 25) %>% 
  select(Week, Date, Total_Profit_Week, Weather_Bad_Weather_days)

uthev3 <- Oppgave2 %>% 
  filter(Week == 3) %>% 
  select(Week, Date, Total_Profit_Week, Weather_Bad_Weather_days)

Oppgave2 %>%
  ggplot(aes(x=Date, y=Total_Profit_Week)) + 
  geom_point(color = "darkgrey", size = 3.5) + 
  geom_line(aes(x=Date, y=Total_Profit_Week),color = "dodgerblue",size = 0.1) + 
  geom_point(data=uthev25, 
             aes(x = Date, y = Total_Profit_Week),
             color='red',
             size=5) +
  geom_point(data=uthev3, 
             aes(x = Date, y = Total_Profit_Week), 
             color='red',
             size=5) +
  labs(title = "Total profitt per uke - Appleton",
       subtitle = "Uke 25 og 3 uthevet - Gjelder alle varer",
       x = "",
       y = "Ukentlig profitt ($)") +
  scale_x_date(date_breaks = "2 weeks") +
  theme(axis.text.x = element_text(angle = -60))

# Dette plotet viser hvordan de to ukene jeg har valgt ligger an i forhold til
# alle andre uker i datasettet basert på total, ukentlig profitt. Som man kan se ligger uke 25
# i det øvre sjiktet av uker med høy profitt, mens uke 3 viser seg å være en av
# de dårligere ukene. Skulle jeg beskrevet utviklingen i dette plotet, ville jeg
# påpekt at butikken opprettholder et relativt høyt ukentlig salg helt til
# sent i Oktober, hvor salget dropper drastisk frem til midten av Desember hvor 
# bedriften opplever en voldsom oppgang av solgte sandwicher igjen. Ved årsskiftet
# faller igjen salget drastisk, og uke 52 er faktisk den uka med lavest profitt
# i hele datasettet. Etter dette har ukentlig profitt en jevnlig oppgang, med 
# unntak av uke 3, som er en av to uker jeg har valgt å se nærmere på.

ukentlig <- Oppgave2 %>%
  select(Date, Week, Weather_Bad_Weather_days) %>% 
  distinct(Week, .keep_all = TRUE)

ukentlig %>% 
  ggplot(aes(x=Date, y=Weather_Bad_Weather_days)) +
  geom_col(aes(y=Weather_Bad_Weather_days), 
           alpha = 1,
           fill = "orange") +
  labs(title = "Antall dager med regn per uke - Appleton",
       subtitle = "",
       y = "Antall dager per uke") +
  scale_x_date(date_breaks = "2 weeks") + 
  scale_y_continuous(breaks = seq(0, 7, by = 1))+
  theme(axis.text.x = element_text(angle = -60))

# Her kan man se hvilke uker som hadde flest dager med dårlig vær. Det er 
# relativt få uker med generelt dårlig vær, helt til vi når slutten av oktober
# hvor antall dager med dårlig vær tar seg, frem til tidlig februar.

profit_weather <- Oppgave2 %>% 
  ggplot(aes(x=Date, y=Total_Profit_Week)) +
  geom_point(color = "darkgrey", size = 3.5) +
  geom_line(aes(x=Date, y=Total_Profit_Week),color = "dodgerblue",size = 0.1) +
  geom_point(data=uthev25, 
             aes(x = Date, y = Total_Profit_Week),
             color='red',
             size=5) +
  geom_point(data=uthev3, 
             aes(x = Date, y = Total_Profit_Week), 
             color='red',
             size=5) +
  geom_col(data = ukentlig,
           aes(y=Weather_Bad_Weather_days*1000, fill = "orange"), 
           alpha = 0.6) +
  labs(title = "Total profitt per uke + Antall regndager per uke",
       subtitle = "Uke 25 og 3 uthevet - Gjelder alle varer",
       x = "",
       y = "Ukentlig profitt ($)") +
  scale_x_date(date_breaks = "2 weeks") +
  scale_y_continuous(name = "Ukentlig profitt ($)",
  sec.axis = sec_axis( trans=~./1000, name="Antall ukentlige regndager", 
                       breaks = seq(0, 7, by = 1)))+
  theme(axis.text.x = element_text(angle = -60),
        legend.position = "none")
profit_weather

# Her kan det se ut til at uker med flere dager med dårlig vær påvirker 
# salgstallene negativt. Da profitten begynte å synke mot slutten av oktober,
# ble det oftere dårlig vær, og da de ukene med dårligst vær hadde passert i 
# tidlig februar, tok også profitten seg opp igjen. For å se om det faktisk
# er en korrelasjon mellom det dårlige været og profitten velger jeg å kjøre
# en regresjonsanalyse.

korrelasjon <- Oppgave2 %>% 
  select(Week, Weather_Bad_Weather_days, Total_Profit_Week) %>% 
  distinct(Week, .keep_all = TRUE)

lm(data = korrelasjon, Weather_Bad_Weather_days ~ Total_Profit_Week)

profit_weather + geom_smooth(method = "lm")

# Den nye blå linjen viser den linjen som har kortest avstand til alle profitt-observasjonene.
# Ettersom fylket opplever flere regndager, synker faktisk profittfunksjonen
# litt. Ut ifra koeffisientene kan man se at total ukentlig profitt marginalt
# når byen opplever flere regndager. Dette er imidlertid veldig marginale 
# verdier, så det er ikke nødvendigvis sånn at dette kan brukes til å forbedre 
# utsalgsstedets drift.

# Oppgave 3

# I denne oppgaven skal jeg velge en måned og se på det totale salget for
# hele kjeden, altså alle utsalgsstedene i datasettet for å lage en langsiktig 
# konsernrapport. Her kan man se på hvilke avdelinger som er mest profitable,
# og hva man kan gjøre med de avdelingene som ikke er så profitable. Jeg ønsker
# å se nærmere på de utsalgsstedene som har høyest profitt, samt høyest profitt
# per innbygger i fylket. Det er disse avdelingene de andre har noe å lære av,
# og å se på hva som skiller konsernets avdelinger kan gjøre de mindre profitable
# avdelingene mer profitable, og dermed øke profitten på lang sikt.
# Jeg velger å se på en måned jeg ikke har berørt enda. 
# Velger April-måned (2012).

Oppgave3 <- stats %>% 
  select(Store_County, Store_Num, Store_Name, Description, Sold, Sales, Price, Profit,
         Unit_Cost, County_Labor_Force, County_Unemployment_Rate, Date, Week, Month, Year,
         Store_Drive_Through) %>%
  group_by(Month) %>%
  mutate(Total_Profit_Month = sum(Profit))

Oppgave3 <- Oppgave3 %>% 
  mutate(YM1 = as.numeric(Month-3),
         YM2 = as.numeric((Month+9)))

Oppgave3$YM1 <- replace(Oppgave3$YM1, which(Oppgave3$YM1 < 1), NA)
Oppgave3$YM2 <- replace(Oppgave3$YM2, which(Oppgave3$YM2 < 9), NA)
Oppgave3$YM2 <- replace(Oppgave3$YM2, which(Oppgave3$YM2 > 12), NA)

Oppgave3 <- Oppgave3 %>% 
  mutate(Month2 = coalesce(YM1, YM2))

# Her har jeg satt uke 14 som den første uka i datasettet, for å sørge for at 
# plotet ikke starter med januar 2013, men heller med april 2012.

Profitt_per_month <- Oppgave3 %>% 
  distinct(Month2, .keep_all = TRUE) %>% 
  ggplot(aes(x=Month2, y=Total_Profit_Month)) +
  geom_point(color = "red", size = 3.5) +
  geom_line(aes(x=Month2, y=Total_Profit_Month),color = "dodgerblue",size = 0.5) +
  labs(title = "Total profitt per maned",
       subtitle = "Gjelder alle varer og utsalgsstedet",
       x = "",
       y = "Manedlig profitt ($)") +
  scale_x_discrete(limits = c("4","5","6","7","8","9","10","11","12","1",
                              "2","3")) +
  theme_light()
Profitt_per_month


Oppgave3 %>% 
  distinct(Month, .keep_all = TRUE) %>%
  select(Month, Total_Profit_Month) %>% 
  arrange(-Total_Profit_Month)

# April viser seg å være måneden med nest høyest profitt, kun slått
# av Juli. Måneden med tredje mest profitt er Desember. Det som umiddelbart
# slår meg er at dette er de tre største 'ferie-månedene' amerikanerne har, med
# sommmerferie i juli, 'spring-break' i april (i 2012) og juleferie i desember.
# Det er fleren faktorer som kan spille inn her; Først og fremst kan det bety
# at man oftere spiser "ute" i ferier. En annen ting kan være at denne bedriften
# har flest "unge" kunder. April og Juli ligger langt over Desember som er nr. 3 når
# det kommer til profitt. Kan dette skyldes at det er da elever/studenter får
# fri? Dette er imidlertid noe jeg ikke får sett nærmere på, ettersom jeg 
# ikke har data fra ulike aldersgrupper. Videre vil jeg se på lønnsomheten
# til de ulike utsalgsstedene.

profitt_utsalgssted <- Oppgave3 %>%
  select(Month, Store_Num, Store_Name, Profit) %>% 
  filter(Month == 4) %>% 
  group_by(Store_Num) %>%
  mutate(Store_Profit_April = sum(Profit)) %>% 
  distinct(Store_Num, .keep_all = TRUE) %>% 
  ggplot(aes(x=Store_Name, y=Store_Profit_April, fill = Store_Name)) +
  geom_col(aes(y=Store_Profit_April), 
           alpha = 1) +
  geom_text(aes(label = Store_Profit_April), vjust=2) +
  labs(title = "Profitt per utsalgssted i April",
       x="",
       y = "Profitt (i $)") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(legend.position = "none")
profitt_utsalgssted

# Her kan vi se profitt per utsalgssted i april, hvor Power City Freestand
# har klart størst profitt. Dette er også den byen i datasettet med nest flest
# innbyggere, jfr. figuren fra oppgave 2. Dette er definitivt en lønnsom by for
# bedriften, ettersom Power City Stripmall også er blant topp
# 3 mest lønnsomme utsalgssteder. Videre kan jeg finne hvert utsalgssted minst
# profitable vare. Jeg ønsker å finne en ordentlig vare, og fjerner derfor 
# ordene 'reward' og 'free' fra description, samt. 0 fra pris-kolonnen
# for å få en ordentlig oversikt over de 'normale' varene.

negativ <- Oppgave3 %>% 
  filter(Month == 4) %>% 
  filter(!str_detect(Description, "REWARD")) %>%
  filter(!str_detect(Description, "FREE")) %>% 
  filter(!str_detect(Price, "0")) %>%
  filter(Price > 0) %>% 
  group_by(Store_Name) %>% 
  arrange(Profit) %>% 
  distinct(Store_Name, .keep_all = TRUE) %>% 
  select(Store_Name, Store_Num, Description, Sold, Price, Unit_Cost, Profit)

negativ %>% 
  mutate(Positiv_profit = Profit >= 0) %>% 
  ggplot(aes(x = Store_Name, y = Profit, fill = Positiv_profit)) +
  geom_col(aes(x= Store_Name, y=Profit), 
           alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1.5) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(limits = c(-8, 1)) +
  labs(title = "Profitt for hvert utsalgssteds minst profitable vare",
       x = "",
       y = "Profitt") +
  theme_light()
  
negativ %>% 
  arrange(Profit) %>% 
  select(Store_Name, Description, Sold, Price, Unit_Cost, Profit)

# Her viser figuren at det er North Town Stripmall som har det enkeltutsalget
# med størst negativ profitt av alle utsalgsstedene. Jeg valgte å lage figuren
# slik at de utsalgsstedene med positiv profitt fikk en blå "stolpe", og de med
# negativ profitt fikk en rød. Det er kun røde stolper, med unntak av 2; 
# Littletown Stripmall og Power City Freestand. Ved å se på tabellen under figuren
# avslører den at det er SS Club Salad som er den enkeltvaren som går mest med underskudd.
# Det er ikke den mest populære varen, men bedriften tapte altså 7.6$ på å selge
# denne salaten i North Town Stripmall. På denne måten kan bedriften forbedre seg
# ved å gjøre endringer på de varene med lavest profitt på hvert utsalgssted.

Profitt_per_innbygger <- Oppgave3 %>% 
  group_by(Store_Num) %>% 
  mutate(Store_Profit = sum(Profit)) %>%
  ungroup() %>% 
  mutate(PLB = Store_Profit/County_Labor_Force) %>% 
  select(Month, Month2, Store_County, Store_Num, Store_Name, Store_Profit,
         County_Labor_Force, PLB, Store_Drive_Through) %>% 
  filter(Month == 4) %>% 
  distinct(Store_Name, .keep_all = TRUE)

profitt_utsalgssted_per_innbygger <- Profitt_per_innbygger %>%
  ggplot(aes(x=Store_Name, y=PLB, fill = Store_Name)) +
  geom_col() +
  labs(title = "Profitt per innbygger for hvert utsalgssted i April",
       x="",
       y = "Profitt per innbygger (i $)") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_text(aes(label = round(PLB, 2), vjust=2)) +
  theme(legend.position = "none")
profitt_utsalgssted_per_innbygger

# Her har jeg laget en figur for å se hvor mye hvert utsalgssted tjener på
# hver innbygger i fylket (regner fylkets arbeidsstyrke som innbyggere).
# På denne måten kan man finne ut hvor befolkningen handler mest hos denne
# bedriften, og kan kanskje planlegge nye utsalgssteder på de travleste stedene?
# Sammenligner profitt per innbygger (per utsalgssted) og profitt per utsalgssted

plot_grid(profitt_utsalgssted,
          profitt_utsalgssted_per_innbygger,
          ncol = 2,
          labels = "AUTO")

stats %>% 
  select(Store_County, Store_Name, Store_Competition_Fastfood, Store_Competition_Otherfood) %>% 
  distinct(Store_Name, .keep_all = TRUE) %>% 
  arrange(Store_Competition_Fastfood)

# Her kan man se at Power City Freestand som vi vet har størst profitt av alle i 
# april, ikke tjener så mye per innbygger som andre. Dette skyldes nok
# at utsalgsstedet er plassert i en stor by med mange innbyggere - og konkurrerende
# restauranter. Allikevel er det kanskje potensiale for forbedring/utvidelse her 
# - noe som kan gjøre profitten fra denne byen enda større enn den allerede er.
# Samtidig tjener avdelingen på North Town Stripmall veldig mye per innbygger 
# i forhold til resten av avdelingene; er det noe man kan lære derfra? Ser man
# på tabellen under figuren kan man se at North Town Stripmall har veldig lav
# konkurranse fra andre fast food-kjeder.

# Oppgave 4
# Å se på hvilke varer bedriften og dens avdelinger opplever lavest profitt
# av å selge, kan gjøre at bedriften kan se nærmere på hva som fører til 
# den lave profitten. Er det for eksempel høye priser på 'råvarene'? Om 
# sandwichen koster mye å produsere på grunn av for eksempel dyrt kjøttpålegg
# kan bedriften se til andre slakterier for å redusere innkjøpsprisen.
# Videre opplever bedriften høyere salgstall, og derav høyere profitt ved
# ferier, særlig 'spring-break' og sommerferien. Derfor kan konsernledelsen
# tjene på å investere i ekstra markedsføring og kampanjer i forhånd av disse.

stats %>% 
  select(Store_County, Store_Name, Store_Drive_Through, Store_Near_School) %>% 
  distinct(Store_Name, .keep_all = TRUE)

Profitt_per_innbygger %>% 
  group_by(Store_Drive_Through) %>% 
  summarise(mean(PLB))

Profitt_vs_competition <- stats %>% 
  select(Store_Name, Profit, Store_Competition_Fastfood, Store_Drive_Through,
         County_Labor_Force) %>% 
  group_by(Store_Name) %>%
  mutate(Store_Profit = sum(Profit),
         Store_Profit_pr = Store_Profit/County_Labor_Force) %>%
  distinct(Store_Name, .keep_all = TRUE)

Profitt_vs_competition %>% 
  ggplot(aes(x=Store_Competition_Fastfood, y=Store_Profit_pr)) +
  geom_point(aes(color=Store_Name, size = Store_Profit,
                 shape = Store_Drive_Through)) + 
  scale_size_continuous(range = c(5, 12)) +
  scale_shape_manual(values = c(20, 18)) +
  guides(size = "none") +
  labs(title = "Profitt per innbygger konkurranse fra Fast food-kjeder",
       x="",
       y="Profitt per innbygger (Labor Force)") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_light()

# Om konsernledelsen ønsker å etablere et nytt utsalg kan de se mot North Town
# Stripmall som er lokalisert i Farm County. Dette er et utsalgssted med høy
# profitt per innbygger i fylket, noe som mest sannsynlig skyldes lav konkurranse
# fra andre fast food-kjeder. Farm County er også et av de mindre fylkene i datasettet,
# noe som kan vitne om at ledelsen bør se til marked som ikke nødvendigvis er så
# store, med lav konkurranse. En annen faktor kan være å velge en county som ikke
# er så alt for stor med tanke på areal. Ser man til Appleton County, det største
# fylket i datasettet, med en av de laveste profittene, vil man helst unngå
# å etablere seg i et slikt marked. Grunnen til at profitten er så lav kan være
# at det er stor spredning blant befolkningen. Utsalgsstedet i Appleton County 
# er i dette datasettet kalt 'Littletown', noe som tilsier at det hører til i en
# liten by i et stort fylke. Velger man heller et mindre sentralisert marked,
# med lav konkurranse fra andre fast food-kjeder, vitner disse dataene om at de
# kan oppleve en større profitt her. Når det er sagt kan ikke ledelsen unngå å
# se til Power City Freestand, som tross alt er utsalgsstedet med størst profitt.
# Den er lokalisert i et fylke med stort innbyggertall og med høy konkurranse,
# hva er det som gjør at denne avdelingen kan tjene stort her da? Ser man nærmere
# på detaljene om lokaliseringen til dette utsalgsstedet, kan man se at 
# de både har drive-through og er lokalisert nærme en skole. Muligheten for at 
# den er lokalisert i nærheten av en motorvei er også stor, mtp. USA's infrastruktur.
# En stor by med mange innbyggere, som har mulighet til å kjøre en rask tur 
# innom drive-through fra jobb mens man henter barna på skolen kan være nok til
# å oppleve stor profitt, selv med høy konkurranse. I tillegg er det lett for 
# skolebarn / studenter å dra innom i lunsjpausen og etter timen for en rask
# matbit. Lake City Stripmall er det utsalgsstedet som opplever nest høyest
# profitt per innbygger. Dette er også den avdelingen som har nest høyest konkurranse, 
# men allikevel velger altså kundene dette konsernet fremfor andre. Igjen er
# dette et utsalgssted med drive-through, og i tillegg er også dette lokalet 
# i nærheten av en skole. Av den grunn kan det være akkurat dette som skiller
# denne kjeden fra konkurrerende kjeder.
# Ser man på den siste figuren som viser oversikten mellom ulike utsalgssteder 
# basert på konkurranse, kan man se at lav konkurranse ikke automatisk betyr 
# høy profitt per innbygger. West Power Stripmall har kun en konkurrent i markedet
# og er fortsatt det utsalgsstedet med lavest profitt per innbygger. Samtidig
# er North Town Stripmall den avdelingen med flest konkurenter, og allikevel
# klarer de å opprettholde en profitt som er høyere enn flere avdelinger
# med lavere konkurranse. I tillegg valgte jeg å ta med en visuell effekt
# som viser om butikken har drive-through eller ikke. Dette viser at 
# avdelinger med drive-through klarer å ha en høyere profitt per innbygger
# enn andre avdelinger uten drive-through, men med like mange konkurrenter (se
# 8 konkurrenter).

# Derfor vil mine tips til konsernledelsen ved planlegging av nye utsalg være å se
# etter lokaler i nærheten av travle motorveier i (eller ved utkanten av) større
# byer, med mulighet for drive-through. I tillegg bør de se etter mindre, 
# sentraliserte marked med lav konkurranse - Slik som Farm County.

