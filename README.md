# AUTISME-TEST
# (Dette er ikke en autismetest)

Dette er en separat Shiny-applikasjon som viser hvordan fravær av sosial, sensorisk og rutinemessig friksjon kan se ut. Den er ikke diagnostisk, men kan være nyttig som en rolig og strukturert selvrefleksjon.

## Funksjon

Applikasjonen består av tretten hovedutsagn. Brukeren vurderer hvor godt hvert utsagn beskriver dem over tid. Det beregnes deretter en enkel gjennomsnittsskår og en kort tolkning.

Svarene lagres i en SQLite-database, slik at vi kan følge utviklingen over tid.

## Database

Som standard bruker appen `/srv/shiny-server/data/autisme.sqlite` når den delen finnes. Lokalt faller den tilbake til `data/autisme.sqlite`.

```
AUTISME_DB_PATH=/srv/shiny-server/data
AUTISME_DB_NAME=autisme.sqlite
```

På lokal utvikling kan `AUTISME_DB_PATH` settes til `data` hvis du vil bruke en lokal kopi.

Tabellen `responses` opprettes automatisk ved første besvarelse og lagrer `timestamp`, `item_id`, `score` og `language`.

## Språk

Appen støtter foreløpig bokmål og engelsk.

## Kort om prosjektet

Dette er laget som en egen, nøktern side for selvrefleksjon uten å late som om den er en diagnose.
