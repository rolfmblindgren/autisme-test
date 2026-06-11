# AUTISME-TEST
# (Dette er ikke en autismetest)

Dette er en separat Shiny-applikasjon som viser hvordan fravær av sosial, sensorisk og rutinemessig friksjon kan se ut. Den er ikke diagnostisk, men kan være nyttig som en rolig og strukturert selvrefleksjon.

## Funksjon

Applikasjonen består av ti hovedutsagn og tre foreløpige spørsmål som ikke inngår i hovedskåren ennå. Brukeren vurderer hvor godt hvert utsagn beskriver dem over tid. Det beregnes deretter en enkel gjennomsnittsskår, samt en kort tolkning.

Svarene lagres i en SQLite-database, slik at vi kan følge utviklingen over tid og senere lage bedre normer.

## Database

Som standard bruker appen `data/autisme.sqlite`.

```
AUTISME_DB_PATH=data
AUTISME_DB_NAME=autisme.sqlite
```

På server kan `AUTISME_DB_PATH` settes til en annen mappe, for eksempel `/srv/shiny-server/data`.

Tabellene `responses` og `experimental_responses` opprettes automatisk ved første besvarelse. Begge lagrer `timestamp`, `item_id`, `score` og `language`.

## Språk

Appen støtter foreløpig bokmål og engelsk.

## Kort om prosjektet

Dette er laget som en egen, nøktern side for selvrefleksjon uten å late som om den er en diagnose.
