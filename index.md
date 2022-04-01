---
title: Release new version of GATEWAy
date: April 2022
---


This repo contains the code to produce the new version (2.0) of the GlobAL daTabasE of traits and food Web Architecture (GATEWAy) database. The original data processed here can be downloaded from: https://idata.idiv.de/ddm/Data/ShowData/283?version=3.

## Download database

You can do manually, but in bash the snippet below downloads the zipped folder into `data/raw` and extracts it there.

```bash
mkdir data
mkdir data/raw
curl -o data/raw/gateway.zip https://idata.idiv.de/ddm/Data/DownloadZip/283?version=756
unzip -d data/raw data/raw/gateway.zip
```

## Remove quotation marks

I replace `,` that happears betwenn quotations with `;`. I use _awk_, because it's good:

```bash
# replace ',' in with ';' when between two double quotes (")
# also removes quotation marks (")
replace-comas() {
	awk -F'"' -v OFS='' '{ for (i=2; i<=NF; i+=2) gsub(",", ";", $i) } 1' $1 | tr -d '"' > $2
}

# unit test
replace-comas tests/replace-quoted-comas.csv tests/replaced-quoted-comas.csv
# install colordiff or just use diff - I don't have good eyes, so I need colors.
colordiff -y tests/replace-quoted-comas.csv tests/replaced-quoted-comas.csv

mkdir data/interim
mkdir data/final

replace-comas data/raw/283_2_FoodWebDataBase_2018_12_10.csv data/interim/no-comas-no-quotes.csv
```

Let's check if `read_csv` returns some problems and what has changed:

```r
library(readr)
library(dplyr)

old <- read_csv("data/raw/283_2_FoodWebDataBase_2018_12_10.csv", show_col_types = FALSE)
new <- read_csv("data/interim/no-comas-no-quotes.csv", show_col_types = FALSE)
comp <- data.frame(old = unique(old$link.citation), new = unique(new$link.citation))
comp[comp[, "old"] != comp[, "new"], ] #example of changes
# get all changes
old.sub <- old[, sapply(names(old), \(x) grepl(";", old[, x]))]
new.sub <- new[, sapply(names(new), \(x) grepl(";", new[, x]))]
old.sub <- distinct_all(old.sub)
new.sub <- distinct_all(new.sub)

# difference in common columns ------------
common.cols <- intersect(names(new.sub), names(old.sub))
diff.common.cols <- list()
for (x in common.cols) {
	diff.common.cols[[x]] <- data.frame(
		old = setdiff(unique(old.sub[, x]), unique(new.sub[, x]))[[1]],
		new = setdiff(unique(new.sub[, x]), unique(old.sub[, x]))[[1]]
	)
}

ans <- data.frame(old = NA, new = NA, field = NA)
for (x in names(diff.common.cols)) {
	add <- diff.common.cols[[x]]
	add <- mutate(add, field = x)
	ans <- bind_rows(ans, add)
}
ans <- ans[-1, ]
write_csv(ans, "data/interim/difference-common-cols.csv")

# changed columns ----------
changed.cols <- setdiff(names(new.sub), names(old.sub)) #empty the other way
ans <- unique(unlist(distinct_all(new.sub[, changed.cols])))
ans <- ans[grepl(";", ans)]
ans <- data.frame(changed = ans)
write_csv(ans, "data/interim/changed-cols.csv")

# I didn't find any 'bad' changes. i.e. all nominal.
```

## Harmonize taxonomy

I want to create a taxonomic backbone to reassign synonyms and unaccepted names to a common name, possibly accepted by experts. I use _rgbif_ and follow the GBIF taxonomic backbone.

```r
library(readr)
library(foreach)
library(doParallel)
library(rgbif)

gw <- read_csv("data/interim/no-comas-no-quotes.csv", show_col_types = FALSE)
sp <- union(gw$res.taxonomy, gw$con.taxonomy) # ~ 5,000 names

# regex remove non-taxonomic terms, e.g. 'sp', 'phyto', etc.
sp <- gsub("^Order ", "", sp)
sp <- gsub("^Family ", "", sp)
sp <- gsub(" [(][^)]*[)]", "", sp) #remove stuff between parantheses '()'
sp <- gsub(" [{][^)]*[}]", "", sp) #remove stuff between parantheses '{}'
sp <- gsub(" larvae$| adult$", "", sp) #remove life stages from names
sp <- gsub(" sp$", "", sp) #this and next remove stuff as 'sp', 'sp.', etc.
sp <- gsub(" spp$", "", sp)
sp <- gsub(" sp.$", "", sp)
sp <- gsub(" spp.$", "", sp)
sp <- gsub(" spec.$", "", sp)
sp <- gsub(" indet.$", "", sp)
sp <- gsub(" phyto$", "", sp)
sp <- gsub(" pred$", "", sp)
sp <- gsub(" sp. [0-9]", "", sp)
sp <- gsub(" sp [0-9]", "", sp)
sp <- gsub(" Type ", " ", sp)
sp <- gsub(" I$| II$| III$", "", sp)
sp <- gsub(" +", " ", sp) #remove multiple spaces

# some of these stuff is still present (e.g. 'Taxon indet. sp. A') - I rely on GBIF to catch these.

# Search:
# 1 - first search using name_backbone()
# 2 - if status is ACCEPTED, keep it
# 3 - otherwise, check if a Key exist in GBIF
# 4.n - if it doesn't, keep the first search, regardless of status
# 4.y - second search using usage_key()
# 5 - is status is ACCEPTED, keep it
# 6 - otherwise discard it and return NA
registerDoParallel(4)
backbone <- foreach(x = sp, .combine = "rbind") %dopar%
  {
    first_search <- name_backbone(x)
    if (!"status" %in% names(first_search)) {
      ans <- tibble(Species = x,
                    GBIF = NA,
                    Class = NA,
                    Order = NA)
      return(ans)
    }
    if (first_search$status == "ACCEPTED" | !("speciesKey" %in% names(name_backbone(x)))) {
      ans <- tibble(Species = x,
                    GBIF = first_search$canonicalName,
                    Class = ifelse("class" %in% names(first_search), first_search$class, NA),
                    Order = ifelse("order" %in% names(first_search), first_search$order, NA))
      
    } else {
      key <- name_backbone(x)$speciesKey
      second_search <- name_usage(key)$data[1, ]
      if (second_search$taxonomicStatus == "ACCEPTED") {
        ans <- tibble(Species = x,
                      GBIF = second_search$canonicalName,
                      Class = ifelse("class" %in% names(second_search), second_search$class, NA),
                      Order = ifelse("order" %in% names(second_search), second_search$order, NA))
      } else {
        ans <- tibble(Species = x,
                      GBIF = NA,
                      Class = NA,
                      Order = NA)
      }
    }
    return(ans)
  }
stopImplicitCluster()

write_csv(backbone, "data/final/backbone.csv")
```

For the names that are not found in GBIF (_NA_), you need to reassign them to the default raw value. I prefer to do this on the fly, e.g.:

```r
backbone <- read_csv("data/final/backbone.csv")

n.harmonized <- backbone %>% pull(GBIF) %>% unique() %>% length()
n.raw <- backbone %>% pull(Species) %>% unique() %>% length()

backbone <- backbone %>% mutate(GBIF = modify2(Species, GBIF, \(x, y) return (ifelse(is.na(y), x, y))))
n.total <- backbone %>% pull(GBIF) %>% unique() %>% length()

n.harmonized / n.raw # = 0.887
n.total / n.raw # = 0.966

length(setdiff(backbone$GBIF, backbone$Species)) # = 715
length(setdiff(backbone$Species, backbone$GBIF)) # = 880

backbone %>% filter(Species != GBIF)
```

```
# A tibble: 903 × 4
   Species                 GBIF                   Class        Order      
   <chr>                   <chr>                  <chr>        <chr>      
 1 Stilicus rufipes        Rugilus rufipes        Insecta      Coleoptera 
 2 Trogophloeus corticinus Carpelimus corticinus  Insecta      Coleoptera 
 3 Aphtona lutescens       Aphthona lutescens     Insecta      Coleoptera 
 4 Gabrius fermoralis      Gabrius                Insecta      Coleoptera 
 5 Diplocampa assimile     Bembidion assimile     Insecta      Coleoptera 
 6 Tetartopeus terminatum  Tetartopeus terminatus Insecta      Coleoptera 
 7 Pirata latitans         Piratula latitans      Arachnida    Araneae    
 8 Cantharis fulvicollis   Cantharis              Insecta      Coleoptera 
 9 Apis mellifera L.       Apis mellifera         Insecta      Hymenoptera
10 Trachelipus rathkei     Trachelipus rathkii    Malacostraca Isopoda    
# … with 893 more rows
```

I also write the backbone to a PG database. First, create the database and add the postgis extension:

```bash
createdb -U postgres release-gateway #create the db first
psql -d release-gateway -U postgres -c "CREATE EXTENSION postgis"; #create POSTGIS extension
```

Now, create the _backbone_ table and populate it:

```python
import psycopg2 as psy

conn = psy.connect(database = "release-gateway", user = "postgres")
curs = conn.cursor()

curs.execute(
	'''
	CREATE TABLE backbone (
		species varchar(255),
		gbif varchar(255),
		taxclass varchar(255),
		taxorder varchar(255)
	);
	''')

conn.commit()
```

(NOTE: if you get a _SyntaxError: multiple statements found while compiling a single statement_, try another interface to python)

```python
import pandas as pd

backbone = pd.read_csv('data/final/backbone.csv')

for i in backbone.index:
	sp = backbone['Species'][i].replace("'", "") #remove quotations from names - nightmare
	# this handles missing values (nan)
	gbif = backbone['GBIF'][i] if backbone['GBIF'][i] == backbone['GBIF'][i] else 'NA'
	order = backbone['Order'][i] if backbone['Order'][i] == backbone['Order'][i] else 'NA'
	cla = backbone['Class'][i] if backbone['Class'][i] == backbone['Class'][i] else 'NA'
	# insert into table
	curs.execute('''
		INSERT INTO backbone (species, gbif, taxclass, taxorder)
		VALUES ('{}', '{}', '{}', '{}');
		'''.format(sp, gbif, order, cla)
	)

conn.commit()
```

Check number of rows match:

```python
curs.execute('SELECT relname, n_live_tup FROM pg_stat_user_tables;')
curs.fetchall()
backbone['Species'].size
```

## Assign missing movement types

```r
library(readr)
library(dplyr)
library(magrittr)
library(tidyr)

gw <- read_csv("data/interim/no-comas-no-quotes.csv", show_col_types = FALSE)

gw %>% 
	select(contains("movement")) %>% 
	group_by(res.movement.type, con.movement.type) %>% 
	tally() %>% 
	pivot_wider(names_from = "con.movement.type", values_from = "n")
```

There are many missing values, mostly _res.movement.type_ equal to _NA_.