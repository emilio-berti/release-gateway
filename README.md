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

replace-comas data/raw/283_2_FoodWebDataBase_2018_12_10.csv data/interim/no-comas-no-quotes.csv
````
