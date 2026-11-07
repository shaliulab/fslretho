# fslretho

fslretho is a Shiny app to analyse the sleep and circadian rhythm of flies recorded with
[ethoscopes](https://gilestrolab.github.io/ethoscope/) or DAM monitors, built on the
[rethomics](https://rethomics.github.io/) packages. The app has a Help tab that explains how it
works and what every parameter means.

The rethomics packages it uses (with changes made at the Sha Liu lab) are git submodules under
`inst/dependencies/`, pinned to the exact versions the app is tested with.

## Run with Docker

```bash
git clone --recursive -b v3 https://github.com/shaliulab/fslretho
cd fslretho
docker build -t fslretho-v3 .

docker run -d --name fslretho-v3 --restart unless-stopped \
  -p 3839:3838 \
  -v /ethoscope_data:/ethoscope_data \
  -v /DAM_data:/DAM_data \
  -v /fslretho_data_v3:/fslretho_data \
  fslretho-v3
```

The app is then at `http://<server>:3839`.

* `/ethoscope_data` must contain `results/` (the ethoscope database) and `cache/` (writable; the
  snapshot viewer also extracts pictures there).
* `/DAM_data/results` holds the DAM monitor files (optional, for DAM data).
* To load ethoscope data from another computer (remote option in the Load tab), mount `/ethoscope_data/results`
  writable: the .db files that are needed are downloaded there. The remote database is set in `remote_results`
  of `inst/configuration/fslretho.conf` (an http(s) or ftp address serving the result folder and its `index.txt`).
* `/fslretho_data` holds the saved sessions and the usage logs (`sessions/` and `logs/`, writable).
  Use a different folder for each version so their sessions don't mix.
* The container runs as user 1000 by default; build with `--build-arg USER_ID=... --build-arg GROUP_ID=...`
  to match the owner of the data folders.

### Running v3 next to the previous version

Each version is a separate image and container, so they don't interfere: keep the old one
(e.g. container `fslretho`, port 3838) running and start v3 with its own name and port (3839 above).
Both can read the same ethoscope database and cache. When everyone has switched, stop the old
container and, if you like, map v3 to port 3838.

## Run without Docker

```r
# from a clone with submodules (git submodule update --init)
for (pkg in c("behavr", "zeitgebr", "ggetho", "damr", "scopr", "sleepr", "esquisse")) {
  install.packages(file.path("inst/dependencies", pkg), repos = NULL, type = "source")
}
install.packages(".", repos = NULL, type = "source")
fslretho::main()
```

The folders are configured in `/etc/fslretho.conf` and `/etc/scopr.conf` (see `inst/configuration/`).

## Updating a dependency

Move the submodule to the new commit, update the matching `Remotes:` entry in `DESCRIPTION`,
and commit both.
