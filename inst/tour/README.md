# Tour video

`inst/www/static/fslretho3-tour.mp4` (shown on the Welcome page) is recorded with `record_tour.js`,
which drives the app in headless Chrome (puppeteer-core) and records it with Chrome's screencast:

```bash
npm install puppeteer-core@21
node record_tour.js http://localhost:3838/ tour_metadata.csv frames/
# frames/frames.json has the timestamp of every frame: turn it into a concat list, then
ffmpeg -f concat -safe 0 -i frames/list.txt -vf "fps=25,format=yuv420p" -c:v libx264 -crf 22 -movflags +faststart tour.mp4
```

The metadata used is the rows of 2026-11-04 (ETHOSCOPE_006, 031 and 060; reference_hour 12) of the lab metadata
`/flyhostel_data/metadata/ethoscope&flyhostel_metadata4 - sleep-lines2.csv`, with the columns machine_name, date,
region_id, reference_hour, genotype and sex.
