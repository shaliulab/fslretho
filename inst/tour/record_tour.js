// Record a guided tour of FSLRetho: node tour.js <url> <metadata.csv> <frames dir>
const puppeteer = require('puppeteer-core');
const fs = require('fs');
const [url, metadata, framesDir] = process.argv.slice(2);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const browser = await puppeteer.launch({
    executablePath: '/usr/bin/google-chrome', headless: 'new',
    args: ['--no-sandbox', '--window-size=1600,900'],
    // waiting for the data or a periodogram can take minutes
    protocolTimeout: 1800000,
  });
  const page = await browser.newPage();
  await page.setViewport({ width: 1600, height: 900 });

  // ---- recording: Chrome screencast, every frame with its timestamp
  fs.mkdirSync(framesDir, { recursive: true });
  const frames = [];
  const cdp = await page.target().createCDPSession();
  // time cut from the video (see fastForward)
  let offset = 0;
  let resumed = false;
  cdp.on('Page.screencastFrame', async (f) => {
    const file = `${framesDir}/${String(frames.length).padStart(6, '0')}.jpg`;
    fs.writeFileSync(file, Buffer.from(f.data, 'base64'));
    if (resumed && frames.length > 0) {
      // the video jumps over the wait, with one second of the last frame before it
      offset += f.metadata.timestamp - offset - frames[frames.length - 1].t - 1;
      resumed = false;
    }
    frames.push({ file, t: f.metadata.timestamp - offset });
    try { await cdp.send('Page.screencastFrameAck', { sessionId: f.sessionId }); } catch (e) {}
  });

  // ---- caption bar and visible cursor, re-added after every page load
  const overlay = async () => page.evaluate(() => {
    if (document.getElementById('tour-caption')) return;
    const c = document.createElement('div');
    c.id = 'tour-caption';
    c.style.cssText = 'position:fixed;left:0;right:0;bottom:0;z-index:100000;background:rgba(17,36,70,0.92);color:#fff;' +
      'font:22px/1.4 Helvetica,Arial,sans-serif;padding:14px 30px;text-align:center;pointer-events:none;';
    document.body.appendChild(c);
    const m = document.createElement('div');
    m.id = 'tour-mouse';
    m.style.cssText = 'position:fixed;width:22px;height:22px;margin:-11px 0 0 -11px;border-radius:50%;z-index:100001;' +
      'background:rgba(255,80,0,0.55);border:2px solid #fff;pointer-events:none;transition:transform .15s;left:-50px;top:-50px;';
    document.body.appendChild(m);
    document.addEventListener('mousemove', (e) => { m.style.left = e.clientX + 'px'; m.style.top = e.clientY + 'px'; }, true);
    document.addEventListener('mousedown', () => { m.style.transform = 'scale(0.6)'; }, true);
    document.addEventListener('mouseup', () => { m.style.transform = 'scale(1)'; }, true);
  });
  const caption = async (text, ms = 3500) => {
    await overlay();
    await page.evaluate((t) => { document.getElementById('tour-caption').innerHTML = t; }, text);
    await sleep(ms);
  };
  // move the cursor smoothly to an element and click it
  let mouse = { x: 800, y: 450 };
  const moveTo = async (x, y) => { await page.mouse.move(x, y, { steps: 25 }); mouse = { x, y }; await sleep(250); };
  const click = async (selector, pause = 1200) => {
    const el = await page.waitForSelector(selector, { visible: true, timeout: 120000 });
    await el.evaluate((e) => e.scrollIntoView({ block: 'center' }));
    await sleep(400);
    const b = await el.boundingBox();
    await moveTo(b.x + b.width / 2, b.y + b.height / 2);
    await page.mouse.down(); await sleep(80); await page.mouse.up();
    await sleep(pause);
  };
  const scrollTo = async (selector, block = 'center') => {
    await page.evaluate((s, b) => { const e = document.querySelector(s); if (e) e.scrollIntoView({ behavior: 'smooth', block: b }); }, selector, block);
    await sleep(1200);
  };
  const closeModal = async () => {
    await page.evaluate(() => { const c = document.querySelector('.modal.in .modal-footer button, .modal.show .modal-footer button'); if (c) c.click(); });
    await sleep(800);
  };
  // background tasks show a progress bar, or a notification ending in "..." (see in_background)
  const waitIdle = async (timeout = 600000) => {
    await sleep(1500);
    await page.waitForFunction(() => ![...document.querySelectorAll('.shiny-notification')].some((n) =>
      n.querySelector('.progress') || /\.\.\.$/.test((n.querySelector('.shiny-notification-content-text') || n).innerText.trim())),
    { timeout, polling: 500 });
  };
  // long waits (loading, computing) are cut from the video
  const screencast = () => cdp.send('Page.startScreencast', { format: 'jpeg', quality: 85, everyNthFrame: 1 });
  const fastForward = async (wait) => {
    await sleep(1500);
    await cdp.send('Page.stopScreencast');
    await wait();
    resumed = true;
    await screencast();
    await sleep(300);
  };
  const closeNotes = async () => {
    await page.evaluate(() => document.querySelectorAll('.shiny-notification-close').forEach((b) => b.click()));
    await sleep(500);
  };

  await page.goto(url, { waitUntil: 'networkidle2' });
  await sleep(2000);
  await screencast();

  // 1. Welcome
  await caption('<b>FSLRetho</b>: sleep and circadian analysis of flies recorded with ethoscopes or DAM monitors', 4500);
  await caption('The Help tab (left) explains every step and parameter. Here is a quick tour.', 3500);

  // 2. Load
  await click('a[data-value="load"]');
  await caption('<b>1. Load</b> &mdash; upload a metadata .csv: one row per fly (machine_name, date, region_id, reference_hour, genotype...)', 4500);
  const input = await page.$('#loadData-metadata');
  await input.uploadFile(metadata);
  await sleep(1500);
  await caption('Data can be on this computer, or downloaded from the node when needed (Remote).', 3500);
  await click('#loadData-link', 300);
  await caption('<b>Link</b> checks the metadata and finds the recording of every fly (downloading it, if Remote).', 1000);
  await page.waitForFunction(() => document.querySelector('table.fsl-flies tbody tr'), { timeout: 600000 });
  await sleep(1500);
  await closeModal();
  await scrollTo('#loadData-flies_ui', 'start');
  await caption('Every fly is listed: when its data starts and ends, how many frames were recorded, and the frame rate. Flies that cannot be loaded say why.', 5500);
  await click('table.fsl-flies tbody tr:nth-child(2) input.fsl-fly-select', 600);
  await caption('Untick the flies you don\'t want to load (or use Select all / Deselect all)...', 3500);
  await click('#loadData-load', 300);
  await caption('...and press <b>Load selected flies</b>: the data is loaded and sleep is scored (progress at the bottom right).', 1000);
  await fastForward(() => page.waitForFunction(() => document.querySelector('table.fsl-flies .label-success'), { timeout: 600000 }));
  await sleep(1500);
  await closeModal();
  await closeNotes();
  if (await page.$('table.fsl-flies tr.fsl-fly-error')) {
    await scrollTo('table.fsl-flies tr.fsl-fly-error');
    await caption('Flies that could not be scored (e.g. too little data) say so in the table.', 4000);
  } else {
    await caption('Loaded flies are marked in the table.', 3000);
  }
  await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'smooth' }));
  await sleep(800);

  // 3. Scoring parameters
  await click('#scoringInput > a.dropdown-toggle', 800);
  await caption('<b>2. Scoring parameters</b> &mdash; when a fly counts as moving, and as asleep (the 5 minute rule)...', 4500);
  await click('#scoring-differentiate_SD', 800);
  await caption('...optionally different during sleep deprivation. Use 0 s of masking for optogenetic (light) stimulation.', 5000);
  await click('#scoring-differentiate_SD', 400);
  await page.mouse.click(1400, 600); await sleep(800);

  // 4. Sleep analysis
  await click('a[data-value="sleep"]', 500);
  await caption('New scoring parameters score the loaded flies again (progress at the bottom right).', 1000);
  await fastForward(() => waitIdle());
  await closeNotes();
  await page.waitForFunction(() => document.querySelector('#sleepPlot-esquisse-plooooooot-plot img'), { timeout: 120000 });
  await sleep(1500);
  await caption('<b>3. Sleep analysis</b> &mdash; fraction of time asleep in 30 min bins. Light and dark phases are shaded.', 5000);
  await caption('Drag any variable (genotype, p_wake, sd_on...) to the X, Y, colour or facet boxes to change the plot.', 4500);
  await click('.esquisse-controls-filters button, [id$="controls-filters"]', 1500);
  await caption('The Data panel filters the data: by fly, by day (ZT 0-24, 24-48...) or by phase (L / D). "Clear all filters" resets them.', 5500);
  await page.mouse.click(1500, 120); await sleep(800);
  await scrollTo('#sleepSummary-plot');
  const bars = await page.$('#sleepSummary-plot img');
  if (bars) {
    const b = await bars.boundingBox();
    await moveTo(b.x + b.width * 0.3, b.y + b.height * 0.75);
    await sleep(1200);
  }
  await caption('One bar per fly, sorted by date, machine and ROI. Hover a bar to see which fly it is.', 4500);
  await caption('Choose the total over the period shown, the average per day, or the light / dark phase only.', 4000);
  await scrollTo('#sleepSummary-groups');
  await caption('And compare groups: one point per fly, grouped by any metadata column (here, genotype).', 5000);

  // 5. Period analysis
  await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'smooth' }));
  await click('a[data-value="period"]', 800);
  await click('#periodAnalysis-periodogram-button', 500);
  await caption('<b>4. Period analysis</b> &mdash; periodograms and spectrograms of the sleep data shown in the Sleep tab.', 1000);
  // the periodogram is computed in the background, then plotted
  await fastForward(() => page.waitForFunction(() => document.querySelector('#periodAnalysis-periodogram-esquisse-esquisse-plooooooot-plot img'), { timeout: 1200000 }));
  await closeNotes();
  await sleep(2000);
  await page.evaluate(() => window.scrollBy({ top: 350, behavior: 'smooth' })); await sleep(1200);
  await caption('Peaks above zero are significant periods.', 4000);

  // 6. Raw data and snapshots
  await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'smooth' }));
  await click('a[data-value="raw"]', 1500);
  await caption('<b>5. Raw data</b> &mdash; the position of one fly in its tube, frame by frame, next to its sleep.', 3500);
  await page.$eval('#rawPlots-sd_only', (e) => { if (e.checked) e.click(); });
  await click('#rawPlots-button', 500);
  await fastForward(() => page.waitForFunction(() => document.querySelector('#rawPlots-plot_x img'), { timeout: 600000 }));
  await sleep(2000);
  const raw = await page.$('#rawPlots-plot_x img');
  const rb = await raw.boundingBox();
  await moveTo(rb.x + rb.width * 0.62, rb.y + rb.height * 0.5);
  await caption('Click on any plot to see the picture the ethoscope took closest to that moment...', 3000);
  await page.mouse.down(); await sleep(80); await page.mouse.up();
  await fastForward(() => page.waitForFunction(() => document.querySelector('#snapshotViewer-snapshot svg'), { timeout: 600000 }));
  await sleep(1500);
  await caption('<b>6. Snapshot viewer</b> &mdash; tubes in white; the sleep-deprived ones turn blue while the SD is running.', 5500);
  await click('input[name="snapshotViewer-show_rois"][value="sd"]', 2500);
  await caption('Show only the flies under SD (or only the others) for figures.', 3500);
  await click('input[name="snapshotViewer-show_rois"][value="all"]', 800);
  await caption('Select snapshots (all, SD only, by time), browse them, or download them as a video.', 4000);

  // 7. Downloads and help
  await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'smooth' }));
  await click('#downloadCenter > a.dropdown-toggle', 800);
  await caption('<b>7. Download</b> any table as .csv, the raw .db files, or a reproducible R script.', 4500);
  await page.mouse.click(1400, 600); await sleep(600);
  await click('a[data-value="help"]', 1000);
  await caption('Everything is explained in the <b>Help</b> tab. Happy analysing!', 4500);

  await cdp.send('Page.stopScreencast');
  await sleep(500);
  fs.writeFileSync(`${framesDir}/frames.json`, JSON.stringify(frames));
  console.log('frames:', frames.length, 'seconds:', (frames[frames.length - 1].t - frames[0].t).toFixed(1));
  await browser.close();
})();
