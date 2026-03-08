//2000/2001 p75
 =====================================================
// =====================================================
// Savar (Dhaka) — SUHI baseline Apr–Sep 2000–2001
// Landsat 7 Collection 2 Level 2
// UPDATED ROADMAP IMPLEMENTATION:
// - 2-year block (2000–2001), Apr–Sep
// - Scene QC: keep scenes with >=50% valid LST pixels over Savar
// - Optional flood filter: drop scenes with >MAX_WATER_FRAC water coverage
// - Thermal composite: p75 LST (°C) used for ALL LST/SUHI metrics
// - Indices/masks from MEDIAN spectral composites (MNDWI = max)
// - Option A JRC Occurrence constraint on Urban mask (unmask fixed)
// =====================================================


// -------------------------
// 0) Study area + globals
// -------------------------
var savar = ee.FeatureCollection("users/hmibrahim/Savar_shapefile");
var savarGeom = savar.geometry();

Map.centerObject(savarGeom, 10);
Map.addLayer(savar.style({color: 'red', fillColor: '00000000', width: 2}), {}, 'Savar Boundary');

var SCALE = 30;
var TS = 4;
var FOLDER = 'GEE_Exports';


// -------------------------
// 1) Period settings (2-year block)
// -------------------------
var PERIOD_NAME = '2000_2001_AprSep';
var START_DATE  = '2000-01-01';
var END_DATE    = '2002-01-01'; 
var MONTH_START = 4;
var MONTH_END   = 7;            

var SCENE_VALID_FRAC_MIN = 0.50;


// -------------------------
// 2) Thresholds used in multiple places (must be defined early)
// -------------------------

// Vegetation / urban thresholds
var NDVI_VEG_THR = 0.20;
var NDVI_URB_MAX = 0.25;
var NDBI_URB_THR = -0.05;

// Water logic thresholds (CONSERVATIVE) — used by BOTH flood filter + final masks
var MNDWI_WATER_THR = 0.00;
var NDVI_WATER_MAX  = 0.25;
var NDBI_WATER_MAX  = 0.00;

// Flood filter threshold (scene exclusion)
var MAX_WATER_FRAC = 0.30; // if too strict, use 0.30


// -------------------------
// 3) QA masking (cloud/shadow/saturation)
// -------------------------
function maskL7C2(img) {
  var qa = img.select('QA_PIXEL');
  var rad = img.select('QA_RADSAT');

  // Bits: 0 fill, 1 dilated cloud, 3 cloud, 4 cloud shadow
  var mask = qa.bitwiseAnd(1 << 0).eq(0)
    .and(qa.bitwiseAnd(1 << 1).eq(0))
    .and(qa.bitwiseAnd(1 << 3).eq(0))
    .and(qa.bitwiseAnd(1 << 4).eq(0))
    .and(rad.eq(0));

  return img.updateMask(mask);
}


// -------------------------
// 4) LST (ST_B6) to Celsius
// -------------------------
function addLST_C(img) {
  var lstC = img.select('ST_B6')
    .multiply(0.00341802)
    .add(149.0)
    .subtract(273.15)
    .rename('LST_C');

  return img.addBands(lstC).copyProperties(img, ['system:time_start']);
}


// -------------------------
// 5) Scene QC: valid LST fraction over Savar
// -------------------------
function addValidFrac(img) {
  var valid = img.select('LST_C').mask();
  var frac = ee.Number(
    valid.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: savarGeom,
      scale: SCALE,
      maxPixels: 1e13,
      tileScale: TS
    }).get('LST_C')
  );
  return img.set('validFrac', frac);
}


// -------------------------
// 6) Scene-level flood filter helper (L7)
// -------------------------
function addWaterFracPerScene_L7(img) {
  var ndvi  = img.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
  var ndbi  = img.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
  var mndwi = img.normalizedDifference(['SR_B2', 'SR_B5']).rename('MNDWI');

  var waterScene = mndwi.gt(MNDWI_WATER_THR)
    .and(ndvi.lt(NDVI_WATER_MAX))
    .and(ndbi.lt(NDBI_WATER_MAX));

  var waterFrac = ee.Number(
    waterScene.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: savarGeom,
      scale: SCALE,
      maxPixels: 1e13,
      tileScale: TS
    }).values().get(0)
  );

  return img.set('waterFrac', waterFrac);
}


// -------------------------
// 7) Build collection (Apr–Sep, 2-year block), apply QC + flood filter
// -------------------------
var l7raw = ee.ImageCollection('LANDSAT/LE07/C02/T1_L2')
  .filterBounds(savarGeom)
  .filterDate(START_DATE, END_DATE)
  .filter(ee.Filter.calendarRange(MONTH_START, MONTH_END, 'month'));

var l7 = l7raw
  .map(maskL7C2)
  .map(addLST_C)
  .map(addValidFrac)
  .filter(ee.Filter.gte('validFrac', SCENE_VALID_FRAC_MIN));

print('L7 scenes (raw):', l7raw.size());
print('L7 scenes retained (QC >= ' + SCENE_VALID_FRAC_MIN + ' validFrac):', l7.size());
print('Retained validFrac list:', l7.aggregate_array('validFrac'));

// Apply flood filter BEFORE compositing
l7 = l7.map(addWaterFracPerScene_L7)
       .filter(ee.Filter.lte('waterFrac', MAX_WATER_FRAC));

print('Scenes retained after flood filter:', l7.size());
print('Retained waterFrac list:', l7.aggregate_array('waterFrac'));


// -------------------------
// 8) Thermal composite: p75 LST (°C) — PRIMARY LST surface
// -------------------------
var lstCol = l7.select('LST_C');

var LST_p75 = lstCol.reduce(ee.Reducer.percentile([75]))
  .rename('LST_p75_C')
  .clip(savarGeom);

// Display-only plausibility mask
var LST_p75_disp = LST_p75.updateMask(LST_p75.gt(15)).updateMask(LST_p75.lt(55));
Map.addLayer(LST_p75_disp, {min: 20, max: 45}, 'LST p75 (°C) ' + PERIOD_NAME);
print(ui.Chart.image.histogram(LST_p75, savarGeom, 30).setOptions({title: 'Histogram LST p75 (°C) ' + PERIOD_NAME}));


// -------------------------
// 9) Indices from spectral composites (median; MNDWI max)
// -------------------------
function addIndicesL7(img) {
  var ndvi  = img.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
  var ndbi  = img.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDBI');
  var ndwi  = img.normalizedDifference(['SR_B2', 'SR_B4']).rename('NDWI');
  var mndwi = img.normalizedDifference(['SR_B2', 'SR_B5']).rename('MNDWI');
  return img.addBands([ndvi, ndbi, ndwi, mndwi]).copyProperties(img, ['system:time_start']);
}

var srCol = l7.map(addIndicesL7);

var NDVI_med  = srCol.select('NDVI').median().clip(savarGeom);
var NDBI_med  = srCol.select('NDBI').median().clip(savarGeom);
var NDWI_med  = srCol.select('NDWI').median().clip(savarGeom);
var MNDWI_max = srCol.select('MNDWI').max().clip(savarGeom);

Map.addLayer(NDVI_med,  {min: 0.0,  max: 0.8}, 'NDVI (median)');
Map.addLayer(NDBI_med,  {min: -0.3, max: 0.4}, 'NDBI (median)');
Map.addLayer(MNDWI_max, {min: -0.4, max: 0.4}, 'MNDWI (max)');


// -------------------------
// 10) Masks (Water / Veg / Urban) + Option A JRC Occurrence constraint
// -------------------------

// JRC ever-water footprint
var gsw = ee.Image('JRC/GSW1_4/GlobalSurfaceWater');
var occ = gsw.select('occurrence').clip(savarGeom);

var EVER_WATER_THR = 20;
var everWater = occ.gte(EVER_WATER_THR).unmask(0).rename('everWater');
Map.addLayer(everWater.selfMask(), {palette:['00ffff']}, 'JRC Ever-water (occ >= ' + EVER_WATER_THR + '%)');

// Water mask (current period, spectral)
var water = MNDWI_max.gt(MNDWI_WATER_THR)
  .and(NDVI_med.lt(NDVI_WATER_MAX))
  .and(NDBI_med.lt(NDBI_WATER_MAX))
  .rename('water');

var land = water.not().rename('land');

var veg = NDVI_med.gt(NDVI_VEG_THR)
  .and(land)
  .rename('veg');

var urban = NDBI_med.gt(NDBI_URB_THR)
  .and(NDVI_med.lt(NDVI_URB_MAX))
  .and(land)
  .and(everWater.not())
  .rename('urban');

Map.addLayer(water.selfMask(), {palette: ['0000ff']}, 'Water mask');
Map.addLayer(veg.selfMask(),   {palette: ['00ff00']}, 'Vegetation mask');
Map.addLayer(urban.selfMask(), {palette: ['ff0000']}, 'Urban mask');


// -------------------------
// 11) SUHI_p75 map + stats (urban pixels only)
// -------------------------
var ruralMean_p75 = ee.Number(
  LST_p75.updateMask(veg).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: savarGeom,
    scale: SCALE,
    maxPixels: 1e13,
    tileScale: TS
  }).get('LST_p75_C')
);

var urbanMean_p75 = ee.Number(
  LST_p75.updateMask(urban).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: savarGeom,
    scale: SCALE,
    maxPixels: 1e13,
    tileScale: TS
  }).get('LST_p75_C')
);

print('Rural veg mean LST p75 (°C):', ruralMean_p75);
print('Urban mean LST p75 (°C):', urbanMean_p75);

var SUHI_p75_urban = LST_p75.updateMask(urban)
  .subtract(ruralMean_p75)
  .rename('SUHI_p75_C');

Map.addLayer(SUHI_p75_urban, {min: -2, max: 10}, 'SUHI p75 (°C)');

var suhiStats = SUHI_p75_urban.reduceRegion({
  reducer: ee.Reducer.mean()
    .combine(ee.Reducer.minMax(), '', true)
    .combine(ee.Reducer.stdDev(), '', true),
  geometry: savarGeom,
  scale: SCALE,
  maxPixels: 1e13,
  tileScale: TS
});

var suhiPct = SUHI_p75_urban.reduceRegion({
  reducer: ee.Reducer.percentile([90, 95]),
  geometry: savarGeom,
  scale: SCALE,
  maxPixels: 1e13,
  tileScale: TS
});

print('SUHI stats:', suhiStats);
print('SUHI percentiles:', suhiPct);

var SUHI_mean_overUrban = ee.Number(suhiStats.get('SUHI_p75_C_mean'));
var SUHI_min = ee.Number(suhiStats.get('SUHI_p75_C_min'));
var SUHI_max = ee.Number(suhiStats.get('SUHI_p75_C_max'));
var SUHI_sd  = ee.Number(suhiStats.get('SUHI_p75_C_stdDev'));
var SUHI_p90 = ee.Number(suhiPct.get('SUHI_p75_C_p90'));
var SUHI_p95 = ee.Number(suhiPct.get('SUHI_p75_C_p95'));


// -------------------------
// 12) Hotspots + density
// -------------------------
var hotspot = SUHI_p75_urban.gte(SUHI_p90).selfMask().toByte().rename('hotspot_p90');
Map.addLayer(hotspot, {palette: ['red']}, 'Hotspots (SUHI >= p90)');

var gridScale = 500;
var proj30 = ee.Projection('EPSG:32646').atScale(30);

var hotspot30 = hotspot.unmask(0).toFloat().setDefaultProjection(proj30);

var hotspotDensity = hotspot30
  .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 4096})
  .reproject({crs: 'EPSG:32646', scale: gridScale})
  .rename('hotspot_density');

Map.addLayer(hotspotDensity, {min: 0, max: 1}, 'Hotspot density (' + gridScale + 'm)');


// -------------------------
// 13) Area table (km²)
// -------------------------
var areaKm2Img = ee.Image.pixelArea().divide(1e6).rename('area_km2');

function sumAreaKm2(maskImg) {
  return ee.Number(
    areaKm2Img.updateMask(maskImg).reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: savarGeom,
      scale: SCALE,
      maxPixels: 1e13,
      tileScale: TS
    }).get('area_km2')
  );
}

var totalKm2 = ee.Number(
  areaKm2Img.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: savarGeom,
    scale: SCALE,
    maxPixels: 1e13,
    tileScale: TS
  }).get('area_km2')
);

var validMask = LST_p75.mask();
var validKm2  = sumAreaKm2(validMask);
var nodataKm2 = totalKm2.subtract(validKm2);

var waterKm2 = sumAreaKm2(water);
var vegKm2   = sumAreaKm2(veg);
var urbanKm2 = sumAreaKm2(urban);

var otherMask = validMask.and(water.not()).and(veg.not()).and(urban.not());
var otherKm2  = sumAreaKm2(otherMask);

function row(name, km2) {
  km2 = ee.Number(km2);
  return ee.Feature(null, {Class: name, Area_km2: km2, Percent_of_total: km2.divide(totalKm2).multiply(100)});
}

var areaTableFC = ee.FeatureCollection([
  row('Total', totalKm2),
  row('Valid_data', validKm2),
  row('NoData', nodataKm2),
  row('Water', waterKm2),
  row('Vegetation', vegKm2),
  row('Urban', urbanKm2),
  row('Other', otherKm2)
]);

print('Area table (km²):', areaTableFC);


// -------------------------
// 14) Table row export
// -------------------------
var table1 = ee.Feature(null, {
  Period: PERIOD_NAME,
  Scenes_retained: l7.size(),
  Scene_validFrac_threshold: SCENE_VALID_FRAC_MIN,
  FloodFilter_MAX_WATER_FRAC: MAX_WATER_FRAC,

  RuralVegMean_LST_p75_C: ruralMean_p75,
  UrbanMean_LST_p75_C: urbanMean_p75,
  Mean_SUHI_p75_C: urbanMean_p75.subtract(ruralMean_p75),

  SUHI_p75_Min_C: SUHI_min,
  SUHI_p75_Max_C: SUHI_max,
  SUHI_p75_Mean_overUrban_C: SUHI_mean_overUrban,
  SUHI_p75_StdDev_C: SUHI_sd,
  SUHI_p75_P90_C: SUHI_p90,
  SUHI_p75_P95_C: SUHI_p95,

  Total_km2: totalKm2,
  Valid_km2: validKm2,
  NoData_km2: nodataKm2,
  Water_km2: waterKm2,
  Veg_km2: vegKm2,
  Urban_km2: urbanKm2,
  Other_km2: otherKm2
});

var table1FC = ee.FeatureCollection([table1]);
print('TABLE 1:', table1FC);


// -------------------------
// 15) Exports
// -------------------------
Export.table.toDrive({
  collection: table1FC,
  description: 'Savar_Table1_SUHI_p75_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_Table1_SUHI_p75_' + PERIOD_NAME,
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection: areaTableFC,
  description: 'Savar_AreaTable_km2_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_AreaTable_km2_' + PERIOD_NAME,
  fileFormat: 'CSV'
});

Export.image.toDrive({
  image: LST_p75.rename('LST_p75_C'),
  description: 'Savar_LST_p75_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_LST_p75_' + PERIOD_NAME,
  region: savarGeom,
  scale: SCALE,
  crs: 'EPSG:32646',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: SUHI_p75_urban,
  description: 'Savar_SUHI_p75_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_SUHI_p75_' + PERIOD_NAME,
  region: savarGeom,
  scale: SCALE,
  crs: 'EPSG:32646',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: hotspot,
  description: 'Savar_Hotspots_p90_SUHI_p75_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_Hotspots_p90_SUHI_p75_' + PERIOD_NAME,
  region: savarGeom,
  scale: SCALE,
  crs: 'EPSG:32646',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: hotspotDensity,
  description: 'Savar_HotspotDensity_' + gridScale + 'm_' + PERIOD_NAME,
  folder: FOLDER,
  fileNamePrefix: 'Savar_HotspotDensity_' + gridScale + 'm_' + PERIOD_NAME,
  region: savarGeom,
  scale: gridScale,
  crs: 'EPSG:32646',
  maxPixels: 1e13
});

print(ui.Chart.image.histogram(SUHI_p75_urban, savarGeom, 30)
  .setOptions({title: 'Histogram SUHI p75 (urban pixels) ' + PERIOD_NAME}));