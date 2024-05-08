 var nightlight = ee.ImageCollection(NOAAVIIRSDNBMONTHLY_V1VCMSLCFG);
 var sonoma = ee.FeatureCollection(sonoma_county);

Map.centerObject(sonoma, 9);
Map.addLayer(sonoma);

var start = ee.Date.fromYMD(2017,3,1);  March 1 2017
var end = ee.Date.fromYMD(2021, 10, 1);  Dec 1 2021
var years = ee.List.sequence(2017,2021);

 filter to range
var nightlights_s2l = nightlight.filterDate(start, end);
print(nightlights_s2l);

 select avg bands
var avg_nightlights = nightlights_s2l.select(avg_rad);

 get mean annual values
var byYear = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return avg_nightlights
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.mean())
        .set('year', y);
    })
  );
  
 clip to AOI
var nightlights_sonoma = byYear.map(function(image){ return image.clip(sonoma)});
Map.addLayer(nightlights_sonoma.first(),{min0,max10,palette['000000','700000','808080','FFFF00','ffffff','ffffff','ffffff']},nightlights);
print(nightlights_sonoma);

 convert IC to multi banded image for export
var nightlights_img = nightlights_sonoma.toBands();

print(nightlights_img.bandNames());
 rename bands
nightlights_img = nightlights_img.select(
  ['0_avg_rad_mean','1_avg_rad_mean','2_avg_rad_mean','3_avg_rad_mean','4_avg_rad_mean'],
  ['2017_avg_rad', '2018_avg_rad', '2019_avg_rad', '2020_avg_rad', '2021_avg_rad']
);
print(nightlights_img.bandNames());

 Export image to drive and use 90m resolution
Export.image.toDrive({
  image nightlights_img, 
  folder VIIRS,
  scale 90,
  region sonoma,
  crs 'EPSG32610'
  })
