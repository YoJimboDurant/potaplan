# potaplan
R package for querying Parks on the Air API

To install:

`devtools::install_git("YoJimboDurant/potaplan")`

# Example of usage
```{r setup}
library(potaplan)
library(leaflet)
library(dplyr)
```


## Finding Parks within a Distance of Point
To find Parks within 100 km of your coordinates:

```{r}

myparks <- ParksRad("EM83aw")
myparks$parks_filtered

```

## Map:

```{r}
leaflet(width = 700) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    data = myparks$parks_filtered,
    color = "blue",
    radius = 5,
    popup = ~ properties.name
  ) %>%
  addPolygons(
    data = myparks$circleDist,
    color = "red",
    weight = 2,
    fill = FALSE
  )

```