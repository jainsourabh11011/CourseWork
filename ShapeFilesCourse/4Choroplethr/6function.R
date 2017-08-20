# example: encapsulating choroplethr in a function
library(choroplethr)

data(df_pop_county)
county_choropleth(df_pop_county)

county_choropleth(df_pop_county, 
                  title         = "County Population", 
                  legend        = "Population",
                  num_colors    = 1,
                  state_zoom    = "new york",
                  reference_map = TRUE)

county_choropleth

sf_supervisor_choropleth = function(df, title = "", legend = "", num_colors = 7, zoom = NULL, reference_map = FALSE)
{
  c = SFChoropleth$new(df)
  c$title = title
  c$legend = legend
  c$set_num_colors(num_colors)
  c$set_zoom(zoom)
  if (reference_map) {
    c$render_with_reference_map()
  } else {
    c$render()
  }
}

sf_supervisor_choropleth(df)
sf_supervisor_choropleth(df, num_colors=1)
sf_supervisor_choropleth(df, num_colors=1, zoom=c(3,6,10))
sf_supervisor_choropleth(df, num_colors=1, zoom=c(3,6,10), reference_map=TRUE)
