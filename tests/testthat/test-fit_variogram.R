
sp_sf <- landcover

# check data class
expect_s3_class(sp_sf, "sf", exact = FALSE)
expect_s3_class(sp_sf, "data.frame", exact = FALSE)


# check structure of the input data

test_that("the size of the data",{
  expect_equal(nrow(sp_sf), 1922)
  expect_equal(ncol(sp_sf), 33)
})


# check fit variogram

df <- data.frame(sf::st_coordinates(sp_sf))
sp_sf <- sf::st_drop_geometry(sp_sf)

ndat<- cbind(df, sp_sf)

asym_fit<- fit_variogram(data = ndat,response = "MPC1",coords = c("X", "Y"))

test_that("check var_model and range",{
  expect_equal(round(asym_fit$var_model[2,3],2), 3468.04)
})
