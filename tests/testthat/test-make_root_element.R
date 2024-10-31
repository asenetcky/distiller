test_that("unknown content group id throws error", {
  expect_error(make_data("unknown"))
  expect_error(make_data("CO_HOSP"))
  expect_error(make_data(123))
  expect_error(make_data(lubridate::now()))
})

test_that("known content group id returns XML node", {
  expect_s3_class(make_data("MI-HOSP"), "xml_node")
  expect_s3_class(make_data("CO-HOSP"), "xml_node")
  expect_s3_class(make_data("COPD-HOSP"), "xml_node")
  expect_s3_class(make_data("HEAT-HOSP"), "xml_node")
  expect_s3_class(make_data("AS-ED"), "xml_node")
  expect_s3_class(make_data("AS-HOSP"), "xml_node")
  expect_s3_class(make_data("CO-ED"), "xml_node")
  expect_s3_class(make_data("COPD-ED"), "xml_node")
  expect_s3_class(make_data("HEAT-ED"), "xml_node")
})

test_that("right root element for the right content type", {
  expect_equal(
    xml2::as_xml_document(make_data("MI-HOSP")) %>% xml2::xml_name(),
    "HospitalizationData"
  )
  expect_equal(
    xml2::as_xml_document(make_data("AS-ED")) %>% xml2::xml_name(),
    "EmergencyDepartmentData"
  )
})
