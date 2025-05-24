test_that("unknown content group id throws error", {
  expect_error(make_root_element("unknown"))
  expect_error(make_root_element("CO_HOSP"))
  expect_error(make_root_element(123))
  expect_error(make_root_element(Sys.time()))
})

test_that("known content group id returns XML node", {
  expect_s3_class(make_root_element("MI-HOSP"), "xml_node")
  expect_s3_class(make_root_element("CO-HOSP"), "xml_node")
  expect_s3_class(make_root_element("COPD-HOSP"), "xml_node")
  expect_s3_class(make_root_element("HEAT-HOSP"), "xml_node")
  expect_s3_class(make_root_element("AS-ED"), "xml_node")
  expect_s3_class(make_root_element("AS-HOSP"), "xml_node")
  expect_s3_class(make_root_element("CO-ED"), "xml_node")
  expect_s3_class(make_root_element("COPD-ED"), "xml_node")
  expect_s3_class(make_root_element("HEAT-ED"), "xml_node")
})

test_that("right root element for the right content type", {
  expect_equal(
    xml2::as_xml_document(make_root_element("MI-HOSP")) %>% xml2::xml_name(),
    "HospitalizationData"
  )
  expect_equal(
    xml2::as_xml_document(make_root_element("AS-ED")) %>% xml2::xml_name(),
    "EmergencyDepartmentData"
  )
})
