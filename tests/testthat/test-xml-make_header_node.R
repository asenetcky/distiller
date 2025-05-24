test_that("require all elements", {
  expect_error(make_header_node())
})


test_that("makes the same xml", {
  expect_equal(
    make_header_node(
      mcn = "test",
      jurisdiction_code = "test",
      content_group_id = "test",
      submitter_email = "test",
      submitter_name = "test",
      submitter_title = "test",
      state_fips_code = "test"
    ),
    xml2::read_xml(
      paste0(
        "<Header><MCN>test</MCN><JurisdictionCode>test",
        "</JurisdictionCode><ContentGroupIdentifier>test",
        "</ContentGroupIdentifier><SubmitterInformation>",
        "<SubmitterEmailAddress>test</SubmitterEmailAddress>",
        "<SubmitterName>test</SubmitterName>",
        "<SubmitterTitle>test</SubmitterTitle>",
        "</SubmitterInformation><StateFIPSCode>test",
        "</StateFIPSCode></Header>"
      )
    )
  )
})
