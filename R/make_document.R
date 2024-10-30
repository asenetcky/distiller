make_document <-
  function(data,
           content_group_id,
           mcn,
           jurisdiction_code,
           state_fips_code,
           submitter_email,
           submitter_name,
           submitter_title) {

  xml_document <- xml2::xml_new_document()

  root_element <- make_data(content_group_id)
  header_node <-
    make_header(
      mcn,
      jurisdiction_code,
      content_group_id,
      submitter_email,
      submitter_name,
      submitter_title,
      state_fips_code
  )

  dataset_node <- make_dataset(data, content_group_id)


  xml2::xml_add_child(root_element, header_node)
  xml2::xml_add_child(root_element, dataset_node)
  xml2::xml_add_child(xml_document, root_element)


}
