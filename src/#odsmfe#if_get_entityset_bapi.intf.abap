INTERFACE /odsmfe/if_get_entityset_bapi
  PUBLIC .


  METHODS gmib_create_entityset
    IMPORTING
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !im_paging                TYPE REF TO if_rap_query_paging OPTIONAL
      !im_filter_string         TYPE string OPTIONAL
      !im_entity_name           TYPE string OPTIONAL
      !im_entity_set_name       TYPE string OPTIONAL
      !im_source_name           TYPE string OPTIONAL
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
    EXPORTING
*      !EX_ENTITYSET type ref to DATA
*      !EX_ENTITY type ref to DATA
      !ex_response              TYPE REF TO if_rap_query_response
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider
     .

  METHODS gmib_read_entityset
    IMPORTING
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !im_paging                TYPE REF TO if_rap_query_paging OPTIONAL
      !im_source_name           TYPE string OPTIONAL
      !im_filter_string         TYPE string OPTIONAL
      !im_entity_name           TYPE string OPTIONAL
      !im_entity_set_name       TYPE string OPTIONAL
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
    EXPORTING
      !ex_response              TYPE REF TO if_rap_query_response
      !ex_response_data         TYPE STANDARD TABLE
*      !EX_ENTITYSET type ref to DATA
*      !EX_ENTITY type ref to DATA
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider
      cx_rap_query_filter_no_range
      cx_rfc_dest_provider_error.


  METHODS gmib_delete_entityset
    IMPORTING
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !im_paging                TYPE REF TO if_rap_query_paging OPTIONAL
      !im_source_name           TYPE string OPTIONAL
      !im_filter_string         TYPE string OPTIONAL
      !im_entity_name           TYPE string OPTIONAL
      !im_entity_set_name       TYPE string OPTIONAL
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
    EXPORTING
      !ex_response              TYPE REF TO if_rap_query_response
*      !EX_ENTITYSET type ref to DATA
*      !EX_ENTITY type ref to DATA
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider
      .

  METHODS gmib_modify_entityset
    IMPORTING
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !im_paging                TYPE REF TO if_rap_query_paging OPTIONAL
      !im_source_name           TYPE string OPTIONAL
      !im_filter_string         TYPE string OPTIONAL
      !im_entity_name           TYPE string OPTIONAL
      !im_entity_set_name       TYPE string OPTIONAL
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
    EXPORTING
*      !EX_ENTITYSET type ref to DATA
*      !EX_ENTITY type ref to DATA
      !ex_response              TYPE REF TO if_rap_query_response
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider
      .

  METHODS gmib_error_handling
    IMPORTING
      !im_return TYPE /odsmfe/tt_bapiret2 OPTIONAL
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider.
  METHODS gmib_bapi_transaction_commit.

ENDINTERFACE.
