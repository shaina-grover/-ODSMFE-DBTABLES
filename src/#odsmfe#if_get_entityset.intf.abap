INTERFACE /odsmfe/if_get_entityset
  PUBLIC .

  TYPES: gtyt_select TYPE STANDARD TABLE OF string .
  TYPES:
    BEGIN OF gtys_ref_tab,
      entityset_name(128) TYPE c,
      join_name           TYPE string,
      query_result        TYPE REF TO data,
    END OF gtys_ref_tab .
  TYPES:
    gtyt_t_ref_tab TYPE STANDARD TABLE OF gtys_ref_tab .

  METHODS gmib_get_additional_where
    IMPORTING
      !im_fnam     TYPE char30 OPTIONAL
      !im_fval     TYPE char30 OPTIONAL
      !im_and_or   TYPE char5 DEFAULT 'AND'
      !im_operator TYPE char5 DEFAULT '='
    CHANGING
      !ch_where    TYPE string OPTIONAL .

  METHODS gmib_get_where_clause
    IMPORTING
      io_request          TYPE REF TO if_rap_query_request
    RETURNING
      VALUE(re_where_cls) TYPE string.
*    raising
*      /IWBEP/CX_MGW_TECH_EXCEPTION
*      /IWBEP/CX_MGW_BUSI_EXCEPTION .

  METHODS gmib_get_selection_fields
    IMPORTING
      !im_entity           TYPE string OPTIONAL
      !im_join_name        TYPE string OPTIONAL
    CHANGING
      VALUE(ch_select_fld) TYPE gtyt_select .

  METHODS gmib_get_odata_parameter .

  METHODS gmib_execute_query
    IMPORTING
      !im_entity_name           TYPE string
      !im_entity_set_name       TYPE string
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs
      !im_filter_string         TYPE string
*      !im_data_ext_class        TYPE REF TO object
      !im_join_name             TYPE string OPTIONAL
      !im_query_index           TYPE int2
      !im_request               TYPE REF TO if_rap_query_request
    EXPORTING
      !ex_response              TYPE REF TO if_rap_query_response
      !ex_response_data         TYPE STANDARD TABLE
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider.
  METHODS gmib_export_data .
  METHODS gmib_execute_addtional_logic
    CHANGING
      !ch_query_res       TYPE gtyt_t_ref_tab OPTIONAL
    RETURNING
      VALUE(re_entityset) TYPE REF TO data .
  METHODS gmib_get_entityset_data
    IMPORTING
      !im_entity_name           TYPE string
      !im_entity_set_name       TYPE string
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs
      !im_filter_string         TYPE string
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
    EXPORTING
      !ex_response              TYPE REF TO if_rap_query_response
      !ex_response_data         TYPE STANDARD TABLE
*      !ex_entityset             TYPE REF TO data
*      !ex_response_context      TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
    RAISING
      cx_rap_query_prov_not_impl
      cx_rap_query_provider.
*  METHODS gmib_set_global_variable
*    IMPORTING
*      !im_context               TYPE REF TO /iwbep/if_mgw_context
*      !im_request_details       TYPE REF TO /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context
*      !im_service_document_name TYPE REF TO string
*      !im_service_version       TYPE REF TO numc4
*      !im_service_namespace     TYPE REF TO string
*      !im_injection             TYPE REF TO /iwbep/if_sb_gen_dpc_injection
*    RAISING
*      /iwbep/cx_mgw_busi_exception
*      /iwbep/cx_mgw_tech_exception .
  METHODS gmib_get_model_instance.

  METHODS gmib_get_from_clause
    IMPORTING
      !im_entity     TYPE string
      !im_join_name  TYPE string OPTIONAL
    RETURNING
      VALUE(re_from) TYPE string .
  METHODS gmib_sort_data
    CHANGING
      !ch_entityset TYPE REF TO data OPTIONAL .
ENDINTERFACE.
