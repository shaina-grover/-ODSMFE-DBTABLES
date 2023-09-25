INTERFACE /odsmfe/if_get_entityset_main
  PUBLIC .

  TYPES:
    BEGIN OF gtys_instance,
      entity_name(50)    TYPE c,
      entityset_name(50) TYPE c,
      instance           TYPE REF TO /odsmfe/if_get_entityset_main,
      content_id         TYPE string,
      content_id_ref     TYPE string,
    END OF gtys_instance .

  CLASS-DATA:
    gitsb_instance TYPE TABLE OF gtys_instance .
  DATA gvib_entityset_name TYPE string .

  METHODS gmib_get_entityset_data
    IMPORTING
      !im_entity_name           TYPE string
      !im_entity_set_name       TYPE string
*      !im_source_name          TYPE string OPTIONAL
      !im_filter_select_options TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
      !im_paging                TYPE REF TO if_rap_query_paging OPTIONAL
*      !im_key_tab              TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
*      !im_navigation_path      TYPE /iwbep/t_mgw_navigation_path OPTIONAL
*      !im_order                TYPE /iwbep/t_mgw_sorting_order OPTIONAL
      !im_filter_string         TYPE string OPTIONAL
*      !im_search_string        TYPE string OPTIONAL
      !im_request               TYPE REF TO if_rap_query_request OPTIONAL
*      !im_tech_request_context_ent_c  TYPE REF TO /iwbep/if_mgw_req_entity_c OPTIONAL
*      !im_tech_request_context_entity TYPE REF TO /iwbep/if_mgw_req_entity OPTIONAL
*      !im_tech_request_context        TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
*      !im_tech_request_context_ent_u  TYPE REF TO /iwbep/if_mgw_req_entity_u OPTIONAL
*      !im_tech_request_context_ent_d  TYPE REF TO /iwbep/if_mgw_req_entity_d OPTIONAL
*      !im_data_provider               TYPE REF TO /iwbep/if_mgw_entry_provider OPTIONAL
*      !im_data_ext_class              TYPE REF TO object
*      !im_changeset_request           TYPE /iwbep/if_mgw_appl_types=>ty_t_changeset_request OPTIONAL
*      !im_changeset_st_request        TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_request OPTIONAL
    EXPORTING
      !ex_response              TYPE REF TO if_rap_query_response
      !ex_response_data         TYPE STANDARD TABLE
*      !ex_entityset             TYPE REF TO data
*      !ex_response_context_entity    TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_entity_cntxt
*      !ex_response_context           TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
*      !ex_entity                TYPE REF TO data
*      !ex_deep_entity           TYPE REF TO data
*      !ex_changeset_response         TYPE /iwbep/if_mgw_appl_types=>ty_t_changeset_response
*      !ex_deleted_entityset     TYPE REF TO data
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
*      !im_http_method           TYPE string OPTIONAL .

  METHODS gmib_get_entityset_filter_data
    IMPORTING
      !im_entity_set_name TYPE string
    EXPORTING
      !ex_filter_values   TYPE any .

  METHODS gmib_get_user_parameters
    IMPORTING
      !im_user             TYPE syuname
      !im_parameter        TYPE /odsmfe/de_parid OPTIONAL
    EXPORTING
      !ex_parameter_value  TYPE /odsmfe/de_parva
      !ex_parameter_values TYPE /odsmfe/tt_core_range_tab
    RAISING
      cx_rfc_dest_provider_error .

  METHODS gmib_get_entityset_config_data
    IMPORTING
      !im_param_name  TYPE /odsmfe/de_mfe_key
    EXPORTING
      !ex_param_value TYPE /odsmfe/de_mfe_value .

  METHODS gmib_get_entityset_serv_config
    IMPORTING
      !im_entity_set_name TYPE string OPTIONAL
    EXPORTING
      !ex_active          TYPE string .



ENDINTERFACE.
