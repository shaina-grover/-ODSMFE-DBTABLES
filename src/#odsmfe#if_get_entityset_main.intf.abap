interface /ODSMFE/IF_GET_ENTITYSET_MAIN
  public .


  types:
    BEGIN OF gtys_instance,
      entity_name    TYPE char50,
      entityset_name TYPE char50,
      instance       TYPE REF TO /odsmfe/if_get_entityset_main,
      content_id     TYPE string,
      content_id_ref TYPE string,
    END OF gtys_instance .

  class-data:
    gitsb_instance TYPE TABLE OF gtys_instance .
  data GVIB_ENTITYSET_NAME type STRING .

  methods GMIB_GET_ENTITYSET_DATA
    importing
      !IM_ENTITY_NAME type STRING
      !IM_ENTITY_SET_NAME type STRING
      !IM_SOURCE_NAME type STRING optional
      !IM_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IM_PAGING type /IWBEP/S_MGW_PAGING optional
      !IM_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IM_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !IM_ORDER type /IWBEP/T_MGW_SORTING_ORDER optional
      !IM_FILTER_STRING type STRING optional
      !IM_SEARCH_STRING type STRING optional
      !IM_TECH_REQUEST_CONTEXT_ENT_C type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
      !IM_TECH_REQUEST_CONTEXT_ENTITY type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IM_TECH_REQUEST_CONTEXT_ENT_U type ref to /IWBEP/IF_MGW_REQ_ENTITY_U optional
      !IM_TECH_REQUEST_CONTEXT_ENT_D type ref to /IWBEP/IF_MGW_REQ_ENTITY_D optional
      !IM_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
      !IM_DATA_EXT_CLASS type ref to OBJECT
      !IM_CHANGESET_REQUEST type /IWBEP/IF_MGW_APPL_TYPES=>TY_T_CHANGESET_REQUEST optional
      !IM_CHANGESET_ST_REQUEST type /IWBEP/IF_MGW_APPL_TYPES=>TY_S_CHANGESET_REQUEST optional
    exporting
      !EX_ENTITYSET type ref to DATA
      !EX_RESPONSE_CONTEXT_ENTITY type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
      !EX_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
      !EX_ENTITY type ref to DATA
      !EX_DEEP_ENTITY type ref to DATA
      !EX_CHANGESET_RESPONSE type /IWBEP/IF_MGW_APPL_TYPES=>TY_T_CHANGESET_RESPONSE
      !EX_DELETED_ENTITYSET type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_SET_GLOBAL_VARIABLE
    importing
      !IM_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT
      !IM_REQUEST_DETAILS type ref to /IWBEP/IF_MGW_CORE_SRV_RUNTIME=>TY_S_MGW_REQUEST_CONTEXT
      !IM_SERVICE_DOCUMENT_NAME type ref to STRING
      !IM_SERVICE_VERSION type ref to NUMC4
      !IM_SERVICE_NAMESPACE type ref to STRING
      !IM_INJECTION type ref to /IWBEP/IF_SB_GEN_DPC_INJECTION
      !IM_HTTP_METHOD type STRING optional .
  methods GMIB_GET_ENTITYSET_FILTER_DATA
    importing
      !IM_ENTITY_SET_NAME type STRING
    exporting
      !EX_FILTER_VALUES type ANY .
  methods GMIB_GET_USER_PARAMETERS
    importing
      !IM_USER type SYUNAME
      !IM_PARAMETER type MEMORYID optional
    exporting
      !EX_PARAMETER_VALUE type XUVALUE
      !EX_PARAMETER_VALUES type /ODSMFE/TT_CORE_RANGE_TAB .
  methods GMIB_GET_ENTITYSET_CONFIG_DATA
    importing
      !IM_PARAM_NAME type /ODSMFE/DE_MFE_KEY
    exporting
      !EX_PARAM_VALUE type /ODSMFE/DE_MFE_VALUE .
  methods GMIB_GET_ENTITYSET_SERV_CONFIG
    importing
      !IM_ENTITY_SET_NAME type STRING optional
    exporting
      !EX_ACTIVE type STRING .
endinterface.
