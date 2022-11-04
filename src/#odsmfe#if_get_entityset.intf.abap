interface /ODSMFE/IF_GET_ENTITYSET
  public .


  types:
    gtyt_select TYPE STANDARD TABLE OF edpline .
  types:
    BEGIN OF gtys_ref_tab,
      entityset_name TYPE /iwbep/sbdm_node_name,
      join_name      TYPE string,
      query_result   TYPE REF TO data,
    END OF gtys_ref_tab .
  types:
    gtyt_t_ref_tab TYPE STANDARD TABLE OF gtys_ref_tab .

  methods GMIB_GET_ADDITIONAL_WHERE
    importing
      !IM_FNAM type CHAR30 optional
      !IM_FVAL type CHAR30 optional
      !IM_AND_OR type CHAR5 default 'AND'
      !IM_OPERATOR type CHAR5 default '='
    changing
      !CH_WHERE type STRING optional .
  methods GMIB_GET_WHERE_CLAUSE
    importing
      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    returning
      value(RE_WHERE_CLS) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_GET_SELECTION_FIELDS
    importing
      !IM_ENTITY type STRING optional
      !IM_JOIN_NAME type STRING optional
    changing
      value(CH_SELECT_FLD) type GTYT_SELECT .
  methods GMIB_GET_ODATA_PARAMETER .
  methods GMIB_EXECUTE_QUERY
    importing
      !IM_ENTITY_NAME type STRING
      !IM_ENTITY_SET_NAME type STRING
      !IM_SOURCE_NAME type STRING
      !IM_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IM_PAGING type /IWBEP/S_MGW_PAGING
      !IM_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IM_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IM_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IM_FILTER_STRING type STRING
      !IM_SEARCH_STRING type STRING
      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IM_DATA_EXT_CLASS type ref to OBJECT
      !IM_JOIN_NAME type STRING optional
      !IM_QUERY_INDEX type INT2
    changing
      !CH_ENTITYSET type ref to DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GMIB_EXPORT_DATA .
  methods GMIB_EXECUTE_ADDTIONAL_LOGIC
    changing
      !CH_QUERY_RES type GTYT_T_REF_TAB optional
    returning
      value(RE_ENTITYSET) type ref to DATA .
  methods GMIB_GET_ENTITYSET_DATA
    importing
      !IM_ENTITY_NAME type STRING
      !IM_ENTITY_SET_NAME type STRING
      !IM_SOURCE_NAME type STRING
      !IM_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IM_PAGING type /IWBEP/S_MGW_PAGING
      !IM_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IM_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IM_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IM_FILTER_STRING type STRING
      !IM_SEARCH_STRING type STRING
      !IM_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IM_DATA_EXT_CLASS type ref to OBJECT
    exporting
      !EX_ENTITYSET type ref to DATA
      !EX_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
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
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GMIB_GET_MODEL_INSTANCE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_GET_FROM_CLAUSE
    importing
      !IM_ENTITY type STRING
      !IM_JOIN_NAME type STRING optional
    returning
      value(RE_FROM) type STRING .
  methods GMIB_SORT_DATA
    changing
      !CH_ENTITYSET type ref to DATA optional .
endinterface.
