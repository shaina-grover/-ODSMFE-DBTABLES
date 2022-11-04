CLASS /odsmfe/cl_pm_workorder_abs DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    DATA pernr_merged TYPE /odsmfe/core_range_tab .

    METHODS get_workorder_detail
          ABSTRACT
      IMPORTING
        !iv_mobileuser            TYPE string
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
        !iv_packet_size           TYPE i OPTIONAL
        !iv_skiptoken             TYPE string OPTIONAL
      EXPORTING
        !et_workorder_operation   TYPE /odsmfe/pm_afvc_tab
        !et_workorder_component   TYPE /odsmfe/pm_resb_tab
        !et_workorder_prt         TYPE /odsmfe/pm_oper_prt_assign_tab
        !et_workorder_longtext    TYPE /odsmfe/pm_longtext_tab
        !et_workorder_header      TYPE /odsmfe/cs_caufv_tab
        !et_workorder_opr_del     TYPE /odsmfe/pm_afvc_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_history_pending
      IMPORTING
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser            TYPE string
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_workorder_history     TYPE /odsmfe/pm_wo_reference_tab
        !et_workorder_pending     TYPE /odsmfe/pm_wo_reference_tab
        !et_workorder_longtext    TYPE /odsmfe/pm_longtext_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_notification
      IMPORTING
        !it_workorder_header         TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser               TYPE string
      EXPORTING
        !et_workorder_notifheader    TYPE /odsmfe/cs_notif_header_tab
        !et_workorder_notifitem      TYPE /odsmfe/pm_qmfe_tab
        !et_workorder_notifitemcause TYPE /odsmfe/pm_qmur_tab
        !et_workorder_notiftask      TYPE /odsmfe/pm_qmsm_tab
        !et_workorder_notifactivity  TYPE /odsmfe/pm_qmma_tab
        !et_workorder_notiflongtext  TYPE /odsmfe/pm_longtext_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_partner_address
      IMPORTING
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser            TYPE string OPTIONAL
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_wo_partner            TYPE /odsmfe/cs_partner_tab
        !et_address               TYPE /odsmfe/cs_bpaddress_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_object
      IMPORTING
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser            TYPE string OPTIONAL
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_workorder_object      TYPE /odsmfe/pm_wo_objk_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_notif_lt
      IMPORTING
        !it_workorder_header         TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser               TYPE string OPTIONAL
      EXPORTING
        !et_workorder_notifheader    TYPE /odsmfe/cs_notif_header_tab
        !et_workorder_notifitem      TYPE /odsmfe/pm_qmfe_tab
        !et_workorder_notifitemcause TYPE /odsmfe/pm_qmur_tab
        !et_workorder_notiftask      TYPE /odsmfe/pm_qmsm_tab
        !et_workorder_notifactivity  TYPE /odsmfe/pm_qmma_tab
        !et_workorder_notiflongtext  TYPE /odsmfe/pm_longtext_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_longtext
          ABSTRACT
      IMPORTING
        !iv_mobileuser            TYPE string
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_workorder_operation   TYPE /odsmfe/pm_afvc_tab
        !et_workorder_component   TYPE /odsmfe/pm_resb_tab
        !et_workorder_prt         TYPE /odsmfe/pm_oper_prt_assign_tab
        !et_workorder_longtext    TYPE /odsmfe/pm_longtext_tab
        !et_workorder_header      TYPE /odsmfe/cs_caufv_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    METHODS get_workorder_his_pend_lt
      IMPORTING
        !it_workorder_header      TYPE /odsmfe/cs_caufv_tab OPTIONAL
        !iv_mobileuser            TYPE string
        !iv_entity_name           TYPE string OPTIONAL
        !iv_entity_set_name       TYPE string OPTIONAL
        !iv_source_name           TYPE string OPTIONAL
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option OPTIONAL
        !is_paging                TYPE /iwbep/s_mgw_paging OPTIONAL
        !it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
        !it_navigation_path       TYPE /iwbep/t_mgw_navigation_path OPTIONAL
        !it_order                 TYPE /iwbep/t_mgw_sorting_order OPTIONAL
        !iv_filter_string         TYPE string OPTIONAL
        !iv_search_string         TYPE string OPTIONAL
        !io_tech_request_context  TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_workorder_history     TYPE /odsmfe/pm_wo_reference_tab
        !et_workorder_pending     TYPE /odsmfe/pm_wo_reference_tab
        !et_workorder_longtext    TYPE /odsmfe/pm_longtext_tab
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
protected section.

  data GT_AUFNR_DELTA type /ODSMFE/PM_VALID_AUFNR_TAB .
  data GEWRK_MERGED type /ODSMFE/CORE_RANGE_TAB .
  data GT_QMNUM_DELTA type /ODSMFE/PM_VALID_QMNUM_TAB .
  data GT_ISTAT type /ODSMFE/CORE_SY_STAT_TXT_TAB .
  data GT_ESTAT type /ODSMFE/PM_USER_STAT_TEXT_TAB .
  data ASSIGNMENT_TYPE type /ODSMFE/PM_ASSIGNMENT_TYPE_DTE .
  data ARBID_MERGED type /ODSMFE/CORE_RANGE_TAB .
  data GT_OPER_OBJECT type /ODSMFE/PM_OPER_OBJECT_TAB .
  data GT_AUFNR_PARNR type /ODSMFE/PM_AUFNR_PARNR_TAB .
private section.
ENDCLASS.



CLASS /ODSMFE/CL_PM_WORKORDER_ABS IMPLEMENTATION.


  method GET_PARTNER_ADDRESS.
  endmethod.


  method GET_WORKORDER_HISTORY_PENDING.
  endmethod.


  method GET_WORKORDER_HIS_PEND_LT.
  endmethod.


  method GET_WORKORDER_NOTIFICATION.
  endmethod.


  method GET_WORKORDER_NOTIF_LT.
  endmethod.


  method GET_WORKORDER_OBJECT.
  endmethod.
ENDCLASS.
