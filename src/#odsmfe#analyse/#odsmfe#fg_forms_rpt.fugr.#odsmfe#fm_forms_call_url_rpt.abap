FUNCTION /ODSMFE/FM_FORMS_CALL_URL_RPT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_URL) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_WO) TYPE  AUFNR OPTIONAL
*"     REFERENCE(IM_MODE) TYPE  STRING DEFAULT 'workorder'
*"     REFERENCE(IM_FORMID) TYPE  /ODSMFE/DE_FORMID OPTIONAL
*"     REFERENCE(IM_BROWSER) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_INSTANCEID) TYPE  /ODSMFE/DE_INSTANCEID
*"     REFERENCE(IM_FUNCTION) TYPE  SALV_DE_FUNCTION OPTIONAL
*"     REFERENCE(IM_USER) TYPE  UNAME OPTIONAL
*"----------------------------------------------------------------------
  TYPE-POOLS abap.

  DATA :lv_accessibility_mode TYPE c,
        lv_langu              TYPE c LENGTH 2,
        lv_urlc               TYPE c LENGTH 1024.

  DATA : lv_container TYPE REF TO cl_gui_container,         "#EC NEEDED
         lv_viewer    TYPE REF TO cl_gui_html_viewer.

  DATA: lit_filters TYPE TABLE OF /odsmfe/tb_filtr,
        lst_filters TYPE /odsmfe/tb_filtr.

* get start url
  DATA : lv_url         TYPE string,
         lv_url_def     TYPE string,
         lv_param       TYPE string,
         lv_protocol    TYPE string,
         lv_host        TYPE string,
         lv_port        TYPE string,
         lv_application TYPE string,
         lv_icf_url     TYPE string,
         lv_sso_active  TYPE abap_bool,
         lv_browser     TYPE string,
         lv_function    TYPE string. "
*  constants
  CONSTANTS: lc_formmasterset TYPE string VALUE 'FormMasterSet',
             lc_abap_on       TYPE abap_bool VALUE 'X',
             lc_application   TYPE string VALUE 'APPLICATION',
             lc_url_def       TYPE string VALUE 'URL_DEF',
             lc_icf_url       TYPE string VALUE 'ICF_URL',
             lc_sap_client    TYPE string VALUE 'sap-client=',
             lc_e             TYPE string VALUE 'E',
             lc_i             TYPE string VALUE 'I',
             lc_s             TYPE string VALUE 'S',
             lc_eq            TYPE string VALUE 'EQ',
             lc_open          TYPE string VALUE 'OPEN',
             lc_scpmsactive   TYPE string VALUE 'SCPMS_ACTIVE',
             lc_bspactive     TYPE string VALUE 'BSP_ACTIVE',
             lc_browser       TYPE string VALUE 'BROWSER',
             lc_x             TYPE string VALUE 'X',
             lc_display       TYPE string VALUE 'Display',
             lc_edit          TYPE string VALUE 'Edit'.
**********************************************************************
* create entries in table HTTPURLLOC for the below application string.
* these entries have to point to the reverse proxy (e.g. SAP WebDispatcher).
**********************************************************************

  SELECT mandt entitysetname tabname field recordno field_descr sign options low high active
    FROM /odsmfe/tb_filtr INTO TABLE lit_filters
    WHERE entitysetname = lc_formmasterset
    AND sign = lc_i
    AND options = lc_eq
    AND active = lc_abap_on.

  IF sy-subrc = 0.
    SORT lit_filters[] BY field.
  ENDIF.

* fetching Configured Browser
  CLEAR: lst_filters,lv_browser.
  READ TABLE lit_filters INTO lst_filters WITH KEY field = lc_browser.
  IF sy-subrc = 0.
    lv_browser = lst_filters-low.
  ENDIF.

* genearating URL for SAP cloud
  CLEAR: lst_filters,lv_url.
  READ TABLE lit_filters INTO lst_filters WITH KEY field = lc_scpmsactive.
  IF sy-subrc = 0.
    lv_url = lst_filters-low.
    TRANSLATE lv_url TO LOWER CASE.
  ENDIF.

* genearating URL for BSF application
  READ TABLE lit_filters TRANSPORTING NO FIELDS
  WITH KEY field  = lc_bspactive
           low = lc_x.
  IF sy-subrc = 0.

    CLEAR:lv_application,lst_filters.
    READ TABLE lit_filters INTO lst_filters WITH KEY field  = lc_application .
    IF sy-subrc = 0.
      lv_application = lst_filters-low.
    ENDIF.

    cl_http_server=>if_http_server~get_location(
    EXPORTING
      application  = lv_application

    IMPORTING
      host         = lv_host
      port         = lv_port
      out_protocol = lv_protocol ).

    IF lv_host IS INITIAL OR lv_port IS INITIAL OR lv_protocol IS INITIAL.
      MESSAGE text-100 TYPE lc_e.
      RETURN.
    ENDIF.

    CONCATENATE lv_protocol '://' lv_host ':' lv_port lv_application INTO lv_url.
    TRANSLATE lv_url TO LOWER CASE.

    IF im_url IS NOT INITIAL.
      lv_url_def = im_url.
    ELSE.
      CLEAR:lv_url_def,lst_filters.
      READ TABLE lit_filters INTO lst_filters WITH KEY field  = lc_url_def .
      IF sy-subrc = 0.
        lv_url_def = lst_filters-low.
      ENDIF.
    ENDIF.

    CONCATENATE lv_url lv_url_def INTO lv_url.

    " client
    CONCATENATE lc_sap_client sy-mandt INTO lv_param.       "#EC NOTEXT

    PERFORM /odsmfe/fo_add_url_param USING lv_param
    CHANGING lv_url.

  ENDIF.
  CLEAR lv_param.
  IF im_mode IS NOT INITIAL.
    CONCATENATE lv_url '#/' im_mode '/' INTO lv_url.
  ENDIF.

  IF im_wo IS NOT INITIAL.
    lv_param  = im_wo.
  ELSEIF im_instanceid IS NOT INITIAL."im_formid
    lv_param  = im_instanceid.
  ENDIF.

  IF lv_param IS NOT INITIAL.
    PERFORM /odsmfe/fo_add_url_param USING lv_param
    CHANGING lv_url.
  ENDIF.

* SOC  adding edit/create ++ES1K902010
  IF im_function IS NOT INITIAL.
    CLEAR: lv_function.
    lv_function = im_function.
    IF lv_function = text-001."DISPLAY
      CONCATENATE lv_url '/' lc_display INTO lv_url.
    ELSEIF lv_function = text-002."EDIT
      CONCATENATE lv_url '/' lc_edit INTO lv_url.
    ENDIF.
  ENDIF.
* EOC ++ES1K902010

  lv_urlc = lv_url.

  IF im_browser IS INITIAL.
* single sign-on

    CLEAR:lv_icf_url,lst_filters.
    READ TABLE lit_filters INTO lst_filters WITH KEY field  = lc_icf_url  .
    IF sy-subrc = 0.
      lv_icf_url = lst_filters-low.
    ENDIF.

    CALL METHOD cl_icf_tree=>if_icf_tree~service_from_url
      EXPORTING
        url                   = lv_icf_url
        hostnumber            = 0
        authority_check       = abap_false
      IMPORTING
        icfactive             = lv_sso_active
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 4
        OTHERS                = 5.
    IF sy-subrc NE 0.
      lv_sso_active = abap_false.
    ENDIF.

* start browser with single sign-on
    IF lv_sso_active = abap_true.

      CREATE OBJECT lv_viewer
        EXPORTING
          parent             = lv_container
        EXCEPTIONS
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4
          OTHERS             = 5.

      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE lc_e.
      ENDIF.

      CALL METHOD lv_viewer->enable_sapsso
        EXPORTING
          enabled    = abap_true
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.

      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE lc_e.
      ENDIF.

      CALL METHOD lv_viewer->detach_url_in_browser
        EXPORTING
          url        = lv_urlc
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE lc_e.
      ENDIF.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE lc_e.
      ENDIF.

* start browser without single-sign-on
    ELSE.
      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          url                    = lv_urlc
          new_window             = abap_true
          browser_type           = lv_browser
        EXCEPTIONS
          frontend_not_supported = 1
          frontend_error         = 2
          prog_not_found         = 3
          no_batch               = 4
          unspecified_error      = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE lc_s NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE lc_e.
      ENDIF.
    ENDIF.

  ELSE.
    IF lv_browser IS INITIAL.
      lv_browser = im_browser.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = lv_browser
        parameter              = lv_url
        operation              = lc_open
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
* error handling
      MESSAGE ID sy-msgid TYPE lc_s NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       DISPLAY LIKE lc_e.
    ENDIF.
  ENDIF.
ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  /odsmfe/fo_add_url_param
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_add_url_param USING up_param TYPE string
CHANGING cp_url TYPE string.

  IF cp_url CA '?'.

    CONCATENATE cp_url  up_param INTO cp_url.               "#EC NOTEXT
  ELSE.
    CONCATENATE cp_url '?' up_param INTO cp_url.            "#EC NOTEXT
  ENDIF.
ENDFORM.
