FUNCTION /ODSMFE/FM_FORMS_RPT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_URL) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_WO) TYPE  AUFNR OPTIONAL
*"     REFERENCE(IM_MODE) TYPE  STRING DEFAULT 'workorder'
*"     REFERENCE(IM_FORMID) TYPE  /ODSMFE/DE_FORMID OPTIONAL
*"     REFERENCE(IM_BROWSER) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_USER) TYPE  UNAME OPTIONAL
*"----------------------------------------------------------------------
  TYPE-POOLS abap.
* get start url
  DATA :lv_url         TYPE string,
        lv_param       TYPE string,
        lv_protocol    TYPE string,
        lv_host        TYPE string,
        lv_port        TYPE string,
        lv_application TYPE string,
        lv_urlc        TYPE c LENGTH 1024,
        lv_url_def     TYPE string VALUE '/sap/ODSMFE/PR_FORMUI/index.html'.
  DATA : lv_accessibility_mode TYPE c,
         lv_langu              TYPE c LENGTH 2.

  CONSTANTS: lc_open    TYPE string VALUE 'OPEN',
             lc_browser TYPE string VALUE 'chrome.exe'.


**********************************************************************
* create entries in table HTTPURLLOC for the below application string.
* these entries have to point to the reverse proxy (e.g. SAP WebDispatcher).
**********************************************************************
  lv_application = '/sap/bc/ui5_ui5'.

  cl_http_server=>if_http_server~get_location(
  EXPORTING
    application  = lv_application
*     for_domain   = for_domain
  IMPORTING
    host         = lv_host
    port         = lv_port
    out_protocol = lv_protocol ).

  IF lv_host IS INITIAL OR lv_port IS INITIAL OR lv_protocol IS INITIAL.
    MESSAGE text-100 TYPE 'E'.
    RETURN.
  ENDIF.

  CONCATENATE lv_protocol '://' lv_host ':' lv_port lv_application INTO lv_url.
  TRANSLATE lv_url TO LOWER CASE.

  IF im_url IS NOT INITIAL.
    lv_url_def = im_url.
  ENDIF.

  CONCATENATE lv_url lv_url_def INTO lv_url.

  " client
  CONCATENATE 'sap-client=' sy-mandt INTO lv_param.         "#EC NOTEXT

  PERFORM /odsmfe/fo_add_url_param USING    lv_param
  CHANGING lv_url.

  CLEAR lv_param.

  IF im_mode IS NOT INITIAL.
    CONCATENATE lv_url '#/' im_mode '/' INTO lv_url.
  ENDIF.

  IF im_wo IS NOT INITIAL.

    lv_param  = im_wo.

  ELSEIF im_formid IS NOT INITIAL.

    lv_param  = im_formid.

  ENDIF.

  IF lv_param IS NOT INITIAL.

    PERFORM /odsmfe/fo_add_url_param USING    lv_param
    CHANGING lv_url.

  ENDIF.

  lv_urlc = lv_url.

  IF im_browser IS INITIAL.

* single sign-on
    CONSTANTS lc_icf_url TYPE string VALUE '/sap/public/myssocntl' .
    DATA lv_sso_active   TYPE abap_bool.

    CALL METHOD cl_icf_tree=>if_icf_tree~service_from_url
      EXPORTING
        url                   = lc_icf_url
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

      DATA lv_container TYPE REF TO cl_gui_container.       "#EC NEEDED
      DATA lv_viewer    TYPE REF TO cl_gui_html_viewer.

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
        DISPLAY LIKE 'E'.
      ENDIF.

      CALL METHOD lv_viewer->enable_sapsso
        EXPORTING
          enabled    = abap_true
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.

      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
      ENDIF.

      CALL METHOD lv_viewer->detach_url_in_browser
        EXPORTING
          url        = lv_urlc
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
      ENDIF.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
      ENDIF.

* start browser without single-sign-on
    ELSE.
      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          url                    = lv_urlc
          new_window             = abap_true
          browser_type           = lc_browser
        EXCEPTIONS
          frontend_not_supported = 1
          frontend_error         = 2
          prog_not_found         = 3
          no_batch               = 4
          unspecified_error      = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ELSE.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = im_browser
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
*  error handling
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
