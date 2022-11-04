*&---------------------------------------------------------------------*
*& Report  /ODSMFE/RPT_MAIN_FORM_ANALYTIC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /odsmfe/rpt_main_form_analytic.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .            "'Select forms:'.
*Selection screen declaration for table input
PARAMETERS :  p_user  LIKE usr02-bname,
              p_forms TYPE /odsmfe/tb_forsp-formid,
              p_vrsn  TYPE /odsmfe/tb_forsp-version.

SELECT-OPTIONS :p_date FOR sy-datum NO-EXTENSION..
SELECTION-SCREEN: END OF BLOCK b1.

DATA: lv_uname      TYPE /odsmfe/de_createdby,                             "ODSMFE Created By
      lv_formid     TYPE /odsmfe/de_formid,
      ls_filter_val TYPE /odsmfe/core_range_str,
      lt_filter_val TYPE /odsmfe/core_range_tab,
      lv_version    TYPE /odsmfe/de_version.                               "ODSMFE Form ID

AT SELECTION-SCREEN OUTPUT.
  PERFORM show_logo.

START-OF-SELECTION.

  IF  p_user IS INITIAL AND p_forms IS INITIAL.
    MESSAGE 'Please Provide User or Form' TYPE 'I'.
    EXIT.
  ENDIF.

  lv_uname = p_user.
  lv_formid = p_forms .
  lv_version = p_vrsn.
  IF p_date[] IS NOT INITIAL.
    ls_filter_val-sign = p_date-sign.
    ls_filter_val-option = p_date-option.
    ls_filter_val-low = p_date-low.
    ls_filter_val-high = p_date-high.
    APPEND ls_filter_val TO lt_filter_val.
  ENDIF.


  CALL FUNCTION '/ODSMFE/FM_FORMS_TOT'
    EXPORTING
      im_uname   = lv_uname
      im_formid  = lv_formid
      im_version = lv_version
      im_date    = lt_filter_val.

*&---------------------------------------------------------------------
**& Form show_logo
*&---------------------------------------------------------------------
FORM show_logo.

  DATA: docking           TYPE REF TO cl_gui_docking_container,
        picture_control_1 TYPE REF TO cl_gui_picture,
        url(256)          TYPE c.

  DATA: query_table    LIKE w3query OCCURS 1 WITH HEADER LINE,
        html_table     LIKE w3html OCCURS 1,
        return_code    LIKE  w3param-ret_code,
        content_type   LIKE  w3param-cont_type,
        content_length LIKE  w3param-cont_len,
        pic_data       LIKE w3mime OCCURS 0,
        pic_size       TYPE i.

  DATA: repid LIKE sy-repid.
  repid = sy-repid.
  CREATE OBJECT picture_control_1 EXPORTING parent = docking.
  CHECK sy-subrc = 0.
  CALL METHOD picture_control_1->set_display_mode
    EXPORTING
      display_mode = cl_gui_picture=>display_mode_stretch.
  CALL METHOD picture_control_1->set_position
    EXPORTING
      height = 52
      left   = 820
      top    = 120
      width  = 350.

  IF url IS INITIAL.
    REFRESH query_table.
    query_table-name  = '_OBJECT_ID'.
    query_table-value = '/ODSMFE/LOGO'.
    APPEND query_table.
    CALL FUNCTION 'WWW_GET_MIME_OBJECT'
      TABLES
        query_string        = query_table
        html                = html_table
        mime                = pic_data
      CHANGING
        return_code         = return_code
        content_type        = content_type
        content_length      = content_length
      EXCEPTIONS
        object_not_found    = 1
        parameter_not_found = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'image'
        subtype  = cndp_sap_tab_unknown
        size     = pic_size
        lifetime = cndp_lifetime_transaction
      TABLES
        data     = pic_data
      CHANGING
        url      = url
      EXCEPTIONS
        OTHERS   = 1.
  ENDIF.
  CALL METHOD picture_control_1->load_picture_from_url
    EXPORTING
      url = url.
ENDFORM.                    "show_pic
