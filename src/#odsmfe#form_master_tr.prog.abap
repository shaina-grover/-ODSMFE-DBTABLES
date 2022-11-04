*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/FORM_MASTER_TR.
*----------------------------------------------------------------------*
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  YSINDHU
* Creation Date          :  /05/2020
* Transport No.          : ES1K901774
* Program Description    : TR creation
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_TR_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_tr_create .


  DATA: lit_users         TYPE scts_users,
        lst_users         TYPE scts_user,
        lv_request_header TYPE trwbo_request_header,
        lit_task_headers  TYPE trwbo_request_headers.

  DATA: lv_trkorr TYPE e070-trkorr,
        lst_e070  TYPE e070,
        lst_e07t  TYPE e07t,
        lst_e070c TYPE e070c,
        lst_e070m TYPE e070m.


  FIELD-SYMBOLS : <lfsst_task> TYPE trwbo_request_header.

  SELECT low FROM tvarvc INTO gv_description UP TO 1 ROWS
             WHERE name EQ '/ODSMFE/FORM_MASTER_TR_DESCR'
             AND type EQ gc_p .
  ENDSELECT.
  IF sy-subrc = 0.
* Check for the TR Name
    SELECT * FROM e07t INTO CORRESPONDING FIELDS OF TABLE git_e07t
    WHERE langu = sy-langu
    AND   as4text = gv_description.
    IF sy-subrc = 0 AND git_e07t[] IS NOT INITIAL.
      SORT: git_e07t[] BY trkorr.
      DELETE ADJACENT DUPLICATES FROM git_e07t[] COMPARING trkorr.
      SELECT * FROM e070 INTO CORRESPONDING FIELDS OF TABLE git_e070
      FOR ALL ENTRIES IN git_e07t
      WHERE strkorr = git_e07t-trkorr
      AND trstatus NE gc_trstatus.
    ENDIF.
  ENDIF.

  IF sy-subrc NE 0.
* function module to create transport
    lst_users-user = sy-uname.
    lst_users-type = 'S'.
    APPEND lst_users TO lit_users.
    CLEAR lst_users.

    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = gc_transport_kind
        iv_text           = gv_description
        iv_owner          = sy-uname
        it_users          = lit_users
      IMPORTING
        es_request_header = lv_request_header
        et_task_headers   = lit_task_headers
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3.

    IF sy-subrc IS INITIAL.
      READ TABLE lit_task_headers ASSIGNING <lfsst_task> INDEX 1.
      IF <lfsst_task> IS ASSIGNED.
        gv_wi_trkorr = <lfsst_task>-trkorr.
      ENDIF.
    ELSE.
      MESSAGE text-003 TYPE gc_e.
    ENDIF.

  ELSE.

    READ TABLE git_e070 INTO gst_e070 WITH KEY as4user = sy-uname.
    IF sy-subrc EQ 0.
      gv_wi_trkorr = gst_e070-trkorr.
    ELSE.

      READ TABLE git_e070 INTO gst_e070 INDEX 1.

      CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
        EXPORTING
          wi_kurztext       = gv_description
          wi_trfunction     = 'S'
          iv_username       = sy-uname
          wi_strkorr        = gst_e070-strkorr
        IMPORTING
          we_trkorr         = lv_trkorr
          we_e070           = lst_e070
          we_e07t           = lst_e07t
          we_e070c          = lst_e070c
          es_e070m          = lst_e070m
        EXCEPTIONS
          no_systemname     = 1
          no_systemtype     = 2
          no_authorization  = 3
          db_access_error   = 4
          file_access_error = 5
          enqueue_error     = 6
          number_range_full = 7
          invalid_input     = 8
          OTHERS            = 9.
      IF sy-subrc <> 0.
        CHECK sy-subrc NE 0.
      ENDIF.
      IF sy-subrc IS INITIAL.
        gv_wi_trkorr = lv_trkorr.
      ELSE.
        MESSAGE text-003 TYPE gc_e.
      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_APPEND_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_append_objects .
  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_trkorr                      = gv_wi_trkorr
      wi_suppress_key_check          = gc_x
    TABLES
      wt_e071                        = git_e071
      wt_e071k                       = git_e071k
    EXCEPTIONS
      key_char_in_non_char_field     = 1
      key_check_keysyntax_error      = 2
      key_inttab_table               = 3
      key_longer_field_but_no_generc = 4
      key_missing_key_master_fields  = 5
      key_missing_key_tablekey       = 6
      key_non_char_but_no_generic    = 7
      key_no_key_fields              = 8
      key_string_longer_char_key     = 9
      key_table_has_no_fields        = 10
      key_table_not_activ            = 11
      key_unallowed_key_function     = 12
      key_unallowed_key_object       = 13
      key_unallowed_key_objname      = 14
      key_unallowed_key_pgmid        = 15
      key_without_header             = 16
      ob_check_obj_error             = 17
      ob_devclass_no_exist           = 18
      ob_empty_key                   = 19
      ob_generic_objectname          = 20
      ob_ill_delivery_transport      = 21
      ob_ill_lock                    = 22
      ob_ill_parts_transport         = 23
      ob_ill_source_system           = 24
      ob_ill_system_object           = 25
      ob_ill_target                  = 26
      ob_inttab_table                = 27
      ob_local_object                = 28
      ob_locked_by_other             = 29
      ob_modif_only_in_modif_order   = 30
      ob_name_too_long               = 31
      ob_no_append_of_corr_entry     = 32
      ob_no_append_of_c_member       = 33
      ob_no_consolidation_transport  = 34
      ob_no_original                 = 35
      ob_no_shared_repairs           = 36
      ob_no_systemname               = 37
      ob_no_systemtype               = 38
      ob_no_tadir                    = 39
      ob_no_tadir_not_lockable       = 40
      ob_privat_object               = 41
      ob_repair_only_in_repair_order = 42
      ob_reserved_name               = 43
      ob_syntax_error                = 44
      ob_table_has_no_fields         = 45
      ob_table_not_activ             = 46
      tr_enqueue_failed              = 47
      tr_errors_in_error_table       = 48
      tr_ill_korrnum                 = 49
      tr_lockmod_failed              = 50
      tr_lock_enqueue_failed         = 51
      tr_not_owner                   = 52
      tr_no_systemname               = 53
      tr_no_systemtype               = 54
      tr_order_not_exist             = 55
      tr_order_released              = 56
      tr_order_update_error          = 57
      tr_wrong_order_type            = 58
      ob_invalid_target_system       = 59
      tr_no_authorization            = 60
      ob_wrong_tabletyp              = 61
      ob_wrong_category              = 62
      ob_system_error                = 63
      ob_unlocal_objekt_in_local_ord = 64
      tr_wrong_client                = 65
      ob_wrong_client                = 66
      key_wrong_client               = 67
      OTHERS                         = 68.

  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE text-003 TYPE gc_e.
  ENDIF.
ENDFORM.
