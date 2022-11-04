*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/FORM_SELECTION_SCREEN
*&---------------------------------------------------------------------*
PARAMETERS : p_file TYPE file_table-filename." OBLIGATORY.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-020.
PARAMETERS p_update AS CHECKBOX." Update Existing Form data
PARAMETERS p_codes AS CHECKBOX. " Update Catalog Code and Code Grp
PARAMETERS p_trans AS CHECKBOX. " Transport Creation
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.

PARAMETERS p_des TYPE c LENGTH 250 LOWER CASE.
* SOC Sravan kumar "++ ES1K902842
PARAMETERS p_fmcat  TYPE c LENGTH 20 MATCHCODE OBJECT /odsmfe/sh_form_category.
PARAMETERS p_funare TYPE c LENGTH 20 MATCHCODE OBJECT /odsmfe/sh_functional_area.
PARAMETERS p_subare TYPE c LENGTH 20 MATCHCODE OBJECT /odsmfe/sh_sub_area.
* EOC Sravan kumar "++ ES1K902842
SELECTION-SCREEN SKIP 1.
PARAMETERS p_active AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-018.
SELECTION-SCREEN PUSHBUTTON 1(10)  but1 USER-COMMAND cli1.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-019.

PARAMETERS p_asinmt AS LISTBOX VISIBLE LENGTH 15 USER-COMMAND m1.
SELECT-OPTIONS s_auart FOR gv_auart NO INTERVALS MODIF ID m1.
SELECT-OPTIONS s_steus FOR gv_steus NO INTERVALS MODIF ID m2.
SELECT-OPTIONS s_eqtyp FOR gv_eqtyp NO INTERVALS MODIF ID m8."++
SELECT-OPTIONS s_fltyp FOR gv_fltyp NO INTERVALS MODIF ID m9."++
SELECT-OPTIONS s_eqart FOR gv_eqart NO INTERVALS MODIF ID m10."++
* SOC by ODS ES1K902363
SELECT-OPTIONS s_plnty FOR gv_plnty NO INTERVALS MODIF ID m11.
SELECT-OPTIONS s_plnnr FOR gv_plnnr NO INTERVALS MODIF ID m12.
SELECT-OPTIONS s_plnal FOR gv_plnal NO INTERVALS MODIF ID m13.
SELECT-OPTIONS s_zaehl FOR gv_zaehl NO INTERVALS MODIF ID m14.
* EOC by ODS ES1K902363
PARAMETERS p_occur TYPE n LENGTH 2 MODIF ID m5.
PARAMETERS p_mandt AS CHECKBOX MODIF ID m3.
PARAMETERS p_theme AS CHECKBOX MODIF ID m6.
PARAMETERS p_roleid TYPE c LENGTH 30 MODIF ID m7 MATCHCODE OBJECT /odsmfe/roleid.

SELECTION-SCREEN END OF BLOCK b4.

* selection screen Validations
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM /odsmfe/fo_browse_file.

AT SELECTION-SCREEN ON p_file .

  IF sy-ucomm EQ gc_uc1."UC1
    IF p_file IS INITIAL .
      MESSAGE text-009 TYPE gc_e.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_asinmt.

  CLEAR: gv_selected_value,gst_values.
  REFRESH git_values.
  gst_values-fieldname = gc_p_asinmt. "P_ASINMT
  APPEND gst_values TO git_values.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = gc_x
    TABLES
      dynpfields         = git_values.

  READ TABLE git_values INDEX 1 INTO gst_values.
  IF sy-subrc = 0 AND gst_values-fieldvalue IS NOT INITIAL.
    READ TABLE git_list INTO gst_list
    WITH KEY key = gst_values-fieldvalue.
    IF sy-subrc = 0.
      gv_selected_value = gst_list-text.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  IF gv_selected_value EQ gc_workorder."WORKORDER.

    LOOP AT SCREEN.
      IF screen-group1 = 'M4'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*      soc ++
      IF screen-group1 = 'M8'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*    eoc ++
*  SOC by ODS ES1K902363
      IF screen-group1 = 'M11'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*      soc ++
      IF screen-group1 = 'M12'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*  EOC by ODS ES1K902363
    ENDLOOP.

  ELSEIF  gv_selected_value = gc_operation."OPERATION

    LOOP AT SCREEN.
      IF screen-group1 = 'M1'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M3'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M4'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
*      soc ++
      IF screen-group1 = 'M8'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*    eoc ++
*  SOC by ODS ES1K902363
      IF screen-group1 = 'M11'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M12'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*  EOC by ODS ES1K902363

    ENDLOOP.
***SOC SKAMMARI
  ELSEIF  gv_selected_value = gc_equip."EQUIPMENT
    LOOP AT SCREEN.
      IF screen-group1 = 'M1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M3'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M8'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
*  SOC by ODS ES1K902363
      IF screen-group1 = 'M11'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M12'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*  EOC by ODS ES1K902363
    ENDLOOP.
  ELSEIF gv_selected_value = gc_floc."FUNCTIONALLOC
    LOOP AT SCREEN.
      IF screen-group1 = 'M1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M3'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M8'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
*  SOC by ODS ES1K902363
      IF screen-group1 = 'M11'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M12'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*  EOC by ODS ES1K902363
    ENDLOOP.

* SOC by ODS ES1K902363
  ELSEIF gv_selected_value EQ gc_tasklist.

    LOOP AT SCREEN.
      IF screen-group1 = 'M1'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M3'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M4'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M8'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M11'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M12'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
* EOC by ODS ES1K902363
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'M1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M2'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M3'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M4'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M6'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M7'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*      soc++
      IF screen-group1 = 'M8'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M9'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M10'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
*    eoc ++
* SOC by ODS ES1K902363
      IF screen-group1 = 'M11'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M12'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M13'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'M14'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
* EOC by ODS ES1K902363
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sscrfields.
    WHEN gc_cli1."CLI1

* Added Authority Check as per SAP Security Checks
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = '/ODSMFE/FORM_ASSIGN'
        EXCEPTIONS
          ok     = 1
          not_ok = 2.
      IF sy-subrc = 1.
        CALL TRANSACTION '/ODSMFE/FORM_ASSIGN'.
      ELSE.
        MESSAGE text-026 TYPE gc_e.
      ENDIF.

  ENDCASE.

  IF sy-ucomm EQ gc_uc1."UC1

    IF p_file IS INITIAL.
      MESSAGE text-023 TYPE gc_s DISPLAY LIKE gc_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_des IS INITIAL.
      MESSAGE text-024 TYPE gc_s DISPLAY LIKE gc_e.
      LEAVE LIST-PROCESSING.
    ENDIF.

* SOC Sravan kumar "++ ES1K902842
** When Form Category is empty shows message
*    IF p_fmcat IS INITIAL.
*      MESSAGE text-027 TYPE gc_s DISPLAY LIKE gc_e.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
** When Functional Area ID is empty shows message
*    IF p_funare IS INITIAL.
*      MESSAGE text-028 TYPE gc_s DISPLAY LIKE gc_e.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
** When Sub Area ID is empty shows message
*    IF p_subare IS INITIAL.
*      MESSAGE text-029 TYPE gc_s DISPLAY LIKE gc_e.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
* EOC Sravan kumar "++ ES1K902842

  ENDIF.
