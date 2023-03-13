*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/CB_TABLES_SELECTION
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : rb_dwnld RADIOBUTTON GROUP rb1 USER-COMMAND test DEFAULT 'X'.
PARAMETERS : p_table TYPE dd02t-tabname MODIF ID abc.
PARAMETERS : p_desc TYPE dd02t-ddtext MODIF ID abc.

*PARAMETERS : rb_dwnd2 RADIOBUTTON GROUP rb1.
*PARAMETERS : p_table2 TYPE dd02t-tabname MODIF ID pqs.
*PARAMETERS : p_desc2 TYPE dd02t-ddtext MODIF ID pqs.

PARAMETERS : rb_uplod RADIOBUTTON GROUP rb1.
PARAMETERS: p_file LIKE rlgrap-filename MODIF ID xyz,
            p_str  TYPE tabname MODIF ID xyz.
*            p_hdr  AS CHECKBOX MODIF ID xyz,
*            p_cli  AS CHECKBOX MODIF ID xyz.

SELECTION-SCREEN  END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS : p_table2 TYPE dd02t-tabname MODIF ID pqs.
PARAMETERS : p_desc2 TYPE dd02t-ddtext MODIF ID pqs.

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF rb_dwnld EQ 'X' AND screen-group1 = 'ABC'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rb_uplod EQ 'X' AND screen-group1 = 'XYZ'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rb_dwnld EQ '' AND screen-group1 = 'ABC'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rb_uplod EQ '' AND screen-group1 = 'XYZ'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    IF screen-name = 'P_DESC'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT SCREEN.
    IF screen-name = 'P_DESC2'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_table.
  gv_val = '%/ODSMFE/TB_%'.

  SELECT tabname ddtext
  FROM dd02t INTO TABLE git_table WHERE tabname LIKE gv_val.

  IF sy-subrc = 0.
    SORT git_table BY tabname.
  ENDIF.

  LOOP AT git_table INTO gst_table.
    gst_tabf4-tabname = gst_table-tabname.
    gst_tabf4-ddtext = gst_table-ddtext.
    APPEND gst_tabf4 TO git_tabf4.
    CLEAR : gst_tabf4  , gst_table.
  ENDLOOP.

  IF git_tabf4 IS NOT INITIAL.

    gst_mapping-fldname = gc_fldnamef1.
    gst_mapping-dyfldname = gc_dyfldname1.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.

    gst_mapping-fldname     = gc_fldnamef2.
    gst_mapping-dyfldname   = gc_dyfldname2.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'TABNAME'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'P_TABLE'
          window_title    = 'Selection of Table Name'
          value_org       = 'S'
        TABLES
          value_tab       = git_tabf4
          return_tab      = git_ret
          dynpfld_mapping = git_mapping
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
    IF sy-subrc = 0.
*      implement error message
    ENDIF.


    READ TABLE git_ret INTO gst_ret INDEX 1.
    IF sy-subrc = 0.
      p_table = gst_ret-fieldval.
    ENDIF.
    CLEAR : git_table , git_tabf4.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_table2.
  gv_val = '%/ODSMFE/TB_%'.

  SELECT tabname ddtext
  FROM dd02t INTO TABLE git_table WHERE tabname LIKE gv_val.

  IF sy-subrc = 0.
    SORT git_table BY tabname.
  ENDIF.

  LOOP AT git_table INTO gst_table.
    gst_tabf4-tabname = gst_table-tabname.
    gst_tabf4-ddtext = gst_table-ddtext.
    APPEND gst_tabf4 TO git_tabf4.
    CLEAR : gst_tabf4  , gst_table.
  ENDLOOP.

  IF git_tabf4 IS NOT INITIAL.

    gst_mapping-fldname = gc_fldnamef3.
    gst_mapping-dyfldname = gc_dyfldname3.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.

    gst_mapping-fldname     = gc_fldnamef4.
    gst_mapping-dyfldname   = gc_dyfldname4.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.


      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'TABNAME'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'P_TABLE2'
          window_title    = 'Selection of Table Name'
          value_org       = 'S'
        TABLES
          value_tab       = git_tabf4
          return_tab      = git_ret
          dynpfld_mapping = git_mapping
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
    IF sy-subrc = 0.
*      implement error message
    ENDIF.

    READ TABLE git_ret INTO gst_ret INDEX 1.
    IF sy-subrc = 0.
      p_table2 = gst_ret-fieldval.
    ENDIF.
    CLEAR : git_table , git_tabf4.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_file.
