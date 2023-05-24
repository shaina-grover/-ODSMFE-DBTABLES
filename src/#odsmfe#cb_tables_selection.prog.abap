*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/CB_TABLES_SELECTION
*&---------------------------------------------------------------------*
"/ Start of selection screen block
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : rb_dwnld RADIOBUTTON GROUP rb1 USER-COMMAND test DEFAULT 'X',           "radio button1
             p_table  TYPE dd02t-tabname MODIF ID abc,                               "table name
             p_desc   TYPE dd02t-ddtext MODIF ID abc.                                "table description

PARAMETERS : rb_dwnd2 RADIOBUTTON GROUP rb1,                                         "radio button2
             p_table2 TYPE dd02t-tabname MODIF ID pqr,                               "table name
             p_desc2  TYPE dd02t-ddtext MODIF ID pqr.                                "table description

PARAMETERS : rb_uplod RADIOBUTTON GROUP rb1,                                         "radio button3
*             p_file   LIKE rlgrap-filename MODIF ID xyz,                             "file name
             p_file   TYPE rlgrap-filename MODIF ID xyz,                             "file name
             p_str    TYPE tabname MODIF ID xyz.                                     "table name

SELECTION-SCREEN  END OF BLOCK b1.                                                   "end of selection screen block

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF rb_dwnld EQ 'X' AND screen-group1 = 'ABC'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rb_dwnd2 EQ 'X' AND screen-group1 = 'PQR'.
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
    ELSEIF rb_dwnd2 EQ '' AND screen-group1 = 'PQR'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rb_uplod EQ '' AND screen-group1 = 'XYZ'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP."/ LOOP AT SCREEN.
  "/ Disable table descripton parameter
  LOOP AT SCREEN.
    IF screen-name = 'P_DESC'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP."/ LOOP AT SCREEN.
  "/ Disable table descripton parameter
  LOOP AT SCREEN.
    IF screen-name = 'P_DESC2'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP."/ LOOP AT SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_table.
  gv_val = '%/ODSMFE/TB_%'.

  "/ fetch only tables having /ODSMFE/ namespace
  SELECT tabname ddtext
  FROM dd02t INTO TABLE git_table WHERE tabname LIKE gv_val.
  "/ Sort the internal table
  IF sy-subrc = 0.
    SORT git_table BY tabname.
  ENDIF."/ IF sy-subrc = 0.

  LOOP AT git_table INTO gst_table.
    gst_tabf4-tabname = gst_table-tabname.
    gst_tabf4-ddtext = gst_table-ddtext.
    APPEND gst_tabf4 TO git_tabf4.
    CLEAR : gst_tabf4  , gst_table.
  ENDLOOP."/ LOOP AT git_table INTO gst_table.

  IF git_tabf4 IS NOT INITIAL.
    "/ Mapping the fields
    gst_mapping-fldname = gc_fldnamef1.
    gst_mapping-dyfldname = gc_dyfldname1.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.

    gst_mapping-fldname     = gc_fldnamef2.
    gst_mapping-dyfldname   = gc_dyfldname2.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.
    "/ for f4 help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TABNAME'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = 'P_TABLE'
        window_title    = text-002
        value_org       = 'S'
      TABLES
        value_tab       = git_tabf4
        return_tab      = git_ret
        dynpfld_mapping = git_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE text-003 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF."/ IF sy-subrc <> 0.

    READ TABLE git_ret INTO gst_ret INDEX 1.
    IF sy-subrc = 0.
      p_table = gst_ret-fieldval.
    ENDIF."/ IF sy-subrc = 0.
    CLEAR : git_table , git_tabf4.
  ENDIF."/ IF git_tabf4 IS NOT INITIAL.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_table2.
  gv_val = '%/ODSMFE/TB_%'.
  "/ Fetch only tables having /ODSMFE/ namespace
  SELECT tabname ddtext
  FROM dd02t INTO TABLE git_table WHERE tabname LIKE gv_val.
  "/ Sort the internal table
  IF sy-subrc = 0.
    SORT git_table BY tabname.
  ENDIF."/ IF sy-subrc = 0.

  LOOP AT git_table INTO gst_table.
    gst_tabf4-tabname = gst_table-tabname.
    gst_tabf4-ddtext = gst_table-ddtext.
    APPEND gst_tabf4 TO git_tabf4.
    CLEAR : gst_tabf4  , gst_table.
  ENDLOOP."/ LOOP AT git_table INTO gst_table.

  IF git_tabf4 IS NOT INITIAL.
    "/ Mapping the fields
    gst_mapping-fldname = gc_fldnamef3.
    gst_mapping-dyfldname = gc_dyfldname3.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.

    gst_mapping-fldname     = gc_fldnamef4.
    gst_mapping-dyfldname   = gc_dyfldname4.
    APPEND gst_mapping TO git_mapping.
    CLEAR gst_mapping.
    "/ For f4 help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TABNAME'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = 'P_TABLE2'
        window_title    = text-002
        value_org       = 'S'
      TABLES
        value_tab       = git_tabf4
        return_tab      = git_ret
        dynpfld_mapping = git_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE text-003 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF."/ IF sy-subrc <> 0.

    READ TABLE git_ret INTO gst_ret INDEX 1.
    IF sy-subrc = 0.
      p_table2 = gst_ret-fieldval.
    ENDIF."/ IF sy-subrc = 0.
    CLEAR : git_table , git_tabf4.
  ENDIF."/ IF git_tabf4 IS NOT INITIAL.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  "/ To select excel file from presentation layer
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_file.
