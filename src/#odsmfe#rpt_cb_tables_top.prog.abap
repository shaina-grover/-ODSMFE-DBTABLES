*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/RPT_CB_TABLES_TOP
*&---------------------------------------------------------------------*

DATA : go_excel_structure      TYPE REF TO data,
       go_source_table_descr   TYPE REF TO cl_abap_tabledescr,
       go_table_row_descriptor TYPE REF TO cl_abap_structdescr,
       gv_content              TYPE xstring,
       git_binary_tab          TYPE TABLE OF sdokcntasc,
       gv_length               TYPE i,
       gv_filename1            TYPE string.

TYPES: BEGIN OF gtys_types,
         colname(255) TYPE c,
       END OF gtys_types.


DATA : go_itab      TYPE REF TO cl_abap_tabledescr,
       go_struct    TYPE REF TO cl_abap_structdescr,
       git_colnames TYPE STANDARD TABLE OF gtys_types,
       gst_colnames TYPE gtys_types.

DATA: go_ref TYPE REF TO data.

FIELD-SYMBOLS : <fs_table> TYPE STANDARD TABLE,
                <fs_comp>  LIKE LINE OF go_struct->components.

DATA : gv_filename TYPE string,
       gv_path     TYPE string,
       gv_fullpath TYPE string,
       gv_result   TYPE i,
       gv_default  TYPE string,
       gv_fname    TYPE string.

TYPES : BEGIN OF gtys_dtab,
          tabname TYPE tabname,
          ddtext  TYPE as4text,
        END OF gtys_dtab.

DATA : git_table TYPE TABLE OF gtys_dtab,
       gst_table TYPE gtys_dtab.

TYPES: BEGIN OF gtys_tabf4,
         tabname TYPE tabname,
         ddtext  TYPE as4text,
       END OF gtys_tabf4.

DATA : git_tabf4 TYPE TABLE OF gtys_tabf4,
       gst_tabf4 TYPE gtys_tabf4.

CONSTANTS: gc_fldnamef1  TYPE string VALUE `F0001`,
           gc_dyfldname1 TYPE string VALUE `P_TABLE`,
           gc_fldnamef2  TYPE string VALUE `F0002`,
           gc_dyfldname2 TYPE string VALUE `P_DESC`.

CONSTANTS: gc_fldnamef3  TYPE string VALUE `F0001`,
           gc_dyfldname3 TYPE string VALUE `P_TABLE2`,
           gc_fldnamef4  TYPE string VALUE `F0002`,
           gc_dyfldname4 TYPE string VALUE `P_DESC2`.


DATA : git_mapping TYPE STANDARD TABLE OF dselc,
       gst_mapping TYPE dselc,
       git_ret     TYPE TABLE OF ddshretval,
       gst_ret     TYPE ddshretval.

DATA : gv_val TYPE string.

DATA: git_dyn_tab             TYPE REF TO data.

FIELD-SYMBOLS : <lfs_dyn_tab> TYPE ANY TABLE.
