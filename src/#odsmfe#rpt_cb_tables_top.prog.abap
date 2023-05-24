*&-----------------------------------------------------------------------*
*&  Include           /ODSMFE/RPT_CB_TABLES_TOP
*&-----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

* types
TYPES: BEGIN OF gtys_types,
         colname(255) TYPE c,
       END OF gtys_types.

TYPES : BEGIN OF gtys_dtab,
          tabname TYPE tabname,
          ddtext  TYPE as4text,
        END OF gtys_dtab.

TYPES : BEGIN OF gtys_tabf4,
          tabname TYPE tabname,
          ddtext  TYPE as4text,
        END OF gtys_tabf4.

* internal tables and structures
DATA :git_colnames   TYPE STANDARD TABLE OF gtys_types,                "internal table for gtys_types
      gst_colnames   TYPE gtys_types,                                  "Structure type for gtys_types
      git_table      TYPE TABLE OF gtys_dtab,                          "internal table for gtys_dtab
      gst_table      TYPE gtys_dtab,                                   "Structure type for gtys_dtab
      git_tabf4      TYPE TABLE OF gtys_tabf4,                         "internal table for F4 help
      gst_tabf4      TYPE gtys_tabf4,                                  "Structure type for F4 help
      git_mapping    TYPE STANDARD TABLE OF dselc,                     "internal table for mapping
      gst_mapping    TYPE dselc,                                       "Structure type for mapping
      git_ret        TYPE TABLE OF ddshretval,                         "internal table
      gst_ret        TYPE ddshretval,                                  "Structure type
      git_binary_tab TYPE TABLE OF sdokcntasc.



* global variables
DATA : gv_filename  TYPE string,                                       "string
       gv_path      TYPE string,                                       "string
       gv_fullpath  TYPE string,                                       "string
       gv_result    TYPE i,                                            "int
       gv_default   TYPE string,                                       "string
       gv_fname     TYPE string,                                       "string
       gv_val       TYPE string,                                       "string
       gv_content   TYPE xstring,                                      "xstring
       gv_length    TYPE i,                                            "int
       gv_filename1 TYPE string.                                       "string

* reference variables
DATA : go_excel_structure      TYPE REF TO data,                      "any data
       go_source_table_descr   TYPE REF TO cl_abap_tabledescr,        "Runtime Type Services
       go_table_row_descriptor TYPE REF TO cl_abap_structdescr,       "Runtime Type Services
       go_itab                 TYPE REF TO cl_abap_tabledescr,        "Runtime Type Services
       go_struct               TYPE REF TO cl_abap_structdescr,       "Runtime Type Services
       go_dyn_tab              TYPE REF TO data,                      "any data
       go_ref                  TYPE REF TO data,                      "any data
       go_alv                  TYPE REF TO cl_salv_table,             "Basis Class for Simple Tables
       go_msg                  TYPE REF TO cx_salv_msg,               "ALV: General Error Class with Message
       go_text                 TYPE REF TO cx_root.                   "Abstract Superclass for All Global Exceptions

* global constants
CONSTANTS : gc_fldnamef1  TYPE string VALUE 'F0001',                  "constant value
            gc_dyfldname1 TYPE string VALUE 'P_TABLE',                "constant value
            gc_fldnamef2  TYPE string VALUE 'F0002',                  "constant value
            gc_dyfldname2 TYPE string VALUE 'P_DESC',                 "constant value

            gc_fldnamef3  TYPE string VALUE 'F0001',                  "constant value
            gc_dyfldname3 TYPE string VALUE 'P_TABLE2',               "constant value
            gc_fldnamef4  TYPE string VALUE 'F0002',                  "constant value
            gc_dyfldname4 TYPE string VALUE 'P_DESC2',                "constant value
            gc_window     TYPE string VALUE 'enter file name'.        ##NO_TEXT
*            gc_window     TYPE string VALUE text-004.                 ##NO_TEXT
* field symbols
FIELD-SYMBOLS : <gfsst_dyn_tab> TYPE ANY TABLE,                       "any table
                <gfsst_table>   TYPE STANDARD TABLE,                  "any standard table
                <gfsst_comp>    LIKE LINE OF go_struct->components.   "any structure
