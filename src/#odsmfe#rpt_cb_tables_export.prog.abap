*&---------------------------------------------------------------------*
*& Report  /ODSMFE/RPT_CB_TABLES_EXPORT
*&---------------------------------------------------------------------*
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  HSANGAM
* Creation Date          :  24/02/2023
* Transport No.          :  ES1K903617
* Program Description    : Upload and Download cb table data report -
*                          Checksheet Builder table data into excel
***********************************************************************
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT /odsmfe/rpt_cb_tables_export.

INCLUDE /odsmfe/rpt_cb_tables_top.    "Declarations
INCLUDE /odsmfe/cb_tables_selection.  "Selection Screen

START-OF-SELECTION.
**download and upload data
  IF rb_dwnld EQ  'X'.
    PERFORM download.                 "download table data to excel
  ELSEIF rb_uplod EQ 'X'.
    PERFORM upload.                   "upload excel data to db table
  ENDIF.

** downlod table structure
  IF p_table2 IS NOT INITIAL.
    PERFORM download_stru.            "download table structure
  ENDIF.

  INCLUDE /odsmfe/rpt_cb_tables_f01.  "Sub-routines
