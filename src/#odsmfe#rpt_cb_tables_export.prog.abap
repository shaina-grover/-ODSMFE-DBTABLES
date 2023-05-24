*&---------------------------------------------------------------------*
*& Report  /ODSMFE/RPT_CB_TABLES_EXPORT
*&---------------------------------------------------------------------*
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  ODS-HSANGAM
* Creation Date          :  24/02/2023
* Transport No.          :  ES1K903617
* Program Description    :  Upload and Download cb table data report -
*                          Checksheet Builder table data into excel
***********************************************************************
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT /odsmfe/rpt_cb_tables_export.

INCLUDE /odsmfe/rpt_cb_tables_top.        "Declarations
INCLUDE /odsmfe/cb_tables_selection.      "Selection Screen

START-OF-SELECTION.
**download and upload data
  IF rb_dwnld EQ  'X'.
    PERFORM /odsmfe/fo_download.          "download table data
  ELSEIF rb_dwnd2 EQ  'X'.
    PERFORM /odsmfe/fo_download_stru.     "download table structure
  ELSEIF rb_uplod EQ 'X'.
    PERFORM /odsmfe/fo_upload.            "upload excel data to db table
  ENDIF."/ IF rb_dwnld EQ  'X'.

  INCLUDE /odsmfe/rpt_cb_tables_exporf01. "Sub-routines
