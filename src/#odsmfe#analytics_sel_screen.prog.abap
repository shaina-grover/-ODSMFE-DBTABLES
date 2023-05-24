*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/ANALYTICS_SEL_SCREEN
*&---------------------------------------------------------------------*

"/ selection screen declaration for table input
SELECT-OPTIONS: s_forms FOR gv_formid1-formid   NO INTERVALS NO-EXTENSION OBLIGATORY.         "FormId
SELECT-OPTIONS: s_vrsn  FOR gst_formrsp-version NO INTERVALS NO-EXTENSION OBLIGATORY.         "Version
SELECT-OPTIONS: s_user  FOR gv_user-aname       NO-EXTENSION NO INTERVALS.                    "User
SELECT-OPTIONS: s_date  FOR sy-datum.                                                         "Date
