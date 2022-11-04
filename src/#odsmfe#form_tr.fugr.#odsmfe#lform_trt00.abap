*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/FORM_TR.................................*
DATA:  BEGIN OF STATUS_/ODSMFE/FORM_TR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/FORM_TR               .
CONTROLS: TCTRL_/ODSMFE/FORM_TR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/FORM_TR               .
TABLES: /ODSMFE/FORM_TR                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
