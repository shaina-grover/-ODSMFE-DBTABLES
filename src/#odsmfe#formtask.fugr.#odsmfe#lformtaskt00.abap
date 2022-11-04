*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ODSMFE/FORMTASK................................*
DATA:  BEGIN OF STATUS_/ODSMFE/FORMTASK              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ODSMFE/FORMTASK              .
CONTROLS: TCTRL_/ODSMFE/FORMTASK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ODSMFE/FORMTASK              .
TABLES: /ODSMFE/FORMTASK               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
