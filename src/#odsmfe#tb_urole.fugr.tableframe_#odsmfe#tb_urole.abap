*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/ODSMFE/TB_UROLE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/ODSMFE/TB_UROLE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
