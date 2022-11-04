class /ODSMFE/CL_DISPLAYFORMS definition
  public
  inheriting from CL_GOS_SERVICE
  create public .

public section.
  type-pools ABAP .

  methods EXECUTE
    redefinition .
protected section.

  methods CHECK_STATUS
    redefinition .
private section.
ENDCLASS.



CLASS /ODSMFE/CL_DISPLAYFORMS IMPLEMENTATION.


METHOD check_status.


***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to display forms based on Validation
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

  DATA: lv_aufnr TYPE aufnr,
        lv_auart TYPE auart.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
  ep_status = mp_status_active.
*  ep_status = mp_status_invisible.

*  IF is_lporb-typeid = 'BUS2007'.
*
*    lv_aufnr  = is_lporb-instid.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_aufnr
*      IMPORTING
*        output = lv_aufnr.
*
*    SELECT SINGLE auart FROM aufk INTO lv_auart
*          WHERE aufnr = lv_aufnr.
*    IF sy-subrc EQ 0.
*
*      SELECT SINGLE ordertype FROM /odsmfe/tb_foass INTO lv_auart
*      WHERE FORMID NE space AND ordertype EQ lv_auart.
*      IF sy-subrc EQ 0.
*
*        SELECT SINGLE wo_num INTO lv_aufnr FROM /odsmfe/tb_forsp
*        WHERE  instanceid NE space
*        AND wo_num = lv_aufnr.
*        IF sy-subrc EQ 0.
*          ep_status = mp_status_active.
*        ELSE.
*          ep_status = mp_status_active.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDMETHOD.


  METHOD execute.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to display forms
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 24/12/2020
* Transport No.          : ES1K902363
* Change Description     : Addition of task list forms assignment functionality
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lv_aufnr TYPE aufnr,                                         "Order Number
          lv_auart TYPE aufart,                                        "Order Type
*   SOC by ODS ES1K902363
          lv_plnty TYPE plnty,
          lv_plnnr TYPE plnnr,
          lv_plnal TYPE plnal,
          lv_zaehl TYPE cim_count,
          lv_arsps TYPE co_posnr,
          lv_warpl TYPE warpl.
*   EOC by ODS ES1K902363
    FIELD-SYMBOLS: <lfsit_aufnr> TYPE caufvd-aufnr,
                   <lfsit_auart> TYPE caufvd-auart,
*   SOC by ODS ES1K902363
                   <lfsit_plnty> TYPE caufvd-plnty,
                   <lfsit_plnnr> TYPE caufvd-plnnr,
                   <lfsit_plnal> TYPE caufvd-plnal,
                   <lfsit_zaehl> TYPE caufvd-zaehl,
                   <lfsit_arsps> TYPE caufvd-arsps,
                   <lfsit_warpl> TYPE caufvd-warpl.
*   SOC by ODS ES1K902363
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    IF sy-tcode EQ 'IW33' OR sy-tcode EQ 'IW32'.

      ASSIGN ('(SAPLCOIH)CAUFVD-AUFNR') TO <lfsit_aufnr>.
      IF sy-subrc = 0.
        lv_aufnr = <lfsit_aufnr>.
      ENDIF.                                                           " IF SY-SUBRC = 0
      ASSIGN ('(SAPLCOIH)CAUFVD-AUART') TO <lfsit_auart>.
      IF sy-subrc = 0.
        lv_auart = <lfsit_auart>.
      ENDIF.
*   SOC by ODS ES1K902363
      ASSIGN ('(SAPLCOIH)CAUFVD-WARPL') TO <lfsit_warpl>.
      IF sy-subrc EQ 0.
        lv_warpl = <lfsit_warpl>.
      ENDIF.
      " operation Number population logic
      ASSIGN ('(SAPLCOIH)CAUFVD-ARSPS') TO <lfsit_arsps>.
      IF sy-subrc EQ 0.
        lv_arsps = <lfsit_arsps>.
      ENDIF.

      ASSIGN ('(SAPLCOIH)CAUFVD-PLNTY') TO <lfsit_plnty>.
      IF sy-subrc = 0.
        lv_plnty = <lfsit_plnty>.
      ENDIF.
      ASSIGN ('(SAPLCOIH)CAUFVD-PLNNR') TO <lfsit_plnnr>.
      IF sy-subrc = 0.
        lv_plnnr = <lfsit_plnnr>.
      ENDIF.
      ASSIGN ('(SAPLCOIH)CAUFVD-PLNAL') TO <lfsit_plnal>.
      IF sy-subrc = 0.
        lv_plnal = <lfsit_plnal>.
      ENDIF.
      ASSIGN ('(SAPLCOIH)CAUFVD-ZAEHL') TO <lfsit_zaehl>.
      IF sy-subrc = 0.
        lv_zaehl = <lfsit_zaehl>.
      ENDIF.
*   EOC by ODS ES1K902363
*      IF lv_aufnr IS NOT INITIAL AND lv_auart IS NOT INITIAL.

        CALL FUNCTION '/ODSMFE/FM_FORMS'
          EXPORTING
            im_aufnr = lv_aufnr
            im_auart = lv_auart
*   SOC by ODS ES1K902363
            im_plnty = lv_plnty
            im_plnnr = lv_plnnr
            im_plnal = lv_plnal
            im_zaehl = lv_zaehl.  " Pass Operation Number instead Internal Count
*   EOC by ODS  ES1K902363
        EXIT.

*      ENDIF.          " IF LV_AUFNR IS NOT INITIAL AND LV_AUART IS NOT INITIAL
    ENDIF.            " IF SY-TCODE EQ 'IW33'

  ENDMETHOD.
ENDCLASS.
