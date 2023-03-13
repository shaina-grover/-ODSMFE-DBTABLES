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
* Program Author (SID)   : SKAMMARI
* Creation Date          : 24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to display forms based on Validation
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Change Date            : 09.03.2023
* Transport No.          : ES1K903619
* Change Description     : Added logic to display the Forms tab based on the Notification
***********************************************************************
*-------------------------------------------------------------*
*                D A T A    D E C L A R A T I O N             *
*-------------------------------------------------------------*

  "/ Variables
  DATA: lv_aufnr TYPE aufnr,
        lv_auart TYPE auart,
        lv_qmart TYPE qmart,
        lv_qmnum TYPE qmnum.

*-------------------------------------------------------------*
*        E N D   O F   D A T A   D E C L A R A T I O N        *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
*                   M A I N   S E C T I O N                   *
*-------------------------------------------------------------*
*  ep_status = mp_status_active.
  ep_status = mp_status_invisible.

  IF is_lporb-typeid = 'BUS2007'.

    lv_aufnr  = is_lporb-instid.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_aufnr
      IMPORTING
        output = lv_aufnr.

    SELECT SINGLE auart FROM aufk INTO lv_auart
          WHERE aufnr = lv_aufnr.
    IF sy-subrc EQ 0.

      SELECT SINGLE ordertype FROM /odsmfe/tb_foass INTO lv_auart
      WHERE formid NE space AND ordertype EQ lv_auart.
      IF sy-subrc EQ 0.

        SELECT SINGLE wo_num INTO lv_aufnr FROM /odsmfe/tb_forsp
        WHERE  instanceid NE space
        AND wo_num = lv_aufnr.
        IF sy-subrc EQ 0.
          ep_status = mp_status_active.
        ELSE.
          ep_status = mp_status_active.
        ENDIF.
      ENDIF.
    ENDIF.

*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
  ELSEIF is_lporb-typeid = 'BUS2038'.
    lv_qmnum = is_lporb-instid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_qmnum
      IMPORTING
        output = lv_qmnum.

    SELECT SINGLE qmart FROM qmel INTO lv_qmart
      WHERE qmnum = lv_qmnum.

    IF sy-subrc = 0.

      SELECT SINGLE ordertype FROM /odsmfe/tb_foass
        INTO lv_qmart
        WHERE formid    <> space
          AND ordertype = lv_qmart.

      IF sy-subrc = 0.
        SELECT SINGLE wo_num INTO lv_aufnr FROM /odsmfe/tb_forsp
          WHERE instanceid <> space
            AND wo_num     = lv_aufnr.

        IF sy-subrc = 0.
          ep_status = mp_status_active.
        ELSE.
          ep_status = mp_status_active.
        ENDIF.
      ENDIF.
    ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
  ENDIF.
ENDMETHOD.


  METHOD execute.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to display forms
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 24/12/2020
* Transport No.          : ES1K902363
* Change Description     : Addition of task list forms assignment functionality
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Change Date            : 12.03.2023
* Transport No.          : ES1K903619
* Change Description     : Added the logic to get the Forms list based in the Notification type
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
          lv_warpl TYPE warpl,
*   EOC by ODS ES1K902363
          lv_qmnum TYPE qmnum,
          lv_qmart TYPE qmart.

    FIELD-SYMBOLS: <lfsit_aufnr> TYPE caufvd-aufnr,
                   <lfsit_auart> TYPE caufvd-auart,
*   SOC by ODS ES1K902363
                   <lfsit_plnty> TYPE caufvd-plnty,
                   <lfsit_plnnr> TYPE caufvd-plnnr,
                   <lfsit_plnal> TYPE caufvd-plnal,
                   <lfsit_zaehl> TYPE caufvd-zaehl,
                   <lfsit_arsps> TYPE caufvd-arsps,
                   <lfsit_warpl> TYPE caufvd-warpl,
*   SOC by ODS ES1K902363
                   <lfsit_qmnum> TYPE viqmel-qmnum,
                   <lfsit_qmart> TYPE viqmel-qmart.

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

*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
    ELSEIF sy-tcode EQ 'IW23' OR sy-tcode EQ 'IW22'.
      ASSIGN ('(SAPLIQS0)VIQMEL-QMNUM') TO <lfsit_qmnum>.
      IF sy-subrc = 0.
        lv_qmnum = <lfsit_qmnum>.
      ENDIF.
      ASSIGN ('(SAPLIQS0)VIQMEL-QMART') TO <lfsit_qmart>.
      IF sy-subrc = 0.
        lv_qmart = <lfsit_qmart>.
      ENDIF.
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNTY') TO <lfsit_plnty>.
      IF sy-subrc = 0.
        lv_plnty = <lfsit_plnty>.
      ENDIF.
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNNR') TO <lfsit_plnnr>.
      IF sy-subrc = 0.
        lv_plnnr = <lfsit_plnnr>.
      ENDIF.
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNAL') TO <lfsit_plnal>.
      IF sy-subrc = 0.
        lv_plnal = <lfsit_plnal>.
      ENDIF.

      CALL FUNCTION '/ODSMFE/FM_FORMS'
        EXPORTING
          im_qmnum = lv_qmnum
          im_qmart = lv_qmart
          im_plnty = lv_plnty
          im_plnnr = lv_plnnr
          im_plnal = lv_plnal
          im_zaehl = lv_zaehl.

*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
    ENDIF. " IF SY-TCODE EQ 'IW33'

  ENDMETHOD.
ENDCLASS.
