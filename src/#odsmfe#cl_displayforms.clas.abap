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
  DATA:
    lv_aufnr TYPE aufnr,                             "Work Order
    lv_auart TYPE auart,                             "Order Type
    lv_qmart TYPE qmart,                             "Notification      - Added by ODS-VSANAGALA : ES1K903619
    lv_qmnum TYPE qmnum.                             "Notification Type - Added by ODS-VSANAGALA : ES1K903619

  "/ Constants
  CONSTANTS:
    lc_wo_object_type TYPE string VALUE 'BUS2007',   "Work Order Object type
    lc_no_object_type TYPE string VALUE 'BUS2038'.   "Notification Object type

*-------------------------------------------------------------*
*        E N D   O F   D A T A   D E C L A R A T I O N        *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
*                   M A I N   S E C T I O N                   *
*-------------------------------------------------------------*

  "/ Initially disabling the Forms Tab
  ep_status = mp_status_invisible.

  "/ Enable the Forms tab in the WorkOrder Transaction codes
  IF is_lporb-typeid = lc_wo_object_type.
    lv_aufnr  = is_lporb-instid.
    "/ Conversion for the WorkOrder Number
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_aufnr
      IMPORTING
        output = lv_aufnr.

    "/ Get the Order type for the requested Work Order
    SELECT SINGLE auart
      FROM aufk
      INTO lv_auart
      WHERE aufnr = lv_aufnr.

    IF sy-subrc EQ 0.
      "/ Check either the Forms are assigned to the requested work order type and enable the Forms tab.
      SELECT SINGLE ordertype
        FROM /odsmfe/tb_foass
        INTO lv_auart
        WHERE formid    NE space
          AND ordertype EQ lv_auart.

      IF sy-subrc EQ 0.
        "/Get the work order number from the Form response table
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
    "/ Enable the Forms tab in the Notification Transaction codes
  ELSEIF is_lporb-typeid = lc_no_object_type.
    lv_qmnum = is_lporb-instid.
    "/ Conversion for the Notification Number
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_qmnum
      IMPORTING
        output = lv_qmnum.

    "/ Get the Notification type for the requested Notification Number
    SELECT SINGLE qmart
      FROM qmel
      INTO lv_qmart
      WHERE qmnum = lv_qmnum.

    IF sy-subrc = 0.
      "/ Check either the Forms are assigned to the requested Notification type and enable the Forms tab.
      SELECT SINGLE ordertype
        FROM /odsmfe/tb_foass
        INTO lv_qmart
        WHERE formid    <> space
          AND ordertype = lv_qmart.

      IF sy-subrc = 0.
        "/Get the Notification number from the Form response table
        SELECT SINGLE wo_num INTO lv_qmnum FROM /odsmfe/tb_forsp
          WHERE instanceid <> space
            AND wo_num     = lv_qmnum.

        IF sy-subrc = 0.
          ep_status = mp_status_active.
        ELSE.
          ep_status = mp_status_active.
        ENDIF.

      ENDIF.
    ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
  ENDIF. "/ IF is_lporb-typeid = 'BUS2007'.
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

*-------------------------------------------------------------*
*                D A T A    D E C L A R A T I O N             *
*-------------------------------------------------------------*

    "/ Variables
    DATA:
      lv_aufnr TYPE aufnr,                          "Order Number
      lv_auart TYPE aufart,                         "Order Type
      lv_plnty TYPE plnty,                          "Task List Type           - Added by ODS ES1K902363
      lv_plnnr TYPE plnnr,                          "Key for Task List Group  - Added by ODS ES1K902363
      lv_plnal TYPE plnal,                          "Group Counter            - Added by ODS ES1K902363
      lv_zaehl TYPE cim_count,                      "Internal counter         - Added by ODS ES1K902363
      lv_arsps TYPE co_posnr,                       "Order Item Number        - Added by ODS ES1K902363
      lv_warpl TYPE warpl,                          "Maintenance Plan         - Added by ODS ES1K902363
      lv_qmnum TYPE qmnum,                          "Notification             - Added by ODS-VSANAGALA : ES1K903619
      lv_qmart TYPE qmart.                          "Notification Type        - Added by ODS-VSANAGALA : ES1K903619

    "/ Field-Symbols
    FIELD-SYMBOLS:
      <lfsst_aufnr> TYPE caufvd-aufnr,              "Order Number
      <lfsst_auart> TYPE caufvd-auart,              "Order Type
      <lfsst_plnty> TYPE caufvd-plnty,              "Task List Type           - Added by ODS ES1K902363
      <lfsst_plnnr> TYPE caufvd-plnnr,              "Key for Task List Group  - Added by ODS ES1K902363
      <lfsst_plnal> TYPE caufvd-plnal,              "Group Counter            - Added by ODS ES1K902363
      <lfsst_zaehl> TYPE caufvd-zaehl,              "Internal counter         - Added by ODS ES1K902363
      <lfsst_arsps> TYPE caufvd-arsps,              "Order Item Number        - Added by ODS ES1K902363
      <lfsst_warpl> TYPE caufvd-warpl,              "Maintenance Plan         - Added by ODS ES1K902363
      <lfsst_qmnum> TYPE viqmel-qmnum,              "Notification             - Added by ODS-VSANAGALA : ES1K903619
      <lfsst_qmart> TYPE viqmel-qmart.              "Notification Type        - Added by ODS-VSANAGALA : ES1K903619

*-------------------------------------------------------------*
*        E N D   O F   D A T A   D E C L A R A T I O N        *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
*                   M A I N   S E C T I O N                   *
*-------------------------------------------------------------*

    "/ Get the Forms assigned to the Order type in the respective WorkOrder transaction codes.
    IF sy-tcode EQ 'IW33' OR sy-tcode EQ 'IW32'.

      "/ Get the Work Order number
      ASSIGN ('(SAPLCOIH)CAUFVD-AUFNR') TO <lfsst_aufnr>.
      IF sy-subrc = 0.
        lv_aufnr = <lfsst_aufnr>.
      ENDIF.

      "/ Get the Order type
      ASSIGN ('(SAPLCOIH)CAUFVD-AUART') TO <lfsst_auart>.
      IF sy-subrc = 0.
        lv_auart = <lfsst_auart>.
      ENDIF.

*---------------------------------- SOC by ODS - ES1K902363 ----------------------------------*
      "/ Get the Maintenance Plan
      ASSIGN ('(SAPLCOIH)CAUFVD-WARPL') TO <lfsst_warpl>.
      IF sy-subrc EQ 0.
        lv_warpl = <lfsst_warpl>.
      ENDIF.

      "/ Get the Operation Number
      ASSIGN ('(SAPLCOIH)CAUFVD-ARSPS') TO <lfsst_arsps>.
      IF sy-subrc EQ 0.
        lv_arsps = <lfsst_arsps>.
      ENDIF.

      "/ Get the Task List Type
      ASSIGN ('(SAPLCOIH)CAUFVD-PLNTY') TO <lfsst_plnty>.
      IF sy-subrc = 0.
        lv_plnty = <lfsst_plnty>.
      ENDIF.

      "/ Get the Key for Task List Group
      ASSIGN ('(SAPLCOIH)CAUFVD-PLNNR') TO <lfsst_plnnr>.
      IF sy-subrc = 0.
        lv_plnnr = <lfsst_plnnr>.
      ENDIF.

      "/ Get the Group Counter
      ASSIGN ('(SAPLCOIH)CAUFVD-PLNAL') TO <lfsst_plnal>.
      IF sy-subrc = 0.
        lv_plnal = <lfsst_plnal>.
      ENDIF.

      "/ Assign space to  TaskList type if group and group counter is empty
      IF lv_plnnr IS INITIAL AND lv_plnal IS INITIAL.
        lv_plnty = space.
      ENDIF.

      "/ Get the Internal counter
      ASSIGN ('(SAPLCOIH)CAUFVD-ZAEHL') TO <lfsst_zaehl>.
      IF sy-subrc = 0.
        lv_zaehl = <lfsst_zaehl>.
      ENDIF.
*---------------------------------- EOC by ODS - ES1K902363 ----------------------------------*

      "/ Calling Function Module to get the Forms Data for the requested WorkOrders
      CALL FUNCTION '/ODSMFE/FM_FORMS'
        EXPORTING
          im_aufnr = lv_aufnr
          im_auart = lv_auart
          im_plnty = lv_plnty     " Added by ODS ES1K902363
          im_plnnr = lv_plnnr     " Added by ODS ES1K902363
          im_plnal = lv_plnal     " Added by ODS ES1K902363
          im_zaehl = lv_zaehl.    " Added by ODS ES1K902363
      EXIT.

*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
      "/ Get the Forms assigned to the Notification type in the respective Notification transaction codes.
    ELSEIF sy-tcode EQ 'IW23' OR sy-tcode EQ 'IW22'.

      "/ Get the Notification Number
      ASSIGN ('(SAPLIQS0)VIQMEL-QMNUM') TO <lfsst_qmnum>.
      IF sy-subrc = 0.
        lv_qmnum = <lfsst_qmnum>.
      ENDIF.

      "/ Get the Notification type
      ASSIGN ('(SAPLIQS0)VIQMEL-QMART') TO <lfsst_qmart>.
      IF sy-subrc = 0.
        lv_qmart = <lfsst_qmart>.
      ENDIF.

      "/ Get the Task List Type
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNTY') TO <lfsst_plnty>.
      IF sy-subrc = 0.
        lv_plnty = <lfsst_plnty>.
      ENDIF.

      "/ Get the Key for Task List Group
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNNR') TO <lfsst_plnnr>.
      IF sy-subrc = 0.
        lv_plnnr = <lfsst_plnnr>.
      ENDIF.

      "/ Get the Group Counter
      ASSIGN ('(SAPLIQS0)VIQMEL-PLNAL') TO <lfsst_plnal>.
      IF sy-subrc = 0.
        lv_plnal = <lfsst_plnal>.
      ENDIF.

      "/ Calling Functiona Module to get the Forms data for the requested Notifications
      CALL FUNCTION '/ODSMFE/FM_FORMS'
        EXPORTING
          im_qmnum = lv_qmnum
          im_qmart = lv_qmart
          im_plnty = lv_plnty
          im_plnnr = lv_plnnr
          im_plnal = lv_plnal
          im_zaehl = lv_zaehl.

*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
    ENDIF. "/ IF sy-tcode EQ 'IW33' OR sy-tcode EQ 'IW32'.

  ENDMETHOD.
ENDCLASS.
