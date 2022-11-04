class /ODSMFE/CL_DISPLAYFORMS_LIST definition
  public
  inheriting from CL_GOS_SERVICE
  create public .

public section.
  type-pools ABAP .

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_DISPLAYFORMS_LIST IMPLEMENTATION.


  METHOD execute.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  02/04/2020
* Transport No.          : ES1K901528
* Program Description    : Method to display total forms list
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------

    DATA: lv_aufnr TYPE aufnr.

    FIELD-SYMBOLS : <lfsit_aufnr> TYPE caufvd-aufnr.
*  constants
    CONSTANTS:lc_browser TYPE string VALUE 'chrome.exe',
              lc_iw33    TYPE string VALUE 'IW33'.
*----------------------------------------------------------------------
* Main Section
*----------------------------------------------------------------------

    IF sy-tcode EQ lc_iw33.

      ASSIGN ('(SAPLCOIH)CAUFVD-AUFNR') TO <lfsit_aufnr>.
      IF  sy-subrc = 0.
        lv_aufnr = <lfsit_aufnr>.
      ENDIF.

      CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
        EXPORTING
          im_wo         = lv_aufnr
          im_mode       = ' '
          im_browser    = lc_browser
          im_instanceid = ' '.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
