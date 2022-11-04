class /ODSMFE/CL_CHARACTERISTICS definition
  public
  final
  create public .

public section.

  methods EQUIPMENT_CHAR_UPDATE
    importing
      !IM_WORKORDER type AUFNR
      !IM_CHAR type /ODSMFE/EQ_CHAR_TT
    exporting
      !EX_RETURN type BAPIRET2 .
  methods UPDATE_CHARACTERSTICS
    importing
      value(LV_OBJECT) type BAPI1003_KEY-OBJECT
      !LV_CLASSNUM type BAPI1003_KEY-CLASSNUM
      !LV_KLART type BAPI1003_KEY-CLASSTYPE
      !IM_CHAR type /ODSMFE/EQ_CHAR_TT .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CHARACTERISTICS IMPLEMENTATION.


  METHOD equipment_char_update.

*--kmadhuri
    CONSTANTS : lc_equi TYPE bapi1003_key-objecttable VALUE 'EQUI',
                lc_e    TYPE char1 VALUE 'E',
                lc_a    TYPE char1 VALUE 'A',
                lc_x    TYPE char1 VALUE 'X'.

    DATA : ls_char TYPE /odsmfe/eq_char.


    DATA: lv_equnr    TYPE equnr,
          lv_tplnr    TYPE tplnr,
          lv_object   TYPE bapi1003_key-object,
          lit_return  TYPE TABLE OF bapiret2,
          lv_classnum TYPE bapi1003_key-classnum,
          lv_klart    TYPE bapi1003_key-classtype,
          lv_message  TYPE bapi_msg,
          lst_return  TYPE bapiret2,
          lit_char    TYPE TABLE OF bapi1003_alloc_values_char.
    "Classification BAPI - Values of Type CHAR, BOOL

    IF NOT im_workorder IS INITIAL.

      READ TABLE im_char ASSIGNING FIELD-SYMBOL(<lfs_char>) INDEX 1.
      IF sy-subrc = 0.
        MOVE <lfs_char>-class TO lv_classnum.
      ENDIF.
*      IF <lfs_char>-woheadereq = lc_x OR
*      <lfs_char>-wooperationeq = lc_x
*     OR <lfs_char>-notficationeq = lc_x.
*        lv_klart    = '002'."for equipment
**                ELSEIF <lfs_char>-wo_object = lc_x.
**                  lv_klart = '016'."Object link
*      ELSE.
*        lv_klart = '003'."Function location
*      ENDIF.

      SELECT SINGLE aufnr ,equnr,iloan, qmnum
        FROM afih INTO @DATA(ls_equnr)
        WHERE aufnr = @im_workorder.
      IF sy-subrc = 0.
        IF NOT ls_equnr-equnr IS INITIAL.
          MOVE ls_equnr-equnr TO lv_equnr.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_equnr
            IMPORTING
              output = lv_equnr.

          CLEAR: lv_object.

          MOVE lv_equnr TO lv_object.
          IF <lfs_char>-woheadereq = lc_x.
            lv_klart    = '002'.
*--Update charactaerstics of the equipment , functional location
            CALL METHOD me->update_characterstics
              EXPORTING
                lv_object   = lv_object
                lv_classnum = lv_classnum
                lv_klart    = lv_klart
                im_char     = im_char.
            CLEAR: lv_equnr.
          ENDIF.

          IF <lfs_char>-woheaderfl = lc_x.
            lv_klart = '003'."Function location
            SELECT SINGLE iloan, tplnr FROM iloa
              INTO @DATA(ls_iloa)
              WHERE iloan = @ls_equnr-iloan.
            IF sy-subrc = 0.
              MOVE ls_iloa-tplnr TO lv_tplnr.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_tplnr
                IMPORTING
                  output = lv_tplnr.
              MOVE lv_tplnr TO lv_object.
*--update charactaerstics of the equipment , functional location
              CALL METHOD me->update_characterstics
                EXPORTING
                  lv_object   = lv_object
                  lv_classnum = lv_classnum
                  lv_klart    = lv_klart
                  im_char     = im_char.
              CLEAR: lv_tplnr.
            ENDIF.
          ENDIF.
          IF <lfs_char>-notficationeq = lc_x.
            lv_klart    = '002'.
            SELECT SINGLE qmnum,iloan,equnr
              FROM qmih INTO @DATA(ls_qmequi)
              WHERE qmnum = @ls_equnr-qmnum.
            IF sy-subrc = 0.
              IF NOT ls_qmequi IS INITIAL.
                MOVE ls_qmequi-equnr TO lv_equnr.
*                IF NOT ls_equnr IS INITIAL.
*                  MOVE ls_equnr-equnr TO lv_equnr.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lv_equnr
                    IMPORTING
                      output = lv_equnr.

                  CLEAR:  lv_object.
                  MOVE lv_equnr TO lv_object.
*--Update charactaerstics of the equipment , functional location
                  CALL METHOD me->update_characterstics
                    EXPORTING
                      lv_object   = lv_object
                      lv_classnum = lv_classnum
                      lv_klart    = lv_klart
                      im_char     = im_char.
                  CLEAR: lv_equnr,lv_tplnr.
                  IF <lfs_char>-notficationfl = lc_x.
                    lv_klart = '003'."Function location
                    SELECT SINGLE iloan, tplnr FROM iloa
                    INTO @DATA(ls_iloa1)
                          WHERE iloan = @ls_equnr-iloan.
                    IF sy-subrc = 0.
                      MOVE ls_iloa1-tplnr TO lv_tplnr.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lv_tplnr
                        IMPORTING
                          output = lv_tplnr.
                      MOVE lv_tplnr TO lv_object.
*--update charactaerstics of the equipment , functional location
                      CALL METHOD me->update_characterstics
                        EXPORTING
                          lv_object   = lv_object
                          lv_classnum = lv_classnum
                          lv_klart    = lv_klart
                          im_char     = im_char.
                      CLEAR: lv_tplnr.
                    ENDIF.
                  ENDIF.
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update_characterstics.
*--KMADHURI
    CONSTANTS : lc_equi TYPE bapi1003_key-objecttable VALUE 'EQUI',
                lc_iloa TYPE  bapi1003_key-objecttable VALUE 'IFLOT',
                lc_e    TYPE char1 VALUE 'E',
                lc_a    TYPE char1 VALUE 'A'.
    DATA :  lit_return TYPE TABLE OF bapiret2,
            lv_objtab  TYPE bapi1003_key-objecttable,
            lit_char   TYPE TABLE OF bapi1003_alloc_values_char,
            lit_num    TYPE TABLE OF bapi1003_alloc_values_num,
            lit_curr   TYPE TABLE OF bapi1003_alloc_values_curr,
            lv_message TYPE bapi_msg,
            lst_return TYPE bapiret2.


    CLEAR: lit_return,lv_objtab.
    IF lv_klart = '002'.
      lv_objtab = lc_equi.
    ELSE.
      lv_objtab = lc_iloa.
    ENDIF.
* --Get Equipment charateristics
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_object
        objecttable     = lv_objtab
        classnum        = lv_classnum
        classtype       = lv_klart
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lit_num
        allocvalueschar = lit_char
        allocvaluescurr = lit_curr
        return          = lit_return.


    LOOP AT im_char ASSIGNING FIELD-SYMBOL(<lfs_char>)." WITH KEY charact = <lfs_char1>-charact.
      READ TABLE lit_char ASSIGNING FIELD-SYMBOL(<lfs_char1>) WITH KEY charact = <lfs_char>-charact.
      IF sy-subrc = 0.
        MOVE <lfs_char>-value TO <lfs_char1>-value_char.
        MOVE <lfs_char>-value TO <lfs_char1>-value_neutral.
      ELSE.
        READ TABLE lit_num ASSIGNING FIELD-SYMBOL(<lfs_char2>) WITH KEY charact = <lfs_char>-charact.
        IF sy-subrc = 0.
          MOVE <lfs_char>-value TO <lfs_char2>-value_from.
        ELSE.
          READ TABLE lit_curr ASSIGNING  FIELD-SYMBOL(<lfs_char3>) WITH KEY charact = <lfs_char>-charact.
          IF sy-subrc = 0.
            MOVE <lfs_char>-value TO <lfs_char3>-value_from.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CLEAR: lit_return.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey          = lv_object
        objecttable        = lv_objtab
        classnum           = lv_classnum
        classtype          = lv_klart
        status             = '1'
        keydate            = sy-datum
      TABLES
        allocvaluesnumnew  = lit_num
        allocvaluescharnew = lit_char
        allocvaluescurrnew = lit_curr
        return             = lit_return.

    LOOP AT lit_return ASSIGNING FIELD-SYMBOL(<lfsst_return>).
      IF <lfsst_return>-type = lc_e OR <lfsst_return>-type = lc_a.
        lv_message = <lfsst_return>-message.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = lv_message.
      ENDIF.
    ENDLOOP.
* Transaction Commit
    CLEAR lst_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = lst_return.
* Error handling
    IF lst_return-type EQ lc_e OR lst_return-type EQ lc_a.
      lv_message = lst_return-message.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = lv_message.
    ENDIF.
    CLEAR: lit_char.


  ENDMETHOD.
ENDCLASS.
