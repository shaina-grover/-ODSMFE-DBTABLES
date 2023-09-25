CLASS /odsmfe/cl_characteristics_v2 DEFINITION
  PUBLIC
    FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: tab TYPE STANDARD TABLE OF REF TO /odsmfe/cl_characteristics_v2 WITH DEFAULT KEY.

*    TYPES: BEGIN OF ty_range_option,
*             sign   TYPE c LENGTH 1,
*             option TYPE c LENGTH 2,
*             low    TYPE string,
*             high   TYPE string,
*           END OF ty_range_option.
    TYPES:
      BEGIN OF ty_data,
        wa(512) TYPE c,
      END OF ty_data.

    TYPES: BEGIN OF ty_options,
             text(72) TYPE c,
           END OF ty_options.

    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.

    DATA: lt_options        TYPE TABLE OF ty_options,
          lt_fields         TYPE TABLE OF ty_fields,
          lt_data           TYPE TABLE OF ty_data,

          lrs_filter_values TYPE /odsmfe/st_workorder_fil_vals,
          lv_aufnr          TYPE aufnr,
          lrs_workorder     TYPE /odsmfe/st_core_range_str.

*          lrs_WoNum         TYPE TABLE OF ty_range_option,
*          lrs_locacct       TYPE TABLE OF ty_range_option,
*          lrs_notifnum      TYPE TABLE OF ty_range_option.

*    DATA : lit_char     TYPE TABLE OF bapi1003_alloc_values_char,
*           lit_val_char TYPE TABLE OF bapi1003_alloc_values_char.

*SOC BY LMETTA

    TYPES : BEGIN OF GTY_bapi1003_alloc_values_char,

              charact(30)       TYPE c,
              value_char(30)    TYPE c,
              inherited(1)      TYPE c,
              instance(1)       TYPE c,
              value_neutral(30) TYPE c,
              charact_descr(30) TYPE c,
            END OF GTY_bapi1003_alloc_values_char.

    DATA : lit_char     TYPE TABLE OF GTY_bapi1003_alloc_values_char,
           lit_val_char TYPE TABLE OF GTY_bapi1003_alloc_values_char.


    TYPES  : BEGIN OF gty_bapi1003_alloc_values_num,

               charact(30)       TYPE c,
               value_from        TYPE f,
               value_to          TYPE f,
               value_relation(1) TYPE c,
               unit_from(3)      TYPE c,    "u,
               unit_to(3)        TYPE c,    "u,
               unit_from_iso(3)  TYPE c,
               unit_to_iso(3)    TYPE c,
               inherited(1)      TYPE c,
               instance(3)       TYPE n,
               charact_descr(30) TYPE c,
             END OF gty_bapi1003_alloc_values_num.

    DATA : lit_num    TYPE TABLE OF gty_bapi1003_alloc_values_num.


    TYPES  : BEGIN OF gty_bapi1003_alloc_values_curr,

               charact(30)          TYPE c,
               value_from           TYPE f,
               value_to             TYPE f,
               value_relation(1)    TYPE c,
               currency_from(5)     TYPE c,           "CUKY
               currency_to(5)       TYPE c,               "CUKY
               currency_from_iso(3) TYPE c,             "CUKY
               inherited(1)         TYPE c,
               instance(3)          TYPE n,
               charact_descr(30)    TYPE c,


             END OF gty_bapi1003_alloc_values_curr.

  DATA : lit_curr    TYPE TABLE OF gty_bapi1003_alloc_values_curr.




*EOC BY LMETTA

*      lit_table         TYPE TABLE OF /ODSMFE/CE_FormAttachment.

*    DATA: lt_workorder           TYPE STANDARD TABLE OF /odsmfe/cds_workorder,
*
*          lrs_equipment          TYPE  TABLE OF ty_range_option,
*          lst_equipment          TYPE /odsmfe/tb_fmatt,
*          lit_workorder          TYPE TABLE OF /odsmfe/tb_wo,
*          lst_workorder          TYPE  /odsmfe/tb_wo,
*          lst_equipmentnum       TYPE /odsmfe/tb_fmatt,
*          lst_notificationnumber TYPE /odsmfe/tb_fmass,
*          lit_notificationnumber TYPE TABLE OF /odsmfe/tb_fmass.

    DATA: lv_rowskip  TYPE int4,
          lv_rowcount TYPE int4.

    DATA: lo_filter  TYPE REF TO if_rap_query_filter,
          lit_return TYPE  if_rap_query_filter=>tt_name_range_pairs.

    CONSTANTS: lc_workordernumber    TYPE string VALUE 'WorkOrderNumber',
               lc_equipmentnumber    TYPE string VALUE 'EquipmentNumber',
               lc_notificationnumber TYPE string VALUE 'NotificationNumber'.


    TYPES: BEGIN OF ty_BAPI1003,
             classnum     TYPE c LENGTH 18,
             classtype    TYPE c LENGTH 3,
             object       TYPE c LENGTH 50,
             objecttable  TYPE c LENGTH 30,
             keydate      TYPE c LENGTH 8,
             status       TYPE c LENGTH 1,
             changenumber TYPE c LENGTH 12,
             stdclass     TYPE c LENGTH 1,
             flag         TYPE c LENGTH 1,
             object_guid  TYPE n LENGTH 18,

           END OF ty_BAPI1003.

    TYPES: BEGIN OF ty_afih,
             aufnr TYPE c LENGTH 12,
             equnr TYPE c LENGTH 18,
             iloan TYPE c LENGTH 12,
             qmnum TYPE c LENGTH 12,
             tplnr TYPE c LENGTH 30,

           END OF ty_afih.

    DATA : lt_afih TYPE TABLE OF ty_afih,
           ls_afih TYPE ty_afih.

    DATA:  ls_qmequi  TYPE  ty_afih.

    DATA: ls_BAPI1003 TYPE ty_BAPI1003.

*    DATA : lv_rfc_dest_name TYPE string.
    DATA : lv_rfc_dest TYPE string.
    DATA : lv_rfc TYPE string.

    METHODS equipment_char_update
      IMPORTING
        !im_workorder TYPE aufnr
        !im_char      TYPE /odsmfe/eq_char_tt
      EXPORTING
        !ex_return    TYPE bapiret2 .
    METHODS update_characterstics
      IMPORTING
*      value(LV_OBJECT) type BAPI1003_KEY-OBJECT
        VALUE(lv_object) TYPE ty_BAPI1003-object
*      !LV_CLASSNUM type BAPI1003_KEY-CLASSNUM
        !lv_classnum     TYPE ty_BAPI1003-classnum
*     !LV_KLART type BAPI1003_KEY-CLASSTYPE
        !lv_klart        TYPE ty_BAPI1003-classtype
        !im_char         TYPE /odsmfe/eq_char_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_CHARACTERISTICS_V2 IMPLEMENTATION.


  METHOD equipment_char_update.

*--kmadhuri

    CONSTANTS : lc_equi TYPE  ty_BAPI1003-objecttable VALUE 'EQUI' ,   " bapi1003_key-objecttable VALUE 'EQUI',
                lc_e    TYPE char1 VALUE 'E',
                lc_a    TYPE char1 VALUE 'A',
                lc_x    TYPE char1 VALUE 'X'.


    DATA : ls_char TYPE /odsmfe/eq_char.


    DATA: lv_equnr    TYPE /odsmfe/tb_fmass-equipment,          "equnr,
          lv_tplnr    TYPE /odsmfe/tb_fmass-functionallocation, "tplnr,
*         lv_object   TYPE bapi1003_key-object,
          lv_object   TYPE  ty_BAPI1003-object,

          lit_return  TYPE TABLE OF bapiret2,
          lv_classnum TYPE ty_BAPI1003-classnum,     "bapi1003_key-classnum,
          lv_klart    TYPE ty_BAPI1003-classtype,    "bapi1003_key-classtype,
          lv_message  TYPE bapi_msg,
          lst_return  TYPE bapiret2,
          lit_char    TYPE TABLE OF /odsmfe/eq_char. "bapi1003_alloc_values_char.

*   DATA : lr_rfc TYPE string.

*DATA: lo_obj TYPE REF TO /odsmfe/cl_get_ent_super_bapi.
*      lo_obj->get_cloud_dest.


    "Classification BAPI - Values of Type CHAR, BOOL

    IF NOT im_workorder IS INITIAL.

      READ TABLE im_char ASSIGNING FIELD-SYMBOL(<lfs_char>) INDEX 1.
      IF sy-subrc = 0.
*        MOVE <lfs_char>-class TO lv_classnum.

        lv_classnum  = <lfs_char>-class.
      ENDIF.
      IF <lfs_char>-woheadereq = lc_x OR
      <lfs_char>-wooperationeq = lc_x
     OR <lfs_char>-notficationeq = lc_x.
        lv_klart    = '002'."for equipment
      ELSEIF <lfs_char>-wo_object = lc_x.
        lv_klart = '016'."Object link
      ELSE.
        lv_klart = '003'."Function location
      ENDIF.

*SOC BY LMETTA
*        SELECT SINGLE aufnr,equnr,iloan,qmnum
*        FROM afih
*        INTO @DATA(ls_equnr)
*        WHERE aufnr = @im_workorder.
*EOC BY LMETTA

*SOC BY LMETTA

*      DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination(
*                 i_name = 'ED1_TEST_001'
*                 ).
*
*      DATA(lv_rfc_dest_name) = lo_rfc_dest->get_destination_name(  ).

      DATA: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.



      CALL METHOD lr_rfc->get_cloud_dest
        IMPORTING
          ex_dest = DATA(lv_rfc).

*EOC BY LMETTA

*      DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
*      DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

      lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                           ( fieldname = 'EQUNR' )
                           ( fieldname = 'ILOAN' )
                           ( fieldname = 'QMNUM' ) ).

      IF ls_afih IS NOT INITIAL.
        lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ im_workorder }| & |'|  ) ).

      ENDIF.
*EOC BY LMETTA

*SOC BY LMETTA
*      IF lrs_equnr IS NOT INITIAL.
*        APPEND VALUE #( text = |{ lv_and }| & | | & |EQUNR| & | | & |{ lrs_equnr[ 1 ]-option }| & | | & |'| & |{ lrs_equnr[ 1 ]-low }| & |'| )  TO lt_options.
*      ENDIF.
*
*     lv_rowskip = lv_Skip.
**
*    IF lv_top > 0.
*       lv_rowcount = lv_top.
*
*     ENDIF.
*EOC BY LMETTA

*SOC BY LMETTA

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc_dest
        EXPORTING
          query_table = 'AFIH'
          rowskips    = lv_rowskip
          rowcount    = lv_rowcount
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      LOOP AT lt_data INTO DATA(ls_data).
        ls_afih-aufnr = ls_data+0(12).
        ls_afih-equnr = ls_data+12(30).
        ls_afih-iloan = ls_data+31(43).
        ls_afih-qmnum = ls_data+44(56).

*        APPEND ls_str TO lt_str.
*        CLEAR ls_str.

      ENDLOOP.


*EOC BY LMETTA

*      ex_response_data[] = lit_workorder[].
      IF sy-subrc NE 0.

        IF sy-subrc = 0.
*          IF NOT ls_equnr-equnr IS INITIAL.
          IF NOT ls_afih-equnr IS INITIAL.

*SOC BT LMETTA
*        MOVE ls_equnr-equnr TO lv_equnr.
*            lv_equnr = ls_equnr-equnr.
*EOC BY LMETTA

            lv_equnr = ls_afih-equnr.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              DESTINATION lv_rfc_dest
              EXPORTING
                input  = lv_equnr
              IMPORTING
                output = lv_equnr.

            CLEAR: lv_object.

*          MOVE lv_equnr TO lv_object.

            lv_object = lv_equnr.

*          IF <lfs_char>-woheadereq = lc_x.
*            lv_klart    = '002'.
**--Update charactaerstics of the equipment , functional location
*            CALL METHOD me->update_characterstics
*              EXPORTING
*                lv_object   = lv_object
*                lv_classnum = lv_classnum
*                lv_klart    = lv_klart
*                im_char     = im_char.
*            CLEAR: lv_equnr.
*          ENDIF.

            IF <lfs_char>-woheaderfl = lc_x.
              lv_klart = '003'."Function location

*            SELECT SINGLE iloan, tplnr FROM iloa
*              INTO @DATA(ls_iloa)
*              WHERE iloan = @ls_equnr-iloan.
*
*            IF sy-subrc = 0.

*SOC BY LMETTA

              CALL METHOD lr_rfc->get_cloud_dest
                IMPORTING
                  ex_dest = DATA(lv_rfc_dest).

*EOC BY LMETTA


              lt_fields = VALUE #( ( fieldname = 'ILOAN' )
                                   ( fieldname = 'TPLNR' ) ).
*SOC BY LMETTA

              IF ls_afih IS NOT INITIAL.
                lt_options = VALUE #( ( text = |ILOAN| & | | & |EQ| & | | & |'| & |{ ls_afih-iloan }| & |'|  ) ).

              ENDIF.
*EOC BY LMETTA

*SOC BY LMETTA

              CALL FUNCTION 'RFC_READ_TABLE'
                DESTINATION lv_rfc_dest
                EXPORTING
                  query_table = 'ILOA'
                  rowskips    = lv_rowskip
                  rowcount    = lv_rowcount
                TABLES
                  options     = lt_options
                  fields      = lt_fields
                  data        = lt_data.

*EOC BY LMETTA

*SOC BY LMETTA
*              MOVE ls_iloa-tplnr TO lv_tplnr.
*               lv_tplnr = ls_iloa-tplnr.

              lv_tplnr = ls_afih-tplnr.
*EOC BY LMETTA

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                DESTINATION lv_rfc_dest
                EXPORTING
                  input  = lv_tplnr
                IMPORTING
                  output = lv_tplnr.

*              MOVE lv_tplnr TO lv_object.

              lv_object = lv_tplnr.

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

*SOC BY LMETTA

*            SELECT SINGLE qmnum,iloan,equnr
*              FROM qmih INTO @DATA(ls_qmequi)
*              WHERE qmnum = @ls_equnr-qmnum.
*            IF sy-subrc = 0.
*EOC BY LMETTA




            lt_fields = VALUE #( ( fieldname = 'QMNUM' )
                                 ( fieldname = 'ILOAN' )
                                 ( fieldname = 'EQUNR' ) ).
*SOC BY LMETTA

*            IF lrs_notifnum IS NOT INITIAL.

            IF ls_afih IS NOT INITIAL.
              lt_options = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ ls_afih-iloan }| & |'|  ) ).

            ENDIF.
*EOC BY LMETTA

*SOC BY LMETTA

            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc_dest
              EXPORTING
                query_table = 'QMIH'
                rowskips    = lv_rowskip
                rowcount    = lv_rowcount
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.


*EOC BY LMETTA

            IF NOT ls_qmequi IS INITIAL.

*                MOVE ls_qmequi-equnr TO lv_equnr.

              lv_equnr = ls_qmequi-equnr.

              IF NOT ls_afih-equnr IS INITIAL.
*                  MOVE ls_equnr-equnr TO lv_equnr.
                lv_equnr = ls_afih-equnr.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  DESTINATION lv_rfc_dest
                  EXPORTING
                    input  = lv_equnr
                  IMPORTING
                    output = lv_equnr.

                CLEAR:  lv_object.

*                  MOVE lv_equnr TO lv_object.

                lv_equnr = lv_object.

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

*                  SELECT SINGLE iloan, tplnr FROM iloa "COMMENTED BY LMETTA
*                  INTO @DATA(ls_iloa1)
*                        WHERE iloan = @ls_equnr-iloan.


*SOC BY LMETTA
                  lt_fields = VALUE #( ( fieldname = 'ILOAN' )
                                       ( fieldname = 'TPLNR' ) ).


                  IF ls_afih IS NOT INITIAL.
                    lt_options = VALUE #( ( text = |ILOAN| & | | & |EQ| & | | & |'| & |{ ls_afih-iloan }| & |'|  ) ).

                  ENDIF.
*EOC BY LMETTA

*SOC BY LMETTA

                  CALL FUNCTION 'RFC_READ_TABLE'
                    DESTINATION lv_rfc_dest
                    EXPORTING
                      query_table = 'ILOA'
                      rowskips    = lv_rowskip
                      rowcount    = lv_rowcount
                    TABLES
                      options     = lt_options
                      fields      = lt_fields
                      data        = lt_data.

*EOC BY LMETTA


                  IF sy-subrc = 0.
*                      MOVE ls_iloa1-tplnr TO lv_tplnr.
*                    lv_tplnr = ls_iloa1-tplnr.

                    lv_tplnr = ls_afih-tplnr.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      DESTINATION lv_rfc_dest "lv_rfc_dest_name
                      EXPORTING
                        input  = lv_tplnr
                      IMPORTING
                        output = lv_tplnr.

*                      MOVE lv_tplnr TO lv_object.

                    lv_object =  lv_tplnr.
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
    CONSTANTS : "lc_equi TYPE bapi1003_key-objecttable VALUE 'EQUI',
      lc_equi TYPE ty_BAPI1003-objecttable VALUE 'EQUI',
      "lc_iloa TYPE  bapi1003_key-objecttable VALUE 'IFLOT',

      lc_iloa TYPE  ty_BAPI1003-objecttable VALUE 'IFLOT',
      lc_e    TYPE char1 VALUE 'E',
      lc_a    TYPE char1 VALUE 'A'.

    DATA : lit_return TYPE TABLE OF bapiret2,
           lv_objtab  TYPE ty_BAPI1003-objecttable,              "bapi1003_key-objecttable,

*           lit_char   TYPE TABLE OF bapi1003_alloc_values_char, "commented by LMETTA
*           lit_num    TYPE TABLE OF bapi1003_alloc_values_num,
*           lit_curr   TYPE TABLE OF bapi1003_alloc_values_curr,
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
      DESTINATION lv_rfc_dest "lv_rfc_dest_name
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
*        MOVE <lfs_char>-value TO <lfs_char1>-value_char.
        <lfs_char>-value = <lfs_char1>-value_char.
*        MOVE <lfs_char>-value TO <lfs_char1>-value_neutral.
        <lfs_char>-value = <lfs_char1>-value_neutral.
      ELSE.
        READ TABLE lit_num ASSIGNING FIELD-SYMBOL(<lfs_char2>) WITH KEY charact = <lfs_char>-charact.
        IF sy-subrc = 0.
*          MOVE <lfs_char>-value TO <lfs_char2>-value_from.
          <lfs_char>-value = <lfs_char1>-value_neutral.
        ELSE.
          READ TABLE lit_curr ASSIGNING  FIELD-SYMBOL(<lfs_char3>) WITH KEY charact = <lfs_char>-charact.
          IF sy-subrc = 0.
*            MOVE <lfs_char>-value TO <lfs_char3>-value_from.
            <lfs_char>-value = <lfs_char3>-value_from.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CLEAR: lit_return.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      DESTINATION lv_rfc_dest "lv_rfc_dest_name            "BY LMETTA
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

*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = lv_message.
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
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = lv_message.
    ENDIF.
    CLEAR: lit_char.

  ENDMETHOD.
ENDCLASS.
