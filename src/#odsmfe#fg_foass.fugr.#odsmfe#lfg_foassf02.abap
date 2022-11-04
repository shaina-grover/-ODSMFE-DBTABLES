*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/LFG_FOASSF02.
* Mark the line item field deleted = X when user delete any records from form assignmen Table
FORM delete_data.

  DATA: lv_formid      TYPE /odsmfe/de_formid,
        lv_formversion TYPE /odsmfe/de_version,
        lv_ordertype   TYPE aufart,
        lv_steus       TYPE steus,
        lv_plnty       TYPE plnty,
        lv_plnnr       TYPE plnnr,
        lv_plnal       TYPE plnal,
        lv_oprnum      TYPE vornr,
        lv_zaehl       TYPE /odsmfe/de_intrncounter,
        lv_eqtyp       TYPE eqtyp,
        lv_fltyp       TYPE fltyp,
        lv_eqart       TYPE eqart,
        lv_roleid      TYPE /odsmfe/de_roleid.

  DATA: ls_itemdel TYPE /odsmfe/tb_foass,
        lt_itemdel TYPE STANDARD TABLE OF /odsmfe/tb_foass.

  LOOP AT total.

    IF <action> EQ geloescht OR <action> EQ update_geloescht OR <action> EQ neuer_geloescht.

      ls_itemdel-formid = total+3(50).
      ls_itemdel-version = total+53(3).
      ls_itemdel-ordertype = total+56(4).
      ls_itemdel-steus = total+60(4).
      ls_itemdel-plnty = total+64(1).
      ls_itemdel-plnnr = total+65(8).
      ls_itemdel-plnal = total+73(2).
      ls_itemdel-oprnum = total+75(4).
      ls_itemdel-zaehl = total+79(8).
      ls_itemdel-eqtyp = total+87(1).
      ls_itemdel-fltyp = total+88(1).
      ls_itemdel-eqart = total+89(10).
      ls_itemdel-roleid = total+99(30).
      ls_itemdel-deleted = abap_true.
      APPEND ls_itemdel TO lt_itemdel.
      INSERT /odsmfe/tb_fmadl FROM TABLE lt_itemdel ACCEPTING DUPLICATE KEYS.
      IF sy-subrc = 0.
        MESSAGE 'Item marked for deleted' TYPE 'I'.
      ENDIF.

    ENDIF.

  ENDLOOP.


ENDFORM.
