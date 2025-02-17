*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/FORM_MASTER_CODEGRP.
*----------------------------------------------------------------------*
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  YSINDHU
* Creation Date          :  /06/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : VSANAGALA
* Change Date            : 10.11.2022
* Transport No.          : ES1K903196
* Change Description     : Added logic to create code and code group for the form created in the checksheet builder
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_CREATE_CODEGROUPPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_create_codegrouppe.
* Catalog type B
  gst_code_group-cat_type = gc_katalog_b.
  gst_code_group-code_group = gv_code_group.
  gst_code_group-status = gc_rel.
  gst_code_grp_shorttexttab-langu = sy-langu.
  gst_code_grp_shorttexttab-short_text = gv_desc.
  APPEND gst_code_grp_shorttexttab TO git_code_grp_shorttexttab.

* replace all occurrences
  REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN gv_formid WITH space.
* generate code first using first 4 letter
  gv_code = gv_formid(4).
  TRANSLATE gv_code TO UPPER CASE.
  gst_codes_of_code_grp-code = gv_code.
* Append to final table
  APPEND gst_codes_of_code_grp TO git_codes_of_code_grp.

  gst_code_shorttexttab-code = gv_code.
  gst_code_shorttexttab-langu = sy-langu.
  gst_code_shorttexttab-short_text = gv_desc.
  APPEND gst_code_shorttexttab TO git_code_shorttexttab.
* append to final table
  CLEAR git_return.
* Call function to SAVE code group
  CALL FUNCTION 'BAPI_QPGR_SAVEREPLICA'
    EXPORTING
      i_code_group          = gst_code_group
    TABLES
      code_grp_shorttexttab = git_code_grp_shorttexttab
      codes_of_code_grp     = git_codes_of_code_grp
      code_shorttexttab     = git_code_shorttexttab
      return                = git_return.
* Error handling
  CLEAR gst_return.
  LOOP AT git_return INTO gst_return WHERE type = gc_e OR type = gc_a.
    CLEAR gv_type.
    gv_type = gst_return-type.
    IF gv_type EQ gc_e OR gv_type EQ gc_a.
      gv_message = gst_return-message.
      MESSAGE gv_message TYPE gc_e.
    ENDIF.
  ENDLOOP.
* clear and Refresh
  CLEAR: gst_code_group, gst_code_grp_shorttexttab,gst_codes_of_code_grp.
  REFRESH : git_code_grp_shorttexttab,git_codes_of_code_grp,git_code_shorttexttab,git_return.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_CODEGRP_CREATE_CATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_codegrp_create_cata .

  "/ Code and Code Groups for the catalog type A
  gst_code_group-cat_type = gc_cat_type.
  gst_code_group-code_group = gv_code_group.
  gst_code_group-status = gc_status.
  gst_code_grp_shorttexttab-langu = sy-langu.
  gst_code_grp_shorttexttab-short_text = gv_desc.
  APPEND gst_code_grp_shorttexttab TO git_code_grp_shorttexttab.

  "/ SOC by VSANAGALA - ES1K903196 on 10.11.2022
  SELECT SINGLE * FROM /odsmfe/tb_apcon INTO gst_xml_tag WHERE param_name = gc_xml_tag.

  READ TABLE git_xml_data INTO gst_xml_data WITH KEY cname = gst_xml_tag-param_value.
  IF sy-subrc = 0.
    gv_xml_cb = gc_x.
  ENDIF.

  "/ Code and Code Groups for the Form created in Checksheet Builder
  IF gv_xml_cb = gc_x.

    CLEAR: gv_counter, gst_xml_data.
    LOOP AT git_xml_data INTO gst_xml_data WHERE hier >= '4'.
      gv_text = gst_xml_data-cname.

      IF gst_xml_data-cname = 'bind'.
        EXIT.
      ELSEIF gst_xml_data-hier >= 6 AND gst_xml_data-type = space OR gst_xml_data-type = 'V'.
        IF gv_text = text-014 OR gv_text = gc_inst_id OR gv_text CS gc_group OR gv_text = text-015 OR gv_text = gc_end OR text-013 = gv_group.
          CONTINUE.
        ELSE.

          REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN gv_text WITH space.

          gv_counter = gv_counter + 1.
          gst_codes_of_code_grp-code = gv_counter.

          gst_code_shorttexttab-code       = gv_counter.
          gst_code_shorttexttab-langu      = sy-langu.
          gst_code_shorttexttab-short_text = gst_xml_data-cname.

          CLEAR gv_cvalue.
          CONCATENATE gst_xml_data-cname gc_label1 INTO gv_cvalue.

          CLEAR gst_xml_data1.
          READ TABLE git_xml_data INTO gst_xml_data1 WITH KEY hier = 6 type = 'A' cname = gc_id cvalue = gv_cvalue.
          IF sy-subrc = 0.
            gv_tabix = sy-tabix.
            gv_tabix = gv_tabix + 1.
          ENDIF.

          IF gst_xml_data1 IS INITIAL.
            CLEAR: gv_cvalue1.
            CONCATENATE '/' gv_cvalue INTO gv_cvalue1.
            LOOP AT git_xml_data INTO gst_xml_data1 WHERE hier = 6 AND type = 'A' AND cname = gc_id AND cvalue CS gv_cvalue1.
              gv_tabix = sy-tabix.
              gv_tabix = gv_tabix + 1.
              EXIT.
            ENDLOOP.
          ENDIF.

          LOOP AT git_xml_data INTO gst_xml_data1 FROM gv_tabix.
            IF gst_xml_data1-cname EQ gc_value AND gst_xml_data1-hier = '7' AND gst_xml_data1-type = 'V'.
              gv_do = strlen( gst_xml_data1-cvalue ).
              IF gv_do GT 132.
                gv_do = 2.
              ELSE.
                gv_do = 1.
              ENDIF.

              DO gv_do TIMES.
                CASE sy-index.
                  WHEN 1.
                    gst_code_longtexttab-text_line = gst_xml_data1-cvalue+0(132).
                  WHEN 2.
                    gst_code_longtexttab-text_line = gst_xml_data1-cvalue+132(123).
                ENDCASE.
                gst_code_longtexttab-code  = gv_counter.
                gst_code_longtexttab-langu = sy-langu.
                CONDENSE gst_code_longtexttab-text_line.

                IF gst_code_longtexttab-text_line IS NOT INITIAL.
                  APPEND gst_code_longtexttab TO git_code_longtexttab.
                ENDIF.

              ENDDO.

            ELSE.
              EXIT.
            ENDIF. "/ IF gst_xml_data1-cname EQ gc_value AND gst_xml_data1-hier = '7' AND gst_xml_data1-type = 'V'.
            CLEAR: gv_tabix.
          ENDLOOP. "/ LOOP AT git_xml_data INTO gst_xml_data1 FROM gv_tabix.
          IF gst_code_longtexttab-text_line IS NOT INITIAL.
            APPEND gst_codes_of_code_grp TO git_codes_of_code_grp.
            APPEND gst_code_shorttexttab TO git_code_shorttexttab.
            CLEAR: gst_code_longtexttab, gst_codes_of_code_grp, gst_code_shorttexttab.
          ELSE.
            gv_counter = gv_counter - 1.
          ENDIF.
        ENDIF. "/ IF gv_text = text-014 OR gv_text = gc_inst_id OR gv_text CS gc_group OR gv_text = text-015 OR gv_text = gc_end OR text-013 = gv_group.
      ENDIF. "/ IF gst_xml_data-cname = 'bind'.
    ENDLOOP. "/ LOOP AT git_xml_data INTO gst_xml_data WHERE hier >= '4'.
    "/ EOC by VSANAGALA - ES1K903196 on 10.11.2022
  ELSE.

    "/ Code and Code Groups for the Form created in KOBO

    CLEAR: gv_counter, gst_xml_data. "(As part of model data being 100+)
    LOOP AT git_xml_data INTO gst_xml_data WHERE hier >= '6'.
      gv_text = gst_xml_data-cname.
      gv_group = gst_xml_data-cname(6).

************************ start of Change***************************************
      " Change added on to cater the XML Form with Group on 19012018 by Mkanungo

      IF text-013 = gv_group OR gv_text = text-015 OR gv_text = gc_end.
        CONTINUE.
      ENDIF.
      IF gv_text = text-014."'meta'.
        EXIT.
      ENDIF.
************************End of Change******************************************
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN gv_text WITH space.

      gv_counter = gv_counter + 1.
      gst_codes_of_code_grp-code = gv_counter.
      APPEND gst_codes_of_code_grp TO git_codes_of_code_grp.

      gst_code_shorttexttab-code = gv_counter.
      gst_code_shorttexttab-langu = sy-langu.
      gst_code_shorttexttab-short_text = gst_xml_data-cname.
      APPEND gst_code_shorttexttab TO git_code_shorttexttab.

      LOOP AT git_xml_data INTO gst_xml_data1 WHERE cname EQ gc_ref AND cvalue CS gst_xml_data-cname. "#EC CI_NESTED
        gv_tabix = sy-tabix.
        gv_tabix = gv_tabix + 1.
        EXIT.
      ENDLOOP.

      LOOP AT git_xml_data INTO gst_xml_data1 FROM gv_tabix.
        IF gst_xml_data1-cname EQ text-017."gc_label.
          gv_do = strlen( gst_xml_data1-cvalue ).
          IF gv_do GT 132.
            gv_do = 2.
          ELSE.
            gv_do = 1.
          ENDIF.
          DO gv_do TIMES.                                "#EC CI_NESTED
            CASE sy-index.
              WHEN 1.
                gst_code_longtexttab-text_line = gst_xml_data1-cvalue+0(132).
              WHEN 2.
                gst_code_longtexttab-text_line = gst_xml_data1-cvalue+132(123).
            ENDCASE.
            gst_code_longtexttab-code = gv_counter.
            gst_code_longtexttab-langu = sy-langu.
            CONDENSE gst_code_longtexttab-text_line.
            IF gst_code_longtexttab-text_line IS NOT INITIAL.
              APPEND gst_code_longtexttab TO git_code_longtexttab.
            ENDIF.
          ENDDO.

        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

* Code Group
  CLEAR git_return.
  CALL FUNCTION 'BAPI_QPGR_SAVEREPLICA'
    EXPORTING
      i_code_group          = gst_code_group
    TABLES
      code_grp_shorttexttab = git_code_grp_shorttexttab
      codes_of_code_grp     = git_codes_of_code_grp
      code_shorttexttab     = git_code_shorttexttab
      code_longtexttab      = git_code_longtexttab
      return                = git_return.
* Error handling
  CLEAR gst_return.
  LOOP AT git_return INTO gst_return WHERE type = gc_e OR type = gc_a.
    CLEAR gv_type.
    gv_type = gst_return-type.
    IF gv_type EQ gc_e OR gv_type EQ gc_a.
      gv_message = gst_return-message.
      MESSAGE gv_message TYPE gc_e.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF p_trans = abap_true.

    CLEAR gst_codes_of_code_grp.
    LOOP AT git_codes_of_code_grp INTO gst_codes_of_code_grp.

      IF git_e071k_temp IS NOT INITIAL.
        LOOP AT git_e071k_temp ASSIGNING <gfsst_e071k_temp>.

          APPEND INITIAL LINE TO git_e071k ASSIGNING <gfsst_e071k>.
          MOVE-CORRESPONDING <gfsst_e071k_temp> TO <gfsst_e071k>.

          <gfsst_e071k>-trkorr = gv_wi_trkorr.

          IF <gfsst_e071k>-objname = 'QPCD'.
*          CONCATENATE sy-mandt gc_katalog_b gv_code_group gv_code gc_num6 INTO gst_e071k-tabkey.
            CONCATENATE sy-mandt gc_katalog_b gv_code_group gst_codes_of_code_grp-code gc_num6 INTO <gfsst_e071k>-tabkey.
          ELSEIF <gfsst_e071k>-objname = 'QPCT'.
*          CONCATENATE sy-mandt gc_katalog_b gv_code_group gv_code gc_e gc_num6 INTO gst_e071k-tabkey.
            CONCATENATE sy-mandt gc_katalog_b gv_code_group gst_codes_of_code_grp-code gc_e gc_num6 INTO <gfsst_e071k>-tabkey.
          ELSEIF <gfsst_e071k>-objname = 'QPGR'.
            CONCATENATE sy-mandt gc_katalog_b gv_code_group INTO <gfsst_e071k>-tabkey.
          ELSEIF <gfsst_e071k>-objname = 'QPGT'.
            CONCATENATE sy-mandt gc_katalog_b gv_code_group gc_e INTO <gfsst_e071k>-tabkey.
          ENDIF.

          CONDENSE <gfsst_e071k>-tabkey.
        ENDLOOP.

        DELETE git_e071k WHERE tabkey EQ space.
      ENDIF.
    ENDLOOP.

* sort and delete
    SORT git_e071k BY tabkey.
    DELETE ADJACENT DUPLICATES FROM git_e071k COMPARING tabkey.
* Call function to append TR objects
    PERFORM /odsmfe/fo_append_objects.
  ENDIF.
ENDFORM.
