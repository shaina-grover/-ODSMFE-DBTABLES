*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/FORM_MASTER_F01.
*----------------------------------------------------------------------*
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  /05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_BROWSE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_browse_file.

  CLEAR gv_filename .
* Function module to open Frontend dialog
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = gc_title
    CHANGING
      file_table              = gst_filetab
      rc                      = gv_filerc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
* error handling
    MESSAGE text-008 TYPE gc_e.
  ELSE .
    READ TABLE gst_filetab INDEX 1 INTO p_file.
    IF sy-subrc NE 0.
      CLEAR: p_file.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_LOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UP_GV_FILENAME  text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_load_file  USING    up_gv_filename.

  REFRESH git_int_tab1.
* Function module to upload GUI
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = up_gv_filename
      filetype                = gc_filetype
    TABLES
      data_tab                = git_int_tab1
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
* error handling
    MESSAGE text-007 TYPE gc_e.
  ENDIF.

  MOVE up_gv_filename TO gv_localfilename.

  PERFORM /odsmfe/fo_parse_data.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_SAVE_FILE_CONTENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_save_file_contents .

  "Delete the ignorables
  DELETE git_xml_data WHERE cname+0(7) EQ gc_ignore.

  CLEAR gst_xml_data.
* read the XML with value as id
  READ TABLE git_xml_data INTO gst_xml_data WITH KEY cname = text-016." gc_titles.
  IF sy-subrc EQ 0.
    gst_upd_tab-formid = gst_xml_data-cvalue.
  ENDIF.
  gv_formid = gst_upd_tab-formid.
  gv_desc  = gv_formid.

  CLEAR gst_xml_data.
* read the XML with value as title
  READ TABLE git_xml_data INTO gst_xml_data WITH KEY cname = text-016."gc_titles.
  IF sy-subrc EQ 0.
    gst_upd_tab-form_name = gst_xml_data-cvalue.
  ENDIF.
  gv_shorttext = gst_upd_tab-form_name.


  CLEAR gst_xml_data.
* ++ES1K901991
  READ TABLE git_xml_data WITH KEY cname = text-011 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    READ TABLE git_xml_data INTO gst_xml_data WITH KEY cname = text-011.
    IF sy-subrc EQ 0.
      gv_theme    = gst_xml_data-cvalue.
    ENDIF.
  ENDIF. " ++ ES1K901991

* Checking if creation of codegrouppe is required or not
** SOC
*  SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_create
*                                         WHERE entitysetname = 'FormMasterSet'
*                                           AND field = 'CODEGROUPE'
*                                           AND active = 'X'.
** EOC

  IF gv_formid IS NOT INITIAL.
    IF p_codes = abap_on. " Check if Code & code group needs to be created
      " Read first 3 characters from ID field
      gv_read = gv_formid(3).
      " Create a string to search if same name already present
      CONCATENATE gv_read gc_wild INTO gv_codegruppe.
      TRANSLATE gv_codegruppe TO UPPER CASE.
* check for Codegroup Name
      SELECT * FROM qpgt                                "#EC CI_GENBUFF
        INTO CORRESPONDING FIELDS OF TABLE git_qpgt
        WHERE katalogart = gc_katalog_b
        AND kurztext = gv_desc.
    ENDIF.

*  Check for count
    gv_max_no = sy-dbcnt.

    IF git_qpgt IS NOT INITIAL.

      IF p_update EQ abap_on.

        SORT git_qpgt BY codegruppe DESCENDING.
        READ TABLE git_qpgt INTO gst_qpgt INDEX 1 TRANSPORTING codegruppe.
        IF sy-subrc EQ 0.
          gv_code_group = gst_qpgt-codegruppe.
          gv_version    = gst_qpgt-codegruppe+5(3).
          CLEAR gst_qpgt.
        ENDIF.
      ELSE.

        IF gv_max_no GT gc_num.
          READ TABLE git_qpgt INTO gst_qpgt WITH KEY kurztext = gv_desc.
          IF sy-subrc EQ 0.
            gv_num = gst_qpgt-codegruppe+3(2).
          ENDIF.
          gv_ver = gv_max_no.
        ELSE.
          "(To check if same name does not exist - increment the counter)
          gv_num = gv_max_no.
          gv_ver = gc_version.
        ENDIF.

        IF gv_ver BETWEEN 0 AND 9. "(As part of model data being 100+)
          CONCATENATE gc_zero1 gv_ver INTO gv_version.
        ENDIF.

        IF gv_ver BETWEEN 10 AND 99. "(As part of model data being 100+)
          CONCATENATE gc_zero gv_ver INTO gv_version.
        ENDIF.

        CONCATENATE gv_read gv_num gv_version INTO gv_code_group.
        TRANSLATE gv_code_group TO UPPER CASE.

      ENDIF.
    ELSE.
      IF p_codes = abap_on. " Check if Code & code group needs to be created
        CONCATENATE gv_read  '%' INTO gv_text.
        SELECT * FROM qpgt                              "#EC CI_GENBUFF
             INTO CORRESPONDING FIELDS OF TABLE git_qpgt1
             WHERE katalogart = gc_katalog_b
             AND kurztext LIKE gv_text.
        SORT git_qpgt1 DESCENDING BY codegruppe.
        READ TABLE git_qpgt1 INTO gst_temp INDEX 1.
        IF sy-subrc EQ 0.
          gv_num = gst_temp-codegruppe+3(2) + 1.
        ELSE.
          gv_num = '0'.
        ENDIF.
        gv_version = '000'.
        IF gv_num BETWEEN 0 AND 9.
          CONCATENATE '0' gv_num INTO gv_num.
        ENDIF.

        CONCATENATE gv_read gv_num gv_version INTO gv_code_group.
        TRANSLATE gv_code_group TO UPPER CASE.

      ELSE. "++ES1K902815
        gv_version = '000'."++ES1K902815
      ENDIF.
    ENDIF.

    CLEAR gst_upd_tab.

    SELECT SINGLE * FROM /odsmfe/tb_fomst
                  INTO gst_upd_tab
                  WHERE formid EQ gv_formid
                  AND version EQ gv_version
                  AND codegruppe EQ gv_code_group.

    IF gst_upd_tab IS NOT INITIAL AND p_update EQ abap_on.

      GET TIME STAMP FIELD gv_changedon.
* SOC SKAMMARI 28-05-2020
      gst_upd_tab-formdata = gv_xml_string.
* EOC SKAMMARI 28-05-2020
      gst_upd_tab-formhtml = gv_xml_form_string. "Form HTML data - XML
      gst_upd_tab-formmodel = gv_xml_model_string. "Form MODEL data - XML
      gst_upd_tab-modified_on = gv_changedon. "Creation Time
      gst_upd_tab-modified_by = sy-uname. " User Name
*      ENDIF.
    ELSE.

      gst_upd_tab-formid = gv_formid.
      gst_upd_tab-form_name = gv_formid.
      gst_upd_tab-theme = gv_theme.
      gst_upd_tab-description = p_des. " Desciption
      gst_upd_tab-active = p_active. " Active flag from Main Program
* SOC SKAMMARI 28-05-2020
      gst_upd_tab-formdata = gv_xml_string.
* EOC SKAMMARI 28-05-2020
      gst_upd_tab-formhtml = gv_xml_form_string. " Form HTML data - XML
      gst_upd_tab-formmodel = gv_xml_model_string. " Form MODEL data - XML
      gst_upd_tab-version = gv_version. " Form Version Number
      GET TIME STAMP FIELD gv_createdon. "  Get Creation Timestamp
      gst_upd_tab-created_on = gv_createdon. "Creation Time
      gst_upd_tab-created_by = sy-uname. " User Name
      gst_upd_tab-modified_on = gv_createdon. "Creation Time
      gst_upd_tab-modified_by = sy-uname. " User Name
* SOC Sravan kumar "++ ES1K902842
      gst_upd_tab-formcategory = p_fmcat.  " Form Category
      gst_upd_tab-funareaid    = p_funare. " Functional Area ID
      gst_upd_tab-subareaid    = p_subare. " Sub Area ID
* EOC Sravan kumar "++ ES1K902842
    ENDIF.
    APPEND gst_upd_tab TO git_upd_tab. " Update Internal Table
    CLEAR gst_upd_tab.

** SOC
*    IF lv_create = 'X'.
    IF p_codes = 'X'.
** EOC
      PERFORM /odsmfe/fo_create_codegrouppe.
      gst_upd_tab-codegruppe = gv_code_group. " Catalog CODEGRUPPE
      MODIFY git_upd_tab FROM gst_upd_tab INDEX 1 TRANSPORTING codegruppe. " Update Internal Table
      CLEAR gst_upd_tab.
    ENDIF.

* Trasport creation code
    IF p_trans = abap_true.
      PERFORM /odsmfe/fo_tr_create.
** SOC
*   IF lv_create = 'X'.
*      IF p_codes = 'X'.
** EOC
* TR Building steps
        SELECT SINGLE * FROM /odsmfe/form_tr INTO CORRESPONDING FIELDS OF gst_e071
                                      WHERE obj_name = 'V_QPGR_CL'.
        IF sy-subrc IS INITIAL .
          gst_e071-trkorr = gv_wi_trkorr.
          APPEND gst_e071 TO git_e071.

          APPEND INITIAL LINE TO git_e071_temp ASSIGNING <gfsst_e071_temp>.
          MOVE-CORRESPONDING gst_e071 TO <gfsst_e071_temp>.

          CLEAR gst_e071.

        ENDIF.

* TR Building steps
        SELECT * FROM /odsmfe/formtask INTO TABLE git_e071k_temp
                 WHERE objname NE space.

        IF sy-subrc IS INITIAL.

          LOOP AT git_e071k_temp ASSIGNING <gfsst_e071k_temp>.

            APPEND INITIAL LINE TO git_e071k ASSIGNING <gfsst_e071k>.
            MOVE-CORRESPONDING <gfsst_e071k_temp> TO <gfsst_e071k>.

            <gfsst_e071k>-trkorr = gv_wi_trkorr.

            IF <gfsst_e071k>-objname = 'QPCD'.
              CONCATENATE sy-mandt gc_katalog_b gv_code_group gv_code gc_num6 INTO <gfsst_e071k>-tabkey.
            ELSEIF <gfsst_e071k>-objname = 'QPCT'.
              CONCATENATE sy-mandt gc_katalog_b gv_code_group gv_code gc_e gc_num6 INTO <gfsst_e071k>-tabkey.
            ELSEIF <gfsst_e071k>-objname = 'QPGR'.
              CONCATENATE sy-mandt gc_katalog_b gv_code_group INTO <gfsst_e071k>-tabkey.
            ELSEIF <gfsst_e071k>-objname = 'QPGT'.
              CONCATENATE sy-mandt gc_katalog_b gv_code_group gc_e INTO <gfsst_e071k>-tabkey.
            ENDIF.

            CONDENSE <gfsst_e071k>-tabkey.
          ENDLOOP.

          DELETE git_e071k WHERE tabkey EQ space.

        ENDIF.
* Call function to append objects

        PERFORM /odsmfe/fo_append_objects.

* TR Building steps
        READ TABLE git_e071_temp ASSIGNING <gfsst_e071_temp> INDEX 1.
        IF <gfsst_e071_temp> IS ASSIGNED.
          APPEND INITIAL LINE TO git_e071 ASSIGNING <gfsst_e071>.
          MOVE-CORRESPONDING <gfsst_e071_temp> TO <gfsst_e071>.
          <gfsst_e071>-trkorr = gv_wi_trkorr.
        ENDIF.
      ENDIF.
****************************************************************************************************
* For catalog type A
**   SOC
*      CLEAR lv_create.
*
*      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_create
*                                         WHERE entitysetname = 'FormMasterSet'
*                                           AND field = 'CODEGROUPE'
*                                           AND active = 'X'.
*      IF sy-subrc IS INITIAL AND lv_create = 'X'.
**   EOC
        IF p_codes = 'X'.
          PERFORM /odsmfe/fo_codegrp_create_cata.
        ENDIF.
***********************************************************************************************
    IF git_upd_tab IS NOT INITIAL.
* Final Form master table update
      MODIFY /odsmfe/tb_fomst FROM TABLE git_upd_tab.    "#EC CI_TABLES
      IF sy-subrc = 0.
        IF p_trans = abap_true.
          "TR Building steps
          REFRESH: git_e071k, git_e071.

          SELECT SINGLE * FROM /odsmfe/form_tr INTO CORRESPONDING FIELDS OF gst_e071
                                           WHERE obj_name = '/ODSMFE/TB_FOMST'.
          IF sy-subrc IS INITIAL .
            gst_e071-trkorr = gv_wi_trkorr.
            APPEND gst_e071 TO git_e071.
            CLEAR gst_e071.
          ENDIF.

          CLEAR gst_upd_tab.
          LOOP AT git_upd_tab INTO gst_upd_tab.
* TR Building steps
            READ TABLE git_e071k_temp ASSIGNING <gfsst_e071k_temp> WITH KEY objname = '/ODSMFE/TB_FOMST'.
            IF <gfsst_e071k_temp> IS ASSIGNED.
              APPEND INITIAL LINE TO git_e071k ASSIGNING <gfsst_e071k>.
              MOVE-CORRESPONDING <gfsst_e071k_temp> TO <gfsst_e071k>.
              <gfsst_e071k>-trkorr     = gv_wi_trkorr.
              CONCATENATE sy-mandt gst_upd_tab-formid gst_upd_tab-version gst_upd_tab-codegruppe
                          INTO <gfsst_e071k>-tabkey RESPECTING BLANKS.
            ENDIF.

          ENDLOOP.

          SORT git_e071k BY tabkey.
          DELETE ADJACENT DUPLICATES FROM git_e071k COMPARING tabkey.
* Call function to append TR objects
          PERFORM /odsmfe/fo_append_objects.
        ENDIF.
*        IF sy-subrc = 0.
        MESSAGE text-002 TYPE gc_s DISPLAY LIKE gc_i.
*        ENDIF.
        IF p_asinmt IS NOT INITIAL.
          PERFORM /odsmfe/fo_call_sm30.
        ENDIF.
      ENDIF.
    ENDIF. " added by shyamala
  ENDIF. " added by shyamala

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_CALL_SM30
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_call_sm30 .
  DATA: lst_foass      TYPE /odsmfe/tb_foass,
        lit_foass_temp TYPE TABLE OF /odsmfe/tb_foass, "
        lv_lines       TYPE i,
        lv_tabix       TYPE sytabix,
        lst_auart      LIKE LINE OF s_auart[],
        lst_steus      LIKE LINE OF s_steus[],
        lst_eqtyp      LIKE LINE OF s_eqtyp[], "++
        lst_fltyp      LIKE LINE OF s_fltyp[], "++
        lst_eqart      LIKE LINE OF s_eqart[], "++
*   SOC by ODS
        lst_plnty      LIKE LINE OF s_plnty[],
        lst_plnnr      LIKE LINE OF s_plnnr[],
        lst_plnal      LIKE LINE OF s_plnal[],
        lst_zaehl      LIKE LINE OF s_zaehl[].
*  EOC by ODS

  DATA:lit_seltab TYPE TABLE OF vimsellist,
       lst_seltab TYPE  vimsellist.

  READ TABLE git_upd_tab INTO gst_upd_tab INDEX 1.
  IF sy-subrc = 0.

    CASE gv_selected_value.
      WHEN 'WORKORDER' OR 'OPERATION'.
        DESCRIBE TABLE s_auart[] LINES lv_lines.
        DO lv_lines TIMES.
          lv_tabix = sy-index.
          CLEAR:lst_auart, lst_steus.
          READ TABLE s_auart[] INTO lst_auart INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_auart.
          ENDIF.
          READ TABLE s_steus[] INTO lst_steus INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_steus.
          ENDIF.
          CLEAR lst_foass.
          lst_foass-ordertype = lst_auart-low.
          lst_foass-steus = lst_steus-low.
          lst_foass-formid = gst_upd_tab-formid.
          lst_foass-version = gst_upd_tab-version.
          lst_foass-createdon = gst_upd_tab-created_on.
          lst_foass-createdby = gst_upd_tab-created_by .
          IF p_roleid IS NOT INITIAL.
            lst_foass-roleid = p_roleid.
          ENDIF.
          IF p_mandt IS NOT INITIAL.
            lst_foass-mandatory = 'X'.
          ENDIF.
          IF p_occur GT '1'.
            lst_foass-multiplesub = 'X'.
          ENDIF.
          lst_foass-occur = p_occur.
          IF p_theme IS NOT INITIAL.
            lst_foass-theme = 'theme-grid'.
          ENDIF.
          lst_foass-active = 'X'.
          IF p_update EQ space.
*            MODIFY /odsmfe/tb_foass FROM lst_foass.
            APPEND lst_foass TO lit_foass_temp."
          ENDIF.
        ENDDO.
        MODIFY /odsmfe/tb_foass FROM TABLE lit_foass_temp.
        IF sy-subrc <> 0.
          MESSAGE text-026 TYPE gc_e.
        ENDIF.
** SOC ++
      WHEN 'EQUIPMENT'.
        DESCRIBE TABLE s_eqtyp[] LINES lv_lines.
        DO lv_lines TIMES.
          lv_tabix = sy-index.
          CLEAR: lst_eqtyp,lst_eqart.
          READ TABLE s_eqtyp[] INTO lst_eqtyp INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_eqtyp.
          ENDIF.
          READ TABLE s_eqart[] INTO lst_eqart INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_eqart.
          ENDIF.
          CLEAR lst_foass.
          lst_foass-eqtyp = lst_eqtyp-low.
          lst_foass-eqart = lst_eqart-low.
          lst_foass-formid = gst_upd_tab-formid.
          lst_foass-version = gst_upd_tab-version.
          lst_foass-createdon = gst_upd_tab-created_on.
          lst_foass-createdby = gst_upd_tab-created_by .
          IF p_roleid IS NOT INITIAL.
            lst_foass-roleid = p_roleid.
          ENDIF.
          IF p_mandt IS NOT INITIAL.
            lst_foass-mandatory = 'X'.
          ENDIF.
          IF p_occur GT '1'.
            lst_foass-multiplesub = 'X'.
          ENDIF.
          lst_foass-occur = p_occur.
          IF p_theme IS NOT INITIAL.
            lst_foass-theme = 'theme-grid'.
          ENDIF.
          lst_foass-active = 'X'.
          IF p_update EQ space.
            APPEND lst_foass TO lit_foass_temp."
          ENDIF.
        ENDDO.
        MODIFY /odsmfe/tb_foass FROM TABLE lit_foass_temp.
        IF sy-subrc <> 0.
          MESSAGE text-026 TYPE gc_e.
        ENDIF.
      WHEN 'FUNCTIONALLOC'.
        DESCRIBE TABLE s_fltyp[] LINES lv_lines.
        DO lv_lines TIMES.
          lv_tabix = sy-index.
          CLEAR: lst_eqtyp, lst_fltyp, lst_eqart.
          READ TABLE s_fltyp[] INTO lst_fltyp INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_fltyp.
          ENDIF.
          READ TABLE s_eqart[] INTO lst_eqart INDEX lv_tabix.
          IF sy-subrc NE 0.
            CLEAR: lst_eqart.
          ENDIF.
          CLEAR lst_foass.
          lst_foass-fltyp = lst_fltyp-low.
          lst_foass-eqart = lst_eqart-low.
          lst_foass-formid = gst_upd_tab-formid.
          lst_foass-version = gst_upd_tab-version.
          lst_foass-createdon = gst_upd_tab-created_on.
          lst_foass-createdby = gst_upd_tab-created_by .
          IF p_roleid IS NOT INITIAL.
            lst_foass-roleid = p_roleid.
          ENDIF.
          IF p_mandt IS NOT INITIAL.
            lst_foass-mandatory = 'X'.
          ENDIF.
          IF p_occur GT '1'.
            lst_foass-multiplesub = 'X'.
          ENDIF.
          lst_foass-occur = p_occur.
          IF p_theme IS NOT INITIAL.
            lst_foass-theme = 'theme-grid'.
          ENDIF.
          lst_foass-active = 'X'.
          IF p_update EQ space.
            APPEND lst_foass TO lit_foass_temp.
          ENDIF.
        ENDDO.
        MODIFY /odsmfe/tb_foass FROM TABLE lit_foass_temp.
        IF sy-subrc <> 0.
          MESSAGE text-026 TYPE gc_e.
        ENDIF.
** EOC ++
*   SOC by ODS ES1K902363
      WHEN gc_tasklist.
        DESCRIBE TABLE s_plnty[] LINES lv_lines.
        DO lv_lines TIMES.
          lv_tabix = sy-index.
          CLEAR: lst_auart, lst_steus,lst_plnty, lst_plnnr,
                 lst_plnal,lst_zaehl,lst_foass.

          READ TABLE s_auart[] INTO lst_auart INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-ordertype = lst_auart-low.
          ENDIF.
          READ TABLE s_steus[] INTO lst_steus INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-steus = lst_steus-low.
          ENDIF.
          READ TABLE s_plnty[] INTO lst_plnty INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-plnty = lst_plnty-low.
          ENDIF.
          READ TABLE s_plnnr[] INTO lst_plnnr INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-plnnr = lst_plnnr-low.
          ENDIF.
          READ TABLE s_plnal[] INTO lst_plnal INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-plnal = lst_plnal-low.
          ENDIF.
          READ TABLE s_zaehl[] INTO lst_zaehl INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lst_foass-zaehl = lst_zaehl-low.
          ENDIF.

          lst_foass-formid = gst_upd_tab-formid.
          lst_foass-version = gst_upd_tab-version.
          lst_foass-createdon = gst_upd_tab-created_on.
          lst_foass-createdby = gst_upd_tab-created_by .
          IF p_roleid IS NOT INITIAL.
            lst_foass-roleid = p_roleid.
          ENDIF.
          IF p_mandt IS NOT INITIAL.
            lst_foass-mandatory =  gc_x."'X'.
          ENDIF.
          IF p_occur GT '1'.
            lst_foass-multiplesub = gc_x."'X'.
          ENDIF.
          lst_foass-occur = p_occur.
          IF p_theme IS NOT INITIAL.
            lst_foass-theme = gc_themegrid."'theme-grid'.
          ENDIF.
          lst_foass-active = gc_x."'X'.
          IF p_update EQ space.
            APPEND lst_foass TO lit_foass_temp.
          ENDIF.
        ENDDO.
        MODIFY /odsmfe/tb_foass FROM TABLE lit_foass_temp.
        IF sy-subrc <> 0.
          MESSAGE text-026 TYPE gc_e.
        ENDIF.
*   EOC by ODS ES1K902363
    ENDCASE.

*   fill lit_seltab from git_upd_tab
    lst_seltab-viewfield = 'FORMID'.
    lst_seltab-operator  = 'EQ'.
    lst_seltab-and_or  = 'AND'.
    lst_seltab-value  = lst_foass-formid.
    APPEND lst_seltab TO  lit_seltab.
    CLEAR lst_seltab.
    lst_seltab-viewfield = 'VERSION'.
    lst_seltab-operator  = 'EQ'.
    lst_seltab-and_or  = 'AND'.
    lst_seltab-value  = lst_foass-version.
    APPEND lst_seltab TO lit_seltab.
    CLEAR lst_seltab.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'U'
        view_name                    = '/ODSMFE/TB_FOASS'
      TABLES
        dba_sellist                  = lit_seltab
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        OTHERS                       = 15.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.
