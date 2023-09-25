class /ODSMFE/CL_USERSASGNFORMS definition
  public
  create private .

public section.
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_USERSASGNFORMS IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : MRAKESH
* Creation Date          : 11/10/2022
* Transport No.          : ES1K903234
* Program Description    : Read the Form Definition Book Mark Data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/ Tables and Structures
    DATA: lit_foass                 TYPE TABLE OF /odsmfe/tb_foass,
          lst_foass                 TYPE /odsmfe/tb_foass,
          lit_fomst                 TYPE TABLE OF /odsmfe/tb_fomst,
          lst_fomst                 TYPE /odsmfe/tb_fomst,
          lst_catid                 TYPE /odsmfe/st_core_range_str,
          lit_catid                 TYPE TABLE OF /odsmfe/st_core_range_str,
          lit_funar                 TYPE TABLE OF /odsmfe/tb_funar,
          lst_funar                 TYPE /odsmfe/tb_funar,
          lit_subar                 TYPE TABLE OF /odsmfe/tb_subar,
          lst_subar                 TYPE /odsmfe/tb_subar,
          lit_fmcat                 TYPE TABLE OF /odsmfe/tb_fmcat,
          lst_fmcat                 TYPE /odsmfe/tb_fmcat,
          lst_filter_select_options TYPE /iwbep/s_mgw_select_option,
          lst_select_options        TYPE /iwbep/s_cod_select_option,
          lit_forsp                 TYPE TABLE OF /odsmfe/tb_forsp,
          lst_forsp                 TYPE /odsmfe/tb_forsp,
          lit_total_forms           TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lit_total_forms1          TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lit_all_draft             TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lit_all_submited          TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lst_total_forms           TYPE /odsmfe/tb_forsp,
          lit_draft_forms           TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lst_draft_forms           TYPE /odsmfe/tb_forsp.

    "/ Variables
    DATA:
      lv_formid   TYPE /odsmfe/de_formid,
      lv_version  TYPE /odsmfe/de_version,
      lv_lines    TYPE i,
      lv_total    TYPE i,
      lv_draft    TYPE i,
      lv_submited TYPE i.

    "/ Constants
    CONSTANTS:
      lc_cat TYPE string VALUE 'CATEGORY',
      lc_x   TYPE c VALUE 'X'.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

*------------------------------------------------------------------------*
*                       M A I N   S E C T I O N                          *
*------------------------------------------------------------------------*
    "/ Read Filter Values
    LOOP AT im_filter_select_options INTO lst_filter_select_options.
      TRANSLATE lst_filter_select_options-property TO UPPER CASE.
      CASE lst_filter_select_options-property.

        WHEN lc_cat.
          READ TABLE lst_filter_select_options-select_options INTO lst_select_options INDEX 1.
          IF sy-subrc = 0.
            lst_catid-sign   = lst_select_options-sign.
            lst_catid-option = lst_select_options-option.
            lst_catid-low    = lst_select_options-low.
            APPEND lst_catid TO lit_catid.
            CLEAR lst_catid.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    "/ Get all functional areas
    SELECT * FROM /odsmfe/tb_funar INTO TABLE lit_funar.
    SORT lit_funar BY funareaid.

    "/ Get all sub areas
    SELECT * FROM /odsmfe/tb_subar INTO TABLE lit_subar.
    SORT lit_subar BY subareaid.

    "/ Get all categories
    SELECT * FROM /odsmfe/tb_fmcat INTO TABLE lit_fmcat.
    SORT lit_fmcat BY catid.

    "/ Get all the form assignment data
    SELECT * FROM /odsmfe/tb_foass
      INTO TABLE lit_foass
      WHERE category IN lit_catid.

    "/ Get all the form Response data.
    IF lit_foass[] IS NOT INITIAL.
      SELECT * FROM /odsmfe/tb_forsp
      INTO TABLE lit_total_forms FOR ALL ENTRIES IN lit_foass
      WHERE formid  = lit_foass-formid
      AND version   = lit_foass-version.

      SORT lit_total_forms BY formid version.

      "/ Get the form master data.
      SELECT * FROM /odsmfe/tb_fomst
        INTO TABLE lit_fomst
        FOR ALL ENTRIES IN lit_foass
        WHERE formid = lit_foass-formid AND
        version = lit_foass-version .

      SORT lit_fomst BY formid version.

      SORT lit_foass BY formid version.
      DELETE ADJACENT DUPLICATES FROM lit_foass COMPARING formid version.

      "/ Mapping properties to the final table.
      LOOP AT lit_foass  INTO lst_foass.
        READ TABLE  lit_fomst INTO lst_fomst WITH KEY formid = lst_foass-formid version = lst_foass-version BINARY SEARCH.
        IF sy-subrc = 0.
          gstib_entity-formid      = lst_foass-formid.
          gstib_entity-version     = lst_foass-version.
          gstib_entity-category    = lst_foass-category.
          gstib_entity-multiplesub = lst_foass-multiplesub.
          gstib_entity-occur       = lst_foass-occur.
          gstib_entity-form_name   = lst_fomst-form_name.
          gstib_entity-description = lst_fomst-description.

          "/ Get Functional area name
          READ TABLE lit_funar INTO lst_funar WITH KEY funareaid = lst_fomst-funareaid BINARY SEARCH.
          IF sy-subrc = 0.
            gstib_entity-funareaid = lst_funar-procname.
          ELSE.
            gstib_entity-funareaid = lst_fomst-funareaid.
          ENDIF.

          "/ Get Sub area name
          READ TABLE lit_subar INTO lst_subar WITH KEY subareaid = lst_fomst-subareaid BINARY SEARCH.
          IF sy-subrc = 0.
            gstib_entity-subareaid = lst_subar-procname.
          ELSE.
            gstib_entity-subareaid = lst_fomst-subareaid.
          ENDIF.

          "/ Get Category
          READ TABLE lit_fmcat INTO lst_fmcat WITH KEY catid = lst_fomst-formcategory BINARY SEARCH.
          IF sy-subrc = 0.
            gstib_entity-formcategory = lst_fmcat-catdesc.
          ELSE.
            gstib_entity-formcategory = lst_fomst-formcategory.
          ENDIF.

          READ TABLE lit_total_forms INTO lst_total_forms WITH KEY formid = lst_foass-formid version = lst_foass-version BINARY SEARCH.
          IF sy-subrc = 0.
            "/ Total Forms Count
            REFRESH: lit_total_forms1, lit_all_draft, lit_all_submited.
            APPEND LINES OF lit_total_forms TO lit_total_forms1.
            DELETE lit_total_forms1 WHERE formid  NE lst_total_forms-formid.
            DELETE lit_total_forms1 WHERE version NE lst_total_forms-version.
            DESCRIBE TABLE lit_total_forms1 LINES lv_total.
            gstib_entity-total_forms = lv_total.

            "/ Draft forms Count
            APPEND LINES OF lit_total_forms TO lit_all_draft.
            DELETE lit_all_draft WHERE isdraft NE lc_x.
            DELETE lit_all_draft WHERE formid  NE lst_total_forms-formid.
            DELETE lit_all_draft WHERE version NE lst_total_forms-version.
            DESCRIBE TABLE lit_all_draft LINES lv_draft.
            gstib_entity-total_drafts = lv_draft.

            "/ Submitted forms count
            APPEND LINES OF lit_total_forms TO lit_all_submited.
            DELETE lit_all_submited WHERE isdraft EQ lc_x.
            DELETE lit_all_submited WHERE formid    NE lst_total_forms-formid.
            DELETE lit_all_submited WHERE version   NE lst_total_forms-version.
            DESCRIBE TABLE lit_all_submited LINES lv_submited.
            gstib_entity-total_submissions = lv_submited.

          ENDIF.
          APPEND gstib_entity TO gitib_entity.
          CLEAR: gstib_entity, lst_foass, lst_fomst, lst_forsp, lv_total, lv_draft, lv_submited.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "/ Get EntitySet method is requested
    IF gitib_entity IS NOT INITIAL.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
