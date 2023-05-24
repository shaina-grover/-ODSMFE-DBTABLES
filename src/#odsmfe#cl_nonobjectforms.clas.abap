class /ODSMFE/CL_NONOBJECTFORMS definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_NONOBJECTFORMS .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_NONOBJECTFORMS .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_NONOBJECTFORMS IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  ODS-HSANGAM
* Creation Date          :  27.04.2023
* Transport No.          :  ES1K903727
* Program Description    :  Download the NonObject Forms
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/ Tables and Structures
    DATA :lit_foass                 TYPE TABLE OF /odsmfe/tb_foass,            "Internal table for ODS : Form Assignment Table
          lit_fomst                 TYPE TABLE OF /odsmfe/tb_fomst,            "Internal table for FORM Master Table
          lst_fomst                 TYPE /odsmfe/tb_fomst,                     "Structure type for FORM Master Table
          lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair,         "name value pair for mgw
          lst_filter_select_options TYPE /iwbep/s_mgw_select_option.           "MGW Framework: Selection Option Parameters for db selects

    "/ Range Table and Range Structures
    DATA :lrt_formid       TYPE TABLE OF /odsmfe/st_core_range_str,            "FormId Range Structure
          lrt_version      TYPE TABLE OF /odsmfe/st_core_range_str,            "Version Range Structure
          lrt_codegroup    TYPE TABLE OF /odsmfe/st_core_range_str,            "CodeGroup Range Structure
          lrs_filter       TYPE /odsmfe/st_core_range_str,                     "Core Range structure
          lrs_filter_range TYPE /iwbep/s_cod_select_option.                    "MGW Framework: Select Options for Queries

    "/ Field symbols
    FIELD-SYMBOLS : <lfsst_form> TYPE /odsmfe/tb_fomst.                        "Form Master Table

    "/ Constants
    CONSTANTS: lc_e         TYPE string VALUE 'E',                             "Single-Character Indicator
               lc_i         TYPE string VALUE 'I',                             "Single-Character Indicator
               lc_eq        TYPE string VALUE 'EQ',                            "Single-Character Indicator
               lc_formid    TYPE string VALUE 'FormID',                        "FormId
               lc_version   TYPE string VALUE 'Version',                       "Version
               lc_codegroup TYPE string VALUE 'CodeGroup',                     "CodeGroup
               lc_category  TYPE string VALUE 'NonObject',                     "NonObject
               lc_form_id   TYPE string VALUE 'FORMID',                        "FORMID
               lc_vers      TYPE string VALUE 'VERSION',                       "VERSION
               lc_code_grp  TYPE string VALUE 'CODEGROUP'.                     "CODEGROUP

*------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
*------------------------------------------------------------------------*
*------------------------------------------------------------------------*
*            M A I N            S E C T I O N                            *
*------------------------------------------------------------------------*

    "/ Read key values
    IF im_key_tab IS NOT INITIAL.
      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_formid.
            lrs_filter-sign   = lc_i.
            lrs_filter-option = lc_eq.
            lrs_filter-low    = lst_key_tab-value.
            APPEND lrs_filter TO lrt_formid.

          WHEN lc_version.
            lrs_filter-sign   = lc_i.
            lrs_filter-option = lc_eq.
            lrs_filter-low    = lst_key_tab-value.
            APPEND lrs_filter TO lrt_version.

          WHEN lc_codegroup.
            lrs_filter-sign   = lc_i.
            lrs_filter-option = lc_eq.
            lrs_filter-low    = lst_key_tab-value.
            APPEND lrs_filter TO lrt_codegroup.
        ENDCASE. "/ CASE lst_key_tab-name.
        CLEAR: lrs_filter.
      ENDLOOP. "/ LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
    ENDIF. "/ IF im_key_tab IS NOT INITIAL.

    "/ Read filter values
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter_select_options.
        TRANSLATE lst_filter_select_options-property TO UPPER CASE.

        CASE lst_filter_select_options-property.
          WHEN lc_form_id.
            READ TABLE lst_filter_select_options-select_options  INTO lrs_filter_range INDEX 1.
            IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.
              lrs_filter-sign   = lc_i.
              lrs_filter-option = lc_eq.
              lrs_filter-low    = lrs_filter_range-low.
              APPEND lrs_filter TO lrt_formid.
            ENDIF. "/ IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.

          WHEN lc_vers.
            READ TABLE lst_filter_select_options-select_options  INTO lrs_filter_range INDEX 1.
            IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.
              lrs_filter-sign   = lc_i.
              lrs_filter-option = lc_eq.
              lrs_filter-low    = lrs_filter_range-low.
              APPEND lrs_filter TO lrt_version.
            ENDIF. "/ IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.

          WHEN lc_code_grp.
            READ TABLE lst_filter_select_options-select_options  INTO lrs_filter_range INDEX 1.
            IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.
              lrs_filter-sign   = lc_i.
              lrs_filter-option = lc_eq.
              lrs_filter-low    = lrs_filter_range-low.
              APPEND lrs_filter TO lrt_codegroup.
            ENDIF. "/ IF sy-subrc = 0 AND lrs_filter_range-low IS NOT INITIAL.
        ENDCASE. "/ CASE lst_filter_select_options-property.

        CLEAR: lrs_filter.
      ENDLOOP. "/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
    ENDIF. "/ IF im_filter_select_options IS NOT INITIAL.

    "/ Get the NonObject Forms from Form Assignment Table
    SELECT * FROM /odsmfe/tb_foass INTO TABLE lit_foass
      WHERE category = lc_category
        AND formid  IN lrt_formid[]
        AND version IN lrt_version[].

    IF sy-subrc = 0 AND lit_foass[] IS NOT INITIAL.
      "/ Get the NonObject Forms details from the Form Master table
      SELECT * FROM /odsmfe/tb_fomst INTO TABLE lit_fomst FOR ALL ENTRIES IN lit_foass
        WHERE formid  = lit_foass-formid
          AND version = lit_foass-version.
    ENDIF. "/ IF sy-subrc = 0 AND lit_foass[] IS NOT INITIAL.

    "/ Map the Form details to the final table
    IF lit_fomst[] IS NOT INITIAL.
      LOOP AT lit_fomst INTO lst_fomst.
        gstib_entity-formid         = lst_fomst-formid.
        gstib_entity-version        = lst_fomst-version.
        gstib_entity-codegroup      = lst_fomst-codegruppe.
        gstib_entity-formname       = lst_fomst-form_name.
        gstib_entity-description    = lst_fomst-description.
        gstib_entity-formdata       = lst_fomst-formdata.
        gstib_entity-formhtml       = lst_fomst-formhtml.
        gstib_entity-formmodel      = lst_fomst-formmodel.
        gstib_entity-active         = lst_fomst-active.
        gstib_entity-theme          = lst_fomst-theme.
        gstib_entity-stylesheet     = lst_fomst-stylesheet.
        gstib_entity-createdon      = lst_fomst-created_on.
        gstib_entity-createdby      = lst_fomst-created_by.
        gstib_entity-modifiedon     = lst_fomst-modified_on.
        gstib_entity-modifiedby     = lst_fomst-modified_by.
        gstib_entity-category       = lst_fomst-formcategory.
        gstib_entity-functionalarea = lst_fomst-funareaid.
        gstib_entity-subarea        = lst_fomst-subareaid.
        APPEND gstib_entity TO gitib_entity.
        CLEAR gstib_entity.
      ENDLOOP. "/ LOOP AT lit_fomst INTO lst_fomst.
    ENDIF. "/ IF lit_fomst[] IS NOT INITIAL.

    "/ Mapping properties from the backend to the Gateway output response table
    IF im_key_tab[] IS NOT INITIAL.
      READ TABLE gitib_entity INTO gstib_entity INDEX 1.
      GET REFERENCE OF gstib_entity INTO ex_entity.
    ELSE. "/ IF im_key_tab[] IS NOT INITIAL.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF. "/ IF im_key_tab[] IS NOT INITIAL.

  ENDMETHOD.
ENDCLASS.
