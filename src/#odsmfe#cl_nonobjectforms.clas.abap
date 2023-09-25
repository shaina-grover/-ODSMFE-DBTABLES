class /ODSMFE/CL_NONOBJECTFORMS definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
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
          lit_fomst                 TYPE TABLE OF /odsmfe/tb_fomst.            "Internal table for FORM Master Table

    "/ Range Table and Range Structures
    DATA :lrt_formid       TYPE TABLE OF /odsmfe/st_core_range_str,            "FormId Range Structure
          lrt_version      TYPE TABLE OF /odsmfe/st_core_range_str,            "Version Range Structure
          lrt_codegroup    TYPE TABLE OF /odsmfe/st_core_range_str,            "CodeGroup Range Structure
          lrs_filter       TYPE /odsmfe/st_core_range_str.                    "Core Range structure

data : gstib_entity TYPE /odsmfe/ce_nonobjectforms,
       gitib_entity TYPE TABLE OF /odsmfe/ce_nonobjectforms.
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

    "/ Read filter values
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO data(lst_filter_select_options).
        TRANSLATE lst_filter_select_options-name TO UPPER CASE.

        CASE lst_filter_select_options-name.
          WHEN lc_form_id.
           lrt_formid = CORRESPONDING #( lst_filter_select_options-range ).
            delete lrt_formid where low is initial.

          WHEN lc_vers.
          lrt_version = CORRESPONDING #( lst_filter_select_options-range ).
            delete lrt_version where low is initial.


          WHEN lc_code_grp.
          lrt_codegroup = CORRESPONDING #( lst_filter_select_options-range ).
            delete lrt_codegroup where low is initial.

        ENDCASE. "/ CASE lst_filter_select_options-property.

        CLEAR: lrs_filter.
      ENDLOOP. "/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
    ENDIF. "/ IF im_filter_select_options IS NOT INITIAL.

    "/ Get the NonObject Forms from Form Assignment Table
    SELECT * FROM /odsmfe/tb_foass
      WHERE category = @lc_category
        AND formid  IN @lrt_formid[]
        AND version IN @lrt_version[]  INTO TABLE @lit_foass.

    IF sy-subrc = 0 AND lit_foass[] IS NOT INITIAL.
      "/ Get the NonObject Forms details from the Form Master table
      SELECT * FROM /odsmfe/tb_fomst  FOR ALL ENTRIES IN @lit_foass
        WHERE formid  = @lit_foass-formid
          AND version = @lit_foass-version INTO TABLE @lit_fomst.
    ENDIF. "/ IF sy-subrc = 0 AND lit_foass[] IS NOT INITIAL.

    "/ Map the Form details to the final table
    IF lit_fomst[] IS NOT INITIAL.
    gitib_entity      = VALUE #( FOR lst_fomst in lit_fomst
   (   formid           = lst_fomst-formid
        version         = lst_fomst-version
        codegroup     = lst_fomst-codegruppe
        formname      = lst_fomst-form_name
        description    = lst_fomst-description
        formdata       = lst_fomst-formdata
        formhtml       = lst_fomst-formhtml
        formmodel    = lst_fomst-formmodel
        active            = lst_fomst-active
        theme           = lst_fomst-theme
        stylesheet     = lst_fomst-stylesheet
        createdon     = lst_fomst-created_on
        createdby     = lst_fomst-created_by
        modifiedon   = lst_fomst-modified_on
        modifiedby   = lst_fomst-modified_by
        category       = lst_fomst-formcategory
        functionalarea = lst_fomst-funareaid
        subarea           = lst_fomst-subareaid ) ).
    ENDIF. "/ IF lit_fomst[] IS NOT INITIAL.

    "/ Mapping properties from the backend to the Gateway output response table
    ex_response_data = CORRESPONDING #( gitib_entity ).


  ENDMETHOD.
ENDCLASS.
