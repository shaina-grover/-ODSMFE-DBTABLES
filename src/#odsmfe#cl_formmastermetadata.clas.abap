class /ODSMFE/CL_FORMMASTERMETADATA definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMASTERMETADATA .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMASTERMETADATA .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMMASTERMETADATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Form Master Meta
*                          Data
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : Sravan Kumar
* Change Date            : 13/10/2021
* Transport No.          : ES1K902842
* Change Description     : Added Description/Funareaid/Subareaid Fields
*                          in the select query
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
* Internal Table
    DATA: lit_formmaster TYPE  STANDARD TABLE OF /odsmfe/tb_fomst,
          lrt_version    TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_formid     TYPE TABLE OF /odsmfe/st_core_range_str,
          lst_key_tab    TYPE /iwbep/s_mgw_name_value_pair,
          lrs_it_formid  TYPE /odsmfe/st_core_range_str,
          lrs_it_version TYPE /odsmfe/st_core_range_str.

* Constants
    CONSTANTS: lc_e       TYPE string VALUE 'E',
               lc_i       TYPE string VALUE 'I',
               lc_eq      TYPE string VALUE 'EQ',
               lc_formid  TYPE string VALUE 'FormID',
               lc_version TYPE string VALUE 'Version'.

* Field Symbols
    FIELD-SYMBOLS: <lfsst_form>    TYPE /odsmfe/tb_fomst,
                   <lfsst_frmdefn> TYPE /odsmfe/tb_frmdf. "Added b Sravan

** SOC Sravan kumar "++ ES1K902842
*    TYPES: BEGIN OF ltys_fomst,
*             formid        TYPE /odsmfe/de_formid,
*             version       TYPE /odsmfe/de_version,
*             form_name     TYPE /odsmfe/de_formname,
*             formdesc      TYPE /odsmfe/de_formdesc,
*             fun_area_name TYPE /odsmfe/de_proc_name,
*             sub_area_name TYPE /odsmfe/de_description,
*             category_desc TYPE /odsmfe/de_cat_desc,
*           END OF ltys_fomst.
*    DATA: lst_form     TYPE  ltys_fomst,
*          lit_form     TYPE STANDARD TABLE OF ltys_fomst,
*          lit_formdefn TYPE STANDARD TABLE OF /odsmfe/tb_frmdf.
*    TYPES:
*      BEGIN OF ts_formmaster01,
*        form_name    TYPE c LENGTH 50,
*        formid       TYPE c LENGTH 50,
*        version      TYPE c LENGTH 3,
*        created_by   TYPE c LENGTH 12,
*        formcategory TYPE c LENGTH 30,
*        active       TYPE flag,
*        description  TYPE c LENGTH 250,
*        funarea      TYPE c LENGTH 50,
*        subarea      TYPE c LENGTH 255,
*        catdesc      TYPE c LENGTH 50,
*      END OF ts_formmaster01.
*
*    DATA: lst_formmstr TYPE ts_formmaster01,
*          lit_formmsrr TYPE STANDARD TABLE OF ts_formmaster01.
** EOC Sravan kumar "++ ES1K902842
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF im_key_tab IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_formid.
            lrs_it_formid-sign = lc_i.
            lrs_it_formid-option = lc_eq.
            lrs_it_formid-low = lst_key_tab-value.
            APPEND lrs_it_formid TO lrt_formid.
            CLEAR lrs_it_formid.

          WHEN lc_version.
            lrs_it_version-sign   = lc_i.
            lrs_it_version-option = lc_eq.
            lrs_it_version-low    = lst_key_tab-value.
            APPEND lrs_it_version TO lrt_version.
            CLEAR lrs_it_version.
        ENDCASE.
      ENDLOOP.
    ENDIF.

* Fetching Active Forms from FormMaster Table
    SELECT formid version form_name description active formcategory created_by funareaid subareaid "Added description/funareaid/subareaid "ES1K902842
    FROM /odsmfe/tb_fomst
    INTO CORRESPONDING FIELDS OF TABLE lit_formmaster
    WHERE /odsmfe/tb_fomst~active EQ abap_true
     AND /odsmfe/tb_fomst~formid  IN lrt_formid
     AND /odsmfe/tb_fomst~version IN lrt_version.

* Sorting & Deleting duplicates Forms
    IF sy-subrc = 0  AND lit_formmaster IS NOT INITIAL.
      SORT lit_formmaster BY formid version.
      DELETE ADJACENT DUPLICATES FROM lit_formmaster COMPARING formid version.
    ENDIF.
* SOC Sravan kumar "++ ES1K902842

** Select all functional areas
*    SELECT * FROM /odsmfe/tb_funar INTO TABLE @DATA(lt_funar).
** Select all sub areas
*    SELECT * FROM /odsmfe/tb_subar INTO TABLE @DATA(lt_subar).
** Select all categories
*    SELECT * FROM /odsmfe/tb_fmcat INTO TABLE @DATA(lt_fmcat).
*
*    IF lit_formmaster IS NOT INITIAL.
** Fecthing Form Definition Table Data
*      SELECT * FROM /odsmfe/tb_frmdf
*               INTO TABLE lit_formdefn
*               FOR ALL ENTRIES IN lit_formmaster
*               WHERE formname = lit_formmaster-formid
*               AND  version = lit_formmaster-version.
**                WHERE  formguid NE space AND
**                       version EQ lit_formmast-version
**                AND    formname EQ lit_formmast-form_name.
*
** Display all data
*      LOOP AT lit_formdefn ASSIGNING <lfsst_frmdefn>.
** Get Functional area name
*        READ TABLE lt_funar INTO DATA(ls_funar) WITH KEY funareaid = <lfsst_frmdefn>-funareaid.
*        IF sy-subrc = 0.
*          lst_form-fun_area_name = ls_funar-procname.
*        ELSE.
*          lst_form-fun_area_name = <lfsst_frmdefn>-funareaid.
*        ENDIF.
** Get Sub area name
*        READ TABLE lt_subar INTO DATA(ls_subar) WITH KEY subareaid = <lfsst_frmdefn>-subareaid.
*        IF sy-subrc = 0.
*          lst_form-sub_area_name = ls_subar-procname.
*        ELSE.
*          lst_form-sub_area_name = <lfsst_frmdefn>-subareaid.
*        ENDIF.
** Get Category
*        READ TABLE lt_fmcat INTO DATA(ls_fmcat) WITH KEY catid = <lfsst_frmdefn>-category.
*        IF sy-subrc = 0.
*          lst_form-category_desc = ls_fmcat-catdesc.
*        ELSE.
*          lst_form-category_desc = <lfsst_frmdefn>-category.
*        ENDIF.
*        APPEND lst_form TO lit_form.
*      ENDLOOP.
*      CLEAR :lst_form,ls_subar,ls_fmcat,ls_funar,lt_fmcat, lt_subar,lt_funar.
*
*    ENDIF.
** EOC Sravan kumar "++ ES1K902842

    IF lit_formmaster IS NOT INITIAL.
* Display all data
      LOOP AT lit_formmaster ASSIGNING <lfsst_form>.
** SOC Sravan kumar "++ ES1K902842
*        READ TABLE lit_form INTO lst_form
*        WITH KEY formid    = <lfsst_form>-formid
*                 version   = <lfsst_form>-version
*                 form_name = <lfsst_form>-form_name.
*        IF sy-subrc EQ 0.
*          lst_formmstr-description = lst_form-formdesc.
*          lst_formmstr-funarea     = lst_form-fun_area_name.
*          lst_formmstr-subarea     = lst_form-sub_area_name.
*          lst_formmstr-catdesc     = lst_form-category_desc.
*        ENDIF.
*        MOVE-CORRESPONDING <lfsst_form> TO lst_formmstr.
** EOC Sravan kumar "++ ES1K902842

        MOVE-CORRESPONDING <lfsst_form> TO gstib_entity.

* Get Entity method is requested
        IF im_key_tab IS NOT INITIAL.
          GET REFERENCE OF gstib_entity INTO ex_entity.
        ELSE.
          APPEND gstib_entity TO gitib_entity.
        ENDIF.
      ENDLOOP.

* Get EntitySet method is requested
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
