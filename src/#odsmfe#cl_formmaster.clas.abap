class /ODSMFE/CL_FORMMASTER definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMASTER .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMASTER .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMMASTER IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 11/05/2020
* Transport No.          : ES1K901774
* Program Description    : Displays form FormMaster data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lst_formmaster TYPE /odsmfe/tb_fomst.
************************************************************************
* Main Section
************************************************************************
* Read data from FE request
    im_data_provider->read_entry_data( IMPORTING es_data = lst_formmaster ).

    IF lst_formmaster IS NOT INITIAL.
* Modify data as per data received from FE
      MODIFY /odsmfe/tb_fomst FROM lst_formmaster.       "#EC CI_SUBRC.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING lst_formmaster TO gstib_entity.
      ENDIF.

    ENDIF.
    GET REFERENCE OF gstib_entity INTO ex_entity.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.


***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 11/05/2020
* Transport No.          : ES1K901774
* Program Description    : Displays form FormMaster data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : Sravan Kumar
* Change Date            : 13/10/2021
* Transport No.          : ES1K902842
* Change Description     : Added Description/Funareaid/Subareaid Fields
*                          in the select query and respetive table
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA:  lit_formmast  TYPE STANDARD TABLE OF /odsmfe/tb_fomst,
           lit_mformmast TYPE STANDARD TABLE OF /odsmfe/tb_fomst,
           lst_entity    TYPE /odsmfe/cl_pr_formui_mpc=>tt_formmaster.

    DATA: lst_formmaster_get_entityset TYPE LINE OF /odsmfe/cl_pr_formui_mpc=>tt_formmaster,
          lst_key_tab                  TYPE /iwbep/s_mgw_name_value_pair.

    DATA: lrt_formid       TYPE TABLE OF /odsmfe/st_core_range_str,
          lrs_it_formid    TYPE /odsmfe/st_core_range_str,
          lrs_it_version   TYPE /odsmfe/st_core_range_str,
          lrt_version      TYPE TABLE OF /odsmfe/st_core_range_str,
          lrs_it_codegroup TYPE /odsmfe/st_core_range_str,
          lrt_codegroup    TYPE TABLE OF /odsmfe/st_core_range_str.

* Constants
    CONSTANTS:     lc_e         TYPE string VALUE 'E',
                   lc_i         TYPE string VALUE 'I',
                   lc_eq        TYPE string VALUE 'EQ',
                   lc_formid    TYPE string VALUE 'FormID',
                   lc_version   TYPE string VALUE 'Version',
                   lc_codegroup TYPE string VALUE 'CodeGroup'.

* field symbols
    FIELD-SYMBOLS: <lfsst_form>    TYPE /odsmfe/tb_fomst,
                   <lfsst_frmdefn> TYPE /odsmfe/tb_frmdf. "Added b Sravan
* SOC Sravan kumar "++ ES1K902842
    TYPES: BEGIN OF ltys_fomst,
             formid        TYPE /odsmfe/de_formid,
             version       TYPE /odsmfe/de_version,
             form_name     TYPE /odsmfe/de_formname,
             formdesc      TYPE /odsmfe/de_formdesc,
             fun_area_name TYPE /odsmfe/de_proc_name,
             sub_area_name TYPE /odsmfe/de_description,
             category_desc TYPE /odsmfe/de_cat_desc,
           END OF ltys_fomst.
    DATA: lst_form     TYPE  ltys_fomst,
          lit_form     TYPE STANDARD TABLE OF ltys_fomst,
          lit_formdefn TYPE STANDARD TABLE OF /odsmfe/tb_frmdf.

    TYPES:
      BEGIN OF ts_formmaster01,
        formid      TYPE /odsmfe/de_formid,
        version     TYPE /odsmfe/de_version,
        codegruppe  TYPE qcodegrp,
        form_name   TYPE /odsmfe/de_formname,
        description TYPE /odsmfe/de_formdesc,
        formdata    TYPE string,
        formhtml    TYPE string,
        formmodel   TYPE string,
        active      TYPE flag,
        theme       TYPE /odsmfe/de_theme,
        stylesheet  TYPE /odsmfe/de_style,
        created_on  TYPE timestamp,
        created_by  TYPE /odsmfe/de_createdby,
        modified_on TYPE timestamp,
        modified_by TYPE /odsmfe/de_modifiedby,
        funarea     TYPE c LENGTH 50,
        subarea     TYPE c LENGTH 255,
        catdesc     TYPE c LENGTH 50,
      END OF ts_formmaster01.

    DATA: lst_formmstr TYPE ts_formmaster01,
          lit_formmsrr TYPE STANDARD TABLE OF ts_formmaster01.
* EOC Sravan kumar "++ ES1K902842
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

** Maps key fields to function module parameters
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

          WHEN lc_codegroup.
            lrs_it_codegroup-sign   = lc_i.
            lrs_it_codegroup-option = lc_eq.
            lrs_it_codegroup-low    = lst_key_tab-value.
            APPEND lrs_it_codegroup TO lrt_codegroup.
            CLEAR lrs_it_codegroup.

        ENDCASE.
      ENDLOOP.
    ENDIF.
* Fetch data from FormMaster checking Form Assignment Tabe
    SELECT
      /odsmfe/tb_fomst~formid
      /odsmfe/tb_fomst~version
      /odsmfe/tb_fomst~codegruppe
      /odsmfe/tb_fomst~form_name
      /odsmfe/tb_fomst~description
      /odsmfe/tb_fomst~formdata
      /odsmfe/tb_fomst~formhtml
      /odsmfe/tb_fomst~formmodel
      /odsmfe/tb_fomst~active
      /odsmfe/tb_fomst~theme
      /odsmfe/tb_fomst~stylesheet
      /odsmfe/tb_fomst~created_on
      /odsmfe/tb_fomst~created_by
      /odsmfe/tb_fomst~modified_on
      /odsmfe/tb_fomst~modified_by
      /odsmfe/tb_fomst~plant
      /odsmfe/tb_fomst~formcategory
      /odsmfe/tb_fomst~funareaid
      /odsmfe/tb_fomst~subareaid
       FROM /odsmfe/tb_fomst
     INNER JOIN /odsmfe/tb_foass
     ON /odsmfe/tb_fomst~formid EQ /odsmfe/tb_foass~formid
     AND /odsmfe/tb_fomst~version EQ /odsmfe/tb_foass~version
     INTO CORRESPONDING FIELDS OF TABLE lit_formmast
     WHERE /odsmfe/tb_foass~active EQ abap_true
      AND /odsmfe/tb_fomst~formid IN lrt_formid
      AND /odsmfe/tb_fomst~version IN lrt_version
      AND /odsmfe/tb_fomst~codegruppe IN lrt_codegroup.

* Fetch data from FormMaster checking Form Manual Assignment Tabe
    SELECT
       /odsmfe/tb_fomst~formid
      /odsmfe/tb_fomst~version
      /odsmfe/tb_fomst~codegruppe
      /odsmfe/tb_fomst~form_name
      /odsmfe/tb_fomst~description
      /odsmfe/tb_fomst~formdata
      /odsmfe/tb_fomst~formhtml
      /odsmfe/tb_fomst~formmodel
      /odsmfe/tb_fomst~active
      /odsmfe/tb_fomst~theme
      /odsmfe/tb_fomst~stylesheet
      /odsmfe/tb_fomst~created_on
      /odsmfe/tb_fomst~created_by
      /odsmfe/tb_fomst~modified_on
      /odsmfe/tb_fomst~modified_by
      /odsmfe/tb_fomst~plant
      /odsmfe/tb_fomst~formcategory
      /odsmfe/tb_fomst~funareaid
      /odsmfe/tb_fomst~subareaid
       FROM /odsmfe/tb_fomst
             INNER JOIN /odsmfe/tb_fmass
      ON /odsmfe/tb_fomst~formid EQ /odsmfe/tb_fmass~formid
      AND /odsmfe/tb_fomst~version EQ /odsmfe/tb_fmass~version
      INTO CORRESPONDING FIELDS OF TABLE lit_mformmast
      WHERE /odsmfe/tb_fmass~active EQ abap_true
      AND /odsmfe/tb_fomst~formid IN lrt_formid
      AND /odsmfe/tb_fomst~version IN lrt_version
      AND /odsmfe/tb_fomst~codegruppe IN lrt_codegroup.

* Fetch UnAssigned Forms when requested
    IF lrt_formid IS NOT INITIAL AND lrt_version IS NOT INITIAL.
      SELECT
       /odsmfe/tb_fomst~formid
      /odsmfe/tb_fomst~version
      /odsmfe/tb_fomst~codegruppe
      /odsmfe/tb_fomst~form_name
      /odsmfe/tb_fomst~description
      /odsmfe/tb_fomst~formdata
      /odsmfe/tb_fomst~formhtml
      /odsmfe/tb_fomst~formmodel
      /odsmfe/tb_fomst~active
      /odsmfe/tb_fomst~theme
      /odsmfe/tb_fomst~stylesheet
      /odsmfe/tb_fomst~created_on
      /odsmfe/tb_fomst~created_by
      /odsmfe/tb_fomst~modified_on
      /odsmfe/tb_fomst~modified_by
      /odsmfe/tb_fomst~plant
      /odsmfe/tb_fomst~formcategory
      /odsmfe/tb_fomst~funareaid
      /odsmfe/tb_fomst~subareaid
         FROM /odsmfe/tb_fomst
        APPENDING CORRESPONDING FIELDS OF TABLE lit_formmast
        WHERE /odsmfe/tb_fomst~active EQ abap_true
    AND /odsmfe/tb_fomst~formid IN lrt_formid
    AND /odsmfe/tb_fomst~version IN lrt_version
    AND /odsmfe/tb_fomst~codegruppe IN lrt_codegroup.
    ENDIF.

    IF lit_mformmast IS NOT INITIAL.
      SORT lit_mformmast BY formid version.
      DELETE ADJACENT DUPLICATES FROM lit_mformmast COMPARING formid version.
      APPEND LINES OF lit_mformmast TO lit_formmast.
    ENDIF.

    IF lit_formmast IS NOT INITIAL.
      SORT lit_formmast BY formid version.
      DELETE ADJACENT DUPLICATES FROM lit_formmast COMPARING formid version.
    ENDIF.
* SOC Sravan kumar "++ ES1K902842

** Select all functional areas
*    SELECT * FROM /odsmfe/tb_funar INTO TABLE @DATA(lt_funar).
** Select all sub areas
*    SELECT * FROM /odsmfe/tb_subar INTO TABLE @DATA(lt_subar).
** Select all categories
*    SELECT * FROM /odsmfe/tb_fmcat INTO TABLE @DATA(lt_fmcat).
*
*    IF lit_formmast IS NOT INITIAL.
** Fecthing Form Definition Table Data
*      SELECT * FROM /odsmfe/tb_frmdf
*               INTO TABLE lit_formdefn
*               FOR ALL ENTRIES IN lit_formmast
*               WHERE formname = lit_formmast-formid
*               AND  version = lit_formmast-version.
**                WHERE  formguid NE space AND
**                       version EQ lit_formmast-version
**                AND    formname EQ lit_formmast-form_name.
*
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
* EOC Sravan kumar "++ ES1K902842

    IF lit_formmast IS NOT INITIAL.
* Display all data
      LOOP AT lit_formmast ASSIGNING <lfsst_form>.

* SOC Sravan kumar "++ ES1K902842
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
* EOC Sravan kumar "++ ES1K902842

        MOVE-CORRESPONDING <lfsst_form> TO gstib_entity.
** Get Entity method is requested
        IF im_key_tab IS NOT INITIAL.
          GET REFERENCE OF gstib_entity INTO ex_entity.
        ELSE.
          APPEND gstib_entity TO gitib_entity.
          CLEAR: gstib_entity.
        ENDIF.
      ENDLOOP.
* Get EntitySet method is requested
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
