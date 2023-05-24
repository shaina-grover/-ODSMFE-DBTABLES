class /ODSMFE/CL_FORMASSINGMENT definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMASSINGMENT .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMASSINGMENT .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMASSINGMENT IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - HSANGAM
* Creation Date          : 11/04/2023
* Transport No.          : ES1K903728
* Program Description    : FormAssignment
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
*    Types
    TYPES : BEGIN OF ltys_foass,
              formid              TYPE /odsmfe/de_formid,                               "ODS Form ID
              version             TYPE /odsmfe/de_version,                              "ODS Version
              ordertype           TYPE aufart,                                          "Order Type
              steus               TYPE steus,                                           "Control key
              plnty               TYPE plnty,                                           "Task List Type
              plnnr               TYPE plnnr,                                           "Key for Task List Group
              plnal               TYPE plnal,                                           "Group Counter
              oprnum              TYPE vornr,                                           "Operation/Activity Number
              zaehl               TYPE /odsmfe/de_intrncounter,                         "ODS MFE: Internal Counter
              eqtyp               TYPE eqtyp,                                           "Equipment category
              fltyp               TYPE fltyp,                                           "Functional location category
              eqart               TYPE eqart,                                           "Type of Technical Object
              roleid              TYPE /odsmfe/de_roleid,                               "Role ID
              category            TYPE /odsmfe/de_formcategory,                         "Form Category
              jobtype             TYPE /odsmfe/de_jobtype,                              "ODS Job type
              mandatory           TYPE /odsmfe/de_mandatory,                            "Mandatory Form
              flowsequence        TYPE /odsmfe/de_flowsequence,                         "Flow Sequence
              multiplesub         TYPE /odsmfe/de_multiplesub,                          "Multiple submissions
              occur               TYPE /odsmfe/de_occur,                                "Occurances
              postnotification    TYPE /odsmfe/de_postnotification,                     "PostNotification
              postcharacteristics TYPE /odsmfe/de_postcharacteristics,                  "Post Characteristics
              theme               TYPE /odsmfe/de_theme,                                "Theme body class value
              stylesheet          TYPE /odsmfe/de_style,                                "Style Sheet
              createdon           TYPE /odsmfe/de_createdon,                            "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
              createdby           TYPE /odsmfe/de_createdby,                            "ODS Created By
              modifiedon          TYPE /odsmfe/de_modifiedon,                           "ModifiedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
              modifiedby          TYPE /odsmfe/de_modifiedby,                           "ODS Modified By
              active              TYPE /odsmfe/de_active,                               "Active
              deleted             TYPE /odsmfe/de_delted,                               "Deleted
            END OF ltys_foass.
*Tables and structures
    DATA : lst_foass TYPE ltys_foass,                                                   "structure for ltys_foass
           lst_form  TYPE /odsmfe/tb_foass.                                             "structure for /odsmfe/tb_foass
*Constants
    CONSTANTS : lc_theme    TYPE string VALUE 'theme-grid'.                             "theme
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    im_data_provider->read_entry_data( IMPORTING es_data = lst_foass ).

    IF lst_foass IS NOT INITIAL.
      lst_form-formid                = lst_foass-formid.
      lst_form-version               = lst_foass-version.
      lst_form-ordertype             = lst_foass-ordertype.
      lst_form-steus                 = lst_foass-steus.
      lst_form-plnty                 = lst_foass-plnty.
      lst_form-plnal                 = lst_foass-plnal.
      lst_form-oprnum                = lst_foass-oprnum.
      lst_form-zaehl                 = lst_foass-zaehl.
      lst_form-eqart                 = lst_foass-eqart.
      lst_form-roleid                = lst_foass-roleid.
      lst_form-category              = lst_foass-category.
      lst_form-jobtype               = lst_foass-jobtype.
      lst_form-mandatory             = lst_foass-mandatory.
      lst_form-flowsequence          = lst_foass-flowsequence.
      lst_form-multiplesub           = lst_foass-multiplesub.
      lst_form-occur                 = lst_foass-occur.
      lst_form-postnotification      = lst_foass-postnotification.
      lst_form-postcharacteristics   = lst_foass-postcharacteristics.
      lst_form-theme                 = lc_theme.
      lst_form-stylesheet            = lst_foass-stylesheet.
      lst_form-createdon             = lst_foass-createdon.
      lst_form-createdby             = lst_foass-createdby.
      lst_form-modifiedon            = lst_foass-modifiedon.
      lst_form-modifiedby            = lst_foass-modifiedby.
      lst_form-active                = abap_true.
      lst_form-deleted               = lst_foass-deleted.
    ENDIF."/ IF lst_foass IS NOT INITIAL.

    "/ Add the requested data to the DB table
    IF lst_foass-formid IS NOT INITIAL.
      MODIFY /odsmfe/tb_foass FROM lst_form.                                                        "#EC CI_TABLES.
      IF sy-subrc <> 0.
        CLEAR lst_form.
      ENDIF."/ IF sy-subrc <> 0.
    ENDIF."/ IF lst_foass-formid IS NOT INITIAL.

    "/ Mapping the data to export in the gateway after updating the table.
    gstib_entity-formid                = lst_foass-formid.
    gstib_entity-version               = lst_foass-version.
    gstib_entity-ordertype             = lst_foass-ordertype.
    gstib_entity-steus                 = lst_foass-steus.
    gstib_entity-plnty                 = lst_foass-plnty.
    gstib_entity-plnal                 = lst_foass-plnal.
    gstib_entity-oprnum                = lst_foass-oprnum.
    gstib_entity-zaehl                 = lst_foass-zaehl.
    gstib_entity-eqart                 = lst_foass-eqart.
    gstib_entity-roleid                = lst_foass-roleid.
    gstib_entity-category              = lst_foass-category.
    gstib_entity-jobtype               = lst_foass-jobtype.
    gstib_entity-mandatory             = lst_foass-mandatory.
    gstib_entity-flowsequence          = lst_foass-flowsequence.
    gstib_entity-multiplesub           = lst_foass-multiplesub.
    gstib_entity-occur                 = lst_foass-occur.
    gstib_entity-postnotification      = lst_foass-postnotification.
    gstib_entity-postcharacteristics   = lst_foass-postcharacteristics.
    gstib_entity-theme                 = lst_foass-theme.
    gstib_entity-stylesheet            = lst_foass-stylesheet.
    gstib_entity-createdon             = lst_foass-createdon.
    gstib_entity-createdby             = lst_foass-createdby.
    gstib_entity-modifiedon            = lst_foass-modifiedon.
    gstib_entity-modifiedby            = lst_foass-modifiedby.
    gstib_entity-active                = lst_foass-active.
    gstib_entity-deleted               = lst_foass-deleted.

    IF gstib_entity IS NOT INITIAL.
      GET REFERENCE OF gstib_entity INTO ex_entity.
    ENDIF."/ IF gstib_entity IS NOT INITIAL.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS-HSANGAM
* Creation Date          :19-04-2023
* Transport No.          :ES1K903728
* Program Description    :To update the assigned form
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
    TYPES : BEGIN OF ltys_foass,
              formid              TYPE /odsmfe/de_formid,                               "ODS Form ID
              version             TYPE /odsmfe/de_version,                              "ODS Version
              ordertype           TYPE aufart,                                          "Order Type
              steus               TYPE steus,                                           "Control key
              plnty               TYPE plnty,                                           "Task List Type
              plnnr               TYPE plnnr,                                           "Key for Task List Group
              plnal               TYPE plnal,                                           "Group Counter
              oprnum              TYPE vornr,                                           "Operation/Activity Number
              zaehl               TYPE /odsmfe/de_intrncounter,                         "ODS MFE: Internal Counter
              eqtyp               TYPE eqtyp,                                           "Equipment category
              fltyp               TYPE fltyp,                                           "Functional location category
              eqart               TYPE eqart,                                           "Type of Technical Object
              roleid              TYPE /odsmfe/de_roleid,                               "Role ID
              category            TYPE /odsmfe/de_formcategory,                         "Form Category
              jobtype             TYPE /odsmfe/de_jobtype,                              "ODS Job type
              mandatory           TYPE /odsmfe/de_mandatory,                            "Mandatory Form
              flowsequence        TYPE /odsmfe/de_flowsequence,                         "Flow Sequence
              multiplesub         TYPE /odsmfe/de_multiplesub,                          "Multiple submissions
              occur               TYPE /odsmfe/de_occur,                                "Occurances
              postnotification    TYPE /odsmfe/de_postnotification,                     "PostNotification
              postcharacteristics TYPE /odsmfe/de_postcharacteristics,                  "Post Characteristics
              theme               TYPE /odsmfe/de_theme,                                "Theme body class value
              stylesheet          TYPE /odsmfe/de_style,                                "Style Sheet
              createdon           TYPE /odsmfe/de_createdon,                            "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
              createdby           TYPE /odsmfe/de_createdby,                            "ODS Created By
              modifiedon          TYPE /odsmfe/de_modifiedon,                           "ModifiedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
              modifiedby          TYPE /odsmfe/de_modifiedby,                           "ODS Modified By
              active              TYPE /odsmfe/de_active,                               "Active
              deleted             TYPE /odsmfe/de_delted,                               "Deleted
            END OF ltys_foass.

    DATA : lst_foass  TYPE ltys_foass,                                                  "Structure for ODS : Form Assignment Table
           lst_form   TYPE /odsmfe/tb_foass,                                            "Structure for ODS : Form Assignment Table
           lst_foass1 TYPE /odsmfe/tb_foass,                                            "Structure for ODS : Form Assignment Table
           lit_foass  TYPE TABLE OF /odsmfe/tb_foass.                                   "Internal table for ODS : Form Assignment Table

    CONSTANTS : lc_theme    TYPE string VALUE 'theme-grid',                             "Theme body class value
                lc_cat      TYPE string VALUE 'NonObject',                              "Form Category
                lc_roleid   TYPE string VALUE 'Forms',                                  "Role ID
                lc_activate TYPE c VALUE 'X'.                                           "Active
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    im_data_provider->read_entry_data( IMPORTING es_data = lst_foass ).

    IF lst_foass IS NOT INITIAL.
      lst_form-formid = lst_foass-formid.
      lst_form-version = lst_foass-version.
      lst_form-ordertype = lst_foass-ordertype.
      lst_form-steus = lst_foass-steus.
      lst_form-plnty = lst_foass-plnty.
      lst_form-plnnr = lst_foass-plnnr.
      lst_form-plnal = lst_foass-plnal.
      lst_form-oprnum = lst_foass-oprnum.
      lst_form-zaehl = lst_foass-zaehl.
      lst_form-eqtyp = lst_foass-eqtyp.
      lst_form-fltyp = lst_foass-fltyp.
      lst_form-eqart = lst_foass-eqart.
      lst_form-roleid = lst_foass-roleid.
      lst_form-category = lst_foass-category.
      lst_form-jobtype = lst_foass-jobtype.
      lst_form-mandatory = lst_foass-mandatory.
      lst_form-flowsequence = lst_foass-flowsequence.
      lst_form-multiplesub = lst_foass-multiplesub.
      lst_form-occur = lst_foass-occur.
      lst_form-postnotification = lst_foass-postnotification.
      lst_form-postcharacteristics = lst_foass-postcharacteristics.
      lst_form-theme = lc_theme.
      lst_form-stylesheet = lst_foass-stylesheet.
      lst_form-createdon = lst_foass-createdon.
      lst_form-createdby = lst_foass-createdby.
      lst_form-modifiedon = lst_foass-modifiedon.
      lst_form-modifiedby = lst_foass-modifiedby.
      lst_form-active = lc_activate.
      lst_form-deleted = lst_foass-deleted.
    ENDIF." /IF lst_foass IS NOT INITIAL.

    SELECT SINGLE mandt formid  version ordertype steus plnty plnnr plnal oprnum zaehl
      eqtyp fltyp eqart roleid category jobtype  mandatory flowsequence multiplesub occur
      postnotification postcharacteristics theme stylesheet createdon createdby modifiedon
      modifiedby active deleted
      FROM /odsmfe/tb_foass INTO lst_foass1
      WHERE formid = lst_foass-formid
      AND version = lst_foass-version
      AND ordertype = lst_foass-ordertype
      AND steus = lst_foass-steus
      AND plnty = lst_foass-plnty
      AND plnnr = lst_foass-plnnr
      AND plnal = lst_foass-plnal
      AND oprnum = lst_foass-oprnum
      AND zaehl = lst_foass-zaehl
      AND eqtyp = lst_foass-eqtyp
      AND fltyp = lst_foass-fltyp.

    IF sy-subrc = 0.
      UPDATE /odsmfe/tb_foass FROM lst_form. "#EC CI_SUBRC  "#EC CI_TABLES.
    ENDIF."/ IF sy-subrc = 0.

    "/ Mapping the data to export in the gateway after updating the table.
    gstib_entity-formid = lst_foass-formid.
    gstib_entity-version = lst_foass-version.
    gstib_entity-ordertype = lst_foass-ordertype.
    gstib_entity-steus = lst_foass-steus.
    gstib_entity-plnty = lst_foass-plnty.
    gstib_entity-plnal = lst_foass-plnal.
    gstib_entity-oprnum = lst_foass-oprnum.
    gstib_entity-zaehl = lst_foass-zaehl.
    gstib_entity-eqart = lst_foass-eqart.
    gstib_entity-roleid = lst_foass-roleid.
    gstib_entity-category = lst_foass-category.
    gstib_entity-jobtype = lst_foass-jobtype.
    gstib_entity-mandatory = lst_foass-mandatory.
    gstib_entity-flowsequence = lst_foass-flowsequence.
    gstib_entity-multiplesub = lst_foass-multiplesub.
    gstib_entity-occur = lst_foass-occur.
    gstib_entity-postnotification = lst_foass-postnotification.
    gstib_entity-postcharacteristics = lst_foass-postcharacteristics.
    gstib_entity-theme = lst_foass-theme.
    gstib_entity-stylesheet = lst_foass-stylesheet.
    gstib_entity-createdon = lst_foass-createdon.
    gstib_entity-createdby = lst_foass-createdby.
    gstib_entity-modifiedon = lst_foass-modifiedon.
    gstib_entity-modifiedby = lst_foass-modifiedby.
    gstib_entity-active = lst_foass-active.
    gstib_entity-deleted = lst_foass-deleted.

    GET REFERENCE OF gstib_entity INTO ex_entity.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :SKAMMARI
* Creation Date          :11/05/2020
* Transport No.          :ES1K901774
* Program Description    :Displays form FormAssingment data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :SKAMMARI
* Change Date            :29/01/2021
* Transport No.          :ES1K902499
* Change Description     :Internal counter field returning zeros as
*                         its Numeric so FE requested to send it as blank
***********************************************************************
* Program Author (SID)   :SKOTRA
* Change Date            :10/03/2022
* Transport No.          :ES1K902969
* Change Description     :Refactoring SP07
***********************************************************************
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :PPRIYANKA
* Change Date            :03/03/2023
* Transport No.          :ES1K903619
* Change Description     :Logic to fetch userrole via PFCG
************************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/ Tables and Structures
    DATA: lit_filter_vals TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
          lst_filter_vals TYPE /odsmfe/tb_filtr,
          lst_filter      TYPE /iwbep/s_mgw_select_option,
          lst_usrroletab  TYPE /odsmfe/cl_exchmechwo=>gty_roles.

    "/ Variables
    DATA :  lv_pfcg_role  TYPE  /odsmfe/de_mfe_value,
            lv_class      TYPE  /odsmfe/de_mfe_value,
            lv_user       TYPE uname,
            lv_role       TYPE agval,       "agrfield
            lv_mobileuser TYPE string,
            lv_userrole   TYPE /odsmfe/de_roleid,
            lv_deleted    TYPE /odsmfe/de_delted.

    "/ Range Tables and Range Structures
    DATA: lrs_filter_range TYPE /iwbep/s_cod_select_option,
          lrs_category     TYPE /iwbep/s_cod_select_option,
          lrt_category     TYPE STANDARD TABLE OF /iwbep/s_cod_select_option.

    "/ Reference Object
    DATA: lo_auth TYPE REF TO object.

    "/ Constants
    CONSTANTS:lc_pfcg_role       TYPE string VALUE 'PFCG_ROLE',
              lc_meth            TYPE string VALUE 'ROLE_ASSIGNMENT',
              lc_true            TYPE string VALUE 'TRUE',
              lc_category        TYPE string VALUE 'CATEGORY',
              lc_i               TYPE char1  VALUE 'I',
              lc_eq              TYPE char2  VALUE 'EQ',
              lc_e               TYPE string VALUE 'E',
              lc_x               TYPE char1  VALUE 'X',
              lc_entity_set_name TYPE string VALUE 'FormAssingmentSet'.

    "/ Field-Symbols
    FIELD-SYMBOLS:  <lfsst_entity> TYPE /odsmfe/cl_pr_formui_mpc=>ts_formassingment. "++ES1K902499

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                      M A I N   S E C T I O N                           *
* -----------------------------------------------------------------------*

    "/ Read Filter Values
    IF im_filter_select_options IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.

          WHEN lc_category.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lrs_filter_range-low IS NOT INITIAL.
              lrs_category-sign   = lc_i.
              lrs_category-option = lc_eq.
              lrs_category-low    = lrs_filter_range-low.
              APPEND lrs_category TO lrt_category.
              CLEAR: lrs_category, lrs_filter_range.
            ENDIF.

        ENDCASE.
      ENDLOOP. "/ LOOP AT im_filter_select_options INTO lst_filter.
    ENDIF. "/ IF im_filter_select_options IS NOT INITIAL.

    "/ Get the Logged in User details
    lv_mobileuser = sy-uname.

**********Start of Modificaion- TO fetch userrole via PFCG *************
    SELECT SINGLE param_value
       FROM /odsmfe/tb_apcon
       INTO lv_pfcg_role
       WHERE param_name = lc_pfcg_role
       AND active = lc_x.

    IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

      lv_user = lv_mobileuser.

      SELECT SINGLE param_value
              FROM /odsmfe/tb_apcon
              INTO lv_class
              WHERE param_name = lc_meth
              AND active = lc_x.

      IF sy-subrc EQ 0.
        CREATE OBJECT lo_auth TYPE (lv_class).
      ENDIF.

      TRY.
          CALL METHOD lo_auth->(lc_meth)    "Get PFCG Role ID
            EXPORTING
              iv_uname = lv_user
            IMPORTING
              ev_field = lv_role.

          IF lv_role IS NOT INITIAL.
            lv_userrole = lv_role.
          ENDIF.

        CATCH /iwbep/cx_mgw_busi_exception.
      ENDTRY.
**********End of Modificaion- TO fetch userrole via PFCG - ES1K903522*****************
    ELSE.

*--Start of changes SKOTRA - ES1K902967
      "/ Get reference for fetching value of user role table
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lst_usrroletab = lr_exchtab->get_userrole_tab( ).
        CONCATENATE lst_usrroletab-low 'AS a INNER JOIN' lst_usrroletab-high 'AS b ON a~roleid = b~roleid' INTO DATA(lv_userroles) SEPARATED BY space.
*--End of changes SKOTRA - ES1K902967

        "/ check User Roles, Assignment type and Dashboard ID
        SELECT SINGLE a~roleid
              FROM (lv_userroles) "Changes SKOTRA - ES1K902969
              INTO lv_userrole
              WHERE a~userid = lv_mobileuser.

        IF sy-subrc NE 0.
          CLEAR lv_userrole.
        ENDIF.

      ENDIF. "Changes SKOTRA - ES1K902969
    ENDIF. "/ IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

    SELECT * FROM /odsmfe/tb_foass
             INTO TABLE gitib_entity
             WHERE formid NE space
               AND roleid EQ lv_userrole
               AND active = abap_true.

    "/ Get the NonObject Forms
    IF lrt_category[] IS NOT INITIAL.
      SELECT *
        FROM /odsmfe/tb_foass
        INTO TABLE gitib_entity
        WHERE category IN lrt_category[]
          AND active   EQ abap_true.
    ENDIF.

    "/ Get the Mobile filter detials
    SELECT entitysetname tabname field recordno
                 field_descr sign options low high active
                 FROM /odsmfe/tb_filtr INTO CORRESPONDING FIELDS OF TABLE lit_filter_vals
                 WHERE entitysetname = lc_entity_set_name
                 AND active = abap_on.
    IF sy-subrc EQ 0.
      SORT lit_filter_vals BY field.
    ENDIF.

    "/ Read Deleted Flag
    READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'DELETED' active = lc_x BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE lst_filter_vals-low TO lv_deleted.
      CLEAR: lst_filter_vals.
    ENDIF.

    IF lv_deleted IS NOT INITIAL.
      DELETE gitib_entity WHERE deleted IS NOT INITIAL.
    ENDIF.

* SOC by ODS ES1K902499
    LOOP AT gitib_entity ASSIGNING <lfsst_entity>.
      IF <lfsst_entity>-zaehl EQ '00000000'.
        CLEAR:<lfsst_entity>-zaehl.
      ENDIF.
    ENDLOOP.
    UNASSIGN :<lfsst_entity>.
* EOC by ODS ES1K902499

    IF sy-subrc = 0 AND gitib_entity IS NOT INITIAL.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
