class /ODSMFE/CL_FORMRESAPRVALSTATUS definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMRESPONSEAPPROVALSTATUS .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMRESPONSEAPPROVALSTATUS .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_DELETE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMRESAPRVALSTATUS IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Create the Form Response Approval
*                          Status
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : SKOTRA
* Change Date            : 11/03/2022
* Transport No.          : ES1K902967
* Change Description     : Refaactoring SP07
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formresponse ,
             formid            TYPE /odsmfe/tb_finst-formid,
             version           TYPE c LENGTH 3,
             forminstanceid    TYPE c LENGTH 50,
             approverid        TYPE c LENGTH 20,
             formsubmittedby   TYPE c LENGTH 35,
             counter           TYPE c LENGTH 12,
             formcontentstatus TYPE c LENGTH 20,
             remarks           TYPE c LENGTH 100,
             createddate       TYPE dats,
             createdtime       TYPE tims,
             formname          TYPE c LENGTH 50,
             iterationrequired TYPE char1,
           END OF ltys_formresponse .

    DATA: lst_formresponse TYPE ltys_formresponse,
          lst_form         TYPE /odsmfe/tb_finst.

    DATA: lv_aufnr          TYPE aufnr.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF lst_formresponse IS NOT INITIAL.
      lst_form-formid            = lst_formresponse-formid.
      lst_form-version           = lst_formresponse-version.
      lst_form-forminstanceid    = lst_formresponse-forminstanceid.
      lst_form-approverid        = lst_formresponse-approverid.
      lst_form-formsubmittedby   = lst_formresponse-formsubmittedby.
      lst_form-counter           = lst_formresponse-counter.
      lst_form-formcontentstatus = lst_formresponse-formcontentstatus.
      lst_form-remarks           = lst_formresponse-remarks.
      lst_form-createddate       = lst_formresponse-createddate.
      lst_form-createdtime       = lst_formresponse-createdtime.
      lst_form-formname          = lst_formresponse-formname.
      lst_form-iterationrequired = lst_formresponse-iterationrequired.

      IF lst_formresponse-formid IS NOT INITIAL.
* Insert data to Table /ODSMFE/TB_FINST
        INSERT /odsmfe/tb_finst FROM lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.

* Iteration Required - Set the form response status to draft
      IF lst_formresponse-iterationrequired = abap_true.
        UPDATE /odsmfe/tb_forsp SET isdraft = abap_true
                    WHERE instanceid = lst_formresponse-forminstanceid.
      ENDIF.

    ENDIF.

* Update Delta table for WO - Exchange Mechanism

    SELECT SINGLE wo_num
           FROM /odsmfe/tb_forsp
           INTO lv_aufnr
           WHERE instanceid = lst_formresponse-forminstanceid.

    IF lv_aufnr IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lv_aufnr ).
      ENDIF.
*--End of changes SKOTRA - ES1K902967
    ENDIF.


* Send the Form ID to Front end application
    gstib_entity-formid            = lst_formresponse-formid.
    gstib_entity-version           = lst_formresponse-version.
    gstib_entity-forminstanceid    = lst_formresponse-forminstanceid.
    gstib_entity-approverid        = lst_formresponse-approverid.
    gstib_entity-formsubmittedby   = lst_formresponse-formsubmittedby.
    gstib_entity-counter           = lst_formresponse-counter.
    gstib_entity-formcontentstatus = lst_formresponse-formcontentstatus.
    gstib_entity-remarks           = lst_formresponse-remarks.
    gstib_entity-createddate       = lst_formresponse-createddate.
    gstib_entity-createdtime       = lst_formresponse-createdtime.
    gstib_entity-formname          = lst_formresponse-formname.
    gstib_entity-iterationrequired = lst_formresponse-iterationrequired.

    GET REFERENCE OF gstib_entity INTO ex_entity.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Delete Response Approval
*                          Status
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    DATA: lrt_formid            TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_version           TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_forminstanceid    TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_approverid        TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_formsubmittedby   TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_counter           TYPE TABLE OF /odsmfe/st_core_range_str,
          lst_key_tab           TYPE /iwbep/s_mgw_name_value_pair,
          lrs_it_formid         TYPE /odsmfe/st_core_range_str,
          lrs_it_version        TYPE /odsmfe/st_core_range_str,
          lrs_it_forminstanceid TYPE /odsmfe/st_core_range_str,
          lrs_it_approverid     TYPE /odsmfe/st_core_range_str,
          lrs_formsubmittedby   TYPE /odsmfe/st_core_range_str,
          lrs_counter           TYPE /odsmfe/st_core_range_str,
          lit_key_tab           TYPE /iwbep/t_mgw_name_value_pair,
          lst_formresponse      TYPE /odsmfe/tb_finst,
          lit_final             TYPE STANDARD TABLE OF /odsmfe/tb_finst.

* Constants
    CONSTANTS: lc_e               TYPE string VALUE 'E',
               lc_i               TYPE string VALUE 'I',
               lc_eq              TYPE string VALUE 'EQ',
               lc_formid          TYPE string VALUE 'FormID',
               lc_version         TYPE string VALUE 'Version',
               lc_forminstanceid  TYPE string VALUE 'FormInstanceID',
               lc_approverid      TYPE string VALUE 'ApproverID',
               lc_formsubmittedby TYPE string VALUE 'FormSubmittedBy',
               lc_counter         TYPE string VALUE 'Counter'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF im_key_tab IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_formid.
            lrs_it_formid-sign = lc_i.
            lrs_it_formid-option = lc_eq.
            lrs_it_formid-low = lst_key_tab-value.
            lst_formresponse-formid = lst_key_tab-value.
            APPEND lrs_it_formid TO lrt_formid.
            CLEAR lrs_it_formid.

          WHEN lc_version.
            lrs_it_version-sign   = lc_i.
            lrs_it_version-option = lc_eq.
            lrs_it_version-low    = lst_key_tab-value.
            lst_formresponse-version = lst_key_tab-value.
            APPEND lrs_it_version TO lrt_version.
            CLEAR lrs_it_version.

          WHEN lc_forminstanceid.
            lrs_it_forminstanceid-sign   = lc_i.
            lrs_it_forminstanceid-option = lc_eq.
            lrs_it_forminstanceid-low    = lst_key_tab-value.
            lst_formresponse-forminstanceid = lst_key_tab-value.
            APPEND lrs_it_forminstanceid TO lrt_forminstanceid.
            CLEAR lrs_it_forminstanceid.

          WHEN lc_approverid.
            lrs_it_approverid-sign   = lc_i.
            lrs_it_approverid-option = lc_eq.
            lrs_it_approverid-low    = lst_key_tab-value.
            lst_formresponse-approverid = lst_key_tab-value.
            APPEND lrs_it_approverid TO lrt_approverid.
            CLEAR lrs_it_approverid.

          WHEN lc_formsubmittedby.
            lrs_formsubmittedby-sign   = lc_i.
            lrs_formsubmittedby-option = lc_eq.
            lrs_formsubmittedby-low    = lst_key_tab-value.
            lst_formresponse-formsubmittedby = lst_key_tab-value.
            APPEND lrs_formsubmittedby TO lrt_formsubmittedby.
            CLEAR lrs_formsubmittedby.

          WHEN lc_counter.
            lrs_counter-sign   = lc_i.
            lrs_counter-option = lc_eq.
            lrs_counter-low    = lst_key_tab-value.
            lst_formresponse-counter = lst_key_tab-value.
            APPEND lrs_counter TO lrt_counter.
            CLEAR lrs_counter.

        ENDCASE.
      ENDLOOP.
    ENDIF.

* Fetching data from table
    SELECT * FROM /odsmfe/tb_finst
             INTO TABLE lit_final
             WHERE   formid          IN lrt_formid
             AND     version         IN lrt_version
             AND     forminstanceid  IN lrt_forminstanceid
             AND     approverid      IN lrt_approverid
             AND     formsubmittedby IN lrt_formsubmittedby
             AND     counter         IN lrt_counter.

    IF sy-subrc EQ 0.
* Delete data to Table /ODSMFE/TB_FINST
      DELETE /odsmfe/tb_finst FROM TABLE lit_final.

* Send the Form ID to Front end application
      gstib_entity-formid            = lst_formresponse-formid.
      gstib_entity-version           = lst_formresponse-version.
      gstib_entity-forminstanceid    = lst_formresponse-forminstanceid.
      gstib_entity-approverid        = lst_formresponse-approverid.
      gstib_entity-formsubmittedby   = lst_formresponse-formsubmittedby.
      gstib_entity-counter           = lst_formresponse-counter.
      gstib_entity-formcontentstatus = lst_formresponse-formcontentstatus.
      gstib_entity-remarks           = lst_formresponse-remarks.
      gstib_entity-createddate       = lst_formresponse-createddate.
      gstib_entity-createdtime       = lst_formresponse-createdtime.
      gstib_entity-formname          = lst_formresponse-formname.

      GET REFERENCE OF gstib_entity INTO ex_entity.
    ENDIF.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Update the Form Response Approval
*                          Status
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : SKOTRA
* Change Date            : 11/03/2022
* Transport No.          : ES1K902967
* Change Description     : Refaactoring SP07
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formresponse ,
             formid            TYPE /odsmfe/tb_finst-formid,
             version           TYPE c LENGTH 3,
             forminstanceid    TYPE c LENGTH 50,
             approverid        TYPE c LENGTH 20,
             formsubmittedby   TYPE c LENGTH 35,
             counter           TYPE c LENGTH 12,
             formcontentstatus TYPE c LENGTH 20,
             remarks           TYPE c LENGTH 100,
             createddate       TYPE dats,
             createdtime       TYPE tims,
             formname          TYPE c LENGTH 50,
             iterationrequired TYPE char1,
           END OF ltys_formresponse .

    DATA: lst_formresponse TYPE ltys_formresponse,
          lst_form         TYPE /odsmfe/tb_finst.

* Variables
    DATA: lv_formid     TYPE char50,
          lv_version    TYPE char3,
          lv_forminstid TYPE char50,
          lv_approverid TYPE char40,
          lv_formsubby  TYPE char35,
          lv_counter    TYPE char12.

    DATA: lv_aufnr          TYPE aufnr.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF lst_formresponse IS NOT INITIAL.
      lst_form-formid            = lst_formresponse-formid.
      lst_form-version           = lst_formresponse-version.
      lst_form-forminstanceid    = lst_formresponse-forminstanceid.
      lst_form-approverid        = lst_formresponse-approverid.
      lst_form-formsubmittedby   = lst_formresponse-formsubmittedby.
      lst_form-counter           = lst_formresponse-counter.
      lst_form-formcontentstatus = lst_formresponse-formcontentstatus.
      lst_form-remarks           = lst_formresponse-remarks.
      lst_form-createddate       = lst_formresponse-createddate.
      lst_form-createdtime       = lst_formresponse-createdtime.
      lst_form-formname          = lst_formresponse-formname.
      lst_form-iterationrequired = lst_formresponse-iterationrequired.

* Fetching data from table
      SELECT SINGLE formid version forminstanceid approverid formsubmittedby counter
      FROM /odsmfe/tb_finst
      INTO (lv_formid,lv_version,lv_forminstid,lv_approverid,lv_formsubby, lv_counter)
      WHERE   formid          = lst_formresponse-formid
      AND     version         = lst_formresponse-version
      AND     forminstanceid  = lst_formresponse-forminstanceid
      AND     approverid      = lst_formresponse-approverid
      AND     formsubmittedby = lst_formresponse-formsubmittedby
      AND     counter         = lst_formresponse-counter.

      IF lv_forminstid IS NOT INITIAL.
* Update data to Table /ODSMFE/TB_FINST
        MODIFY /odsmfe/tb_finst FROM lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.
* Iteration Required - Set the form response status to draft
      IF lst_formresponse-iterationrequired = abap_true.
        UPDATE /odsmfe/tb_forsp SET isdraft = abap_true
                    WHERE instanceid = lst_formresponse-forminstanceid.
      ENDIF.
    ENDIF.


* Update Delta table for WO - Exchange Mechanism

    SELECT SINGLE wo_num
           FROM /odsmfe/tb_forsp
           INTO lv_aufnr
           WHERE instanceid = lst_formresponse-forminstanceid.

    IF lv_aufnr IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lv_aufnr ).
      ENDIF.
*--End of changes SKOTRA - ES1K902967
    ENDIF.
* Update Delta table for WO - Exchange Mechanism

* Send the Form ID to Front end application
    gstib_entity-formid            = lst_formresponse-formid.
    gstib_entity-version           = lst_formresponse-version.
    gstib_entity-forminstanceid    = lst_formresponse-forminstanceid.
    gstib_entity-approverid        = lst_formresponse-approverid.
    gstib_entity-formsubmittedby   = lst_formresponse-formsubmittedby.
    gstib_entity-counter           = lst_formresponse-counter.
    gstib_entity-formcontentstatus = lst_formresponse-formcontentstatus.
    gstib_entity-remarks           = lst_formresponse-remarks.
    gstib_entity-createddate       = lst_formresponse-createddate.
    gstib_entity-createdtime       = lst_formresponse-createdtime.
    gstib_entity-formname          = lst_formresponse-formname.
    gstib_entity-iterationrequired = lst_formresponse-iterationrequired.

    GET REFERENCE OF gstib_entity INTO ex_entity.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
*&----------------------------------------------------------------------*
* PROGRAM ID           :/ODSMFE/IF_GET_ENTITYSET_BAPI                   *
* developer id :hsangam                                      *
* SUPPLIER             :OnDevice Solutions                              *
* DATE                 :2023/05/15                                      *
* DEVELOPMENT ID       :/ODSMFE/MA\ HPQC                                *
* CHANGE REQUEST (CTS) :DR0K######                                      *
*=======================================================================*
* COPIED FROM         : (CLONED PROGRAM)                                *
* TITLE               : (PROGRAM TITLE)                                 *
* OTHER RELATED OBJ   : (OBJECT NAMES)                                  *
*=======================================================================*
* CHANGE HISTORY LOG                                                    *
* CHANGE HISTORY LOG                                                    *
*-----------------------------------------------------------------------*
* CHANGE HISTORY LOG                                                    *
* MOD.NO.| DATE     | NAME           | CORRECTION NUMBER  |CHANGE       *
*                                                          REFERENCE    *
*-----------------------------------------------------------------------*
*        |          |                |                    |             *
*                                                                       *
* DESCRIPTION:                                                          *
*-----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 31/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Form Response Approval
*                          Status
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : ODS
* Change Date            : 08/02/2023
* Transport No.          :
* Change Description     : Added Filter Options
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
* Internal Table
    DATA: lit_forminstance          TYPE  STANDARD TABLE OF /odsmfe/tb_finst,
          lrt_formid                TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_version               TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_forminstid            TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_approverid            TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_formsubby             TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_counter               TYPE TABLE OF /odsmfe/st_core_range_str,
          lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair,
          lrs_formid                TYPE /odsmfe/st_core_range_str,
          lrs_version               TYPE /odsmfe/st_core_range_str,
          lrs_forminstid            TYPE /odsmfe/st_core_range_str,
          lrs_approverid            TYPE /odsmfe/st_core_range_str,
          lrs_formsubby             TYPE /odsmfe/st_core_range_str,
          lrs_counter               TYPE /odsmfe/st_core_range_str,
          lst_filter_select_options TYPE /iwbep/s_mgw_select_option,
          lst_filter_range          TYPE /iwbep/s_cod_select_option.


* Constants
    CONSTANTS: lc_e          TYPE string VALUE 'E',
               lc_i          TYPE string VALUE 'I',
               lc_eq         TYPE string VALUE 'EQ',
               lc_formid     TYPE string VALUE 'FormID',
               lc_version    TYPE string VALUE 'Version',
               lc_forminstid TYPE string VALUE 'FormInstanceID',
               lc_approverid TYPE string VALUE 'ApproverID',
               lc_formsubby  TYPE string VALUE 'FormSubmittedBy',
               lc_counter    TYPE string VALUE 'Counter'.

* Field Symbols
    FIELD-SYMBOLS <lfsst_form> TYPE /odsmfe/tb_finst.
    DATA: lit_entity_temp TYPE TABLE OF /odsmfe/tb_forsp,
          lst_entity_temp TYPE /odsmfe/tb_forsp,
          lst_valid_wo    TYPE  /odsmfe/pm_valid_aufnr_str,
          lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str.

    DATA: lv_mobileuser  TYPE string,
          lv_delta_token TYPE timestamp.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF im_key_tab IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_formid.
            lrs_formid-sign = lc_i.
            lrs_formid-option = lc_eq.
            lrs_formid-low = lst_key_tab-value.
            APPEND lrs_formid TO lrt_formid.
            CLEAR lrs_formid.

          WHEN lc_version.
            lrs_version-sign   = lc_i.
            lrs_version-option = lc_eq.
            lrs_version-low    = lst_key_tab-value.
            APPEND lrs_version TO lrt_version.
            CLEAR lrs_version.

          WHEN lc_forminstid.
            lrs_forminstid-sign   = lc_i.
            lrs_forminstid-option = lc_eq.
            lrs_forminstid-low    = lst_key_tab-value.
            APPEND lrs_forminstid TO lrt_forminstid.
            CLEAR lrs_forminstid.

          WHEN lc_approverid.
            lrs_approverid-sign   = lc_i.
            lrs_approverid-option = lc_eq.
            lrs_approverid-low    = lst_key_tab-value.
            APPEND lrs_approverid TO lrt_approverid.
            CLEAR lrs_approverid.

          WHEN lc_formsubby.
            lrs_formsubby-sign   = lc_i.
            lrs_formsubby-option = lc_eq.
            lrs_formsubby-low    = lst_key_tab-value.
            lv_mobileuser        = lst_key_tab-value.
            APPEND lrs_formsubby TO lrt_formsubby.
            CLEAR lrs_formsubby.

          WHEN lc_counter.
            lrs_counter-sign   = lc_i.
            lrs_counter-option = lc_eq.
            lrs_counter-low    = lst_key_tab-value.
            APPEND lrs_counter TO lrt_counter.
            CLEAR lrs_counter.
        ENDCASE.
      ENDLOOP.
    ENDIF.

"SOC by ODS
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter_select_options.
        CASE lst_filter_select_options-property.
          WHEN lc_formid.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_formid-sign = lc_i.
              lrs_formid-option = lc_eq.
              lrs_formid-low = lst_filter_range-low.
              APPEND lrs_formid to lrt_formid.
            ENDIF.

          WHEN lc_version.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_version-sign = lc_i.
              lrs_version-option = lc_eq.
              lrs_version-low = lst_filter_range-low.
              APPEND lrs_version to lrt_version.
            ENDIF.

          WHEN lc_forminstid.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_forminstid-sign = lc_i.
              lrs_forminstid-option = lc_eq.
              lrs_forminstid-low = lst_filter_range-low.
              APPEND lrs_forminstid to lrt_forminstid.
            ENDIF.

          WHEN lc_approverid.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_approverid-sign = lc_i.
              lrs_approverid-option = lc_eq.
              lrs_approverid-low = lst_filter_range-low.
              APPEND lrs_approverid to lrt_approverid.
            ENDIF.

          WHEN lc_formsubby.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_formsubby-sign = lc_i.
              lrs_formsubby-option = lc_eq.
              lrs_formsubby-low = lst_filter_range-low.
              lv_mobileuser = lst_filter_range-low.
              APPEND lrs_formsubby to lrt_formsubby.
            ENDIF.

          WHEN lc_counter.
            READ TABLE lst_filter_select_options-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL.
              lrs_counter-sign = lc_i.
              lrs_counter-option = lc_eq.
              lrs_counter-low = lst_filter_range-low.
              APPEND lrs_counter to lrt_counter.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.
"EOC by ODS

    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

    DATA(lr_getworkorder) = NEW /odsmfe/cl_get_workorder_data( ).

    IF lr_getworkorder IS BOUND.
      lr_getworkorder->gmib_get_workorder_data(
      EXPORTING
        im_mobileuser = lv_mobileuser
        im_tech_request_context = im_tech_request_context
        im_filter_select_options = im_filter_select_options
        im_entity_name =  im_entity_name
      IMPORTING
        lit_valid_wo = lit_valid_wo ).
    ENDIF.

    SORT lit_valid_wo.
    DELETE ADJACENT DUPLICATES FROM lit_valid_wo COMPARING ALL FIELDS.
* Fetch Response Capture from /ODS/RESPCAP for entered user
    IF lit_valid_wo IS NOT INITIAL.
* Fetch the Forms associated with work orders
      SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
      INTO CORRESPONDING FIELDS OF TABLE lit_entity_temp
      FOR ALL ENTRIES IN lit_valid_wo
      WHERE wo_num = lit_valid_wo-aufnr.                "#EC CI_NOFIELD
      IF sy-subrc = 0.
        SORT lit_entity_temp BY instanceid.
      ENDIF.
    ENDIF.
* Fetch the non associated forms
    SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
    APPENDING CORRESPONDING FIELDS OF TABLE lit_entity_temp
    WHERE nonobjtype = abap_true                        "#EC CI_NOFIELD
      AND created_by = lv_mobileuser.
    IF sy-subrc = 0.
      SORT lit_entity_temp BY instanceid.
    ENDIF.

* Fetching data
    IF lit_entity_temp IS NOT INITIAL.
      SELECT  *
              FROM /odsmfe/tb_finst
              INTO CORRESPONDING FIELDS OF TABLE lit_forminstance
              FOR ALL ENTRIES IN lit_entity_temp
              WHERE /odsmfe/tb_finst~formid         = lit_entity_temp-formid
              AND /odsmfe/tb_finst~version          = lit_entity_temp-version
              AND /odsmfe/tb_finst~forminstanceid   = lit_entity_temp-instanceid
              AND /odsmfe/tb_finst~approverid       IN lrt_approverid
              AND /odsmfe/tb_finst~formsubmittedby  IN lrt_formsubby
              AND /odsmfe/tb_finst~counter  IN lrt_counter.
    ENDIF.
* Sorting & Deleting duplicates Forms
    IF sy-subrc = 0  AND lit_forminstance IS NOT INITIAL.
      SORT lit_forminstance BY formid version forminstanceid.
      DELETE ADJACENT DUPLICATES FROM lit_forminstance COMPARING ALL FIELDS.
    ENDIF.

* TimeStamp Field for supplying Delta token
    GET TIME STAMP FIELD lv_delta_token.

* Export the delta token
    ex_response_context-deltatoken = lv_delta_token.

    IF lit_forminstance IS NOT INITIAL.
* Display all data
      LOOP AT lit_forminstance ASSIGNING <lfsst_form>.
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
    ENDIF.                                                                " IF LIT_FORMINSTANCE IS NOT INITIAL Line No. :253

  ENDMETHOD.
ENDCLASS.
