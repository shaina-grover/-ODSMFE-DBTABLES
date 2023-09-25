class /ODSMFE/CL_MYFORMS_APPROVAL definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_DELETE_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_MYFORMS_APPROVAL IMPLEMENTATION.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : ODS-HSANGAM
* Creation Date          : 16-05-2023
* Transport No.          : ES1K903727
* Program Description    : Submitting the instance for Approve or Reject
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

    "/ Types
    TYPES: BEGIN OF ltys_formresponse,
             formid            TYPE /odsmfe/tb_finst-formid,                            "ODS Form ID
             version           TYPE c LENGTH 3,                                         "ODS Version
             forminstanceid    TYPE c LENGTH 50,                                        "ODS MFE InstanceId
             approverid        TYPE c LENGTH 20,                                        "ODS MFE: Form Approver ID
             formsubmittedby   TYPE c LENGTH 35,                                        "ODS MFE: Form Submitted BY
             counter           TYPE c LENGTH 12,                                        "ODS: Counter
             formcontentstatus TYPE c LENGTH 20,                                        "ODS MFE: Form Content Status
             remarks           TYPE c LENGTH 100,                                       "ODS MFE: Remarks
             createddate       TYPE dats,                                               "Field of type DATS
             createdtime       TYPE timn,                                               "ODS MFE: Created Time
             formname          TYPE c LENGTH 50,                                        "ODSMFE Form Name
             iterationrequired TYPE c LENGTH 1,                                              "Single-Character Indicator
           END OF ltys_formresponse,

          BEGIN OF ltys_forsp,
             instanceid TYPE /odsmfe/de_instanceid,                                     "ODS MFE InstanceId
             formid     TYPE /odsmfe/de_formid,                                         "ODS Form ID
             version    TYPE /odsmfe/de_version,                                        "ODS Version
           END OF ltys_forsp.

    "/ Internal tables & Structures
    DATA: lst_formresponse TYPE ltys_formresponse,                                      "Internal table for ltys_formresponse
          lst_form         TYPE /odsmfe/tb_finst,                                       "Structure for ODS MFE: FormInstance Status Table
          lit_forsp        TYPE TABLE OF ltys_forsp,                                    "Internal table for Table to Capture Response
          lst_forsp        TYPE ltys_forsp.                                             "Structure for Table to Capture Response

    "/ Variables
    DATA : lv_instanceid TYPE /odsmfe/de_instanceid.                                    "ODS MFE InstanceId

*------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
*------------------------------------------------------------------------*
*------------------------------------------------------------------------*
*            M A I N  S E C T I O N                                      *
*------------------------------------------------------------------------*

    "/ Get the requested details
*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).
*
*    "/ Mapping requested data to create an entry in the table
*    IF lst_formresponse IS NOT INITIAL.
*      lst_form-formid            = lst_formresponse-formid.
*      lst_form-version           = lst_formresponse-version.
*      lst_form-forminstanceid    = lst_formresponse-forminstanceid.
*      lst_form-approverid        = lst_formresponse-approverid.
*      lst_form-formsubmittedby   = lst_formresponse-formsubmittedby.
*      lst_form-counter           = lst_formresponse-counter.
*      lst_form-formcontentstatus = lst_formresponse-formcontentstatus.
*      lst_form-remarks           = lst_formresponse-remarks.
*      lst_form-createddate       = lst_formresponse-createddate.
*      lst_form-createdtime       = lst_formresponse-createdtime.
*      lst_form-formname          = lst_formresponse-formname.
*      lst_form-iterationrequired = lst_formresponse-iterationrequired.
*    ENDIF."/ IF lst_formresponse IS NOT INITIAL.
*
*    "/ Insert requested data to table ODS MFE: FormInstance Status Table
*    IF lst_form IS NOT INITIAL.
*      INSERT /odsmfe/tb_finst FROM lst_form.
*      IF sy-subrc <> 0.
*        CLEAR lst_form.
*      ENDIF.
*    ENDIF."/ IF sy-subrc = 0.
*
*    GET REFERENCE OF gstib_entity INTO ex_entity.
*
  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_DELETE_ENTITYSET.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : ODS-HSANGAM
* Creation Date          : 22-05-2023
* Transport No.          : ES1K903727
* Program Description    : Delete the requested instances
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

    "/ Tables & Structures
    DATA : lit_final             TYPE STANDARD TABLE OF /odsmfe/tb_finst,                        "Internal table for ODS : Form Assignment Table
               lst_formresponse      TYPE /odsmfe/tb_finst.                                          "Structure for ODS : Form Assignment Table

    "/ Rangen Tables & Structures
    DATA: lrt_formid            TYPE TABLE OF /odsmfe/st_core_range_str,                         "FormId Range Structure
          lrt_version           TYPE TABLE OF /odsmfe/st_core_range_str,                         "Version Range Structure
          lrt_forminstanceid    TYPE TABLE OF /odsmfe/st_core_range_str,                         "FormInstanceId Range Structure
          lrt_approverid        TYPE TABLE OF /odsmfe/st_core_range_str,                         "ApproverId Range Structure
          lrt_formsubmittedby   TYPE TABLE OF /odsmfe/st_core_range_str,                         "FormSubmitted Range Structure
          lrt_counter           TYPE TABLE OF /odsmfe/st_core_range_str.                         "Counter Range Structure

    "/ Constants
    CONSTANTS: lc_e               TYPE string VALUE 'E',                                         "Single-Character Indicator
               lc_i               TYPE string VALUE 'I',                                         "Single-Character Indicator
               lc_eq              TYPE string VALUE 'EQ',                                        "Single-Character Indicator
               lc_formid          TYPE string VALUE 'FormID',                                    "Filter Options: FormId
               lc_version         TYPE string VALUE 'Version',                                   "Filter Options: Version
               lc_forminstanceid  TYPE string VALUE 'FormInstanceID',                            "Filter Options: FormInstanceId
               lc_approverid      TYPE string VALUE 'ApproverID',                                "Filter Options: ApproverId
               lc_formsubmittedby TYPE string VALUE 'FormSubmittedBy',                           "Filter Options: FormSubmittedBy
               lc_counter         TYPE string VALUE 'Counter'.                                   "Filter Options: Counter
*------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
*------------------------------------------------------------------------*

*------------------------------------------------------------------------*
*            M A I N            S E C T I O N                            *
*------------------------------------------------------------------------*

*    "/ Read the key values
*    IF im_key_tab IS NOT INITIAL.
*      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*        CASE lst_key_tab-name.
*          WHEN lc_formid.
*            lrs_filter-sign = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low = lst_key_tab-value.
*            lst_formresponse-formid = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_formid.
*
*          WHEN lc_version.
*            lrs_filter-sign   = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low    = lst_key_tab-value.
*            lst_formresponse-version = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_version.
*
*          WHEN lc_forminstanceid.
*            lrs_filter-sign   = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low    = lst_key_tab-value.
*            lst_formresponse-forminstanceid = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_forminstanceid.
*
*          WHEN lc_approverid.
*            lrs_filter-sign   = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low    = lst_key_tab-value.
*            lst_formresponse-approverid = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_approverid.
*
*          WHEN lc_formsubmittedby.
*            lrs_filter-sign   = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low    = lst_key_tab-value.
*            lst_formresponse-formsubmittedby = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_formsubmittedby.
*
*          WHEN lc_counter.
*            lrs_filter-sign   = lc_i.
*            lrs_filter-option = lc_eq.
*            lrs_filter-low    = lst_key_tab-value.
*            lst_formresponse-counter = lst_key_tab-value.
*            APPEND lrs_filter TO lrt_counter.
*        ENDCASE."/ CASE lst_key_tab-name.
*        CLEAR : lrs_filter , lst_key_tab.
*      ENDLOOP."/ LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*    ENDIF."/ IF im_key_tab IS NOT INITIAL.
*
*    "/ Fetching data from table ODS MFE: FormInstance Status Table
*    SELECT * FROM /odsmfe/tb_finst
*             INTO TABLE lit_final
*             WHERE   formid          IN lrt_formid[]
*             AND     version         IN lrt_version[]
*             AND     forminstanceid  IN lrt_forminstanceid[]
*             AND     approverid      IN lrt_approverid[]
*             AND     formsubmittedby IN lrt_formsubmittedby[]
*             AND     counter         IN lrt_counter[].
*
*    IF sy-subrc EQ 0.
*      "/ Delete requested data from Table ODS MFE: FormInstance Status Table
*      DELETE /odsmfe/tb_finst FROM TABLE lit_final.
*      IF sy-subrc = 0.
*        clear lit_final.
*      ENDIF.
*    ENDIF."/ IF sy-subrc EQ 0.

  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : ODS-HSANGAM
* Creation Date          : 16-05-2023
* Transport No.          : ES1K903727
* Program Description    : Approving and Rejecting instances in myForms Application
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

    "/ Types
    TYPES: BEGIN OF ltys_formresponse,
             formid            TYPE /odsmfe/tb_finst-formid,                          "ODS Form ID
             version           TYPE c LENGTH 3,                                       "ODS Version
             forminstanceid    TYPE c LENGTH 50,                                      "ODS MFE InstanceId
             approverid        TYPE c LENGTH 20,                                      "ODS MFE: Form Approver ID
             formsubmittedby   TYPE c LENGTH 35,                                      "ODS MFE: Form Submitted BY
             counter           TYPE c LENGTH 12,                                      "ODS: Counter
             formcontentstatus TYPE c LENGTH 20,                                      "ODS MFE: Form Content Status
             remarks           TYPE c LENGTH 100,                                     "ODS MFE: Remarks
             createddate       TYPE dats,                                             "Field of type DATS
             createdtime       TYPE timn,                                             "ODS MFE: Created Time
             formname          TYPE c LENGTH 50,                                      "ODSMFE Form Name
             iterationrequired TYPE c LENGTH 1,                                            "Single-Character Indicator
           END OF ltys_formresponse.

    "/ Table & Structures
    DATA: lst_formresponse TYPE ltys_formresponse,                                      "Structure for types ltys_formresponse
          lst_form         TYPE /odsmfe/tb_finst.                                       "Structure for ODS MFE: FormInstance Status Table

    "/ Variables
    DATA: lv_formid     TYPE c LENGTH 50,                                                    "ODS Form ID
          lv_version    TYPE c LENGTH 3,                                                     "ODS Version
          lv_forminstid TYPE c LENGTH 50,                                                    "ODS MFE InstanceId
          lv_approverid TYPE c LENGTH 40,                                                    "ODS MFE: Form Approver ID
          lv_formsubby  TYPE c LENGTH 35,                                                    "ODS MFE: Form Submitted BY
          lv_counter    TYPE c LENGTH 12,                                                    "ODS: Counter
          lv_date       TYPE  timestamp.                                           "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)

*------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
*------------------------------------------------------------------------*
*------------------------------------------------------------------------*
*                      M A I N    S E C T I O N                          *
*------------------------------------------------------------------------*

    "/ Get the request body details
*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).
*
*    "/ Mapping requested data for updating the table
*    IF lst_formresponse IS NOT INITIAL.
*      lst_form-formid            = lst_formresponse-formid.
*      lst_form-version           = lst_formresponse-version.
*      lst_form-forminstanceid    = lst_formresponse-forminstanceid.
*      lst_form-approverid        = lst_formresponse-approverid.
*      lst_form-formsubmittedby   = lst_formresponse-formsubmittedby.
*      lst_form-counter           = lst_formresponse-counter.
*      lst_form-formcontentstatus = lst_formresponse-formcontentstatus.
*      lst_form-remarks           = lst_formresponse-remarks.
*      lst_form-createddate       = sy-datum.
*      lst_form-createdtime       = sy-timlo.
*      lst_form-formname          = lst_formresponse-formname.
*      lst_form-iterationrequired = lst_formresponse-iterationrequired.
*
*      "/ Fetching data from table ODS MFE: FormInstance Status Table
*      SELECT SINGLE formid version forminstanceid approverid formsubmittedby counter
*      FROM /odsmfe/tb_finst
*      INTO (lv_formid,lv_version,lv_forminstid,lv_approverid,lv_formsubby, lv_counter)
*      WHERE   formid          = lst_formresponse-formid
*      AND     version         = lst_formresponse-version
*      AND     forminstanceid  = lst_formresponse-forminstanceid
*      AND     approverid      = lst_formresponse-approverid
*      AND     formsubmittedby = lst_formresponse-formsubmittedby
*      AND     counter         = lst_formresponse-counter.
*
*      IF lv_forminstid IS NOT INITIAL.
*        "/ Update requested data into Form Approval table
*        MODIFY /odsmfe/tb_finst FROM lst_form.
*        IF sy-subrc <> 0.
*          CLEAR lst_form.
*        ENDIF."/ IF sy-subrc <> 0.
*      ENDIF."/ IF lv_forminstid IS NOT INITIAL.
*
*      "/ Set the Form status to draft in Response Capture Table when the Form Instance is rejected
*      IF lst_formresponse-iterationrequired = abap_true.
*        GET TIME STAMP FIELD lv_date.
*        IF sy-subrc = 0.
*          UPDATE /odsmfe/tb_forsp SET isdraft = abap_true
*                                      modified_on = lv_date
*                                      modified_by = sy-uname
*                                      modified_date = sy-datum
*                                      modified_time = sy-timlo
*          WHERE instanceid = lst_formresponse-forminstanceid.
*        ENDIF."/ IF sy-subrc = 0.
*        IF sy-subrc = 0.
*          CLEAR lst_form.
*        ENDIF.
*      ENDIF."/ IF lst_formresponse-iterationrequired = abap_true.
*    ENDIF."/ IF lst_formresponse IS NOT INITIAL.
*
*    "/ Mapping properties from the backend to the Gateway output response table
*    gstib_entity-formid            = lst_formresponse-formid.
*    gstib_entity-version           = lst_formresponse-version.
*    gstib_entity-forminstanceid    = lst_formresponse-forminstanceid.
*    gstib_entity-approverid        = lst_formresponse-approverid.
*    gstib_entity-formsubmittedby   = lst_formresponse-formsubmittedby.
*    gstib_entity-counter           = lst_formresponse-counter.
*    gstib_entity-formcontentstatus = lst_formresponse-formcontentstatus.
*    gstib_entity-remarks           = lst_formresponse-remarks.
*    gstib_entity-createddate       = lst_formresponse-createddate.
*    gstib_entity-createdtime       = lst_formresponse-createdtime.
*    gstib_entity-formname          = lst_formresponse-formname.
*    gstib_entity-iterationrequired = lst_formresponse-iterationrequired.
*
*    GET REFERENCE OF gstib_entity INTO ex_entity.
  endmethod.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : ODS-HSANGAM
* Creation Date          : 16-05-2023
* Transport No.          : ES1K903727
* Program Description    : Get the submitted  instances for approve or reject
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

    "/ Types
    TYPES : BEGIN OF ltys_fomst,
             formid  TYPE /odsmfe/de_formid,                                   "ODS Form ID
             version TYPE /odsmfe/de_version,                                  "ODS Version
            END OF ltys_fomst,

           BEGIN OF ltys_forsp,
             instanceid TYPE /odsmfe/de_instanceid,                            "ODS MFE InstanceId
             formid     TYPE /odsmfe/de_formid,                                "ODS Form ID
             version    TYPE /odsmfe/de_version,                               "ODS Version
            END OF ltys_forsp.

   	"/ Internal Table
    DATA :lit_foass                 TYPE TABLE OF /odsmfe/tb_foass,                            "Internal table for ODS : Form Assignment Table
          lst_foass                 TYPE /odsmfe/tb_foass,                                     "Structure for ODS : Form Assignment Table
          lit_fomst                 TYPE TABLE OF ltys_fomst,                                  "Internal table for FORM Master Table
          lst_fomst                 TYPE ltys_fomst,                                           "Structure for FORM Master Table
          lit_forsp                 TYPE TABLE OF ltys_forsp,                                  "Internal table for Table to Capture Response
          lst_forsp                 TYPE ltys_forsp,                                           "Structure for Table to Capture Response
          lit_finst                 TYPE TABLE OF /odsmfe/tb_finst,                            "Internal table for ODS MFE: FormInstance Status Table
          lit_forminstance          TYPE STANDARD TABLE OF /odsmfe/tb_finst.   "Internal table for ODS MFE: FormInstance Status Table

      "/ Global table
      DATA: gitib_entity TYPE TABLE OF /ODSMFE/CE_MYFORMSRESPAPPROVAL.

    "/ Range tables and structures
    DATA: lrs_filters               TYPE /odsmfe/st_core_range_str,            "Filters Range Structure
          lrt_formid                TYPE TABLE OF /odsmfe/st_core_range_str,   "FormId Range Structure
          lrt_version               TYPE TABLE OF /odsmfe/st_core_range_str,   "Version Range Structure
          lrt_forminstid            TYPE TABLE OF /odsmfe/st_core_range_str,   "FormInstanceId Range Structure
          lrt_approverid            TYPE TABLE OF /odsmfe/st_core_range_str,   "ApproverId Range Structure
          lrt_formsubby             TYPE TABLE OF /odsmfe/st_core_range_str,   "FormSubmitted Range Structure
          lrt_counter               TYPE TABLE OF /odsmfe/st_core_range_str,   "Counter Range Structure
          lrt_formstatus            TYPE TABLE OF /odsmfe/st_core_range_str.   "FormStatus Range Structure

    "/ Variables
    DATA: lv_mobileuser  TYPE string,                                          "String
          lv_delta_token TYPE timestamp.                                       "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)

    "/ Constants
    CONSTANTS: lc_e          TYPE string VALUE 'E',                            "Single-Character Indicator
               lc_i          TYPE string VALUE 'I',                            "Single-Character Indicator
               lc_eq         TYPE string VALUE 'EQ',                           "Single-Character Indicator
               lc_formid     TYPE string VALUE 'FormID',                       "Filter Options: FormId
               lc_version    TYPE string VALUE 'Version',                      "Filter Options: Version
               lc_forminstid TYPE string VALUE 'FormInstanceID',               "Filter Options: FormInstanceId
               lc_approverid TYPE string VALUE 'ApproverID',                   "Filter Options: ApproverId
               lc_formsubby  TYPE string VALUE 'FormSubmittedBy',              "Filter Options: FormSubmittedBy
               lc_counter    TYPE string VALUE 'Counter',                      "Filter Options: Counter
               lc_formstatus TYPE string VALUE 'FormContentStatus',            "Filter Options: FormContentStatus
               lc_nonobject  TYPE string VALUE 'NonObject'.                    "Filter Options: NonObject

    "/ Field Symbols
    FIELD-SYMBOLS <lfsst_form> TYPE /odsmfe/tb_finst.                          "ODS MFE: FormInstance Status Table
*------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
*------------------------------------------------------------------------*

*------------------------------------------------------------------------*
*            M A I N            S E C T I O N                            *
*------------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO DATA(lst_filter_select_options).
        CASE lst_filter_select_options-name.
          WHEN lc_formid.
           lrt_formid = CORRESPONDING #( lst_filter_select_options-range ).
           DELETE lrt_formid WHERE low IS INITIAL.

          WHEN lc_version.
            lrt_version = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_version WHERE low IS INITIAL.

          WHEN lc_forminstid.
            lrt_forminstid = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_forminstid WHERE low IS INITIAL.

          WHEN lc_approverid.
            lrt_approverid = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_approverid WHERE low IS INITIAL.

          WHEN lc_formsubby.
             lrt_formsubby = CORRESPONDING #( lst_filter_select_options-range ).
             DELETE lrt_formsubby WHERE low IS INITIAL.

          WHEN lc_counter.
             lrt_counter = CORRESPONDING #( lst_filter_select_options-range ).
             DELETE lrt_counter WHERE low IS INITIAL.

          WHEN lc_formstatus.
             lrt_formstatus = CORRESPONDING #( lst_filter_select_options-range ).
         DELETE lrt_formstatus WHERE low IS INITIAL.

        ENDCASE."/ CASE lst_filter_select_options-property.
        CLEAR : lrs_filters , lst_filter_select_options.
      ENDLOOP."/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
    ENDIF."/ IF im_filter_select_options IS NOT INITIAL.

    "/ Fetch all the forms from FormMaster table having category as NonObject
    SELECT * FROM /odsmfe/tb_foass  WHERE category = @lc_nonobject
      AND formid IN @lrt_formid
      AND version IN @lrt_version
      INTO TABLE @lit_foass.

    "/ Fetch all the forms from FormMaster table which are available in FormAssignment table
    IF lit_foass[] IS NOT INITIAL.
      SELECT formid, version FROM /odsmfe/tb_fomst  FOR ALL ENTRIES IN @lit_foass
        WHERE formid = @lit_foass-formid
        AND version = @lit_foass-version
        INTO TABLE @lit_fomst.

      "/ Fetch Response Capture from RESPCAP table for all the forms in FormMaster table
      IF lit_fomst[] IS NOT INITIAL.
        SELECT instanceid, formid, version FROM /odsmfe/tb_forsp  FOR ALL ENTRIES IN @lit_fomst
          WHERE formid = @lit_fomst-formid
          AND  version = @lit_fomst-version
          AND  instanceid IN @lrt_forminstid[]
          INTO TABLE @lit_forsp.

        "/ Fetch all the forms from FormInstanceStatus table for all the Instances in ResponseCapture
        IF lit_forsp[] IS NOT INITIAL.
          SELECT * FROM /odsmfe/tb_finst FOR ALL ENTRIES IN @lit_forsp
            WHERE formid = @lit_forsp-formid
            AND version  = @lit_forsp-version
            AND formcontentstatus IN @lrt_formstatus[]
            INTO TABLE @lit_finst .

        ENDIF."/ IF lit_forsp IS NOT INITIAL.
      ENDIF."/ IF lit_fomst IS NOT INITIAL.
    ENDIF."/ IF lit_foass IS NOT INITIAL.

    IF lit_finst[] IS NOT INITIAL.
       gitib_entity =
               VALUE #( FOR lst_finst IN lit_finst
                 ( FormID                   = lst_finst-formid
                    Version                 = lst_finst-version
                    FormInstanceID    = lst_finst-forminstanceid
                    ApproverID           = lst_finst-approverid
                    FormSubmittedBy = lst_finst-formsubmittedby
                    Counter                  = lst_finst-counter
                    FormContentStatus = lst_finst-formcontentstatus
                    Remarks                 = lst_finst-remarks
                    CreatedDate             = lst_finst-createddate
                    CreatedTime             = lst_finst-createdtime
                    FormName                = lst_finst-formname
                    IterationRequired = lst_finst-iterationrequired ) ).
    ENDIF."/ IF lit_finst IS NOT INITIAL.

    "/ Mapping properties from the backend to the Gateway output response table
     ex_response_data = CORRESPONDING #( gitib_entity ).


  ENDMETHOD.
ENDCLASS.
