class /ODSMFE/CL_MODEL definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF gtys_final,
        count      TYPE gernr,                                    "Serial Number
*        fname      TYPE fpname,                                   "Name of Form Object
        fname      TYPE /odsmfe/de_formid,
        version    TYPE /odsmfe/de_version,                       "ODS Version
        roleid     TYPE /odsmfe/de_roleid,                        "Role ID
        operation  TYPE vornr,                                    "Operation/Activity Number
        credt      TYPE /odsmfe/de_createdon,                     "Date
        creby      TYPE /odsmfe/de_createdby,                     "User Name
        instanceid TYPE /odsmfe/de_instanceid,                    "ODS MFE InstanceId
        draft      TYPE /odsmfe/de_isdraft,                       "IS Draft Flag
        status     TYPE /odsmfe/de_formcontentstatus,             "ODS MFE: Form Content Status
        reason     TYPE /odsmfe/de_remarks,
      END OF gtys_final .
  types:
    BEGIN OF gtys_forms,
        wo_num       TYPE /odsmfe/tb_forsp-wo_num,                "Order Number
        formid       TYPE /odsmfe/tb_forsp-formid,                "ODS Form ID
        version      TYPE /odsmfe/tb_forsp-version,               "ODS Version
        instanceid   TYPE /odsmfe/tb_forsp-instanceid,            "ODS MFE InstanceId
        responsedata TYPE /odsmfe/tb_forsp-responsedata,          "ODS MFE Form ResponseData
        created_on   TYPE /odsmfe/tb_forsp-created_on,            "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
        created_by   TYPE /odsmfe/tb_forsp-created_by,            "ODS Created By
        form_name    TYPE /odsmfe/tb_fomst-formid,                "ODS Form ID
        formhtml     TYPE /odsmfe/tb_fomst-formhtml,              "ODSMFE FORMHTML
        formmodel    TYPE /odsmfe/tb_fomst-formmodel,             "ODSMFE FormModel
        isdraft      TYPE /odsmfe/tb_forsp-isdraft,               "ODS MFE IsDraft Flag
      END OF gtys_forms .
  types:
    BEGIN OF gtys_forms_resp,
        cname  TYPE char255,                                      "Char255
        cvalue TYPE char255,                                      "Char255
      END OF gtys_forms_resp .
  types:
    BEGIN OF gtys_foass,
        formid      TYPE /odsmfe/tb_foass-formid,                   "ODS Form ID
        version     TYPE /odsmfe/tb_foass-version,                  "ODS Version
        roleid      TYPE /odsmfe/tb_foass-roleid,                   "Role id
        operation   TYPE vornr,                                    "Operation/Activity Number
        category    TYPE /odsmfe/tb_foass-category,                 "Form Category
        mandatory   TYPE /odsmfe/tb_foass-mandatory,                "Mandatory Form
        occur       TYPE /odsmfe/tb_foass-occur,                    "Occurances
        submitted   TYPE char10,                                    "char03,
        cell_type   TYPE salv_t_int4_column,
        wo_num      TYPE aufnr,                                     "++ES1K902140
        status_accp TYPE /odsmfe/de_statusaccp,
        status_rejc TYPE /odsmfe/de_statusrejc,
      END OF gtys_foass .

  data:
    gitib_final TYPE TABLE OF gtys_final .
  data:
    gitib_forms TYPE TABLE OF gtys_forms .
  data:
    gitib_resp TYPE TABLE OF /odsmfe/st_forms_resp_data .
  data:
    gitib_response TYPE TABLE OF /odsmfe/tb_forsp .                                              " gtys_forms_resp .
  data:
    gitib_txt TYPE TABLE OF char255 .
  data:
    gitib_foass TYPE TABLE OF gtys_foass .

  methods GMIB_GET_DATA
    importing
      !IM_AUFNR type AUFNR optional
      !IM_FORMS type GTYS_FOASS optional
      !IM_AUART type AUFART optional
      !IM_AUNAME type UNAME optional
      !IM_PLNTY type PLNTY optional
      !IM_PLNNR type PLNNR optional
      !IM_PLNAL type PLNAL optional
      !IM_ZAEHL type CIM_COUNT optional
      !IM_ARSPS type CO_POSNR optional
      !IM_QMNUM type QMNUM optional
      !IM_QMART type QMART optional .
  methods GMIB_PARSE_DATA
    importing
      !IM_XML_STRING type XSTRING
    exporting
      !EX_XML_DATA type /ODSMFE/XML_DATA
      !EX_RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_MODEL IMPLEMENTATION.


  METHOD gmib_get_data.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 18/03/2020
* Transport No.          : ES1K901528
* Program Description    : Method to get forms data and fills final table
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : SKAMMARI
* Change Date            : 24/12/2020
* Transport No.          : ES1K902363
* Change Description     : Addition of task list forms assignment functionality
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Change Date            : 12.03.2023
* Transport No.          : ES1K903619
* Change Description     : Added the logic to get the Forms list based in the Notification type
***********************************************************************

*-------------------------------------------------------------*
*                D A T A    D E C L A R A T I O N             *
*-------------------------------------------------------------*

    "/ Types
    TYPES:
      BEGIN OF ltys_formass1,
        formid    TYPE /odsmfe/de_formid,                            "Form ID
        version   TYPE /odsmfe/de_version,                           "Version
        ordertype TYPE aufart,                                       "Order Type
        oprnum    TYPE vornr,                                        "Operation/Activity Type
        plnty     TYPE plnty,                                        "Task List Type           - Added by ODS ES1K902363
        plnnr     TYPE plnnr,                                        "Key for Task List Group  - Added by ODS ES1K902363
        plnal     TYPE plnal,                                        "Group Counter            - Added by ODS ES1K902363
        zaehl     TYPE char8,                                        "Internal counter         - Added by ODS ES1K902363 - Changed by ODS ES1K902499
        roleid    TYPE /odsmfe/de_roleid,                            "Role ID
        category  TYPE /odsmfe/de_formcategory,                      "Category
        mandatory TYPE /odsmfe/de_mandatory,                         "Mandatory Form
        allowed   TYPE /odsmfe/de_occur,                             "Occurances
        form_name TYPE /odsmfe/de_formname,                          "ODS Form Name
      END OF ltys_formass1.

    "/ Tables and Structures
    DATA:
      lit_formass1    TYPE STANDARD TABLE OF ltys_formass1,          "Form Assignment table
      lit_formresp    TYPE TABLE OF /odsmfe/tb_forsp,                "Table: Capture Response
      lst_final_resp  TYPE /odsmfe/tb_forsp,                         "Structure: Capture Response
      lit_salv_column TYPE salv_t_int4_column,                       "Table: Values Column
      lst_salv_column TYPE salv_s_int4_column,                       "Structure: Values Column
      lit_foass       TYPE TABLE OF gtys_foass,                      "Table: Form Assignment global types
      lst_tmp_foass   TYPE gtys_foass,                               "Structure: Form Assignment global types
      lst_final       TYPE /odsmfe/cl_model=>gtys_final.             "Final structure

    "/ Range Tables and Structures
    DATA:
      lrt_auart    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_auart TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrt_plnty    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_plnty TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrt_plnnr    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_plnnr TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrt_plnal    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_plnal TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrt_zaehl    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_zaehl TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrt_aufnr    TYPE TABLE OF /odsmfe/st_core_range_str,          "Range table                - Added by ODS ES1K902363
      lrs_it_aufnr TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS ES1K902363
      lrs_qmnum    TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS-VSANAGALA : ES1K903619
      lrt_qmnum    TYPE STANDARD TABLE OF /odsmfe/st_core_range_str, "Range table                - Added by ODS-VSANAGALA : ES1K903619
      lrs_qmart    TYPE /odsmfe/st_core_range_str,                   "Range structure            - Added by ODS-VSANAGALA : ES1K903619
      lrt_qmart    TYPE STANDARD TABLE OF /odsmfe/st_core_range_str. "Range table                - Added by ODS-VSANAGALA : ES1K903619

    "/ Variables
    DATA:
      lv_subrc     TYPE sy-subrc,                                    "System variable for subrc
      lv_index1    TYPE sy-index,                                    "ABAP System Field: Loop Index
      lv_submitted TYPE i,                                           "Submitted Forms variable
      lv_aufnr     TYPE aufnr,                                       "Work Order
      lv_index2    TYPE sy-index,                                    "Row Index of Internal Tables
      lv_count     TYPE i.                                           "Count

    "/ Constants
    CONSTANTS:
      lc_submitted TYPE lvc_fname VALUE 'SUBMITTED',                 "Submitted constant
      lc_i         TYPE string VALUE 'I',                            "Value I                 - Added by ODS ES1K902363
      lc_eq        TYPE string VALUE 'EQ'.                           "Value EQ                - Added by ODS ES1K902363

    "/ Field Symbols
    FIELD-SYMBOLS:
      <lfsst_form> TYPE ltys_formass1,                               "Form Assignment structure
      <lfsst_resp> TYPE /odsmfe/tb_forsp.                            "Form Response Structure

*-------------------------------------------------------------*
*        E N D   O F   D A T A   D E C L A R A T I O N        *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
*                   M A I N   S E C T I O N                   *
*-------------------------------------------------------------*

*---------------------------------- SOC by ODS - ES1K902363 ----------------------------------*
    "/ Filling the Order type range table
    IF im_auart IS NOT INITIAL.
      lrs_it_auart-sign   = lc_i.
      lrs_it_auart-option = lc_eq.
      lrs_it_auart-low    = im_auart.
      APPEND lrs_it_auart TO lrt_auart.
      CLEAR lrs_it_auart.
    ENDIF.

    "/ Filling Task list type range table
    IF im_plnty IS NOT INITIAL.
      lrs_it_plnty-sign   = lc_i.
      lrs_it_plnty-option = lc_eq.
      lrs_it_plnty-low    = im_plnty.
      APPEND lrs_it_plnty TO lrt_plnty.
      CLEAR lrs_it_plnty.
    ENDIF.

    "/ Filling tasklist group range table
    IF im_plnnr IS NOT INITIAL.
      lrs_it_plnnr-sign   = lc_i.
      lrs_it_plnnr-option = lc_eq.
      lrs_it_plnnr-low    = im_plnnr.
      APPEND lrs_it_plnnr TO lrt_plnnr.
      CLEAR lrs_it_plnnr.
    ENDIF.

    "/ Filling task list group counter
    IF im_plnal IS NOT INITIAL.
      lrs_it_plnal-sign   = lc_i.
      lrs_it_plnal-option = lc_eq.
      lrs_it_plnal-low    = im_plnal.
      APPEND lrs_it_plnal TO lrt_plnal.
      CLEAR lrs_it_plnal.
    ENDIF.

    "/ Filling internal counter range table
    IF im_zaehl IS NOT INITIAL.
      lrs_it_zaehl-sign   = lc_i.
      lrs_it_zaehl-option = lc_eq.
      lrs_it_zaehl-low    = im_zaehl.
      APPEND lrs_it_zaehl TO lrt_zaehl.
      CLEAR lrs_it_zaehl.
    ENDIF.

    "/ Get the Work Order Number/Notication number from the ALV List
    IF gitib_foass IS NOT INITIAL.
      lit_foass[] = gitib_foass[].
      DELETE ADJACENT DUPLICATES FROM lit_foass COMPARING wo_num.
      READ TABLE lit_foass INTO lst_tmp_foass INDEX 1.
      IF sy-subrc = 0.
        lv_aufnr = lst_tmp_foass-wo_num.
      ENDIF.
    ENDIF.

    "/ Filling the Work Order range table with the requested work order number
    IF im_aufnr IS NOT INITIAL.
      lrs_it_aufnr-sign   = lc_i.
      lrs_it_aufnr-option = lc_eq.
      lrs_it_aufnr-low    = im_aufnr.
      APPEND lrs_it_aufnr TO lrt_aufnr.
      CLEAR lrs_it_aufnr.
    ENDIF.

    "/ Filling the Work Order range table from the Form assignment data from the ALV, if the work order number is not requested
    IF im_aufnr IS INITIAL AND lv_aufnr IS NOT INITIAL.
      lrs_it_aufnr-sign   = lc_i.
      lrs_it_aufnr-option = lc_eq.
      lrs_it_aufnr-low    = lv_aufnr.
      APPEND lrs_it_aufnr TO lrt_aufnr.
      CLEAR lrs_it_aufnr.
    ENDIF.
*---------------------------------- EOC by ODS - ES1K902363 ----------------------------------*

*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
    "/ Filling the Notification into the range table
    IF im_qmnum IS NOT INITIAL.
      lrs_qmnum-sign   = lc_i.
      lrs_qmnum-option = lc_eq.
      lrs_qmnum-low    = im_qmnum.
      APPEND lrs_qmnum TO lrt_aufnr.
      CLEAR lrs_qmnum.
    ENDIF.

    "/ Filling the range table from the Form assignment data from the ALV with the Notification, if the notification is not requested
    IF im_qmnum IS INITIAL AND lv_aufnr IS NOT INITIAL.
      lrs_qmnum-sign   = lc_i.
      lrs_qmnum-option = lc_eq.
      lrs_qmnum-low    = lv_aufnr.
      APPEND lrs_qmnum TO lrt_aufnr.
      CLEAR lrs_qmnum.
    ENDIF.

    "/ Filling the Notification type into the range table
    IF im_qmart IS NOT INITIAL.
      lrs_qmart-sign   = lc_i.
      lrs_qmart-option = lc_eq.
      lrs_qmart-low    = im_qmart.
      APPEND lrs_qmart TO lrt_auart.
      CLEAR lrs_qmart.
    ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*

    "/  Fetching All forms from Form master which are assigned to work order type/Notification Type
    SELECT a~formid
           a~version
           a~ordertype
           a~oprnum
           a~plnty
           a~plnnr
           a~plnal
           a~zaehl
           a~roleid
           a~category
           a~mandatory
           a~occur AS allowed
           b~form_name
      FROM /odsmfe/tb_foass AS a
      INNER JOIN /odsmfe/tb_fomst AS b
      ON b~form_name EQ a~formid AND b~version EQ a~version
      INTO CORRESPONDING FIELDS OF TABLE  lit_formass1
      WHERE a~ordertype IN lrt_auart[]                        " Added by ODS ES1K902363
        AND a~plnty     IN lrt_plnty[]                        " Added by ODS ES1K902363
        AND a~plnnr     IN lrt_plnnr[]                        " Added by ODS ES1K902363
        AND a~plnal     IN lrt_plnal[]                        " Added by ODS ES1K902363
        AND a~version   EQ b~version
        AND a~active    EQ 'X'.

    "/ Fetch crieria for Manual form assignment
    SELECT formid version oprnum formcategory AS category mandatory occur AS allowed
      FROM /odsmfe/tb_fmass
      APPENDING CORRESPONDING FIELDS OF TABLE lit_formass1
      WHERE workordernum IN lrt_aufnr[]                     "#EC CI_NOFIRST
        AND active       EQ abap_true
        AND deleted      NE abap_true.

    IF lit_formass1 IS NOT INITIAL.
      "/ Get the Form response data for the requested Work Orders/Notifcations
      SELECT *
        FROM /odsmfe/tb_forsp
        INTO TABLE lit_formresp
        FOR ALL ENTRIES IN lit_formass1
        WHERE wo_num     IN lrt_aufnr
          AND version    EQ lit_formass1-version
          AND formid     EQ lit_formass1-formid      "#EC CI_NOFIELD.
          AND instanceid NE space
          AND deleted    NE abap_true.

      IF sy-subrc <> 0 AND lit_formresp IS INITIAL.
        "/ If no forms are filled
        LOOP AT lit_formass1 ASSIGNING <lfsst_form>.
          MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.
          lst_tmp_foass-submitted = text-004.
          lst_tmp_foass-operation = <lfsst_form>-oprnum.
          lst_tmp_foass-occur     = <lfsst_form>-allowed.
*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
          IF im_aufnr IS NOT INITIAL.
            lst_tmp_foass-wo_num = im_aufnr.
          ELSEIF im_qmnum IS NOT INITIAL.
            lst_tmp_foass-wo_num = im_qmnum.
          ELSE.
            lst_tmp_foass-wo_num = lv_aufnr.
          ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
          APPEND lst_tmp_foass TO gitib_foass.
          CLEAR: lst_tmp_foass.
        ENDLOOP. "/ LOOP AT lit_formass1 ASSIGNING <lfsst_form>.

      ELSE. "/ IF sy-subrc <> 0 AND lit_formresp IS INITIAL.

        "/ Filling final internal table from response
        SORT: lit_formass1[] BY formid version roleid,
              lit_formresp[] BY formid version roleid.

        LOOP AT lit_formass1 ASSIGNING <lfsst_form>.
          CLEAR: lv_submitted,lst_tmp_foass,lst_salv_column.
          READ TABLE lit_formresp TRANSPORTING NO FIELDS WITH KEY formid = <lfsst_form>-formid version = <lfsst_form>-version BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR: lv_submitted.
            lv_index1 = sy-tabix.

            LOOP AT lit_formresp ASSIGNING <lfsst_resp> FROM lv_index1. "#EC CI_NESTED.
              IF <lfsst_resp>-formid <> <lfsst_form>-formid OR <lfsst_form>-version <> <lfsst_resp>-version.
                EXIT.
              ENDIF.
              ADD 1 TO lv_submitted.
            ENDLOOP. "/ LOOP AT lit_formresp ASSIGNING <lfsst_resp> FROM lv_index1.

            MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.
            lst_tmp_foass-submitted = lv_submitted.
            lst_tmp_foass-occur     = <lfsst_form>-allowed.
*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
            IF im_aufnr IS NOT INITIAL.
              lst_tmp_foass-wo_num = im_aufnr.
            ELSEIF im_qmnum IS NOT INITIAL.
              lst_tmp_foass-wo_num = im_qmnum.
            ELSE.
              lst_tmp_foass-wo_num = lv_aufnr.
            ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
            lst_tmp_foass-operation = <lfsst_form>-oprnum.
            IF lst_tmp_foass-submitted NE 000.
              lst_salv_column-columnname = lc_submitted.
              lst_salv_column-value      = if_salv_c_cell_type=>hotspot.
              APPEND lst_salv_column TO lit_salv_column.
              lst_tmp_foass-cell_type = lit_salv_column.
            ENDIF.
            APPEND lst_tmp_foass TO gitib_foass.

          ELSE. "/ IF sy-subrc = 0

            MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.
            lst_tmp_foass-occur = <lfsst_form>-allowed.
            IF im_aufnr IS NOT INITIAL."----------------------- Added by ODS-VSANAGALA - ES1K903619
              lst_tmp_foass-wo_num = im_aufnr."++ ES1K902140
*----------------------------- SOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
            ELSEIF im_qmnum IS NOT INITIAL.
              lst_tmp_foass-wo_num = im_qmnum.
            ENDIF.
*----------------------------- EOC by ODS-VSANAGALA - ES1K903619 -----------------------------*
            lst_tmp_foass-operation = <lfsst_form>-oprnum.

            IF lst_tmp_foass-submitted NE 000 .
              lst_salv_column-columnname = lc_submitted.
              lst_salv_column-value      = if_salv_c_cell_type=>hotspot.
              APPEND lst_salv_column TO lit_salv_column.
              lst_tmp_foass-cell_type = lit_salv_column.
            ENDIF.

            lst_tmp_foass-submitted = text-004.
            APPEND lst_tmp_foass TO gitib_foass.
          ENDIF. "/  IF sy-subrc = 0.
        ENDLOOP. "/ LOOP AT lit_formass1 ASSIGNING <lfsst_form>.

        IF lit_formresp IS NOT INITIAL.
          gitib_response = lit_formresp.
          SORT: gitib_response BY formid version.
        ENDIF.
      ENDIF. "/ IF sy-subrc <> 0 AND lit_formresp IS INITIAL.
    ENDIF. "/ IF lit_formass1 IS NOT INITIAL.

    IF im_forms IS NOT INITIAL.
      REFRESH: gitib_final.
      "/ Fetching froms based on form instance id
      READ TABLE gitib_response TRANSPORTING NO FIELDS WITH KEY formid = im_forms-formid version = im_forms-version BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR: lv_count.
        lv_index2 = sy-tabix.

        LOOP AT gitib_response INTO lst_final_resp FROM lv_index2.
          IF lst_final_resp-formid <> im_forms-formid OR im_forms-version <> lst_final_resp-version.
            EXIT.
          ENDIF.
          CLEAR: lst_final.
          ADD 1 TO lv_count.
          lst_final-fname      = lst_final_resp-formid.
          lst_final-version    = lst_final_resp-version.
          lst_final-roleid     = lst_final_resp-roleid.
          lst_final-operation  = lst_final_resp-vornr.

          IF lst_final_resp-modified_on IS NOT INITIAL.
            lst_final-credt      = lst_final_resp-modified_on.
          ELSE.
            lst_final-credt      = lst_final_resp-created_on.
          ENDIF.

          lst_final-creby      = lst_final_resp-created_by.
          lst_final-instanceid = lst_final_resp-instanceid.
          lst_final-draft      = lst_final_resp-isdraft.
          lst_final-count      = lv_count.
          APPEND lst_final TO gitib_final.

        ENDLOOP. "/ LOOP AT lit_formass1 ASSIGNING <lfsst_form>.
      ENDIF. "/ IF sy-subrc = 0.

     "/ Dispay forms
      IF gitib_final IS NOT INITIAL.
        CALL FUNCTION '/ODSMFE/FM_FORM_DATA'.
      ELSE.
     "/ If no forms available in respanse capture table
        MESSAGE i398(00) WITH text-005 im_forms-formid.
      ENDIF. "/ IF gitib_final IS NOT INITIAL.
    ENDIF. "/ IF im_forms IS NOT INITIAL.

  ENDMETHOD.


  METHOD gmib_parse_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/03/2020
* Transport No.          : ES1K901528
* Program Description    : Method to parse xml data
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

    DATA: lo_ref_xml         TYPE REF TO cl_xml_document.
    DATA: lit_data           TYPE swxmlcont,
          lit_retcode        TYPE sysubrc ,
          lv_resp_xml_string TYPE xstring,
          lv_subrc           TYPE sy-subrc, "#EC NEEDED
          lv_size            TYPE sytabix .
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = im_xml_string
      TABLES
        binary_tab = lit_data.

    CREATE OBJECT lo_ref_xml.

* parse data
    CALL METHOD lo_ref_xml->create_with_table
      EXPORTING
        table   = lit_data

      RECEIVING
        retcode = lit_retcode.

* render_2_xstring
    CALL METHOD lo_ref_xml->render_2_xstring
      IMPORTING
        retcode = lv_subrc
        stream  = lv_resp_xml_string.

* convert xml to internal table
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_resp_xml_string
      TABLES
        xml_table = ex_xml_data
        return    = ex_return.

  ENDMETHOD.
ENDCLASS.
