class /ODSMFE/CL_MODEL_RPT definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF gtys_final,
        count      TYPE gernr,                                    "Serial Number
        fname      TYPE fpname,                                   "Name of Form Object
        version    TYPE /odsmfe/de_version,                       "ODS Version
        roleid     TYPE /odsmfe/de_roleid,                        "Role ID
        operation  TYPE vornr,                                    "Operation/Activity Number
        credt      TYPE /odsmfe/de_createdon,                     "Date
        creby      TYPE /odsmfe/de_createdby,                     "User Name
        instanceid TYPE /odsmfe/de_instanceid,                    "ODS MFE InstanceId
        draft      TYPE /odsmfe/de_isdraft,
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
        formid    TYPE /odsmfe/tb_foass-formid,                   "ODS Form ID
        version   TYPE /odsmfe/tb_foass-version,                  "ODS Version
        roleid    TYPE /odsmfe/tb_foass-roleid,                   "Role id
        user      TYPE /odsmfe/tb_foass-createdby,                  "ODS Version
        category  TYPE /odsmfe/tb_foass-category,                 "Form Category
        mandatory TYPE /odsmfe/tb_foass-mandatory,                "Mandatory Form
        occur     TYPE /odsmfe/tb_foass-occur,                    "Occurances
        submitted TYPE char10,                                     "char03,
        cell_type TYPE salv_t_int4_column,                        "
      END OF gtys_foass .
  types:
    BEGIN OF gtys_foass_graph,
*             count     TYPE gernr,                                     "Serial Number
        formid    TYPE /odsmfe/tb_foass-formid ,                   "ODS Form ID
*        version   TYPE /odsmfe/tb_foass-version,                  "ODS Version
        user      TYPE /odsmfe/tb_foass-createdby,                  "ODS Version
*        category  TYPE /odsmfe/tb_foass-category,                 "Form Category
*        mandatory TYPE /odsmfe/tb_foass-mandatory,                "Mandatory Form
*        occur     TYPE /odsmfe/tb_foass-occur,                    "Occurances
        submitted TYPE int4,                                     "char03,
*        submitted TYPE char10,                                     "char03,
*        cell_type TYPE salv_t_int4_column,                        "
      END OF gtys_foass_graph .

  data:
    gitib_final TYPE TABLE OF gtys_final .
  data:
    gitib_forms TYPE TABLE OF gtys_forms .
  data:
    gitib_resp TYPE TABLE OF /odsmfe/st_forms_resp_data .
  data:
    gitib_response TYPE TABLE OF /odsmfe/tb_forsp .                                          " gtys_forms_resp .
  data:
    gitib_txt TYPE TABLE OF char255 .
  data:
    gitib_foass TYPE TABLE OF gtys_foass .
  data:
    gitib_response_graph TYPE TABLE OF gtys_foass_graph WITH KEY formid user .

  methods GMIB_GET_DATA
    importing
      !IM_AUFNR type AUFNR optional
      !IM_FORMS type GTYS_FOASS optional
      !IM_AUART type AUFART optional
      !IM_UNAME type /ODSMFE/DE_CREATEDBY optional
      !IM_FORMID type /ODSMFE/DE_FORMID optional
      !IM_VERSION type /ODSMFE/DE_VERSION optional
      !IM_DATE type /ODSMFE/CORE_RANGE_TAB optional .
  methods GMIB_PARSE_DATA
    importing
      !IM_XML_STRING type XSTRING
    exporting
      !EX_XML_DATA type /ODSMFE/XML_DATA
      !EX_RETURN type BAPIRET2_T .
protected section.
private section.

  data LV_DATE type /ODSMFE/CORE_RANGE_TAB   .
ENDCLASS.



CLASS /ODSMFE/CL_MODEL_RPT IMPLEMENTATION.


  METHOD gmib_get_data.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 18/03/2020
* Transport No.          : ES1K901528
* Program Description    : Method to get forms data and fills final table
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    TYPES: BEGIN OF ltys_formass1,
             formid    TYPE /odsmfe/de_formid,                                 "ODS Form ID
             version   TYPE /odsmfe/de_version,                                "ODS Version
             ordertype TYPE aufart,                                            "ODS Version
             category  TYPE /odsmfe/de_formcategory,                           "Form Category
             mandatory TYPE /odsmfe/de_mandatory,                              "Mandatory Form
             allowed   TYPE /odsmfe/de_occur,                                  "Occurances
             form_name TYPE /odsmfe/de_formname,                               "ODS Form Name
             submitted TYPE numc3,                                             "Numc3, internal use
             uname     TYPE /odsmfe/tb_forsp-created_by,                       "ODS Version
           END OF ltys_formass1.

    DATA :lit_formass1    TYPE STANDARD TABLE OF ltys_formass1,        "
          lit_formresp    TYPE TABLE OF /odsmfe/tb_forsp,              "Table to Capture Response
          lt_timestamp    TYPE piq_selopt_t,                           "
          lst_final_resp  TYPE /odsmfe/tb_forsp,                       "Table to Capture Response
          lit_salv_column TYPE salv_t_int4_column,                     "
          lst_salv_column TYPE salv_s_int4_column,                     "Values Column
          lst_tmp_foass   TYPE gtys_foass,                             "
          lst_tmp_foass1  TYPE gtys_foass_graph,                       "
          lst_final       TYPE /odsmfe/cl_model_rpt=>gtys_final.       "

* Variables
    DATA: lv_dat   TYPE d,                                         "
          lv_timlo TYPE t VALUE '000000',                          "
          lv_timhi TYPE t VALUE '235959',                          "
          lv_ts    TYPE timestamp,                                 "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
          lv_tz    TYPE ttzz-tzone.                                "Time Zone
    DATA: lv_subrc     TYPE sy-subrc,                                  "ABAP System Field: Return Code of ABAP Statements
          lv_index1    TYPE sy-index,                                  "ABAP System Field: Loop Index
          lv_submitted TYPE i,                                         "
          lv_user      TYPE uname,                                     "
          lv_index2    TYPE sy-index,                                  "Row Index of Internal Tables
          lv_count     TYPE i.                                         "

*    Constants
    CONSTANTS: lc_submitted TYPE lvc_fname VALUE 'SUBMITTED'.

*   Field Symbols
    FIELD-SYMBOLS: <lfsst_form> TYPE ltys_formass1,                    "
                   <lfsst_resp> TYPE /odsmfe/tb_forsp.                 "Table to Capture Response
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    IF  im_uname IS NOT INITIAL OR im_formid IS NOT INITIAL.

*  Fetching All forms from Form master which are assigned to work order type
      SELECT  a~formid a~version a~ordertype a~category a~mandatory a~occur AS allowed
              b~form_name
             FROM /odsmfe/tb_foass AS a
             INNER JOIN /odsmfe/tb_fomst AS b
             ON b~form_name EQ a~formid AND b~version EQ a~version
             INTO CORRESPONDING FIELDS OF TABLE  lit_formass1
             WHERE    a~active    EQ 'X'.

      IF im_formid IS NOT INITIAL.
        DELETE lit_formass1 WHERE formid NE im_formid.
      ENDIF.                                                           " IF IM_FORMID IS NOT INITIAL Line No. :109

      IF im_version IS NOT INITIAL.
        DELETE lit_formass1 WHERE version NE im_version.
      ENDIF.                                                           " IF IM_VERSION IS NOT INITIAL Line No. :113
      " IF IM_VERSION IS NOT INITIAL Line No. :104
      IF lit_formass1 IS NOT INITIAL.

        SELECT * FROM /odsmfe/tb_forsp
               INTO CORRESPONDING FIELDS OF TABLE lit_formresp
               FOR ALL ENTRIES IN lit_formass1
               WHERE version = lit_formass1-version
               AND formid = lit_formass1-formid        "#EC CI_NOFIELD.
               AND instanceid NE space.

        IF im_uname IS NOT INITIAL.
          DELETE lit_formresp WHERE created_by NE im_uname.
        ENDIF.                                                         " IF IM_UNAME IS NOT INITIAL Line No. :126



        IF im_date IS NOT INITIAL.

*          lv_date[] =    lt_timestamp = im_date.
*
*          READ TABLE lt_timestamp ASSIGNING FIELD-SYMBOL(<lfs_time>) INDEX 1.
*
*          IF <lfs_time> IS ASSIGNED.
*
*
*            lv_dat = <lfs_time>-low.
*
*            CONVERT DATE lv_dat TIME lv_timlo DAYLIGHT SAVING TIME 'X'
*                    INTO TIME STAMP DATA(lv_time_stamp) TIME ZONE sy-timlo.
*
*            <lfs_time>-low = lv_time_stamp.
*
*            lv_dat = <lfs_time>-high.
*
*            CONVERT DATE lv_dat TIME lv_timhi DAYLIGHT SAVING TIME 'X'
*                                              INTO TIME STAMP lv_ts TIME ZONE sy-timlo.
*
*
*            <lfs_time>-high = lv_ts.
*
*          ENDIF.                                                       " IF <LFS_TIME> IS ASSIGNED Line No. :138

          DELETE lit_formresp WHERE created_date NOT IN im_date.
        ENDIF.                                                         " IF IM_DATE IS NOT INITIAL Line No. :132

        IF  lit_formresp IS INITIAL.
* If no forms are filled
          SORT:lit_formass1[] BY formid version.

          DELETE ADJACENT DUPLICATES FROM lit_formass1 COMPARING formid version.
          LOOP AT lit_formass1 ASSIGNING <lfsst_form>.
            MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.
            lst_tmp_foass-submitted = text-004.
            lst_tmp_foass-occur     = <lfsst_form>-allowed.
            APPEND lst_tmp_foass TO gitib_foass.
            CLEAR:lst_tmp_foass.
          ENDLOOP.                                                     " LOOP AT LIT_FORMASS1 Line No. :166

        ELSE.                                                          " IF LIT_FORMRESP IS INITIAL Line No. :161

* filling final internal table from response
          SORT:lit_formass1[] BY formid version,
               lit_formresp[] BY formid version  created_by.


          DELETE ADJACENT DUPLICATES FROM lit_formass1 COMPARING formid version.

          LOOP AT lit_formass1 ASSIGNING <lfsst_form>.

            CLEAR: lv_submitted,lst_tmp_foass,lst_salv_column.


            LOOP AT lit_formresp ASSIGNING <lfsst_resp> WHERE formid = <lfsst_form>-formid
                                                        AND version = <lfsst_form>-version. "#EC CI_NESTED.

              IF   lv_user IS INITIAL.
                lv_user = <lfsst_resp>-created_by.
              ENDIF.                                                   " IF LV_USER IS INITIAL Line No. :191

              IF lv_user <> <lfsst_resp>-created_by.

                CLEAR lst_tmp_foass.
                MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.

                lst_tmp_foass-submitted = lv_submitted.
                lst_tmp_foass-user      = lv_user.                     " <lfsst_resp>-created_by.
                lst_tmp_foass-occur     = <lfsst_form>-allowed.

                IF lst_tmp_foass-submitted NE 000.
                  lst_salv_column-columnname = lc_submitted.
                  lst_salv_column-value      = if_salv_c_cell_type=>hotspot.  "
                  APPEND lst_salv_column TO lit_salv_column.
                  lst_tmp_foass-cell_type = lit_salv_column.
                ENDIF.                                                 " IF LST_TMP_FOASS-SUBMITTED NE 000 Line No. :204

                lv_user = <lfsst_resp>-created_by.

                APPEND lst_tmp_foass TO gitib_foass.
                CLEAR lst_tmp_foass.

                " for the current lv_user
                lv_submitted = 1.

              ELSE.                                                    " IF LV_USER <> <LFSST_RESP>-CREATED_BY Line No. :195

                ADD 1 TO lv_submitted.

              ENDIF.                                                   " IF LV_USER <> <LFSST_RESP>-CREATED_BY Line No. :195

            ENDLOOP.                                                   " LOOP AT LIT_FORMRESP Line No. :188

            IF sy-subrc IS INITIAL.

              CLEAR lst_tmp_foass.
              MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.

              lst_tmp_foass-submitted = lv_submitted.
              lst_tmp_foass-user      = <lfsst_resp>-created_by.
              lst_tmp_foass-occur     = <lfsst_form>-allowed.

              IF lst_tmp_foass-submitted NE 000.

                lst_salv_column-columnname = lc_submitted.
                lst_salv_column-value      = if_salv_c_cell_type=>hotspot.  "
                APPEND lst_salv_column TO lit_salv_column.
                lst_tmp_foass-cell_type = lit_salv_column.

              ENDIF.                                                   " IF LST_TMP_FOASS-SUBMITTED NE 000 Line No. :236

              APPEND lst_tmp_foass TO gitib_foass.
              CLEAR lst_tmp_foass.

            ELSE.                                                      " IF SY-SUBRC IS INITIAL Line No. :227

              CLEAR lst_tmp_foass.
              MOVE-CORRESPONDING <lfsst_form> TO lst_tmp_foass.
              lst_tmp_foass-occur = <lfsst_form>-allowed.

              IF lst_tmp_foass-submitted NE 000 .

                lst_salv_column-columnname = lc_submitted.
                lst_salv_column-value      = if_salv_c_cell_type=>hotspot.  "
                APPEND lst_salv_column TO lit_salv_column.
                lst_tmp_foass-cell_type = lit_salv_column.

              ENDIF.                                                   " IF LST_TMP_FOASS-SUBMITTED NE 000 Line No. :254

              lst_tmp_foass-submitted = text-004.
              APPEND lst_tmp_foass TO gitib_foass.

            ENDIF.                                                     " IF SY-SUBRC IS INITIAL Line No. :227

          ENDLOOP.                                                     " LOOP AT LIT_FORMASS1 Line No. :183


        ENDIF.                                                         " IF LIT_FORMRESP IS INITIAL Line No. :161
        IF lit_formresp IS NOT INITIAL.

          gitib_response = lit_formresp.
          SORT: gitib_response BY formid version.
        ENDIF.                                                         " IF LIT_FORMRESP IS NOT INITIAL Line No. :272

      ENDIF.                                                           " IF LIT_FORMASS1 IS NOT INITIAL Line No. :117

*          SORT: gitib_foass BY   submitted ASCENDING .

      CLEAR gitib_response_graph.

      LOOP AT gitib_foass INTO lst_tmp_foass.


        CLEAR: lst_tmp_foass1.
        TRY .
            MOVE-CORRESPONDING lst_tmp_foass TO lst_tmp_foass1.

          CATCH cx_sy_conversion_no_number.
            CONTINUE.
        ENDTRY.

        CHECK: lst_tmp_foass1 IS NOT INITIAL.

        APPEND lst_tmp_foass1 TO gitib_response_graph.
      ENDLOOP.                                                         " LOOP AT GITIB_FOASS Line No. :284

    ENDIF.                                                             " IF IM_UNAME IS NOT INITIAL OR IM_FORMID IS NOT INITIAL Line No.



    IF im_forms IS NOT INITIAL.

      CLEAR: gitib_final.
* Fetching froms based on form instance id
      READ TABLE gitib_response TRANSPORTING NO FIELDS WITH KEY formid = im_forms-formid version = im_forms-version BINARY SEARCH.

      IF sy-subrc = 0.
        CLEAR: lv_count.
        lv_index2 = sy-tabix.
        LOOP AT gitib_response INTO lst_final_resp FROM lv_index2.
          IF lst_final_resp-formid <> im_forms-formid  OR im_forms-version <> lst_final_resp-version.
            EXIT.
          ENDIF.                                                       " IF LST_FINAL_RESP-FORMID <> IM_FORMS-FORMID OR IM_FORMS-VERSI L
          CLEAR: lst_final.
          ADD 1 TO lv_count.
          lst_final-fname     = lst_final_resp-formid.
          lst_final-version   = lst_final_resp-version.
          lst_final-operation = lst_final_resp-vornr.
          IF lst_final_resp-modified_on IS NOT INITIAL.
            lst_final-credt      = lst_final_resp-modified_on.
          ELSE.                                                        " IF LST_FINAL_RESP-MODIFIED_ON IS NOT INITIAL Line No. :322
            lst_final-credt      = lst_final_resp-created_on.
          ENDIF.                                                       " IF LST_FINAL_RESP-MODIFIED_ON IS NOT INITIAL Line No. :322
          lst_final-creby      = lst_final_resp-created_by.
          lst_final-instanceid = lst_final_resp-instanceid.
          lst_final-draft      = lst_final_resp-isdraft.
          lst_final-count      = lv_count.
          APPEND lst_final TO gitib_final.

        ENDLOOP.                                                       " LOOP AT GITIB_RESPONSE Line No. :313

      ENDIF.                                                           " IF SY-SUBRC = 0 Line No. :310

* dispay forms
      IF gitib_final IS NOT INITIAL.

        CALL FUNCTION '/ODSMFE/FM_FORM_DATA_RPT'
          EXPORTING
            im_formid  = im_forms-formid
            im_uname   = im_forms-user
            im_version = im_forms-version
            im_date    = lv_date.

      ELSE.                                                            " IF GITIB_FINAL IS NOT INITIAL Line No. :338
* if no forms available in respanse capture table
        MESSAGE i398(00) WITH text-005 im_forms-formid.
      ENDIF.                                                           " IF GITIB_FINAL IS NOT INITIAL Line No. :338
    ENDIF.                                                             " IF IM_FORMS IS NOT INITIAL Line No. :304

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
          lit_retcode        TYPE sysubrc , "##NEEDED
          lv_resp_xml_string TYPE xstring,
          lv_subrc           TYPE sy-subrc,                 "#EC NEEDED
          lv_size            TYPE sytabix . "##NEEDED
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
* Xstring to Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = im_xml_string
      TABLES
        binary_tab = lit_data.

    CREATE OBJECT lo_ref_xml.

* Parse data
    CALL METHOD lo_ref_xml->create_with_table
      EXPORTING
        table   = lit_data
      RECEIVING
        retcode = lit_retcode.

* Render_2_xstring
    CALL METHOD lo_ref_xml->render_2_xstring
      IMPORTING
        retcode = lv_subrc
        stream  = lv_resp_xml_string.

* Convert xml to internal table
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_resp_xml_string
      TABLES
        xml_table = ex_xml_data
        return    = ex_return.

  ENDMETHOD.
ENDCLASS.
