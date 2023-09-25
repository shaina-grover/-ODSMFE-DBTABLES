CLASS lhc_ResponseCapture DEFINITION INHERITING FROM cl_abap_behavior_handler.
  INTERFACES /odsmfe/if_get_entityset_bapi.
  INTERFACES if_oo_adt_classrun_out.
  PUBLIC SECTION.
    METHODS get_cloud_dest
      EXPORTING
        ex_dest TYPE rfcdest.

    METHODS gmib_parse_responsedata
      IMPORTING
        im_responsedata TYPE xstring OPTIONAL
        im_wonum        TYPE aufnr OPTIONAL
        im_formid       TYPE /odsmfe/de_formid OPTIONAL
        im_version      TYPE /odsmfe/de_version OPTIONAL
        im_qmnum        TYPE /odsmfe/st_bapi2080_nothdre-notif_no OPTIONAL "QMNUM
      EXPORTING
        ex_return       TYPE /ODSMFE/TT_bapiret2.

    METHODS gmib_form_inst_post_process
      IMPORTING
        im_form_response_data TYPE /odsmfe/ce_responsecapture OPTIONAL
        im_xml_table_data     TYPE /odsmfe/xml_data OPTIONAL
      EXPORTING
        ex_return             TYPE /odsmfe/tt_bapiret2
      .

    METHODS get_xml_content
      IMPORTING
        im_responsedata TYPE xstring
      EXPORTING
        ex_return       TYPE /ODSMFE/TT_bapiret2
        ex_xml_data     TYPE /odsmfe/xml_data."smum_xmltb

    METHODS gmib_create_notif_item_act
      IMPORTING
        im_wonum    TYPE aufnr OPTIONAL
        im_formid   TYPE /odsmfe/de_formid OPTIONAL
        im_version  TYPE /odsmfe/de_version OPTIONAL
        im_xml_data TYPE  /odsmfe/xml_data OPTIONAL
        im_qmnum    TYPE /odsmfe/st_bapi2080_nothdre-notif_no  OPTIONAL "QMNUM
      EXPORTING
        ex_return   TYPE /ODSMFE/TT_bapiret2 ."bapiret2_t

    METHODS gmib_delta_table_update
      IMPORTING
        !im_qmnum TYPE /odsmfe/st_bapi2080_nothdre-notif_no OPTIONAL . "BAPI2080_NOTHDRE-NOTIF_NO
    METHODS modify_delta_table
      IMPORTING
        order_number TYPE aufnr OPTIONAL
        time_token   TYPE string OPTIONAL
        notification TYPE /odsmfe/st_bapi2080_nothdre-notif_no  OPTIONAL . "QMNUM
    METHODS read_delta_table
      IMPORTING
        delta_token   TYPE timestamp OPTIONAL
      EXPORTING
        ex_aufnr_data TYPE /odsmfe/tt_ex_aufnr .
    METHODS equipment_char_update
      IMPORTING
        VALUE(im_workorder) TYPE aufnr
        VALUE(im_char)      TYPE /odsmfe/eq_char_tt
      EXPORTING
        ex_return           TYPE /ODSMFE/ST_bapiret2."bapiret2 .
    METHODS update_characterstics
      IMPORTING
        VALUE(lv_object) TYPE /odsmfe/de_formid "bapi1003_key-object
        lv_classnum      TYPE /odsmfe/st_bapi2080_nothdre-assembly "bapi1003_key-classnum
        lv_klart         TYPE /odsmfe/st_bapi2080_nothdre-plangroup  "bapi1003_key-classtype
        im_char          TYPE /odsmfe/eq_char_tt .
    "/Types
    TYPES: BEGIN OF ltys_data,
             wa(512) TYPE c,
           END OF ltys_data,

           BEGIN OF ltys_option,
             text(72) TYPE c,
           END OF ltys_option,

           BEGIN OF ltys_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ltys_fields,

           BEGIN OF ltys_roles,
             low  TYPE /odsmfe/de_mfe_low100,                                  "Low Range
             high TYPE /odsmfe/de_mfe_high,                                    "High Range
           END OF ltys_roles ,

           BEGIN OF lt_char,
             name  TYPE c LENGTH 255,
             value TYPE c LENGTH 255,
           END OF lt_char.

    DATA: lst_response            TYPE /odsmfe/tb_forsp,
          lit_option              TYPE TABLE OF ltys_option,
          lit_data                TYPE TABLE OF ltys_data,
          lit_fields              TYPE TABLE OF ltys_fields,
          "lst_formresponse TYPE ltys_responsecapture,                          "Response Capture Fieds
          lst_return              TYPE  /ODSMFE/ST_bapiret2, "bapiret2,                                      "Return Parameter
          lit_return              TYPE  /ODSMFE/TT_bapiret2, " bapiret2_t,                                    "Return parameter table
          lit_xml_data            TYPE  /odsmfe/xml_data, "standard table of  /odsmfe/st_snum_xml_tb,                         "STANDARD TABLE OF /odsmfe/xml_data,                  "XML Table structure used for retreive and output XML doc
          lst_xml_data            TYPE  /odsmfe/st_snum_xml_tb,
          ls_xml_data             TYPE  /odsmfe/xml_data  ,                                "XML Table structure used for retreive and output XML doc
          lit_char                TYPE  /odsmfe/eq_char_tt,                 "/odsmfe/tt_char,                               "ODSMFE: Table Type for Equipment characterstics
          lst_char                TYPE  /odsmfe/eq_char,                        " /odsmfe/st_char,                               "ODSMFE: Structure for Equipment characterstics
          lit_focha               TYPE STANDARD TABLE OF /odsmfe/tb_focha,            "Table to update Equipment characterstics
          lst_focha               TYPE /odsmfe/tb_focha,                              "Structure to update Equipment characterstics
          lst_inst_data           TYPE /odsmfe/ce_responsecapture, "Gateway properties of Response Capture Service
          "lit_data         TYPE raw LENGTH 255, "swxmlcont,
          lit_file_content_binary TYPE TABLE OF /odsmfe/st_bapiconten,
          lst_file_content_binary TYPE /odsmfe/st_bapiconten,
          lit_retcode             TYPE sysubrc,
          lit_fomrsp              TYPE STANDARD TABLE OF /odsmfe/st_snum_xml_tb,
          lst_fomrsp              TYPE /odsmfe/st_snum_xml_tb,
          lit_NoNum               TYPE TABLE OF /odsmfe/st_core_range_str.
    "/Reference Class
    DATA: lr_request       TYPE REF TO if_rap_query_request,
          lr_base64_decode TYPE REF TO cl_web_http_utility.

* Variables
    DATA: lv_message          TYPE bapi_msg,                                   "Message Text
          lv_msg              TYPE char128,                                    "128 character
          lv_postnotification TYPE c LENGTH 1,                                     "Single-Character Indicator
          lv_cdate            TYPE sy-datlo,                                   "ABAP System Field: Local Date of Current User
          lv_mdate            TYPE sy-datlo,                                   "ABAP System Field: Local Date of Current User
          lv_ctime            TYPE sy-timlo,                                   "ABAP System Field: Local Time of Current User
          lv_mtime            TYPE sy-timlo,                                   "ABAP System Field: Local Time of Current User
          lv_roleid           TYPE /odsmfe/de_roleid,                          "Role ID
          lv_instanceid       TYPE c LENGTH 50,                                    "Comment
          lv_postnotif        TYPE c LENGTH 1,                                      "Char1
          lv_postchar         TYPE char1,                                      "Char1
          lv_date             TYPE sy-datlo,
          lv_file_content_bin TYPE xstring,                                    "Date
          lv_time             TYPE sy-timlo   ,                                   "Time
          lv_sys_time_token   TYPE string,                                     "System Time Token
          lv_count            TYPE i,                                          "Count
          lv_sys_tzone        TYPE  tznzone,                                 "System Time Zone
          lv_timestamp        TYPE timestamp,                                  "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
          lv_date1            TYPE datn,                                      "Date
          lv_time1            TYPE  timn,                                      "Time
          lv_sys_time_token1  TYPE string,                                     "System Time Token
          lv_mobileuser       TYPE string,                                     "Mobile User
          lv_userrole         TYPE /odsmfe/de_roleid,                          "Role ID
          lv_sys_tzone1       TYPE tznzone,                                 "System Time Zone
          lv_timestamp1       TYPE timestamp,                                  "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
          lv_msg1             TYPE string,                                     "String
          lv_wo               TYPE aufnr,                                      "Order Number
          lv_wonum            TYPE aufnr,                                      "Order Number
          lv_form_date        TYPE datn,                                      "Date
          lv_qmart            TYPE c LENGTH 2, "qmart,
          lv_form_time        TYPE  timn,                                      "Time
          lv_aufnr            TYPE aufnr,                                      "Order Number,
          lv_aufnr1           TYPE aufnr,                                      "Order Number
          lv_qmnum            TYPE c LENGTH 12,                                     "Notification No
          lv_pfcg_role        TYPE /odsmfe/de_mfe_value,                       "ODS MFE: Parameter Value
          lv_class            TYPE /odsmfe/de_mfe_value,                       "ODS MFE: Parameter Value
          lv_user             TYPE uname,                                      "User Name
          lv_role             TYPE c LENGTH 40,                                      "agrfield
          lv_workorder        TYPE aufnr,                                      "Order Number
          lv_xml_string       TYPE xstring,                                    "Xstring
          lv_subrc            TYPE sy-subrc,                                   "ABAP System Field: Return Code of ABAP Statements
          lv_size             TYPE sy-tabix,                                   "Row Index of Internal Tables
          lv_rowskip          TYPE int4,
          lv_rowcount         TYPE int4,
          lv_Dest             TYPE string,
          lv_line             TYPE string,
          lv_usrroletab       TYPE ltys_roles.                                 "User role tab

    "/Constants
    CONSTANTS: lc_dest               TYPE string VALUE  'CLOUD_DEST',
               lc_i                  TYPE c LENGTH 1 VALUE 'I',
               lc_wi                 TYPE c LENGTH 2 VALUE 'WI',
               lc_aewi               TYPE c LENGTH 4 VALUE 'AEWI',
               lc_w                  TYPE c LENGTH 1 VALUE 'W',
               lc_e                  TYPE c LENGTH 1 VALUE 'E',
               lc_a                  TYPE c LENGTH 1 VALUE 'A',
               lc_low1               TYPE c LENGTH 1 VALUE '1',
               lc_low2               TYPE c LENGTH 1 VALUE '2',
               lc_low3               TYPE c LENGTH 1 VALUE '3',
               lc_responsecaptureset TYPE string VALUE 'ResponseCaptureSet',  "Filter Options: ResponseCaptureSet
               lc_postnotification   TYPE /odsmfe/de_mfe_fieldname VALUE 'POSTNOTIFICATION',  "Filter Options: POSTNOTIFICATION
               lc_pfcg_role          TYPE string VALUE 'PFCG_ROLE',
               lc_meth               TYPE string VALUE 'ROLE_ASSIGNMENT',               "ROLE_ASSIGNMENT
               lc_x                  TYPE c LENGTH 1 VALUE 'X',
               lc_true               TYPE string VALUE 'TRUE'.

* Reference Objects
    DATA: "lo_error   TYPE REF TO /iwbep/if_message_container,                  "Message Container
      lo_auth    TYPE REF TO object,                                       "Object
      lo_msg1    TYPE REF TO cx_root,                                      "Abstrakte Oberklasse aller globalen Exceptions
      lo_msg     TYPE REF TO cx_root,                                      "Abstrakte Oberklasse aller globalen Exceptions
      lo_exchtab TYPE REF TO /odsmfe/cl_exchmechwo.                       "Exchange Mechanism Class
    " lo_rfc     TYPE REF TO /odsmfe/cl_get_ent_super_bapi,
    "lo_xml     TYPE REF TO cl_xml_document.                              "XML-Dokument für WF- WEB-Aktivität
  PRIVATE SECTION.
    METHODS modify FOR BEHAVIOR IMPORTING
                                  roots_to_create FOR CREATE ResponseCapture
                                  roots_to_update FOR UPDATE ResponseCapture
                                  roots_to_delete FOR DELETE ResponseCapture .


    METHODS read FOR READ
      IMPORTING lit_ResponseCapture FOR READ ResponseCapture RESULT ex_ResponseCapture.

ENDCLASS.

CLASS lhc_ResponseCapture IMPLEMENTATION.

  METHOD get_cloud_dest.
    SELECT SINGLE param_value FROM /odsmfe/tb_apcon
    WHERE param_name = @lc_dest
    INTO @lv_dest.
    TRY.
        DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination(
                         i_name = lv_dest ).
        ex_dest = lo_rfc_dest->get_destination_name(  ).
      CATCH  cx_rfc_dest_provider_error INTO DATA(lx_dest).
    ENDTRY.

  ENDMETHOD.

  METHOD gmib_form_inst_post_process.
*&----------------------------------------------------------------------*
* PROGRAM ID           :/ODSMFE/CL_FORMPUBLISHED                        *
* DEVELOPER ID         :VSANAGALA                                       *
* SUPPLIER             :OnDevice Solutions                              *
* DATE                 :14.07.2023                                      *
* DEVELOPMENT ID       :/ODSMFE/FRM_OBJ                                 *
* CHANGE REQUEST (CTS) :ES1K903614                                      *
* DESCRIPTION          :Method to create code and code groups           *
*=======================================================================*
* COPIED FROM         : (CLONED PROGRAM)                                *
* TITLE               : (PROGRAM TITLE)                                 *
* OTHER RELATED OBJ   : (OBJECT NAMES)                                  *
*=======================================================================*
*-----------------------------------------------------------------------*
* CHANGE HISTORY LOG                                                    *
* MOD.NO.| DATE     | NAME           | CORRECTION NUMBER  |CHANGE       *
*                                                          REFERENCE    *
*-----------------------------------------------------------------------*
*        |          |                |                    |             *
* DESCRIPTION:                                                          *
*-----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

* Tables and Structures
    DATA: lit_return TYPE /ODSMFE/TT_bapiret2,                                          "Return Parameter
          lst_return TYPE bapiret2.                                            "Return Parameter

* Variables
    DATA: lv_user TYPE c LENGTH 12."xubname.                                                "User ID

* Reference Objects
*    DATA: lo_finst_process_badi TYPE REF TO /odsmfe/bd_form_inst_process.      "BADI Definition
*
** -----------------------------------------------------------------------*
**            E N D   O F   D A T A   D E C L A R A T I O N             *
** -----------------------------------------------------------------------*
*
*    GET BADI lo_finst_process_badi.
*    lv_user = sy-uname.
** ----------------------------------------------------------------------*
** --- Calling Badi for the Initial Chaecks------------------------------*
** ----------------------------------------------------------------------*
*    CALL BADI lo_finst_process_badi->initial_checks
*      EXPORTING
*        im_user   = lv_user                                                    " User Name in User Master Record
*      CHANGING
*        ch_return = lit_return.                                                " Return parameter table

    IF lit_return[] IS NOT INITIAL.
      CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_error_handling
        EXPORTING
          im_return = lit_return.
    ENDIF.                                                                     " IF LIT_RETURN[] IS NOT INITIAL.

    IF lit_return[] IS INITIAL.
* ----------------------------------------------------------------------*
* --- Calling Badi for the Pre-processing ------------------------------*
* ----------------------------------------------------------------------*
*      CALL BADI lo_finst_process_badi->pre_processing
*        EXPORTING
*          im_form_response_data = im_form_response_data                        " ResponseCaptureSet Gateway Properties
*          im_xml_table_data     = im_xml_table_data
*        CHANGING
*          ch_return             = lit_return.                                  " Return parameter table

      IF lit_return[] IS NOT INITIAL.
        CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_error_handling
          EXPORTING
            im_return = lit_return.
      ENDIF.                                                                   " IF LIT_RETURN[] IS NOT INITIAL.

      IF lit_return[] IS INITIAL.
* ----------------------------------------------------------------------*
* --- Calling Badi for the Post-processing -----------------------------*
* ----------------------------------------------------------------------*
*        CALL BADI lo_finst_process_badi->post_processing
*          EXPORTING
*            im_form_response_data = im_form_response_data                      " ResponseCaptureSet Gateway Properties
*            im_xml_table_data     = im_xml_table_data
*          CHANGING
*            ch_return             = lit_return.                                " Return parameter table

        IF lit_return[] IS NOT INITIAL.
*          CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_error_handling
*            EXPORTING
*              im_return = lit_return.
        ENDIF.                                                                 " IF LIT_RETURN[] IS NOT INITIAL.

      ENDIF.
    ENDIF.

    IF lit_return[] IS NOT INITIAL.
      ex_return[] = lit_return[].
    ENDIF.

  ENDMETHOD.
  METHOD get_xml_content.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 12/05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
************************************************************************
* Data Declaration
************************************************************************
* Class
    DATA: lo_xml  TYPE REF TO if_ixml_document."cl_xml_document,
    " lo_error TYPE REF TO /iwbep/if_message_container.

* Tables & Structures
    DATA: "lit_data    TYPE swxmlcont,
      lit_retcode TYPE sysubrc,
      lit_return1 TYPE STANDARD TABLE OF bapiret2.

* Field symbols
    FIELD-SYMBOLS : <lfsst_return> TYPE bapiret2.

* Variables
    DATA: lv_xml_string TYPE xstring,
          lv_message    TYPE bapi_msg,
          lv_subrc      TYPE sy-subrc,                      "#EC NEEDED
          lv_size       TYPE sy-tabix,
          l_lines  type sy-tabix,
          lit_return14  TYPE STANDARD TABLE OF /ODSMFE/ST_bapiret2. "#EC NEEDED
* Constants
    CONSTANTS: lc_a TYPE string VALUE 'A',
               lc_e TYPE string VALUE 'E'.
************************************************************************
* Main Section
************************************************************************
    "CREATE OBJECT lo_xml.
* Check Response data
    IF im_responsedata IS NOT INITIAL.
      lv_xml_string = im_responsedata.
    ELSE.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = TEXT-024.
    ENDIF.
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

* Convert XString to Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      DESTINATION lv_rfc
      EXPORTING
        buffer     = lv_xml_string
      TABLES
        binary_tab = lit_data.
    IF lit_data IS INITIAL.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = TEXT-025.
    ENDIF.
* Parse data




*    CALL METHOD lo_xml->create_with_table
*      EXPORTING
*        table   = lit_data
**       SIZE    = 0
*      RECEIVING
*        retcode = lit_retcode.
** render_2_xstring

*    CALL METHOD lo_xml->render_2_xstring
*      IMPORTING
*        retcode = lv_subrc
*        stream  = lv_xml_string
*        size    = lv_size.
*    IF lv_xml_string IS INITIAL.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = TEXT-026.
*    ENDIF.

* Convert XML to internal table
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc1).
    CALL FUNCTION 'SMUM_XML_PARSE'
      DESTINATION lv_rfc1
      EXPORTING
        xml_input = lv_xml_string
      TABLES
        xml_table = ex_xml_data
        return    = lit_return1.

    IF lit_return1 IS NOT INITIAL.


      DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
      IF lit_return1 IS NOT INITIAL.
*        lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*        lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
*          iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*          iv_add_to_response_header = abap_true ).
*
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            message_container = lo_error.
      ENDIF.
    ENDIF.
* Error handling
    DATA(lit_return12) = ex_return.
*    APPEND ex_return TO lit_return14.
    LOOP AT lit_return12 ASSIGNING <lfsst_return>.
      IF <lfsst_return>-type = lc_e OR <lfsst_return>-type = lc_a.
        lv_message = <lfsst_return>-message.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = lv_message.
      ENDIF.
    ENDLOOP.
    "Delete the ignorables
    DELETE ex_xml_data WHERE cname+0(7) EQ '_IGNORE'.    "#EC CI_STDSEQ
* Delete Data from XML based on the below conditions
    DELETE ex_xml_data WHERE cname = TEXT-028."instanceID      "#EC CI_STDSEQ
    DELETE ex_xml_data WHERE cname = TEXT-029."start        "#EC CI_STDSEQ
    DELETE ex_xml_data WHERE cname = TEXT-030."end             "#EC CI_STDSEQ
  ENDMETHOD.
  METHOD gmib_create_notif_item_act.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method creates Notification Item activity
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************
* Data Declaration
***********************************************************************
*SOC BY Priyanka
    TYPES: BEGIN OF ty_tq80,
             qmart TYPE  c LENGTH 2,
             qmtyp TYPE  c LENGTH 2,
           END OF ty_tq80.
    TYPES:BEGIN OF ty_options,
            text(72) TYPE c,
          END OF ty_options.

    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.

    TYPES: BEGIN OF ty_data,
             wa(512) TYPE c,
           END OF ty_data.
*EOC BY Priyanka

    TYPES: BEGIN OF ltys_notificationitem,
             qmnum TYPE /odsmfe/tb_fmass-notification, "qmfe-qmnum,
             fenum TYPE /odsmfe/tb_fmass-notificationitem, "qmfe-fenum,
           END OF ltys_notificationitem.
    TYPES : BEGIN OF ltys_notificationact1,
              qmnum TYPE  /odsmfe/tb_fmass-notification, "qmma-qmnum,
              manum TYPE  /odsmfe/tb_fmass-notificationtask, "qmma-manum,
            END OF ltys_notificationact1.

    TYPES : BEGIN OF ltys_data,
              qmnum TYPE /odsmfe/tb_fmass-notification, "qmnum,                                         "Notification No
              manum TYPE /odsmfe/tb_fmass-notificationtask, "manum,                                         "Sequential Task Number
              name  TYPE c LENGTH 255,                                       "Char255
              value TYPE c LENGTH 255,                                       "Char255
              longt TYPE string,
            END OF ltys_data.
* Variables
    DATA: lv_aufnr         TYPE aufnr,                                         "Order Number
          lv_qmnum         TYPE /odsmfe/tb_fmass-notification,                                         "Notification No
          lv_count         TYPE n LENGTH 4,                                          "Four-digit number
          lv_act_key       TYPE /odsmfe/tb_fmass-notificationtask, "manum,                                         "Sequential Task Number
          lv_item_key      TYPE /odsmfe/tb_fmass-notificationitem, "fenum,                                         "Notification Print Item Number
          lv_formid        TYPE /odsmfe/tb_fmass-formid, "string,                                        "30 Characters
          lv_codegruppe    TYPE /odsmfe/tb_fomst-codegruppe, "qcodegrp,                                      "Code Group
          lv_code          TYPE c LENGTH 4, "qcode,                                         "Code
          lv_version       TYPE /odsmfe/de_version,                            "ODS Version
          lv_key           TYPE /odsmfe/tb_fmass-notificationtask, "manum,                                         "Sequential Task Number
          lv_actcodegruppe TYPE /odsmfe/tb_fomst-codegruppe, "qcodegrp,                                      "Code Group
          lv_actcode       TYPE c LENGTH 4, "qcode,                                         "Code
          lv_longt         TYPE string,
          lv_qmart         TYPE c LENGTH 2, "qmart,                                         "Notification Type
          lv_number        TYPE /odsmfe/tb_fmass-notification, "bapi2080_nothdre-notif_no,                     "Notification No
          lv_message       TYPE bapi_msg,                                      "Message Text
          lv_msg           TYPE string,
          lv_msg1          TYPE string.

* Table & Structure
    DATA: lit_notificationitem   TYPE STANDARD TABLE OF  ltys_notificationitem,
          lst_notificationitem   TYPE  ltys_notificationitem,
          lit_notitem            TYPE STANDARD TABLE OF /odsmfe/st_bapi2080_notitemi,  "Notification item for creation
          lit_notificationact    TYPE STANDARD TABLE OF ltys_notificationact1, " qmma,           "Quality notification - activities
          lit_notificationact1   TYPE TABLE OF ltys_notificationact1,
          lst_notification1      TYPE ltys_notificationact1, "qmma,                             " added by shyamal
          lst_notifitem          TYPE /odsmfe/st_bapi2080_notitemi,                "Notification item for creation
          lit_notifactv          TYPE STANDARD TABLE OF /odsmfe/st_bapi2080_notactvi,  "Notification activity for creation
          lst_notifact           TYPE /odsmfe/st_bapi2080_notactvi,                "Notification activity for creation
          lst_xml_data           TYPE /odsmfe/st_snum_xml_tb,
          "XML Table structure used for retreive and output XML doc
          lit_tq80               TYPE STANDARD TABLE OF ty_tq80,           "Notification Types
          lst_tq80               TYPE ty_tq80,                             "Notification Types
          lit_notifheader_export TYPE /odsmfe/st_bapi2080_nothdre, "bapi2080_nothdre,     "#EC NEEDED
          lit_notifhdtext        TYPE /odsmfe/st_bapi2080_nothdtxte, "#EC NEEDED
          lit_notifheader        TYPE /odsmfe/st_bapi2080_nothdre,  "bapi2080_nothdre,     "#EC NEEDED
          lst_return             TYPE bapiret2.                         "Return Parameter

    TYPES:BEGIN OF ty_tline,
            tdformat TYPE c LENGTH 2,
            tdline   TYPE c LENGTH 132,
          END OF ty_tline.
    TYPES:BEGIN OF ty_swastrtab,
            len TYPE int4,
            str TYPE c LENGTH 255,
          END OF ty_swastrtab.

    FIELD-SYMBOLS : <lfsst_return> TYPE bapiret2.

    DATA :lit_txt TYPE TABLE OF ltys_data,
          lst_txt TYPE ltys_data.
    DATA :lst_lines TYPE ty_tline,                                  "SAPscript: Text Lines
          lit_lines TYPE TABLE OF ty_tline.                         "SAPscript: Text Lines

    DATA :lit_longtext    TYPE TABLE OF ty_swastrtab,               "WF: Display string in 255-character lines
          lst_longtext    TYPE ty_swastrtab,
          lit_return1     TYPE  /odsmfe/st_bapiret2,
          lit_return11    TYPE STANDARD TABLE OF bapiret2,
*          lo_error        TYPE REF TO /iwbep/if_message_container,
          lv_manum        TYPE /odsmfe/tb_fmass-notificationtask, "aknum,                                  "Consecutive Number of Activity
          lv_actlongtext  TYPE c LENGTH 50,                                 "Comment
          lv_actshorttext TYPE c LENGTH 50,
          lv_name1        TYPE  c LENGTH 255.
    DATA: lo_msg     TYPE REF TO cx_root,
          lo_msg1    TYPE REF TO cx_root,
          lt_options TYPE TABLE OF ty_options,
          lt_fields  TYPE TABLE OF ty_fields,
          lt_data    TYPE TABLE OF ty_data,
          ls_data    TYPE ty_data,
          ls3_data   TYPE ty_data,
          ls4_data   TYPE ty_data.
**********************************************************
* Main Section
**********************************************************
    TRY.
* Conversion Exit
*        lv_aufnr = im_wonum.

        IF lv_aufnr IS NOT INITIAL.
        lv_aufnr = |{ im_wonum ALPHA = OUT }|.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = lv_aufnr
*            IMPORTING
*              output = lv_aufnr.
* Get the Notification number from work order number
*          SELECT SINGLE qmnum  FROM afih
*          WHERE aufnr = @lv_aufnr INTO @lv_qmnum .
* SOC By Priyanka
          CALL METHOD me->get_cloud_dest
            IMPORTING
              ex_dest = DATA(lv_rfc).

          lt_fields = VALUE #( ( fieldname = 'QMNUM' ) ).

          lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_aufnr }| & |'| ) ).

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'AFIH'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          lv_qmnum = ls_data-wa.
          IF sy-subrc = 0.
            lv_number = lv_qmnum.
          ENDIF.
        ENDIF.

        IF lv_number IS NOT INITIAL.
* Look for Notificaion Items
*          SELECT qmnum,fenum FROM qmfe
*          WHERE qmnum = @lv_qmnum INTO CORRESPONDING FIELDS OF TABLE lit_notificationitem.

          lt_fields = VALUE #( ( fieldname = 'QMNUM' )
                                ( fieldname = 'FENUM' )
                                  ).

          lt_options = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ lv_qmnum }| & |'| ) ).

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMFE'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          LOOP AT LT_data INTO DATA(wa_data).
            lst_notificationitem-fenum = wa_data+0(4).
            lst_notificationitem-qmnum = wa_data+5(16).
            APPEND lst_notificationitem TO lit_notificationitem.
          ENDLOOP.
*EOC BY Priyanka

          IF sy-subrc = 0.
* get the count of notification items
            lv_count = lines( lit_notificationitem ).
          ENDIF.
* increment the count by 1
          lv_item_key = lv_count + 1.

*          CLEAR lst_xml_data.
*          READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'id'.
*          IF sy-subrc = 0.
*            lv_formid = lst_xml_data-cvalue.
*          ENDIF.
*          lv_version = im_version.

          CLEAR lst_xml_data.
*          READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'id'.
*          IF sy-subrc <> 0.
*            READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'name'.
*          ENDIF.

          IF lst_xml_data-cvalue IS NOT INITIAL.
            SPLIT lst_xml_data-cvalue AT ' [' INTO lv_formid lv_version.
            CLEAR lv_version.
          ENDIF.

          lv_version = im_version.

          CLEAR: lv_codegruppe, lv_code.
* Fetch code Group based on type = 'B'
*          SELECT SINGLE qpct~codegruppe qpct~code INTO (@lv_codegruppe,lv_code)
*          FROM qpct INNER JOIN d/osmfe/tb_fomst ON /odsmfe/tb_fomst~codegruppe = qpct~codegruppe
*          WHERE /odsmfe/tb_fomst~formid =  lv_formid
*          AND   /odsmfe/tb_fomst~version = lv_version
*          AND   qpct~katalogart = 'B'
*          AND   qpct~sprache = sy-langu
*          AND   qpct~inaktiv NE 'X'.                   "#EC CI_BUFFJOIN

          lt_fields = VALUE #( ( fieldname = 'CODEGRUPPE' )
                            ( fieldname = 'CODE' )
                              ).

          lt_options = VALUE #( ( text = |KATALOGART| & | | & |EQ| & | | & |'| & |'B'| & |'| ) ).
          DATA(lv_and) = 'AND'.
          APPEND VALUE #( text = |{ lv_and }| & | | & |SPRACHE| & | | & |EQ| & | | & |'| & |SY-LANGU| & |'| ) TO lt_options.
          APPEND VALUE #( text = |{ lv_and }| & | | & |INAKTIV| & | | & |NE| & | | & |'| & |'X'| & |'| ) TO lt_options.

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMFE'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          LOOP AT LT_data INTO DATA(LS1_data).
            lv_codegruppe = LS1_data+0(8).
            lv_code = ls1_data+8(12).
          ENDLOOP.

          SELECT SINGLE codegruppe FROM /odsmfe/tb_fomst
          WHERE
            /odsmfe/tb_fomst~codegruppe = @lv_codegruppe
           AND /odsmfe/tb_fomst~formid =  @lv_formid
           AND   /odsmfe/tb_fomst~version = @lv_version INTO  @lv_codegruppe.
*EOC BY Priyanka



          IF lv_codegruppe IS INITIAL OR lv_code IS INITIAL.
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                message = TEXT-t02.
          ENDIF.

          IF sy-subrc = 0.
            lst_notifitem-dl_codegrp   = lv_codegruppe.
            lst_notifitem-dl_code      = lv_code.
            lst_notifitem-item_key     = lv_item_key.
            lst_notifitem-item_sort_no = lv_item_key.
            APPEND lst_notifitem TO lit_notitem.
            CLEAR: lst_notifitem.
          ENDIF.

          CLEAR: lv_count, lv_act_key.
* Look for Notification Activities
*          SELECT qmnum manum INTO CORRESPONDING FIELDS OF TABLE lit_notificationact FROM qmma
*          WHERE qmnum = lv_qmnum
*          AND   fenum = lv_item_key
*          AND mngrp = lv_codegruppe.
*
*SOC BY Priyanka
          lt_fields = VALUE #( ( fieldname = 'QMNUM' )
                                     ( fieldname = 'MANUM' )
                                       ).

          lt_options = VALUE #( ( text = |QMNUMT| & | | & |EQ| & | | & |'| & |{ lv_qmnum }| & |'| ) ).
          DATA(lv1_and) = 'AND'.
          APPEND VALUE #( text = |{ lv1_and }| & | | & |FENUM| & | | & |EQ| & | | & |'| & |{ lv_item_key }| & |'| ) TO lt_options.
          APPEND VALUE #( text = |{ lv1_and }| & | | & |MNGRP| & | | & |EQ| & | | & |'| & |{ lv_codegruppe }| & |'| ) TO lt_options.

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMMA'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          LOOP AT LT_data INTO DATA(LS2_data).
            lSt_notification1-manum = ls2_data+0(4).
            lSt_notification1-qmnum = LS2_data+4(16).
            APPEND lst_notification1 TO lit_notificationact.
          ENDLOOP.


          IF sy-subrc = 0.
* get the count of notification activities
            lv_count = lines( lit_notificationact ).
          ENDIF.
* Incremement the count by 1
          lv_act_key = lv_count + 1.
          lv_manum = lv_item_key - 1.

*          SELECT qmnum manum INTO CORRESPONDING FIELDS OF TABLE lit_notificationact1 FROM qmma
*          WHERE qmnum = lv_qmnum
*          AND   fenum = lv_manum.
          lt_fields = VALUE #( ( fieldname = 'QMNUM' )
                           ( fieldname = 'MANUM' )
                             ).

          lt_options = VALUE #( ( text = |QMNUMT| & | | & |EQ| & | | & |'| & |{ lv_qmnum }| & |'| ) ).
          DATA(lv2_and) = 'AND'.
          APPEND VALUE #( text = |{ lv2_and }| & | | & |FENUM| & | | & |EQ| & | | & |'| & |{ lv_manum }| & |'| ) TO lt_options.

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMMA'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          LOOP AT LT_data INTO LS3_data.
            lSt_notification1-manum = ls3_data+0(4).
            lSt_notification1-qmnum = LS3_data+4(16).
            APPEND lst_notification1 TO lit_notificationact1.
          ENDLOOP.

          IF sy-subrc = 0.
            SORT lit_notificationact1 DESCENDING BY manum.
          ENDIF.
          READ TABLE lit_notificationact1 INTO lst_notification1 INDEX 1.
* Get the original activity key
          lv_key = lst_notification1-manum.

          SELECT SINGLE low FROM /odsmfe/tb_filtr
          WHERE entitysetname = 'WoHeaderSet'
          AND field =    'ACTIVITYLONGTEXTLENGHT' INTO @lv_actlongtext.

          SELECT SINGLE low FROM /odsmfe/tb_filtr
          WHERE entitysetname = 'WoHeaderSet'
          AND field =    'UPDATEACTIVITYSHORTTEXT' INTO @lv_actshorttext.

          CLEAR lst_xml_data.
          LOOP AT im_xml_data INTO lst_xml_data WHERE type = 'V' OR type = '+'.

            IF lst_xml_data-type = 'V'.
              lv_key = lv_key + 1.
            ENDIF.
            lst_notifact-act_key      = lv_act_key.
            lst_notifact-act_sort_no  = lv_act_key.
            lst_notifact-item_sort_no = lv_item_key.
            CLEAR : lv_actcodegruppe , lv_actcode.

* Fetch code Group based on type = 'A'
*            SELECT SINGLE codegruppe,code FROM qpct INTO (@lv_actcodegruppe,@lv_actcode)
*            WHERE kurztext =  @lst_xml_data-cname
*            AND codegruppe = @lv_codegruppe
*            AND katalogart = 'A'
*            AND inaktiv NE 'X'
*            AND sprache = @sy-langu.

*SOC BY Priyanka
            lt_fields = VALUE #( ( fieldname = 'CODEGRUPPE' )
                                       ( fieldname = 'CODE' )
                                         ).

            lt_options = VALUE #( ( text = |KURZTEXT| & | | & |EQ| & | | & |'| & |{ lst_xml_data-cname }| & |'| ) ).
            DATA(lv3_and) = 'AND'.
            APPEND VALUE #( text = |{ lv3_and }| & | | & |CODEGRUPPE| & | | & |EQ| & | | & |'| & |{ lv_codegruppe }| & |'| ) TO lt_options.
            APPEND VALUE #( text = |{ lv3_and }| & | | & |KATALOGART| & | | & |EQ| & | | & |'| & |'A'| & |'| ) TO lt_options.
            APPEND VALUE #( text = |{ lv3_and }| & | | & |INAKTIVE| & | | & |NE| & | | & |'| & |'X'| & |'| ) TO lt_options.
            APPEND VALUE #( text = |{ lv3_and }| & | | & |SPRACHE| & | | & |EQ| & | | & |'| & |SY-LANGU| & |'| ) TO lt_options.
            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'QPCT'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            lv_actcodegruppe = ls4_data+0(8).
            lv_actcode = LS4_data+8(12).



            IF sy-subrc = 0.
              IF lv_actcodegruppe IS INITIAL OR lv_actcode IS INITIAL.
*                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                  EXPORTING
*                    textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                    message = TEXT-t03.
              ENDIF.
              lst_notifact-act_codegrp = lv_actcodegruppe.
              lst_notifact-act_code    = lv_actcode.
              IF lst_xml_data-type = '+'.
                lv_name1 = lst_xml_data-cname.
              ENDIF.
              IF lv_name1 = lst_xml_data-cname.
                CONCATENATE lv_longt lst_xml_data-cvalue INTO lv_longt.
              ELSE.
                CLEAR lv_longt.
              ENDIF.
            ENDIF.
* Fill the activity texts into a table which are more than 40 characters.
            IF lst_xml_data-type = 'V' .

              IF strlen( lst_xml_data-cvalue ) GT lv_actlongtext OR lv_longt IS NOT INITIAL.
                lst_txt-qmnum = lv_qmnum.
                lst_txt-manum = lv_key.
                lst_txt-name  = lst_xml_data-cname.
                lst_txt-value = lst_xml_data-cvalue.
                lst_txt-longt = lv_longt.
                APPEND lst_txt TO lit_txt.
                CLEAR lst_txt.
              ENDIF.

              IF lv_actshorttext = 'X' AND ( strlen( lst_xml_data-cvalue )
              GT lv_actlongtext OR lv_longt IS NOT INITIAL ).
                lst_notifact-acttext = TEXT-t04.
              ELSE.
                lst_notifact-acttext = lst_xml_data-cvalue.
              ENDIF.
              APPEND lst_notifact TO lit_notifactv.
              lv_act_key = lv_act_key + 1.
            ENDIF.
            CLEAR: lst_notifact , lst_xml_data.
          ENDLOOP.

* Look for Notification Type
*          SELECT SINGLE qmart FROM qmel  WHERE qmnum = @lv_number INTO @lv_qmart.
*          IF sy-subrc = 0.
*SOC BY Priyanka

          lt_fields = VALUE #( ( fieldname = 'QMART' )
                              ).

          lt_options = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ lv_number }| & |'| ) ).

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMEL'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.
          LOOP AT lt_data INTO DATA(wa1_data).
            lv_qmart = wa1_data-wa.
          ENDLOOP.

* look for Notification type and Category
*            SELECT qmart qmtyp FROM tq80 INTO CORRESPONDING FIELDS OF TABLE lit_tq80
*            WHERE qmart = lv_qmart.
          lt_fields = VALUE #( ( fieldname = 'QMART' )
                                ( fieldname = 'QMTYP' ) ).

          lt_options = VALUE #( ( text = |QMART| & | | & |EQ| & | | & |'| & |{ lv_qmart }| & |'| ) ).


          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'TQ80'
            TABLES
              options     = lt_options
              fields      = lt_fields
              data        = lt_data.

          LOOP AT lt_data INTO DATA(wa4_data).
            lst_tq80-qmart = wa4_data+0(2).
            lst_tq80-qmtyp = wa4_data+2(4).
          ENDLOOP.
          IF sy-subrc = 0.
            SORT lit_tq80 BY  qmart.
          ENDIF.
          READ TABLE lit_tq80 INTO lst_tq80 WITH KEY qmart = lv_qmart   BINARY SEARCH.
          "  ENDIF.
          IF sy-subrc = 0 AND lst_tq80-qmtyp = '03'.
            CLEAR ex_return.
* Update Service Notification
            IF lit_notitem IS NOT INITIAL AND lit_notifactv IS NOT INITIAL.
              CALL FUNCTION 'BAPI_SERVNOT_ADD_DATA'
                DESTINATION lv_rfc
                EXPORTING
                  number      = lv_number
                IMPORTING
                  notifheader = lit_notifheader_export
                TABLES
                  notitem     = lit_notitem
                  notifactv   = lit_notifactv
                  return      = lit_return11.
            ENDIF.

            IF lit_return11 IS NOT INITIAL.

              DELETE lit_return11 WHERE type NE 'E' OR type = 'A'.
              IF lit_return11 IS NOT INITIAL.
*              lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*              lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return11
*                iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                iv_add_to_response_header = abap_true ).
*
*              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                EXPORTING
*                  message_container = lo_error.
              ENDIF.
            ENDIF.
* Error handling
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.

            LOOP AT lit_return11 ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.

                lv_message = <lfsst_return>-message.
*              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                EXPORTING
*                  textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                  message = lv_message.
              ENDIF.
            ENDLOOP.
          ELSE.
* Update Notification
            IF lit_notitem IS NOT INITIAL AND lit_notifactv IS NOT INITIAL.
              CLEAR ex_return.
              CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
                DESTINATION lv_rfc
                EXPORTING
                  number             = lv_number
                IMPORTING
                  notifhdtext        = lit_notifhdtext
                  notifheader_export = lit_notifheader_export
                TABLES
                  notitem            = lit_notitem
                  notifactv          = lit_notifactv
                  return             = lit_return11.
            ENDIF.
* Error handling
            IF lit_return11  IS NOT INITIAL.

              DELETE lit_return11 WHERE type NE 'E' OR type = 'A'.
              IF lit_return11 IS NOT INITIAL.
*              lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*              lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return11
*                iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                iv_add_to_response_header = abap_true ).
*
*              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                EXPORTING
*                  message_container = lo_error.
              ENDIF.
            ENDIF.
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.

            LOOP AT lit_return11 ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.

                lv_message = <lfsst_return>-message.
*              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                EXPORTING
*                  textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                  message = lv_message.
              ENDIF.
            ENDLOOP.
*        ENDIF.

            IF lv_number IS NOT INITIAL.
              CLEAR ex_return.
* Save Notification
              CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
                DESTINATION lv_rfc
                EXPORTING
                  number      = lv_number
                IMPORTING
                  notifheader = lit_notifheader
                TABLES
                  return      = lit_return11.


              DELETE lit_return11 WHERE type NE 'E' OR type = 'A'.
              IF lit_return1 IS NOT INITIAL.
*            lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*            lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return11
*              iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*              iv_add_to_response_header = abap_true ).
*
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                message_container = lo_error.
              ENDIF.
            ENDIF.
* Error handling
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.

            LOOP AT lit_return11 ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.
                lv_message = <lfsst_return>-message.
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                textid  = /iwbep/cx_mgw_busi_exception=>business_error
**                message = lv_message.
              ENDIF.
            ENDLOOP.

            CLEAR lst_return.
* Database Commit
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = 'X'
              IMPORTING
                return = lst_return.
* Error handling
            IF lst_return-type EQ 'E' OR lst_return-type EQ 'A'.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              lv_message = lst_return-message.
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*            EXPORTING
*              textid  = /iwbep/cx_mgw_busi_exception=>business_error
**              message = lv_message.
            ENDIF.

* Update the notification activity longtext
            IF lit_txt IS NOT INITIAL.

              LOOP AT lit_txt INTO lst_txt.

                IF lst_txt-longt IS NOT INITIAL.
                  CALL FUNCTION 'SWA_STRING_SPLIT'
                    DESTINATION lv_rfc
                    EXPORTING
                      input_string                 = lst_txt-longt
                      max_component_length         = '132'
                    TABLES
                      string_components            = lit_longtext
                    EXCEPTIONS
                      max_component_length_invalid = 1
                      OTHERS                       = 2.

                  IF sy-subrc = 0.
                    LOOP AT lit_longtext INTO lst_longtext. "#EC CI_NESTED.
                      lst_lines-tdformat = '*'.
                      lst_lines-tdline   = lst_longtext-str.
                      APPEND lst_lines TO lit_lines.
                      CLEAR lst_lines.
                    ENDLOOP.
                  ENDIF.
                ELSE.

                  lst_lines-tdformat = '*'.
                  lst_lines-tdline   = lst_txt-value+0(132).
                  APPEND lst_lines TO lit_lines.
                  CLEAR lst_lines.

                  lst_lines-tdformat = '*'.
                  lst_lines-tdline   = lst_txt-value+132(123).
                  APPEND lst_lines TO lit_lines.
                  CLEAR lst_lines.
                ENDIF.

                CALL FUNCTION 'IQS0_ADD_ACTIVITY_LONGTEXT'
                  DESTINATION lv_rfc
                  EXPORTING
                    i_qmnum       = lst_txt-qmnum
                    i_manum       = lst_txt-manum
                    i_post        = 'X'
                  TABLES
                    t_inlines     = lit_lines
                  EXCEPTIONS
                    show_messages = 1
                    OTHERS        = 2.

                IF sy-subrc = 0.
                  CALL FUNCTION 'IQS1_REFRESH_ALL'
                    DESTINATION lv_rfc. " To Reset the notification buffer
                ENDIF.
              ENDLOOP.
            ENDIF.
* Update Exchange Table entry
*        TRY.
            CALL METHOD me->gmib_delta_table_update
              EXPORTING
                im_qmnum = lv_number.

*          CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
*            CLEAR lv_msg.
*            lo_msg->get_text( RECEIVING result = lv_msg ).
*            lo_msg->get_longtext( RECEIVING result = lv_msg ).
*            "MESSAGE lv_msg TYPE 'I'.
*            MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg INTO DATA(lv_string).
*
*          CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
*            CLEAR lv_msg1.
*            lo_msg1->get_text( RECEIVING result = lv_msg1 ).
*            lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
*            "MESSAGE lv_msg1 TYPE 'I'.
*            MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg1 INTO DATA(lv_string2).
*        ENDTRY.

*      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
*        CLEAR lv_msg.
*        lo_msg->get_text( RECEIVING result = lv_msg ).
*        lo_msg->get_longtext( RECEIVING result = lv_msg ).
*        "MESSAGE lv_msg TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg INTO DATA(lv_dummy).
*      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
*        CLEAR lv_msg1.
*        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
*        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
*        "MESSAGE lv_msg1 TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg1 INTO DATA(lv_dummy2).

          ENDIF.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD gmib_delta_table_update.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method to update Delta table
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************
* Data Declaration
***********************************************************************
    DATA: lv_aufnr          TYPE aufnr,
          lv_mobile_app     TYPE string,
          lv_timestamp      TYPE timestamp,
          lv_sys_tzone      TYPE tznzone, "tznzonesys,
          lv_date_1         TYPE datn,
          lv_time_1         TYPE timn,
          lv_sys_time_token TYPE string.
    DATA: lit_delta TYPE STANDARD TABLE OF /odsmfe/tb_wo_ex,
          lst_delta TYPE /odsmfe/tb_wo_ex.

******************************************************************
* Main Section
******************************************************************
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'  INTO @lv_mobile_app.
* Get System time
    GET TIME STAMP FIELD lv_timestamp.
*    /syclo/cl_core_bapi_tools=>get_system_time(
*    IMPORTING ev_sys_tzone = lv_sys_tzone ).
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

    lit_fields  = VALUE #( ( fieldname = 'TZONESYS' ) ).


    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'TTZCU'
      TABLES
        fields      = lit_fields
        data        = lit_data.
    LOOP AT lit_data INTO DATA(ls_data).
      lv_sys_tzone = ls_Data+0(6).
    ENDLOOP.
    CONVERT TIME STAMP lv_timestamp TIME ZONE lv_sys_tzone
    INTO DATE lv_date_1
    TIME lv_time_1.
    CONCATENATE lv_date_1 lv_time_1 INTO lv_sys_time_token.
    CLEAR: lit_delta.
    CLEAR: lst_delta.

    CALL METHOD me->modify_delta_table
      EXPORTING
        notification = im_qmnum
        time_token   = lv_sys_time_token.

* Clear final internal table
    CLEAR: lit_delta.
    lst_delta-mandt = sy-mandt.
    lst_delta-mobile_app = lv_mobile_app.
    lst_delta-changed_ts = lv_sys_time_token.
    lst_delta-changed_by = sy-uname.
* Get WO Num
*    SELECT SINGLE aufnr FROM qmel WHERE qmnum = @im_qmnum INTO @lv_aufnr.

    lit_fields = VALUE #( ( fieldname = 'AUFNR' )
                               ).

    lit_option = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ im_qmnum }| & |'| ) ).


    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'QMEL'
      TABLES
        options     = lit_option
        fields      = lit_fields
        data        = lit_data.
    LOOP AT lit_data INTO DATA(lst_data).
      lv_aufnr = lst_data+0(12).
    ENDLOOP.
    IF sy-subrc = 0.
      lst_delta-objkey  = lv_aufnr.
* Update Exchange Table

      CALL METHOD me->modify_delta_table
        EXPORTING
          order_number = lv_aufnr
          time_token   = lv_sys_time_token.

    ENDIF.

  ENDMETHOD.

  METHOD gmib_parse_responsedata.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 12/05/2020
* Transport No.          : ES1K901774
* Program Description    : This Method is used to pares data and updates the Notification Items
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************
************************************************************************
* Data Declaration
************************************************************************
* Tables & Structures
    DATA :lit_xml_data TYPE table of /odsmfe/st_snum_xml_tb,                            "XML Table structure used for retreive and output XML doc
          lit_return1  TYPE STANDARD TABLE OF bapiret2,
          lSt_return1  TYPE  bapiret2.                     "Return Parameter
    "lo_error     TYPE REF TO /iwbep/if_message_container.
    DATA: lo_msg  TYPE REF TO cx_root,
          lo_msg1 TYPE REF TO cx_root,
          lv_msg  TYPE string,
          lv_msg1 TYPE string.

    CONSTANTS: lc_e TYPE c LENGTH 1 VALUE 'E',
               lc_a TYPE c LENGTH 2 VALUE 'A'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
* Convert XML Response into Internal Table
    "TRY.
        CALL METHOD me->get_xml_content
          EXPORTING
            im_responsedata = im_responsedata
          IMPORTING
            ex_xml_data     = lit_xml_data
            ex_return       = ex_return.

    IF ex_return IS NOT INITIAL.
      lIt_return1 = ex_return.
      DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
      IF lit_return1 IS NOT INITIAL.
*            lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*            lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
*              iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*              iv_add_to_response_header = abap_true ).
*
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                message_container = lo_error.
      ENDIF.
    ENDIF.

*      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
*        CLEAR lv_msg.
*        lo_msg->get_text( RECEIVING result = lv_msg ).
*        lo_msg->get_longtext( RECEIVING result = lv_msg ).
*        "MESSAGE lv_msg TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg INTO DATA(lv_string).
*
*      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
*        CLEAR lv_msg1.
*        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
*        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
*        "MESSAGE lv_msg1 TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg1 INTO DATA(lv_string1).
*    ENDTRY.

* Create Notification Item & Activities for Form
*    TRY.
    " CLEAR ex_return[].
    CALL METHOD me->gmib_create_notif_item_act
      EXPORTING
        im_wonum    = im_wonum
        im_qmnum    = im_qmnum
        im_formid   = im_formid
        im_version  = im_version
        im_xml_data = ls_xml_data
      IMPORTING
        ex_return   = ex_return.
    IF ex_return IS NOT INITIAL.
      CLEAR lit_return1[].
      lIt_return1 = ex_return.
      APPEND lSt_return1 TO lit_return1.
      DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
      IF lit_return1 IS NOT INITIAL.
*            lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*            lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
*              iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*              iv_add_to_response_header = abap_true ).
*
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                message_container = lo_error.
      ENDIF.
    ENDIF.

*      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
*        CLEAR lv_msg.
*        lo_msg->get_text( RECEIVING result = lv_msg ).
*        lo_msg->get_longtext( RECEIVING result = lv_msg ).
*        " MESSAGE lv_msg TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg INTO DATA(lv_string2).
*
*      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
*        CLEAR lv_msg1.
*        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
*        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
*        "MESSAGE lv_msg1 TYPE 'I'.
*        MESSAGE i397(/odsmfe/myjobcard) WITH lv_msg1 INTO DATA(lv_string3).
    "ENDTRY.

  ENDMETHOD.


  METHOD modify_delta_table.

    DATA : lst_delta         TYPE /odsmfe/tb_wo_ex,
           lv_count          TYPE i,
           lv_mobile_app     TYPE string,
           lv_exchange_table TYPE /odsmfe/de_mfe_low100.

    CONSTANTS: lc_formattach TYPE string    VALUE 'FormAttachmentSet', "/iwbep/sbdm_node_name
               lc_wo         TYPE /odsmfe/de_mfe_fieldname  VALUE 'WORKORDER',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
****************************************************************************************
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME' INTO @lv_mobile_app.

    " Insert data
    lst_delta-mandt      = sy-mandt.
    lst_delta-mobile_app = lv_mobile_app.
    lst_delta-changed_ts = time_token.
    lst_delta-changed_by = sy-uname.

    IF order_number IS NOT INITIAL.

      SELECT SINGLE low FROM /odsmfe/tb_filtr
                          WHERE entitysetname = @lc_formattach
                          AND tabname = @lc_exchtab
                          AND field = @lc_wo  INTO @lv_exchange_table.

      SELECT COUNT(*) FROM (lv_exchange_table)
            WHERE objkey = @order_number INTO @lv_count.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = order_number.

    ELSEIF notification IS NOT INITIAL.
      CLEAR lv_exchange_table.
      SELECT SINGLE low FROM /odsmfe/tb_filtr
                                WHERE entitysetname = 'FormAttachmentSet'
                                  AND tabname = 'NOTIFICATION'
                                  AND field = 'EXCHANGE_TABLE' INTO @lv_exchange_table.

      SELECT COUNT(*) FROM (lv_exchange_table)
          WHERE objkey = @notification INTO @lv_count.

      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = notification.

    ENDIF.

    IF lv_exchange_table IS NOT INITIAL.

      MODIFY (lv_exchange_table) FROM @lst_delta.
      IF sy-subrc = 0 .
        " COMMIT WORK .
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD read_delta_table.

*Variables
    DATA: "lv_exchobj        TYPE /syclo/core_exchobj_dte,
      lv_mobile_app     TYPE string, "/syclo/core_mobile_app_dte,
      " lv_bor_objtyp     TYPE oj_name,
      lv_sys_tzone      TYPE tznzone, "tznzonesys,
      lv_ts             TYPE timestamp,
      lv_ts_str         TYPE string,
      lv_date           TYPE datn,
      lv_time           TYPE timn,
      lv_sys_time_token TYPE string.
    DATA: lv_filter TYPE string.

*Tables & Structures
    DATA: BEGIN OF ls_date_time,
            date TYPE datn,
            time TYPE timn,
          END OF ls_date_time.

    DATA : lit_ex_aufnr          TYPE TABLE OF /odsmfe/ex_aufnr,
           lv_exchange_table(20) TYPE c.

    CONSTANTS : lc_tzone     TYPE tznzone VALUE 'UTC',
                lc_tzone_utc TYPE tznzone VALUE 'UTC'.

    lv_ts_str = delta_token.
    ls_date_time = lv_ts_str.

*    /syclo/cl_core_bapi_tools=>get_system_time(
*      IMPORTING ev_sys_tzone = lv_sys_tzone ).
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

    lit_fields  = VALUE #( ( fieldname = 'TZONESYS' ) ).


    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'TTZCU'
      TABLES
        fields      = lit_fields
        data        = lit_data.
    LOOP AT lit_data INTO DATA(ls_data).
      lv_sys_tzone = ls_Data+0(6).
    ENDLOOP.
    IF lv_sys_tzone = lc_tzone.
      lv_sys_time_token = delta_token.
    ELSEIF lc_tzone = lc_tzone_utc.
      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
        INTO DATE lv_date
             TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
    ELSE.
      lv_date = ls_date_time-date.
      lv_time = ls_date_time-time.
      CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_ts TIME ZONE lc_tzone.
      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
       INTO DATE lv_date
            TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
    ENDIF.


    CONCATENATE 'CHANGED_TS > ' '''' lv_sys_time_token '''' INTO lv_filter SEPARATED BY space.
    CONCATENATE lv_filter ' AND MOBILE_APP = ' '''' lv_mobile_app ''''
      INTO lv_filter.

    SELECT SINGLE low FROM /odsmfe/tb_filtr
                    WHERE entitysetname = 'FormAttachmentSet'
                      AND field = 'EXCHANGE_TABLE' INTO @lv_exchange_table.

    IF lv_exchange_table IS NOT INITIAL.

      SELECT * FROM (lv_exchange_table)
                                     WHERE (lv_filter) INTO TABLE @lit_ex_aufnr.


      IF lit_ex_aufnr IS NOT INITIAL.
        ex_aufnr_data[] = lit_ex_aufnr[].
      ENDIF.
    ENDIF.


  ENDMETHOD.
  METHOD update_characterstics.
*--KMADHURI


    CONSTANTS : lc_equi TYPE c LENGTH 30 VALUE 'EQUI', "bapi1003_key-objecttable
                lc_iloa TYPE c LENGTH 30  VALUE 'IFLOT', "bapi1003_key-objecttable
                lc_e    TYPE c LENGTH 1 VALUE 'E',
                lc_a    TYPE c LENGTH 1 VALUE 'A'.
    DATA : lit_return TYPE TABLE OF bapiret2,
           lv_objtab  TYPE c LENGTH 30, "bapi1003_key-objecttable,
           lit_char   TYPE TABLE OF /odsmfe/bapi1003_alloc_char, "bapi1003_alloc_values_char,
           lit_num    TYPE TABLE OF /odsmfe/bapi1003_alloc_num, "bapi1003_alloc_values_num,
           lit_curr   TYPE TABLE OF /odsmfe/bapi1003_alloc_curr, " bapi1003_alloc_values_curr,
           lv_message TYPE bapi_msg,
           lst_return TYPE bapiret2.


    CLEAR: lit_return,lv_objtab.
    IF lv_klart = '002'.
      lv_objtab = lc_equi.
    ELSE.
      lv_objtab = lc_iloa.
    ENDIF.
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).
* --Get Equipment charateristics
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      DESTINATION lv_rfc
      EXPORTING
        objectkey       = lv_object
        objecttable     = lv_objtab
        classnum        = lv_classnum
        classtype       = lv_klart
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lit_num
        allocvalueschar = lit_char
        allocvaluescurr = lit_curr
        return          = lit_return.


    LOOP AT im_char ASSIGNING FIELD-SYMBOL(<lfs_char>)." WITH KEY charact = <lfs_char1>-charact.
      READ TABLE lit_char ASSIGNING FIELD-SYMBOL(<lfs_char1>) WITH KEY charact = <lfs_char>-charact.
      IF sy-subrc = 0.
        " MOVE <lfs_char>-value TO <lfs_char1>-value_char.
        <lfs_char1>-value_char = <lfs_char>-value.
        "MOVE <lfs_char>-value TO <lfs_char1>-value_neutral.
        <lfs_char1>-value_neutral = <lfs_char>-value.
      ELSE.
        READ TABLE lit_num ASSIGNING FIELD-SYMBOL(<lfs_char2>) WITH KEY charact = <lfs_char>-charact.
        IF sy-subrc = 0.
          "MOVE <lfs_char>-value TO <lfs_char2>-value_from.
          <lfs_char2>-value_from = <lfs_char>-value.
        ELSE.
          READ TABLE lit_curr ASSIGNING  FIELD-SYMBOL(<lfs_char3>) WITH KEY charact = <lfs_char>-charact.
          IF sy-subrc = 0.
            "MOVE <lfs_char>-value TO <lfs_char3>-value_from.
            <lfs_char3>-value_from = <lfs_char>-value.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CLEAR: lit_return.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      DESTINATION lv_rfc
      EXPORTING
        objectkey          = lv_object
        objecttable        = lv_objtab
        classnum           = lv_classnum
        classtype          = lv_klart
        status             = '1'
        keydate            = sy-datum
      TABLES
        allocvaluesnumnew  = lit_num
        allocvaluescharnew = lit_char
        allocvaluescurrnew = lit_curr
        return             = lit_return.

    LOOP AT lit_return ASSIGNING FIELD-SYMBOL(<lfsst_return>).
      IF <lfsst_return>-type = lc_e OR <lfsst_return>-type = lc_a.
        lv_message = <lfsst_return>-message.

*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = lv_message.
      ENDIF.
    ENDLOOP.
* Transaction Commit
    CLEAR lst_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = lst_return.
* Error handling
    IF lst_return-type EQ lc_e OR lst_return-type EQ lc_a.
      lv_message = lst_return-message.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = lv_message.
    ENDIF.
    CLEAR: lit_char.


  ENDMETHOD.
  METHOD equipment_char_update.

*--kmadhuri
    CONSTANTS : lc_equi TYPE c LENGTH 30 VALUE 'EQUI', "bapi1003_key-objecttable
                lc_e    TYPE c LENGTH 1 VALUE 'E',
                lc_a    TYPE c LENGTH 1 VALUE 'A',
                lc_x    TYPE c LENGTH 1 VALUE 'X'.

    DATA : ls_char TYPE /odsmfe/eq_char.


    DATA:lv_equnr    TYPE /odsmfe/tb_forsp-equnr, " equnr,
         lv_tplnr    TYPE /odsmfe/tb_forsp-tplnr, "tplnr
         lv_object   TYPE c LENGTH 50, "bapi1003_key-object
         lit_return  TYPE TABLE OF bapiret2,
         lv_classnum TYPE c LENGTH 18, "bapi1003_key-classnum,
         lv_klart    TYPE c LENGTH 3, "bapi1003_key-classtype,
         lv_message  TYPE bapi_msg,
         lst_return  TYPE bapiret2,
         lit_char    TYPE STANDARD TABLE OF /ODSMFE/bapi1003_alloc_char.
    "Classification BAPI - Values of Type CHAR, BOOL
*     SOC by Priyanka
    TYPES:BEGIN OF ty_options,
            text(72) TYPE c,
          END OF ty_options.

    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.

    TYPES: BEGIN OF ty_data,
             wa(512) TYPE c,
           END OF ty_data.
    TYPES: BEGIN OF ty_equnr,
             aufnr TYPE /odsmfe/tb_forsp-wo_num,
             equnr TYPE /odsmfe/tb_forsp-equnr,
             iloan TYPE c LENGTH 12,
             qmnum TYPE /odsmfe/tb_fmass-notification,
           END OF ty_equnr.
    TYPES: BEGIN OF ty_iloan,
             iloan TYPE c LENGTH 12,
             tplnr TYPE /odsmfe/tb_forsp-tplnr,
           END OF ty_iloan.
    TYPES: BEGIN OF ty_qmequi,
             qmnum TYPE /odsmfe/tb_fmass-notification,
             iloan TYPE c LENGTH 12,
             equnr TYPE /odsmfe/tb_forsp-equnr,
           END OF TY_qmequi.
    DATA:
      lt_options TYPE TABLE OF ty_options,
      lt_fields  TYPE TABLE OF ty_fields,
      lt_data    TYPE TABLE OF ty_data,
      ls_equnr   TYPE ty_equnr,
      ls_iloan   TYPE ty_iloan,
      ls_qmequi  TYPE ty_qmequi.

*    EOC by Priyanka

    IF NOT im_workorder IS INITIAL.

      READ TABLE im_char ASSIGNING FIELD-SYMBOL(<lfs_char>) INDEX 1.
      IF sy-subrc = 0.
        " MOVE <lfs_char>-class TO lv_classnum.
        lv_classnum = <lfs_char>-class.
      ENDIF.
*      IF <lfs_char>-woheadereq = lc_x OR
*      <lfs_char>-wooperationeq = lc_x
*     OR <lfs_char>-notficationeq = lc_x.
*        lv_klart    = '002'."for equipment
**                ELSEIF <lfs_char>-wo_object = lc_x.
**                  lv_klart = '016'."Object link
*      ELSE.
*        lv_klart = '003'."Function location
*      ENDIF.

*      SELECT SINGLE aufnr ,equnr,iloan, qmnum
*        FROM afih INTO @DATA(ls_equnr)
*        WHERE aufnr = @im_workorder.
*    SOC By Priyanka


      CALL METHOD me->get_cloud_dest
        IMPORTING
          ex_dest = DATA(lv_rfc).

      lt_fields = VALUE #( ( fieldname = 'AUFNR' )
                           ( fieldname = 'EQUNR' )
                           ( fieldname = 'ILOAN' )
                            ( fieldname = 'QMNUM' ) ).

      lt_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ im_workorder }| & |'| ) ).

      "Call RFC to get work orders
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc
        EXPORTING
          query_table = 'AFIH'
        TABLES
          options     = lt_options
          fields      = lt_fields
          data        = lt_data.

      LOOP AT lt_data INTO DATA(ls_Data).
        ls_equnr-aufnr = ls_data+0(12).
        ls_equnr-equnr = ls_data+12(18).
        ls_equnr-iloan = ls_data+19(12).
      ENDLOOP.
      IF sy-subrc = 0.
        IF NOT ls_equnr-equnr IS INITIAL.
*          lv_equnr = ls_equnr-equnr.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          DESTINATION lv_rfc
*            EXPORTING
*              input  = lv_equnr
*            IMPORTING
*              output = lv_equnr.
          lv_equnr = |{ ls_equnr-equnr ALPHA = OUT }|.

          CLEAR: lv_object.

          "MOVE lv_equnr TO lv_object.
          lv_object = lv_equnr.
          IF <lfs_char>-woheadereq = lc_x.
            lv_klart    = '002'.
*--Update charactaerstics of the equipment , functional location
            CALL METHOD me->update_characterstics
              EXPORTING
                lv_object   = lv_object
                lv_classnum = lv_classnum
                lv_klart    = lv_klart
                im_char     = im_char.
            CLEAR: lv_equnr.
          ENDIF.

          IF <lfs_char>-woheaderfl = lc_x.
            lv_klart = '003'."Function location
*            SELECT SINGLE iloan, tplnr FROM iloa
*              INTO @DATA(ls_iloa)
*              WHERE iloan = @ls_equnr-iloan.
*          SOC By Priyanka
            lt_fields = VALUE #( ( fieldname = 'ILOAN' )
                                 ( fieldname = 'TPLNR' )
                                ).

            lt_options = VALUE #( ( text = |ILOAN| & | | & |EQ| & | | & |'| & |{ ls_equnr-iloan }| & |'| ) ).

            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'ILOA'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO DATA(lv_Data).
              ls_iloan-iloan = lv_Data+0(12).
              ls_iloan-tplnr = lv_Data+12(42).
            ENDLOOP.
            DATA(ls_iloan1) = ls_iloan.
*EOC By Priyanka
            IF sy-subrc = 0.
              "MOVE ls_iloa-tplnr TO lv_tplnr.
*              lv_tplnr = ls_iloan-tplnr.
*             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*             DESTINATION lv_rfc
*                EXPORTING
*                  input  = lv_tplnr
*                IMPORTING
*                  output = lv_tplnr.
              lv_tplnr = |{ ls_iloan-tplnr ALPHA = OUT }|.

              "MOVE lv_tplnr TO lv_object.
              lv_object = lv_tplnr.
*--update charactaerstics of the equipment , functional location
              CALL METHOD me->update_characterstics
                EXPORTING
                  lv_object   = lv_object
                  lv_classnum = lv_classnum
                  lv_klart    = lv_klart
                  im_char     = im_char.
              CLEAR: lv_tplnr.
            ENDIF.
          ENDIF.
          IF <lfs_char>-notficationeq = lc_x.
            lv_klart    = '002'.
*            SELECT SINGLE qmnum,iloan,equnr
*              FROM qmih INTO @DATA(ls_qmequi)
*              WHERE qmnum = @ls_equnr-qmnum.
*          SOC By Priyanka
            lt_fields = VALUE #( ( fieldname = 'QMNUM' )
                                 ( fieldname = 'ILOAN' )
                                 ( fieldname = 'EQUNR' )
                                ).

            lt_options = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ ls_equnr-qmnum }| & |'| ) ).

            "Call RFC to get work orders
            CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
                query_table = 'QMIH'
              TABLES
                options     = lt_options
                fields      = lt_fields
                data        = lt_data.

            LOOP AT lt_data INTO DATA(WA_Data).
              ls_qmequi-equnr = wa_Data+0(18).
              ls_qmequi-iloan = wa_Data+19(30).
              ls_qmequi-qmnum = wa_Data+30(42).
            ENDLOOP.
*EOC By Priyanka
            IF sy-subrc = 0.
              IF NOT ls_qmequi IS INITIAL.
                " MOVE ls_qmequi-equnr TO lv_equnr.
*                lv_equnr = ls_qmequi-equnr.
**
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                DESTINATION lv_rfc
*                  EXPORTING
*                    input  = lv_equnr
*                  IMPORTING
*                    output = lv_equnr.
                lv_equnr = |{ ls_qmequi-equnr ALPHA = OUT }|.



                CLEAR:  lv_object.
                lv_object = lv_equnr.
*--Update charactaerstics of the equipment , functional location
                CALL METHOD me->update_characterstics
                  EXPORTING
                    lv_object   = lv_object
                    lv_classnum = lv_classnum
                    lv_klart    = lv_klart
                    im_char     = im_char.
                CLEAR: lv_equnr,lv_tplnr.
                IF <lfs_char>-notficationfl = lc_x.
                  lv_klart = '003'."Function location
*                    SELECT SINGLE iloan, tplnr FROM iloa
*                    INTO @DATA(ls_iloa1)
*                          WHERE iloan = @ls_equnr-iloan.
                  IF sy-subrc = 0.
                    " MOVE ls_iloa1-tplnr TO lv_tplnr.
*                    lv_tplnr = ls_iloan1-tplnr.
*                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                      EXPORTING
*                        input  = lv_tplnr
*                      IMPORTING
*                        output = lv_tplnr.
                    lv_tplnr = |{ ls_iloan1-tplnr ALPHA = OUT }|.
                    "MOVE lv_tplnr TO lv_object.
                    lv_object = lv_tplnr .
*--update charactaerstics of the equipment , functional location
                    CALL METHOD me->update_characterstics
                      EXPORTING
                        lv_object   = lv_object
                        lv_classnum = lv_classnum
                        lv_klart    = lv_klart
                        im_char     = im_char.
                    CLEAR: lv_tplnr.
                  ENDIF.
                ENDIF.
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD modify.

    IF roots_to_create IS NOT INITIAL.
      LOOP AT roots_to_create INTO DATA(lst_formresponse).
        lst_response-instanceid      = lst_formresponse-InstanceID.
        lst_response-formid       = lst_formresponse-FormID.
        lst_response-version      = lst_formresponse-Version.
        "TRANSLATE lst_formresponse-ResponseData TO UPPER CASE.
        lst_response-responsedata = lst_formresponse-ResponseData.
        lst_response-wo_num       = lst_formresponse-WoNum.
        lst_response-wo_num       = |{  lst_response-wo_num ALPHA = IN }|.
        lst_response-created_on   = lst_formresponse-CreatedOn.
        "TRANSLATE lst_formresponse-created_by TO UPPER CASE.
        lst_response-created_by   = sy-uname.
        lst_response-modified_on  = lst_formresponse-ModifiedOn.
        " TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
        lst_response-modified_by  = lst_formresponse-ModifiedBy.
        lst_response-nonobjtype   = lst_formresponse-NonObjType.                  "
        lst_response-counter      = lst_formresponse-Counter.
        lst_response-isdraft      = lst_formresponse-IsDraft.
        lst_response-remarks      = lst_formresponse-Remarks.
        lst_response-order_type   = lst_formresponse-OrderType.
      ENDLOOP.

      lst_response-instanceid = lst_formresponse-instanceid.

      IF lst_response-instanceid IS INITIAL.

*        CLEAR : lv_timestamp1,lv_sys_tzone1,lv_date1,lv_time1,lv_sys_time_token1,lv_wo.
*
*        GET TIME STAMP FIELD lv_timestamp1.
*
*        /syclo/cl_core_bapi_tools=>get_system_time( IMPORTING ev_sys_tzone = lv_sys_tzone1 ).
*
*        CONVERT TIME STAMP lv_timestamp1 TIME ZONE lv_sys_tzone1 INTO DATE lv_date1 TIME lv_time1.
*
*        CONCATENATE lv_date1 lv_time1 INTO lv_sys_time_token1.

        lv_wo = lst_formresponse-WoNum.
        CALL METHOD me->get_cloud_dest
          IMPORTING
            ex_dest = DATA(lv_rfc).
        DATA(lv_top) = lr_request->get_paging(  )->get_page_size(  ).
        DATA(lv_skip) = lr_request->get_paging(  )->get_offset(  ).

*          SELECT SINGLE aufnr FROM aufk
*            WHERE aufnr = @lv_wo INTO @lv_workorder.
        lit_fields = VALUE #( ( fieldname = 'AUFNR' ) ).                                                     .
        lit_option = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lv_wo }| & |'| ) ).
        lv_rowskip = lv_skip.
        IF lv_top > 0.
          lv_rowcount = lv_top.
        ENDIF. "/ if lv_top > 0.

        lit_fields  = VALUE #( ( fieldname = 'TZONESYS' ) ).


        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'TTZCU'
          TABLES
            fields      = lit_fields
            data        = lit_data.
        LOOP AT lit_data INTO DATA(lst_data).
          lv_sys_tzone1 = lst_Data+0(6).
        ENDLOOP.


        CONVERT TIME STAMP lv_timestamp1 TIME ZONE lv_sys_tzone1
        INTO DATE lv_date1 TIME lv_time1.
        CONCATENATE lv_date1 lv_time1 INTO lv_sys_time_token1.

        "/Call RFC to get workorder
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc
          EXPORTING
            query_table = 'AUFK'
            rowcount    = lv_rowcount
            rowskips    = lv_rowskip
          TABLES
            data        = lit_data
            fields      = lit_fields
            options     = lit_option.

        LOOP AT lit_data INTO lst_data.
          lv_wonum = lst_data+0(12).
        ENDLOOP. "/LOOP AT lit_data INTO DATA(lst_data).


        IF sy-subrc = 0.
          lv_workorder = |{ lv_wonum  ALPHA = OUT }|.
        ELSE.                                                                  " IF SY-SUBRC = 0
          IF lv_wonum IS NOT INITIAL.
            lv_workorder = |{ lv_wonum  ALPHA = IN }|.

          ENDIF.                                                               " IF lv_wo IS NOT INITIAL
        ENDIF.

        lv_workorder = |{ lst_formresponse-WoNum  ALPHA = OUT }|.

        CONCATENATE 'SAP' lv_wo lv_sys_time_token1 INTO lst_formresponse-instanceid SEPARATED BY '_'.
        CONCATENATE 'SAP' lv_wo lv_sys_time_token1 INTO lst_response-instanceid SEPARATED BY '_'.
      ENDIF.                                                                   " IF SY-SUBRC = 0


      CREATE OBJECT lo_exchtab.
      SELECT SINGLE param_value
        FROM /odsmfe/tb_apcon
        WHERE param_name = @lc_pfcg_role
        AND   active     = @lc_x  INTO @lv_pfcg_role.
* ----------------------------------------------------------------------*
* --- Get the user's role from the assigned PFCG role-------------------*
* ----------------------------------------------------------------------*
      IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

        lv_user = sy-uname .

        SELECT SINGLE param_value
          FROM /odsmfe/tb_apcon
          WHERE param_name = @lc_meth
          AND   active     = @lc_x INTO @lv_class.

        IF sy-subrc EQ 0.
          CREATE OBJECT lo_auth TYPE (lv_class).
        ENDIF.                                                                 " IF SY-SUBRC EQ 0

        TRY.
            CALL METHOD lo_auth->(lc_meth)
              EXPORTING
                iv_uname = lv_user
              IMPORTING
                ev_field = lv_role.
            IF lv_role IS NOT INITIAL.
              lst_response-roleid = lv_role.
            ENDIF.                                                             " IF LV_ROLE IS NOT INITIAL
          CATCH  cx_rfc_dest_provider_error INTO DATA(lx_dest).
        ENDTRY.

      ELSE.                                                                    " IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

* ----------------------------------------------------------------------*
* --- Get reference for fetching value of user role table---------------*
* ----------------------------------------------------------------------*

        lv_usrroletab = lo_exchtab->get_userrole_tab( ).

        IF lo_exchtab IS BOUND.
          SELECT SINGLE roleid
            FROM (lv_usrroletab-low)
            WHERE userid    = @sy-uname
            AND   startdate LE @sy-datum
            AND   enddate   GE @sy-datum INTO @lv_roleid.

          IF sy-subrc = 0.
            lst_response-roleid = lv_roleid.
          ENDIF.                                                               " IF SY-SUBRC = 0
        ENDIF.                                                                 " IF lo_exchtab IS BOUND
      ENDIF.                                                                   " IF LV_PFCG_ROLE EQ LC_TRUE

      IF lst_formresponse-CreatedOn IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Converting timestamp into date and time---------------------------*
* ----------------------------------------------------------------------*
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*          DESTINATION lv_rfc
*          EXPORTING
*            iv_timestamp     = lst_formresponse-CreatedOn
*          IMPORTING
*            o_date           = lv_cdate
*            o_time           = lv_ctime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
        cl_abap_tstmp=>systemtstmp_utc2syst(
       EXPORTING
         utc_tstmp = lst_formresponse-CreatedOn
       IMPORTING
         syst_date = lv_cdate
         syst_time = lv_ctime
     ).
        IF sy-subrc = 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
          lv_form_date              = lv_cdate.
          lv_form_time              = lv_ctime.
        ENDIF.                                                                 " IF SY-SUBRC = 0

      ENDIF.                                                                   " IF LST_FORMRESPONSE-CREATED_ON IS NOT INITIAL

      IF lst_formresponse-ModifiedOn IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Converting timestamp into date and time---------------------------*
* ----------------------------------------------------------------------*
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*        DESTINATION lv_rfc
*          EXPORTING
*            iv_timestamp     = lst_formresponse-ModifiedOn
*          IMPORTING
*            o_date           = lv_mdate
*            o_time           = lv_mtime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
        cl_abap_tstmp=>systemtstmp_utc2syst(
     EXPORTING
       utc_tstmp = lst_formresponse-ModifiedOn
     IMPORTING
       syst_date = lv_cdate
       syst_time = lv_ctime
   ).
        IF sy-subrc = 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
          lv_form_date               = lv_mdate.
          lv_form_time               = lv_mtime.
        ENDIF.                                                                 " IF SY-SUBRC = 0
      ENDIF.                                                                   " IF LST_FORMRESPONSE-MODIFIED_ON IS NOT INITIAL

* ----------------------------------------------------------------------*
* --- Insert the Submitted response data into the Form response table---*
* ----------------------------------------------------------------------*
      IF lst_formresponse-instanceid IS NOT INITIAL.
        SELECT SINGLE instanceid
          FROM /odsmfe/tb_forsp
          WHERE instanceid = @lst_formresponse-instanceid INTO @lv_instanceid.

        IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.
          INSERT /odsmfe/tb_forsp FROM @lst_response.
          IF sy-subrc <> 0.
            CLEAR lst_response.
          ENDIF.                                                               " IF SY-SUBRC <> 0
        ELSE.                                                                  " IF SY-SUBRC NE 0 AND LV_INSTANCEID IS INITIAL
          UPDATE /odsmfe/tb_forsp FROM @lst_response.
          IF sy-subrc <> 0.
            CLEAR lst_response.
          ENDIF.                                                               " IF SY-SUBRC <> 0
        ENDIF.                                                                 " IF SY-SUBRC NE 0 AND LV_INSTANCEID IS INITIAL
        IF lst_response IS NOT INITIAL AND lst_response-isdraft IS INITIAL.

          lv_xml_string = lst_response-responsedata.

*          CREATE OBJECT lo_xml.

          "/ Convert XString to Binary
*          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*          DESTINATION lv_rfc
*            EXPORTING
*              buffer     = lv_xml_string
*            TABLES
*              binary_tab = lit_data.
          CREATE OBJECT lr_base64_decode.
          "/Convert SCMS Base64 Decode
          CALL METHOD lr_base64_decode->decode_x_base64
            EXPORTING
              encoded = lv_line
            RECEIVING
              decoded = lv_file_content_bin.

          "/Added code for Xstring to Binary conversion. Need to find alternative way of doing this.
          DATA: b(1)      TYPE n,
                lv_binary TYPE string.

          DO.
            GET BIT sy-index OF lv_file_content_bin INTO b.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            CONCATENATE lv_binary b INTO lv_binary.
            CONDENSE lv_binary NO-GAPS.
            lst_file_content_binary-line = lv_binary.
            APPEND lst_file_content_binary TO lit_file_content_binary.
            CLEAR: lst_file_content_binary.
          ENDDO.
*                 "/End of code for conversion

          IF lit_file_content_binary[] IS INITIAL.
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                message = 'Converting Xstring to Binary data Failed'.
          ENDIF. "/ IF lit_data[] IS INITIAL.

          "/ Parse data
*          CALL METHOD lo_xml->create_with_table
*            EXPORTING
*              table   = lit_data
*            RECEIVING
*              retcode = lit_retcode.

          "/ Render_2_xstring
*          CALL METHOD lo_xml->render_2_xstring
*            IMPORTING
*              retcode = lv_subrc
*              stream  = lv_xml_string
*              size    = lv_size.
          lv_xml_string = lv_binary.

          IF lv_binary IS INITIAL.
*            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*              EXPORTING
*                textid  = /iwbep/cx_mgw_busi_exception=>business_error
*                message = 'Render to XString Failed'.
          ENDIF. "/ IF lv_xml_string IS INITIAL.

          "/ Parsing the Form Data from Hexa decimal to table

          "/ Need to find alternative
*          CALL FUNCTION 'SMUM_XML_PARSE'
*            DESTINATION lv_rfc
*            EXPORTING
*              xml_input = lv_xml_string
*            TABLES
*              xml_table = lit_fomrsp
*              return    = lit_return.

          READ TABLE lit_fomrsp INTO lst_fomrsp WITH KEY cname = 'post_processing' type = 'V'.
          IF sy-subrc = 0.
            " MOVE-CORRESPONDING lst_response TO lst_inst_data.
* ----------------------------------------------------------------------*
* --- Method to Call BADI for Form Instance Post Processing-------------*
* ----------------------------------------------------------------------*
            CALL METHOD me->gmib_form_inst_post_process
              EXPORTING
                im_form_response_data = lst_inst_data
                im_xml_table_data     = lit_fomrsp
              IMPORTING
                ex_return             = lit_return.

            IF lit_return[] IS NOT INITIAL.
              CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_error_handling
                EXPORTING
                  im_return = lit_return.
            ENDIF.
          ENDIF.
        ENDIF.
* ----------------------------------------------------------------------*
* --- E: MOD-006 |01.08.2023| VSANAGALA |   ES1K903585  |/ODSMFE/MAIN---*
* ----------------------------------------------------------------------*
*--------------------------------------------- Form Instance Post Processing -------------------------------------------------*

* ----------------------------------------------------------------------*
* --- Updating the Workorder/Notification exchange table----------------*
* --- S: MOD-005|24.02.2023| VSANAGALA | ES1K903619      |/ODSMFE/MAIN--*
* ----------------------------------------------------------------------*
*        SELECT SINGLE aufnr
*          FROM aufk
*          INTO @lv_workorder
*          WHERE aufnr = @lst_response-wo_num.
        CALL METHOD me->get_cloud_dest
          IMPORTING
            ex_dest = DATA(lv_rfc1).
        lit_fields = VALUE #( ( fieldname = 'AUFNR' ) ).                                                     .
        lit_option = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_response-wo_num }| & |'| ) ).
        lv_rowskip = lv_skip.
        IF lv_top > 0.
          lv_rowcount = lv_top.
        ENDIF. "/ if lv_top > 0.

        "/Call RFC to get workorder
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc1
          EXPORTING
            query_table = 'AUFK'
            rowcount    = lv_rowcount
            rowskips    = lv_rowskip
          TABLES
            data        = lit_data
            fields      = lit_fields
            options     = lit_option.

        LOOP AT lit_data INTO lst_data.
          lv_workorder = lst_data+0(12).
        ENDLOOP. "/LOOP AT lit_data INTO DATA(lst_data).

        IF sy-subrc = 0 AND lv_workorder IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- E: MOD-005|24.02.2023| VSANAGALA | ES1K903619      |/ODSMFE/MAIN--*                        " MOD
* ----------------------------------------------------------------------*

          IF lo_exchtab IS BOUND.
            lo_exchtab->exch_table_update( lv_workorder ).
          ENDIF.                                                               " IF lo_exchtab IS BOUND

* ----------------------------------------------------------------------*
* --- S: MOD-005|24.02.2023| VSANAGALA | ES1K903619      |/ODSMFE/MAIN--*
* ----------------------------------------------------------------------*
        ELSE.                                                                  " IF SY-SUBRC = 0 AND LV_WORKORDER IS NOT INITIAL
*          SELECT SINGLE qmnum
*            INTO lv_qmnum
*            FROM qmel
*            WHERE qmnum = lst_response-wo_num.
          lit_fields = VALUE #( ( fieldname = 'QMART' )
                           ).

          lit_option = VALUE #( ( text = |QMNUM| & | | & |EQ| & | | & |'| & |{ lst_response-wo_num }| & |'| ) ).

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'QMEL'
            TABLES
              options     = lit_option
              fields      = lit_fields
              data        = lit_data.
          LOOP AT lit_data INTO DATA(ls_data).
            lv_qmart = ls_data-wa.
          ENDLOOP.

          IF sy-subrc = 0 AND lv_qmnum IS NOT INITIAL.

            IF lo_exchtab IS BOUND.
              lo_exchtab->exch_table_update( lv_qmnum )."exch_table_update_notification( lv_qmnum ).
            ENDIF.                                                            " IF lo_exchtab IS BOUND
          ENDIF.                                                               " IF sy-subrc = 0 AND lv_qmnum IS NOT INITIAL.
        ENDIF.                                                                 " IF sy-subrc = 0 AND lst_response-wo_num IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- E: MOD-005|24.02.2023| VSANAGALA | ES1K903619      |/ODSMFE/MAIN--*                        " MOD
* ----------------------------------------------------------------------*
      ENDIF.                                                                   " IF SY-SUBRC = 0 AND LV_QMNUM IS NOT INITIAL

      IF lst_formresponse-nonobjtype IS INITIAL.

        SELECT SINGLE low
          FROM /odsmfe/tb_filtr
          WHERE entitysetname = 'ResponseCaptureSet'
          AND field = 'POSTNOTIFICATION' INTO @lv_postnotification.

        IF lv_workorder IS NOT INITIAL.
          lv_aufnr = lst_response-wo_num.

* ----------------------------------------------------------------------*
* --- Get the Notification number from work order number----------------*
* ----------------------------------------------------------------------*
*          SELECT SINGLE qmnum
*            FROM afih
*            WHERE aufnr = @lv_aufnr INTO @lv_qmnum.
*        ENDIF.

          lit_fields = VALUE #( ( fieldname = 'QMNUM' )
                          ).

          IF lit_NoNum IS NOT INITIAL.
            lit_option = VALUE #( ( text = |QMNUM| & | | & |{ lit_NoNum[ 1 ]-option }| & | | & |'| & |{ lit_NoNum[ 1 ]-low }| & |'|  ) ).

          ENDIF.

          "Call RFC to get work orders
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc1
            EXPORTING
              query_table = 'AFIH'
            TABLES
              options     = lit_option
              fields      = lit_fields
              data        = lit_data.
          LOOP AT lit_data INTO DATA(ls_data1).
            lv_qmnum = ls_data1+0(12).
          ENDLOOP.
          DATA(lv_qmnum1) = lv_qmnum.                                                               " IF LV_WORKORDER IS NOT INITIAL

* ----------------------------------------------------------------------*
* --- Update Work Order Notification------------------------------------*
* ----------------------------------------------------------------------*
          IF lv_postnotification = abap_true AND lv_qmnum IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- S: MOD-002|14.12.2020| ODS       | ES1K902363      |/ODSMFE/MAIN--*
* ----------------------------------------------------------------------*
            SELECT SINGLE postnotification
              FROM /odsmfe/tb_foass
              WHERE formid  EQ @lst_formresponse-formid
              AND   version EQ @lst_formresponse-version INTO @lv_postnotif.

            IF lv_postnotif IS INITIAL.
              CLEAR:lv_postnotif.
            ENDIF.                                                               " IF LV_POSTNOTIF IS INITIAL
* ----------------------------------------------------------------------*
* --- E: MOD-002|14.12.2020| ODS       | ES1K902363      |/ODSMFE/MAIN--*                        " MOD
* ----------------------------------------------------------------------*

            IF lv_postnotif IS NOT INITIAL.
              IF lst_response-isdraft IS INITIAL.

                CALL METHOD me->gmib_parse_responsedata
                  EXPORTING
                    im_responsedata = lst_response-responsedata
                    im_wonum        = lv_workorder
                    im_qmnum        = lv_qmnum
                    im_formid       = lst_response-formid
                    im_version      = lst_response-version
                  IMPORTING
                    ex_return       = lIt_return.
* ----------------------------------------------------------------------*
* --- Error Handling----------------------------------------------------*
* ----------------------------------------------------------------------*
                IF lit_return IS NOT INITIAL.
                  DELETE lit_return WHERE type NE lc_e OR type = lc_a.
                  IF lit_return IS NOT INITIAL.
*                  lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*                  lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return
*                    iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                    iv_add_to_response_header = abap_true ).
*
*                  RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                    EXPORTING
*                      message_container = lo_error.
                  ENDIF.                                                         " IF LIT_RETURN IS NOT INITIAL
                ENDIF.                                                           " IF LIT_RETURN IS NOT INITIAL

              ENDIF.                                                             " IF lst_response-isdraft IS INITIAL.
            ENDIF.                                                               " IF LV_POSTNOTIF IS INITIAL
          ENDIF.                                                                 " IF LV_POSTNOTIFICATION = ABAP_TRUE AND LV_QMNUM IS NOT INITIAL
        ENDIF.                                                                   " IF LST_FORMRESPONSE-NONOBJTYPE IS INITIAL

* ----------------------------------------------------------------------*
* --- Send the instance ID to Front end application---------------------*
* ----------------------------------------------------------------------*
*      gstib_entity-instanceid = lst_response-instanceid.
*      GET REFERENCE OF gstib_entity INTO ex_entity.

        IF lv_workorder IS NOT INITIAL.                                          "Added by VSANAGALA - MOD-005
* ----------------------------------------------------------------------*
* --- S: MOD-001|22.09.2020| ODS       | ES1K902140      |/ODSMFE/MAIN--*
* --- Update Equipment Characterstics-----------------------------------*
* --- Update the characterstics of Equipment based on the table content-*
* ----------------------------------------------------------------------*
          SELECT SINGLE postcharacteristics FROM /odsmfe/tb_foass
           WHERE formid EQ @lst_formresponse-formid
             AND version EQ @lst_formresponse-version INTO @lv_postchar.

          IF sy-subrc = 0 AND lv_postchar IS NOT INITIAL.

            SELECT * FROM /odsmfe/tb_focha
              WHERE formid  EQ @lst_formresponse-formid
                AND version EQ @lst_formresponse-version INTO TABLE @lit_focha.

            IF sy-subrc = 0.
              IF NOT lit_focha IS INITIAL.

                CLEAR: lit_return.
                TRY.
* ----------------------------------------------------------------------*
* --- Convert the repsonse data to internal table data------------------*
* ----------------------------------------------------------------------*
*                  CALL METHOD me->get_xml_content
*                    EXPORTING
*                      im_responsedata = lst_response-responsedata
*                    IMPORTING
*                      ex_xml_data     = lst_xml_data
*                      ex_return       = lst_return.

                    IF lit_return IS NOT INITIAL.
                      DELETE lit_return WHERE type NE lc_e OR type = lc_a.
                      IF lit_return IS NOT INITIAL.
*                      lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*                      lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return
*                        iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                        iv_add_to_response_header = abap_true ).
*
*                      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                        EXPORTING
*                          message_container = lo_error.
                      ENDIF.                                                     " IF lit_return IS NOT INITIAL
                    ENDIF.                                                       " IF lit_return IS NOT INITIAL
*
*                CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg.                " Business Exception
*                  CLEAR lv_msg.
*                  lo_msg->get_text( RECEIVING result = lv_msg ).
*                  lo_msg->get_longtext( RECEIVING result = lv_msg ).
**                  MESSAGE lv_msg TYPE 'I'.
*
*                CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1.               " Technical Exception
*                  CLEAR lv_msg1.
*                  lo_msg1->get_text( RECEIVING result = lv_msg1 ).
*                  lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
**                  MESSAGE lv_msg1 TYPE 'I'.
                ENDTRY.

* ----------------------------------------------------------------------*
* --- Prepare internal table to send to new class for equipment char update*
* ----------------------------------------------------------------------*
              LOOP AT lit_xml_data INTO lst_xml_data.
                MOVE-CORRESPONDING  lst_xml_data TO lst_char.
               " MOVE-CORRESPONDING lst_xml_data-cvalue TO lst_char-value.
                TRANSLATE lst_xml_data-cname TO UPPER CASE.
                LOOP AT lit_focha INTO lst_focha WHERE field = lst_xml_data-cname.
                   lst_char-value = lst_xml_data-cvalue.
                  MOVE-CORRESPONDING lst_focha TO lst_char.
                  APPEND lst_char TO lit_char.
                  CLEAR : lst_focha, lst_char, lst_xml_data.
                ENDLOOP.
                endloop.                                                     " LOOP AT lit_focha

                " LOOP AT lit_xml_data

* ----------------------------------------------------------------------*
* --- Method to update characterstics-----------------------------------*
* ----------------------------------------------------------------------*
                CALL METHOD me->equipment_char_update
                  EXPORTING
                    im_workorder = lst_response-wo_num
                    im_char      = lit_char
                  IMPORTING
                    ex_return    = lst_return.
                CLEAR: lst_return, lit_char.
              ENDIF.
            ENDIF.                                                            " IF NOT lit_focha IS INITIAL
          ENDIF.                                                           " IF SY-SUBRC = 0
        ENDIF.                                                         " IF SY-SUBRC = 0 AND LV_POSTCHAR IS NOT INITIAL
      ENDIF.                                                            " IF LV_WORKORDER IS NOT INITIAL
    ENDIF.                                                                 " IF lv_workorder IS NOT INITIAL.


    IF roots_to_update IS NOT INITIAL.
      LOOP AT roots_to_update INTO DATA(lst_to_update).
        lst_response-instanceid = lst_to_update-InstanceID.
        lst_response-formid     = lst_to_update-FormID.
        lst_response-version    = lst_to_update-Version.
        "TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
        lst_response-responsedata = lst_to_update-ResponseData.
        lst_response-wo_num       = lst_to_update-WoNum.
* ----------------------------------------------------------------------*
* --- S: MOD-002 |14/12/2020| ODS |ES1K902363| /ODSMFE/MAIN ------------*
* ----------------------------------------------------------------------*
        lst_response-plnty = lst_to_update-TaskListType.                      "
        lst_response-plnal = lst_to_update-GroupCounter.
        lst_response-zaehl = lst_to_update-InternalCounter.
* ----------------------------------------------------------------------*
* --- E: MOD-002 |14/12/2020| ODS |ES1K902363| /ODSMFE/MAIN ------------*      " MOD
* ----------------------------------------------------------------------*
        " lst_response-equnr      = lst_formresponse-e.
        "lst_response-tplnr      = lst_to_update-.
        lst_response-created_on = lst_to_update-CreatedOn.
        "TRANSLATE lst_formresponse-created_by TO UPPER CASE.
        lst_response-created_by  = sy-uname.
        lst_response-modified_on = lst_formresponse-ModifiedOn.
        "TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
        lst_response-modified_by = sy-uname.
        lst_response-nonobjtype  = lst_to_update-NonObjType.                  "
        lst_response-counter     = lst_to_update-Counter.
        lst_response-isdraft     = lst_to_update-IsDraft.
        lst_response-remarks     = lst_to_update-Remarks.
        lst_response-order_type  = lst_to_update-OrderType.                       "
      ENDLOOP.
      CREATE OBJECT lo_exchtab.

* ----------------------------------------------------------------------*
* --- S: MOD-004 |27/01/2023| PPRIYANKA |ES1K903522|/ODSMFE/MAIN--------*
* ----------------------------------------------------------------------*
* --- Get the User's Role based on the assigned PFCG roles--------------*
* ----------------------------------------------------------------------*
      SELECT SINGLE param_value
         FROM /odsmfe/tb_apcon
         WHERE param_name = @lc_pfcg_role
         AND active = @lc_x INTO @lv_pfcg_role.

      IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

        lv_user = sy-uname .

        SELECT SINGLE param_value
            FROM /odsmfe/tb_apcon
            WHERE param_name = @lc_meth
            AND active = @lc_x INTO @lv_class.

        IF sy-subrc EQ 0.
          CREATE OBJECT lo_auth TYPE (lv_class).
        ENDIF.                                                                 " IF SY-SUBRC EQ 0

        TRY.
            CALL METHOD lo_auth->(lc_meth)
              EXPORTING
                iv_uname = lv_user
              IMPORTING
                ev_field = lv_role.
            IF lv_role IS NOT INITIAL.
              lst_response-roleid = lv_role.
            ENDIF.                                                             " IF LV_ROLE IS NOT INITIAL
*          CATCH /iwbep/cx_mgw_busi_exception.
        ENDTRY.
* ----------------------------------------------------------------------*
* --- E: MOD-004 |27/01/2023| PPRIYANKA |ES1K903522|/ODSMFE/MAIN--------*      " MOD
* ----------------------------------------------------------------------*
      ELSE.                                                                    " IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

        IF lo_exchtab IS BOUND.
          lv_usrroletab = lo_exchtab->get_userrole_tab( ).
* ----------------------------------------------------------------------*
* --- Get Role ID from the ODS roles table -----------------------------*
* ----------------------------------------------------------------------*
          SELECT SINGLE roleid FROM (lv_usrroletab-low)
            WHERE userid   EQ @sy-uname
             AND startdate LE @sy-datum
             AND enddate   GE @sy-datum INTO @lv_roleid.

          IF sy-subrc = 0.
            lst_response-roleid = lv_roleid.
          ENDIF.                                                               " IF SY-SUBRC = 0

        ENDIF.                                                                 " IF lo_exchtab IS BOUND
      ENDIF.                                                                   " IF sy-subrc = 0 AND lv_pfcg_role EQ lc_true.

      IF lst_to_update-CreatedOn IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Converting timestamp into date and time---------------------------*
* ----------------------------------------------------------------------*
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*        DESTINATION lv_rfc
*          EXPORTING
*            iv_timestamp     = lst_to_update-CreatedOn
*          IMPORTING
*            o_date           = lv_cdate
*            o_time           = lv_ctime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.

        cl_abap_tstmp=>systemtstmp_utc2syst(
EXPORTING
utc_tstmp = lst_to_update-CreatedOn
IMPORTING
syst_date = lv_cdate
syst_time = lv_ctime
).
        IF sy-subrc = 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
          lv_form_date              = lv_cdate.
          lv_form_time              = lv_ctime.
        ENDIF.                                                                 " IF SY-SUBRC = 0
      ENDIF.                                                                   " IF LST_FORMRESPONSE-CREATED_ON IS NOT INITIAL

      IF lst_to_update-ModifiedOn IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Converting timestamp into date and time---------------------------*
* ----------------------------------------------------------------------*
        cl_abap_tstmp=>systemtstmp_utc2syst(
      EXPORTING
        utc_tstmp = lst_to_update-CreatedOn
      IMPORTING
        syst_date = lv_cdate
        syst_time = lv_ctime
    ).
        IF sy-subrc = 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
          lv_form_date               = lv_mdate.
          lv_form_time               = lv_mtime.
        ENDIF.                                                                 " IF SY-SUBRC = 0

      ENDIF.                                                                   " IF LST_FORMRESPONSE-MODIFIED_ON IS NOT INITIAL

* ----------------------------------------------------------------------*
* --- Insert or update logic for Isdraft functionality------------------*
* ----------------------------------------------------------------------*
      SELECT SINGLE instanceid
        FROM /odsmfe/tb_forsp
        WHERE instanceid = @lst_response-instanceid  INTO @lv_instanceid.

      IF sy-subrc = 0 AND lv_instanceid IS NOT INITIAL.
        MODIFY /odsmfe/tb_forsp FROM @lst_response.
        IF sy-subrc <> 0.
          CHECK sy-subrc NE 0.
        ENDIF.                                                                 " IF SY-SUBRC <> 0

* ----------------------------------------------------------------------*
* --- S: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*
* ----------------------------------------------------------------------*
*        SELECT SINGLE aufnr
*          FROM aufk
*          INTO lv_workorder
*          WHERE aufnr = lst_response-wo_num.
               CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc2).
        lit_fields = VALUE #( ( fieldname = 'AUFNR' ) ).
        lit_option = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_response-wo_num }| & |'| ) ).
*                     lv_rowskip = lv_skip.
*                     if lv_top > 0.
*                        lv_rowcount = lv_top.
*                     endif. "/ if lv_top > 0.

        "/Call RFC to get workorder
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc2
          EXPORTING
            query_table = 'AUFK'
            rowcount    = lv_rowcount
            rowskips    = lv_rowskip
          TABLES
            data        = lit_data
            fields      = lit_fields
            options     = lit_option.

        LOOP AT lit_data INTO lst_data.
          lv_workorder = lst_data+0(12).
        ENDLOOP. "/LOOP AT lit_data INTO DATA(lst_data).
        CLEAR:  lv_rowskip, lv_rowcount, lst_data, lit_data.


        IF sy-subrc = 0 AND lv_workorder IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- E: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*      " MOD
* ----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- updating work order exchange table--------------------------------*
* ----------------------------------------------------------------------*
          IF lo_exchtab IS BOUND.
            lo_exchtab->exch_table_update( lv_workorder ).
          ENDIF.                                                               " IF lo_exchtab IS BOUND

* ----------------------------------------------------------------------*
* --- S: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*
* ----------------------------------------------------------------------*
        ELSE.                                                                  " IF SY-SUBRC = 0 AND LV_WORKORDER IS NOT INITIAL
*          SELECT SINGLE qmnum
*            FROM qmel
*            WHERE qmnum = @lst_response-wo_num INTO @lv_qmnum.
*

          lit_fields  = VALUE #( ( fieldname = 'QMNUM' ) ).
          lit_option = VALUE #( ( text = |QMNUM| & | | & |EQ| & |'| & |{ lst_response-wo_num }| & |'| ) ).
*                         lv_rowskip = lv_skip.
*                         if lv_top > 0.
*                           lv_rowcount = lv_top.
*                        endif. "/ if lv_top > 0.

          "/Call RFC to get Notification
          CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc2
            EXPORTING
              query_table = 'QMEL'
              rowcount    = lv_rowcount
              rowskips    = lv_rowskip
            TABLES
              data        = lit_data
              fields      = lit_fields
              options     = lit_option.

          LOOP AT lit_data INTO lst_data.
            lv_qmnum = lst_data+0(12).
          ENDLOOP. "/LOOP AT lit_data INTO lst_data.
          CLEAR: lv_rowcount, lv_rowskip, lst_data, lit_data.


          IF sy-subrc = 0 AND lv_qmnum IS NOT INITIAL.

            IF lo_exchtab IS BOUND.
              lo_exchtab->exch_table_update( lv_qmnum ).
            ENDIF.                                                             "IF lo_exchtab IS BOUND
          ENDIF.                                                               "IF sy-subrc = 0 AND lv_qmnum IS NOT INITIAL.
        ENDIF.                                                                 "IF sy-subrc = 0 AND lst_response-wo_num IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- E: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*      " MOD
* ----------------------------------------------------------------------*
      ENDIF.                                                                   " IF SY-SUBRC = 0 AND LV_QMNUM IS NOT INITIAL
    ENDIF.                                                                     " IF SY-SUBRC = 0 AND LV_WORKORDER IS NOT INITIAL

*--------------------------------------------- Form Instance Post Processing -------------------------------------------------*
    IF lst_response IS NOT INITIAL AND lst_response-isdraft IS INITIAL.
      MOVE-CORRESPONDING lst_response TO lst_inst_data.
* ----------------------------------------------------------------------*
* --- S: MOD-006 |01.08.2023| VSANAGALA |   ES1K903585  |/ODSMFE/MAIN---*
* --- Method to Call BADI for Form Instance Post Processing-------------*
* ----------------------------------------------------------------------*
      CALL METHOD me->gmib_form_inst_post_process
        EXPORTING
          im_form_response_data = lst_inst_data
        IMPORTING
          ex_return             = lit_return.

      IF lit_return[] IS NOT INITIAL.
        CALL METHOD me->/odsmfe/if_get_entityset_bapi~gmib_error_handling
          EXPORTING
            im_return = lit_return.
      ENDIF.
    ENDIF.
* ----------------------------------------------------------------------*
* --- E: MOD-006 |01.08.2023| VSANAGALA |   ES1K903585  |/ODSMFE/MAIN---*
* ----------------------------------------------------------------------*
*--------------------------------------------- Form Instance Post Processing -------------------------------------------------*

    IF lst_formresponse-nonobjtype IS INITIAL.

      IF lv_workorder IS NOT INITIAL.
        lv_aufnr = lst_response-wo_num.
* ----------------------------------------------------------------------*
* --- Get the Notification number from work order number----------------*
* ----------------------------------------------------------------------*
*        SELECT SINGLE qmnum
*          INTO lv_qmnum
*          FROM afih
*          WHERE aufnr = lv_aufnr.
               CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc3).
        lit_fields  = VALUE #( ( fieldname = 'QMNUM' ) ).
        lit_option = VALUE #( ( text = |QMNUM| & | | & |EQ| & |'| & |{ lv_aufnr }| & |'| ) ).
*                         lv_rowskip = lv_skip.
*                         if lv_top > 0.
*                           lv_rowcount = lv_top.
*                        endif. "/ if lv_top > 0.

        "/Call RFC to get Notification
        CALL FUNCTION 'RFC_READ_TABLE'
          DESTINATION lv_rfc3
          EXPORTING
            query_table = 'AFIH'
            rowcount    = lv_rowcount
            rowskips    = lv_rowskip
          TABLES
            data        = lit_data
            fields      = lit_fields
            options     = lit_option.

        LOOP AT lit_data INTO lst_data.
          lv_qmnum = lst_data+0(12).
        ENDLOOP. "/LOOP AT lit_data INTO lst_data.
        CLEAR: lv_rowcount, lv_rowskip, lst_data, lit_data.
      ENDIF.                                                                   " IF LV_WORKORDER IS NOT INITIAL

      SELECT SINGLE low
        FROM /odsmfe/tb_filtr
        WHERE entitysetname = @lc_responsecaptureset
        AND   field = @lc_postnotification INTO @lv_postnotification.

* ----------------------------------------------------------------------*
* --- Update Work Order Notification------------------------------------*
* ----------------------------------------------------------------------*
      IF lv_postnotification = abap_true AND lv_qmnum IS NOT INITIAL.

        SELECT SINGLE postnotification
            FROM /odsmfe/tb_foass
            WHERE formid  EQ @lst_formresponse-formid
            AND   version EQ @lst_formresponse-version  INTO @lv_postnotif.

        IF lv_postnotif IS INITIAL.
          CLEAR:lv_postnotif.
        ENDIF.                                                                 " IF LV_POSTNOTIF IS INITIAL Line No. :365

        IF lv_postnotif IS NOT INITIAL.
          IF lst_response-isdraft IS INITIAL.
            CALL METHOD me->gmib_parse_responsedata
              EXPORTING
                im_responsedata = lst_response-responsedata
                im_wonum        = lv_workorder
                im_qmnum        = lv_qmnum
                im_formid       = lst_response-formid
                im_version      = lst_response-version
              IMPORTING
                ex_return       = lit_return.
* ----------------------------------------------------------------------*
* --- Error Handling----------------------------------------------------*
* ----------------------------------------------------------------------*
            IF lit_return[] IS NOT INITIAL.
              DELETE lit_return WHERE type NE lc_e OR type = lc_a.
              IF lit_return[] IS NOT INITIAL.
*                lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

*                lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return
*                  iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                  iv_add_to_response_header = abap_true ).
*
*                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                  EXPORTING
*                    message_container = lo_error.
              ENDIF.                                                           " IF lit_return IS NOT INITIAL
            ENDIF.                                                             " IF LIT_RETURN IS NOT INITIAL
          ENDIF.                                                               " IF LST_RESPONSE-ISDRAFT IS INITIAL
        ENDIF.                                                                 " IF lv_postnotif IS NOT INITIAL.
      ENDIF.                                                                   " IF LV_POSTNOTIFICATION = ABAP_TRUE AND LV_QMNUM IS NOT INITIAL
    ENDIF.                                                                     " IF LST_FORMRESPONSE-NONOBJTYPE IS INITIAL

* ----------------------------------------------------------------------*
* --- S: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*
* ----------------------------------------------------------------------*
    IF lv_workorder IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- E: MOD-005|24.02.2023|ODS-VSANAGALA|ES1K903619|/ODSMFE/MAIN-------*      " MOD
* ----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- update equipment characterstics-----------------------------------*
* --- Update the characterstics of Equipment based on the table content-*
* ----------------------------------------------------------------------*

      SELECT SINGLE postcharacteristics
        FROM /odsmfe/tb_foass
        WHERE formid  EQ @lst_formresponse-formid
          AND version EQ @lst_formresponse-version INTO @lv_postchar.

      IF sy-subrc = 0 AND lv_postchar IS NOT INITIAL.

        SELECT *
          FROM /odsmfe/tb_focha
          WHERE formid  EQ @lst_formresponse-formid
            AND version EQ @lst_formresponse-version INTO TABLE @lit_focha.

        IF sy-subrc = 0.
          IF NOT lit_focha IS INITIAL.
* ----------------------------------------------------------------------*
* --- Convert the repsonse data to internal table data------------------*
* ----------------------------------------------------------------------*
            CLEAR: lit_return.
            TRY.
                CALL METHOD me->get_xml_content
                  EXPORTING
                    im_responsedata = lst_response-responsedata
                  IMPORTING
                    ex_xml_data     = lit_xml_data
                    ex_return       = lit_return.
                IF lit_return IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Catch exceptions--------------------------------------------------*
* ----------------------------------------------------------------------*
                  DELETE lit_return WHERE type NE lc_e OR type = lc_a.
                  IF lit_return IS NOT INITIAL.
*                    lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
*
*                    lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return
*                      iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
*                      iv_add_to_response_header = abap_true ).

*                    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
**                      EXPORTING
**                        message_container = lo_error.
                  ENDIF.                                                       " IF lit_return IS NOT INITIAL
                ENDIF.                                                         " IF lit_return IS NOT INITIAL


*              CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg.                  " Business Exception
                CLEAR lv_msg.
                lo_msg->get_text( RECEIVING result = lv_msg ).
                lo_msg->get_longtext( RECEIVING result = lv_msg ).
*                MESSAGE lv_msg TYPE 'I'.

*              CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1.                 " Technical Exception
                CLEAR lv_msg1.
                lo_msg1->get_text( RECEIVING result = lv_msg1 ).
                lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
*                MESSAGE lv_msg1 TYPE 'I'.
            ENDTRY.
* ----------------------------------------------------------------------*
* --- Prepare internal table to send to new class for equipment char update*
* ----------------------------------------------------------------------*
*            LOOP AT lit_xml_data INTO lst_xml_data.
*              MOVe-CORRESPONDING lst_data- TO lst_char-name.
*              MOVE lst_xml_data-cvalue TO lst_char-value.
*              TRANSLATE lst_xml_data-cname TO UPPER CASE.
*              LOOP AT lit_focha INTO lst_focha WHERE field = lst_xml_data-cname.
*                MOVE lst_xml_data-cvalue  TO lst_char-value.
*                MOVE-CORRESPONDING lst_focha TO lst_char.
*                APPEND lst_char TO lit_char.
*                CLEAR : lst_focha.
*              ENDLOOP.                                                         " LOOP AT lit_focha
*
*            ENDLOOP.                                                           " LOOP AT lit_xml_data
* ----------------------------------------------------------------------*
* --- Method to update characterstics-----------------------------------*
* ----------------------------------------------------------------------*
            CALL METHOD me->equipment_char_update
              EXPORTING
                im_workorder = lst_response-wo_num
                im_char      = lit_char
              IMPORTING
                ex_return    = lst_return.
            CLEAR: lst_return, lit_char.
          ENDIF.                                                               " IF NOT lit_focha IS INITIAL
        ENDIF.                                                                 " IF SY-SUBRC = 0
      ENDIF.                                                                   " IF SY-SUBRC = 0 AND LV_POSTCHAR IS NOT INITIAL

    ENDIF.
    " IF lit_return IS NOT INITIAL


    IF roots_to_delete IS NOT INITIAL.
      LOOP AT roots_to_delete INTO DATA(lst_to_delete).
        lst_response-instanceid = lst_to_delete-InstanceID.
      ENDLOOP.

      IF lst_response IS NOT INITIAL.
        DELETE /odsmfe/tb_forsp FROM @lst_response.
      ENDIF. "/if lst_response IS NOT INITIAL.

*      DATA(lv_deleted) = abap_true.
*
*      UPDATE /odsmfe/tb_forsp SET deleted = @lv_deleted   "#EC CI_SUBRC
*        WHERE instanceid =  @lv_instanceid.

* ----------------------------------------------------------------------*
* --- Update Exchange Table---------------------------------------------*
* ----------------------------------------------------------------------*
      SELECT SINGLE wo_num FROM /odsmfe/tb_forsp
        WHERE instanceid = @lst_to_delete-InstanceID INTO @lv_aufnr.

      IF sy-subrc = 0 AND lv_aufnr IS NOT INITIAL.
        CREATE OBJECT lo_exchtab.
        IF lo_exchtab IS BOUND.
          lo_exchtab->exch_table_update( lv_aufnr ).
        ENDIF.                                                                   " IF lo_exchtab IS BOUND
      ENDIF.


    ENDIF.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.


ENDCLASS.
