class /ODSMFE/CL_FORM_PDF_ATTACHMENT definition
  public
  create private .

public section.
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORM_PDF_ATTACHMENT IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 27/04/2023
* Transport No.          : ES1K903727
* Program Description    : GOS Attachment Upload Content to process order
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

    "/ Tables & Structures
    DATA:
      lit_objheader          TYPE STANDARD TABLE OF solisti1,                       "SAPoffice: Single List with Column Length 255
      lit_hexcont            TYPE STANDARD TABLE OF solix,                          "SAPoffice: Binary data, length 255
      lst_objheader          TYPE solisti1,                                         "SAPoffice: Single List with Column Length 255
      lst_docdata            TYPE sodocchgi1,                                       "Data of an object which can be changed
      lst_formpdf_attachment TYPE /odsmfe/cl_pr_formui_mpc=>ts_formpdfattachment,   "FormPDFAttachment properties
      lst_docinfo            TYPE sofolenti1,                                       "Docunment info
      lst_rolea              TYPE borident,                                         "Object Relationship Service: BOR object identifier
      lst_roleb              TYPE borident.                                         "Object Relationship Service: BOR object identifier

    "/ Variables
    DATA :
      lv_name             TYPE string,                                              "String
      lv_extension        TYPE so_obj_tp,                                           "Code for document class " SDOK_FILNM SOODK-OBJTP
      lv_file_content_bin TYPE xstring,                                             "xstring
      lv_message          TYPE bapi_msg,                                            "Message Text
      lv_folderid         TYPE soodk,                                               "SAPoffice: ID of a SAPoffice object
      lv_filter           TYPE string,
      lv_folder_id        TYPE soobjinfi1-object_id.                                "SAPoffice: ID of a SAPoffice object

    "/ Constants
    CONSTANTS:
      lc_message       TYPE so_obj_nam      VALUE 'MESSAGE',                        "Name of document, folder or distribution list
      lc_en            TYPE so_obj_la       VALUE 'E',                              "Language in Which Document Is Created
      lc_owner         TYPE soud-usrnam     VALUE ' ',                              "SAPoffice: user definition
      lc_region        TYPE sofd-folrg      VALUE 'B',                              "SAPoffice: Object Definition
      lc_unescape      TYPE string          VALUE 'X',                              "String
      lc_relation      TYPE binreltyp       VALUE 'ATTA',                           "Object Relationship Service binary link type
      lc_entitysetname TYPE string          VALUE 'FormPDFAttachmentSet',           "FormPDFAttachmentSet
      lc_field         TYPE string          VALUE 'FORM_PDF'.                       "Form PDF

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                     M A I N   S E C T I O N                            *
* -----------------------------------------------------------------------*

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formpdf_attachment ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lst_formpdf_attachment-object_key
      IMPORTING
        output = lst_formpdf_attachment-object_key.

    "/ Check if Requested Attachment is not blank
    IF lst_formpdf_attachment IS NOT INITIAL.

      "/ FM to Find the ID of a root folder (pers. Or general filing)
      CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
        EXPORTING
          owner                 = lc_owner
          region                = lc_region
        IMPORTING
          folder_id             = lv_folderid
        EXCEPTIONS
          communication_failure = 1
          owner_not_exist       = 2
          system_failure        = 3
          x_error               = 4
          OTHERS                = 5.
      "/ Error Handling
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lv_message WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        "/ Check if error message is not blank
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = lv_message.
        ENDIF. "/ IF lv_message IS NOT INITIAL.
      ELSE.
        lv_folder_id = lv_folderid.
        CLEAR lv_message.
      ENDIF. "/ IF sy-subrc <> 0.

      "/ Filling the Structure
      lst_docdata-obj_name  = lc_message.
      lst_docdata-obj_descr = lst_formpdf_attachment-description.
      lst_docdata-obj_langu = lc_en.
      lst_docdata-doc_size  = lst_formpdf_attachment-file_size.

      SPLIT lst_formpdf_attachment-file_name AT '.' INTO lv_name lv_extension.

      CONCATENATE '&SO_FILENAME=' lst_formpdf_attachment-file_name INTO lst_objheader-line.
      APPEND lst_objheader TO lit_objheader.
      lst_objheader-line = '&SO_FILENAME='.
      APPEND lst_objheader TO lit_objheader.

      "/ FM to convert BASE64
      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input    = lst_formpdf_attachment-line
          unescape = lc_unescape
        IMPORTING
          output   = lv_file_content_bin
        EXCEPTIONS
          failed   = 1
          OTHERS   = 2.
      "/ Error Handling
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lv_message WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = lv_message.
        ENDIF. "/ IF lv_message IS NOT INITIAL.
        CLEAR lv_message.
      ENDIF. "/ IF sy-subrc <> 0.

      "/ Xstring to Solix
      CALL METHOD cl_bcs_convert=>xstring_to_solix
        EXPORTING
          iv_xstring = lv_file_content_bin
        RECEIVING
          et_solix   = lit_hexcont.

      "/ FM to Create new office document using RFC
      CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
        EXPORTING
          folder_id                  = lv_folder_id
          document_data              = lst_docdata
          document_type              = lv_extension
        IMPORTING
          document_info              = lst_docinfo
        TABLES
          object_header              = lit_objheader
          contents_hex               = lit_hexcont
        EXCEPTIONS
          folder_not_exist           = 1
          document_type_not_exist    = 2
          operation_no_authorization = 3
          parameter_error            = 4
          x_error                    = 5
          enqueue_error              = 6
          OTHERS                     = 7.

      "/ Error Handling
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lv_message WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        "/ Check if error message is not blank
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = lv_message.
        ENDIF. "/ IF lv_message IS NOT INITIAL.
        CLEAR lv_message.
      ENDIF. "/ IF sy-subrc <> 0.

      lst_rolea-objkey  = lst_formpdf_attachment-object_key.
*      lst_rolea-objtype = 'BUS0001'.

      SELECT SINGLE low                                ##WARN_OK
        FROM /odsmfe/tb_filtr
        INTO lv_filter
        WHERE entitysetname EQ lc_entitysetname
        AND   field         EQ lc_field.

      lst_rolea-objtype = lv_filter.

      lst_roleb-objkey  = lst_docinfo-doc_id.
      lst_roleb-objtype = lc_message.

      "/ FM to Create a Binary Relationship
      CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
        EXPORTING
          obj_rolea      = lst_rolea
          obj_roleb      = lst_roleb
          relationtype   = lc_relation
        EXCEPTIONS
          no_model       = 1
          internal_error = 2
          unknown        = 3
          OTHERS         = 4.

      "/ Error Handling
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lv_message WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        "/ Check if error message is not blank
        IF lv_message IS NOT INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = lv_message.
        ENDIF. "/ IF lv_message IS NOT INITIAL.
        CLEAR lv_message.
      ENDIF. "/ IF sy-subrc <> 0.

      MOVE-CORRESPONDING lst_formpdf_attachment TO gstib_entity.
      gstib_entity-doc_id = lst_docinfo-doc_id.
      GET REFERENCE OF gstib_entity INTO ex_entityset.

    ENDIF. "/IF lst_formPDF_attachment IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.
