class /ODSMFE/CL_FORMATTACHMENT definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
*  type-pools ABAP .

  types:
    BEGIN OF gtys_filter_vals,
        srv_classname TYPE /odsmfe/st_core_range_str,
        wo_classname  TYPE /odsmfe/st_core_range_str,
        gos_active    TYPE /odsmfe/st_core_range_str,
        bds_active    TYPE /odsmfe/st_core_range_str,
      END OF gtys_filter_vals .
  types:
    gtyt_filter_vals TYPE STANDARD TABLE OF gtys_filter_vals .

  TYPES:
      BEGIN OF ty_data,
        wa(512) TYPE c,
      END OF ty_data.

    TYPES: BEGIN OF ty_options,
             text(72) TYPE c,
           END OF ty_options.
    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.

    DATA: lt_options TYPE TABLE OF ty_options,
          lt_fields  TYPE TABLE OF ty_fields,
          lt_data    TYPE TABLE OF ty_data.

     DATA: lv_rowskip  TYPE int4,
          lv_rowcount TYPE int4.
*  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMATTACHMENT .
*  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMATTACHMENT .
  data GITIB_FILTER_VALS type GTYT_FILTER_VALS .

  methods MODIFY_DELTA_TABLE
    importing
      !ORDER_NUMBER type AUFNR optional
      !TIME_TOKEN type STRING
      !NOTIFICATION type char12 optional .
  methods READ_DELTA_TABLE
    importing
      !DELTA_TOKEN type TIMESTAMP
    exporting
      !EX_AUFNR_DATA type /ODSMFE/TT_EX_AUFNR .

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



CLASS /ODSMFE/CL_FORMATTACHMENT IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
*
************************************************************************
*********************** CREATED HISTORY ********************************
** Program Author (SID)   : ODS
** Creation Date          : 15/05/2020
** Transport No.          : ES1K901774
** Program Description    : Creates Form attachments
************************************************************************
*********************** CHANGE HISTORY ********************************
** Program Author (SID)   :  ODS
** Change Date            :  2020/09/29
** Transport No.          :  ES1K902140
** Change Description     :  Added logic to Update work order exchange table
************************************************************************
*********************** CHANGE HISTORY **********************
** Program Author (SID)   : ODS
** Change Date            : 2022/03/11
** Transport No.          : ES1K902967
** Change Description     : ODSMFE Refactoring
************************************************************************
**----------------------------------------------------------------------
**  Data declaration
**----------------------------------------------------------------------
*
*    DATA: lst_formatt        TYPE /odsmfe/st_form_attachment,
*          lt_formatt         TYPE TABLE of /odsmfe/st_form_attachment,
*          lst_formattahcment TYPE /odsmfe/tb_fmatt.
*
**    DATA: lit_t350                TYPE STANDARD TABLE OF t350,
**          lst_t350                TYPE t350,
*      data:    lit_file_content_binary TYPE TABLE OF bapiconten,
*          lit_file_content_ascii  TYPE TABLE OF bapiascont.
*
*    DATA: lit_signature   TYPE TABLE OF bapisignat,
*          lit_component   TYPE TABLE OF bapicompon,
*          lst_uploadonent TYPE bapicompon,
*          lst_signature   TYPE bapisignat,
*          lit_objhdr      TYPE STANDARD TABLE OF solisti1,
*          lst_objhdr      TYPE solisti1,
*          lit_hexcont     TYPE STANDARD TABLE OF solix,
*          lst_folderid    TYPE soodk,
*          lst_docinfo     TYPE sofolenti1,
*          lst_bizojb      TYPE borident,
*          lst_docdata     TYPE sodocchgi1,
*          lst_attachment  TYPE borident.
*
*    DATA: lit_filter_vals TYPE STANDARD TABLE OF /odsmfe/tb_filtr,
*          lst_filter_vals TYPE /odsmfe/tb_filtr.
*
** Variables
*    DATA:  lv_object_key       TYPE bapibds01-objkey,
*           lv_operationnum     TYPE vornr,
*           lv_binary_flg       TYPE bapibds01-x,
*           lv_tempid           TYPE /odsmfe/de_middleware_objkey,
*           lv_target_obj_key   TYPE /odsmfe/de_middleware_objkey,
*           lv_workordernum     TYPE aufnr,
*           lv_line             TYPE string,
*           lv_file_content_bin TYPE xstring,
*           lv_description      TYPE string,
*           lv_filetype         TYPE string,
*           lv_desc             TYPE string,
*           lv_mimetype         TYPE w3conttype,
*           lv_auart            TYPE auart,
*           lv_service          TYPE C length 1,"i_servauf,
*           lv_var              TYPE char1,
*           lv_gos_active       TYPE bapibds01-x,
*           lv_bds_active       TYPE bapibds01-x,
*           lv_wo_classname     TYPE bds_clsnam,
*           lv_srv_classname    TYPE bds_clsnam,
*           lv_classname        TYPE bapibds01-classname,
*           lv_name             TYPE string,
*           lv_extension        TYPE soodk-objtp,
*           lv_folder_objid     TYPE soobjinfi1-object_id.
** SOC by ODS - ES1K902140
*    DATA:  lv_date           TYPE datum,        "Date
*           lv_time           TYPE uzeit,        "Time
*           lv_sys_time_token TYPE string,
*           lv_count          TYPE i,
*           lv_sys_tzone      TYPE tznzonesys,   "System Time Zone
*           lv_timestamp      TYPE timestamp.    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
** EOC by ODS - ES1K902140
** constants
*    CONSTANTS: lc_a                 TYPE char1 VALUE 'A',
*               lc_b                 TYPE char1 VALUE 'B',
*               lc_x                 TYPE char1 VALUE 'X',
*               lc_i                 TYPE string VALUE 'I',
*               lc_l                 TYPE char1 VALUE 'L',
*               lc_entity_set_name   TYPE string VALUE 'FormAttachmentSet',
*               lc_relation_type     TYPE breltyp-reltype VALUE 'ATTA',
*               lc_language          TYPE bds_propna VALUE 'LANGUAGE',
*               lc_description       TYPE bds_propna VALUE 'DESCRIPTION',
*               lc_bds_documenttype  TYPE bds_propna VALUE 'BDS_DOCUMENTTYPE',
*               lc_bds_contrep       TYPE bds_propna VALUE 'BDS_CONTREP',
*               lc_bds_documentclass TYPE bds_propna VALUE 'BDS_DOCUMENTCLASS',
*               lc_message           TYPE char10 VALUE 'MESSAGE',
*               lc_wo_classtype      TYPE bds_clstyp VALUE 'BO'.
*
*************************************************************************
** Main Section
*************************************************************************
** Read data from FE request
*   " im_data_provider->read_entry_data( IMPORTING es_data = lst_formatt ).
*
*    IF lst_formatt IS NOT INITIAL.
*
** Cross Referencing
*
*      MOVE-CORRESPONDING lst_formatt TO lst_formattahcment.
*      lst_formattahcment-created_by = sy-uname."++ES1K902011
*      CLEAR lv_tempid.
*      CLEAR lv_var.
*      lv_tempid = lst_formatt-instanceid .
*      IF lst_formatt-wo_num IS NOT INITIAL.
*        lv_workordernum        =  lst_formatt-wo_num.
** Converson exits
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lv_workordernum
*          IMPORTING
*            output = lv_workordernum.
*      ENDIF.
*
** Fill the values
*      lv_line = lst_formatt-imagedata.
*      SPLIT lst_formatt-file_name AT '.' INTO lv_description lv_filetype.
*      lv_desc     = lst_formatt-description.
*      lv_mimetype = lst_formatt-mimetype.
** get the order type
**      SELECT SINGLE auart FROM aufk INTO @lv_auart WHERE aufnr = @lv_workordernum.
**      IF sy-subrc <> 0 .
**        CLEAR lv_auart.
**      ENDIF.
*
*      data: lr_rfc TYPE REF TO /odsmfe/cl_get_ent_super_bapi.
*
*   call method lr_rfc->get_cloud_dest
*    IMPORTING
*      ex_dest = data(lv_rfc).
*
*      DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
*      DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).
*
*      lt_fields = VALUE #( ( fieldname = 'AUART' ) ).
*
*      IF lv_workordernum IS NOT INITIAL.
*        lt_options = VALUE #( ( text = |AUFNR| & | | & |{ lv_workordernum }| & | | & |'| & |{ lv_workordernum }| & |'|  ) ).
*        DATA(lv_and) = 'AND'.
*      ENDIF.
*
*      lv_rowskip = lv_Skip.
*      IF lv_top > 0.
*        lv_rowcount = 1.
*      ENDIF.
*
*      CALL FUNCTION 'RFC_READ_TABLE'
*        DESTINATION lv_rfc
*        EXPORTING
*          query_table = 'AUFK'
*          rowskips    = lv_rowskip
*          rowcount    = lv_rowcount
*        TABLES
*          options     = lt_options
*          fields      = lt_fields
*          data        = lt_data.
*
*        READ TABLE lt_data INTO data(ls_data) INDEX 1.
*
*        lv_auart = ls_Data-wa.
*
*        clear ls_data.
*
*
** Check the Values
*      lv_object_key =  lv_workordernum.
*      IF lv_operationnum IS NOT INITIAL.
*        CONCATENATE lv_object_key lv_operationnum INTO lv_object_key.
*      ENDIF.
*      lv_binary_flg = lc_x.
*
*      lst_signature-doc_count  = 1.
*      lst_signature-prop_name  = lc_bds_documentclass.
*      lst_signature-prop_value = lv_filetype.
*      APPEND lst_signature TO lit_signature.
*
*      lst_signature-prop_name  = lc_bds_contrep.
*      lst_signature-prop_value = ' '.
*      APPEND lst_signature TO lit_signature.
*
*      lst_signature-prop_name  = lc_bds_documenttype.
*      lst_signature-prop_value = ' '.
*      APPEND lst_signature TO lit_signature.
*
*      lst_signature-prop_name  = lc_description.
*      lst_signature-prop_value = lv_desc.
*      APPEND lst_signature TO lit_signature.
*
*      lst_signature-prop_name  = lc_language."'LANGUAGE'.
*      lst_signature-prop_value = sy-langu.
*      APPEND lst_signature TO lit_signature.
*
*      lst_uploadonent-doc_count  = 1.
*      lst_uploadonent-comp_count = 1.
*      lst_uploadonent-mimetype   = lv_mimetype.
*      lst_uploadonent-comp_id    = lst_formatt-file_name.
*      lst_uploadonent-comp_size  = lst_formatt-file_size.
*      APPEND lst_uploadonent TO lit_component.
** Convert SCMS_BASE64_DECODE_STR
*      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
*        EXPORTING
*          input  = lv_line
**         UNESCAPE       = lc_x
*        IMPORTING
*          output = lv_file_content_bin
*        EXCEPTIONS
*          failed = 1
*          OTHERS = 2.
*
**      IF sy-subrc <> 0.
**        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception   " changed
**          EXPORTING
**            textid  = /iwbep/cx_mgw_busi_exception=>business_error
**            message = text-t07.
**      ENDIF.
** Convert SCMS_BASE64_DECODE_STR
*      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*        EXPORTING
*          buffer     = lv_file_content_bin
*        TABLES
*          binary_tab = lit_file_content_binary.
*
*      SELECT entitysetname, tabname, field, recordno,
*             field_descr, sign, options, low, high, active
*             FROM /odsmfe/tb_filtr
*             WHERE entitysetname = @lc_entity_set_name
*             AND active = @abap_on
*             INTO CORRESPONDING FIELDS OF TABLE @lit_filter_vals.
*      IF sy-subrc EQ 0.
*        SORT lit_filter_vals BY field.
*      ENDIF.
*
** Read BDC_ACTIVE value
*      READ TABLE lit_filter_vals INTO lst_filter_vals
*      WITH KEY field = 'BDS_ACTIVE' active = lc_x BINARY SEARCH.
*      IF sy-subrc = 0.
*       " MOVE lst_filter_vals-low TO lv_bds_active.
*        lst_filter_vals-low = lv_bds_active.
*
*        CLEAR: lst_filter_vals.
*      ENDIF.
*
**   Read GOS_ACTIVE value
*      READ TABLE lit_filter_vals INTO lst_filter_vals
*      WITH KEY field = 'GOS_ACTIVE' active = lc_x BINARY SEARCH.
*      IF sy-subrc = 0.
*        "MOVE lst_filter_vals-low TO lv_gos_active.
*        lst_filter_vals-low = lv_gos_active.
*        CLEAR: lst_filter_vals.
*      ENDIF.
*
** Read WO_CLASSNAME Value
*      READ TABLE lit_filter_vals INTO lst_filter_vals
*      WITH KEY field = 'WO_CLASSNAME' active = lc_x BINARY SEARCH.
*      IF sy-subrc = 0.
*       " MOVE lst_filter_vals-low TO lv_wo_classname.
*        lst_filter_vals-low = lv_wo_classname.
*        CLEAR: lst_filter_vals.
*      ENDIF.
*
** Read Service WO_CLASSNAME value
*      READ TABLE lit_filter_vals INTO lst_filter_vals
*      WITH KEY field = 'SRV_CLASSNAME' active = lc_x BINARY SEARCH.
*      IF sy-subrc = 0.
*       " MOVE lst_filter_vals-low TO lv_srv_classname.
*        lst_filter_vals-low = lv_srv_classname.
*        CLEAR: lst_filter_vals.
*      ENDIF.
*
** Read data Service for Order Type.
**      SELECT SINGLE service FROM t350 INTO @lv_service
**      WHERE auart = @lv_auart.
*
*    clear lt_data.
*   call method lr_rfc->get_cloud_dest
*    IMPORTING
*      ex_dest = lv_rfc.
*
*      lv_top     = im_request->get_paging( )->get_page_size( ).
*      lv_skip    = im_request->get_paging( )->get_offset( ).
*
*      lt_fields = VALUE #( ( fieldname = 'SERVICE' ) ).
*
*      IF lv_auart IS NOT INITIAL.
*        lt_options = VALUE #( ( text = |AUFNR| & | | & |{ lv_auart }| & | | & |'| & |{ lv_auart }| & |'|  ) ).
*        lv_and = 'AND'.
*      ENDIF.
*
*      lv_rowskip = lv_Skip.
*      IF lv_top > 0.
*        lv_rowcount = 1.
*      ENDIF.
*
*      CALL FUNCTION 'RFC_READ_TABLE'
*        DESTINATION lv_rfc
*        EXPORTING
*          query_table = 't350'
*          rowskips    = lv_rowskip
*          rowcount    = lv_rowcount
*        TABLES
*          options     = lt_options
*          fields      = lt_fields
*          data        = lt_data.
*
*
*         READ TABLE lt_data INTO ls_data index 1.
*         lv_service = ls_data-wa.
*
*
*      IF sy-subrc = 0 AND lv_service = lc_x.
*        lv_classname = lv_srv_classname.
*      ELSE.
*        lv_classname = lv_wo_classname.
*      ENDIF.
*
*      IF  lv_bds_active = lc_x .
** Call standard BAPI to create BDS Document
*        CALL FUNCTION 'BDS_BUSINESSDOCUMENT_CREA_TAB'
*          EXPORTING
*            classname       = lv_classname
*            classtype       = lc_wo_classtype
*            client          = sy-mandt
*            object_key      = lv_object_key
*            binary_flag     = lv_binary_flg
*          TABLES
*            signature       = lit_signature
*            components      = lit_component
*            content         = lit_file_content_binary
*            ascii_content   = lit_file_content_ascii
*          EXCEPTIONS
*            nothing_found   = 1
*            parameter_error = 2
*            not_allowed     = 3
*            error_kpro      = 4
*            internal_error  = 5
*            not_authorized  = 6
*            OTHERS          = 7.
**        IF sy-subrc <> 0.
**          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
**            EXPORTING
**              textid  = /iwbep/cx_mgw_busi_exception=>business_error    "changed
**              message = text-t06.
**
**        ENDIF.
*        READ TABLE lit_signature INTO lst_signature INDEX 1.
*        lst_formattahcment-doc_id = lst_signature-doc_id.
*        lst_formatt-doc_id = lst_signature-doc_id.
*      ELSEIF  lv_gos_active = lc_x.
***GOS Attachment
*        CLEAR lv_folder_objid.
*        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
*          EXPORTING
**           owner                 = ' '
*            region                = lc_b
*          IMPORTING
*            folder_id             = lst_folderid
*          EXCEPTIONS
*            communication_failure = 1
*            owner_not_exist       = 2
*            system_failure        = 3
*            x_error               = 4.
*        IF sy-subrc <> 0.
*          CLEAR: lst_folderid.
*        ENDIF.
*
*        lst_docdata-obj_name  = lc_message.
*        lst_docdata-obj_descr = lst_formatt-description.
*        lst_docdata-obj_langu = 'EN'.
*        lv_folder_objid = lst_folderid.
*
*        CONCATENATE '&SO_FILENAME=' lst_formatt-file_name INTO lst_objhdr-line.
*        APPEND lst_objhdr TO lit_objhdr.
*        lst_objhdr-line = '&SO_FORMAT=BIN'.
*        APPEND lst_objhdr TO lit_objhdr.
****Xstring to Solix
*
*        CALL METHOD cl_bcs_convert=>xstring_to_solix
*          EXPORTING
*            iv_xstring = lv_file_content_bin
*          RECEIVING
*            et_solix   = lit_hexcont.
*
*        SPLIT lst_formatt-file_name AT '.' INTO lv_name lv_extension.
*
*        CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
*          EXPORTING
*            folder_id                  = lv_folder_objid
*            document_data              = lst_docdata
*            document_type              = lv_extension
*          IMPORTING
*            document_info              = lst_docinfo
*          TABLES
*            object_header              = lit_objhdr
*            contents_hex               = lit_hexcont
*          EXCEPTIONS
*            folder_not_exist           = 1
*            document_type_not_exist    = 2
*            operation_no_authorization = 3
*            parameter_error            = 4
*            x_error                    = 5
*            enqueue_error              = 6
*            OTHERS                     = 7.
*
**        IF sy-subrc <> 0.
**          WRITE:/ 'SO_DOCUMENT_INSERT_API1 SY-SUBRC = ', sy-subrc. " Changed by Pratheesh
**        ENDIF.
*
*        lst_bizojb-objkey  = lv_workordernum.
*        lst_bizojb-objtype = lv_classname.
*
**Attachment folder id is in lst_docinfo
*        lst_attachment-objkey  = lst_docinfo-doc_id.
*        lst_attachment-objtype = lc_message.
*
*        CALL FUNCTION 'BINARY_RELATION_CREATE'
*          EXPORTING
*            obj_rolea    = lst_bizojb
*            obj_roleb    = lst_attachment
*            relationtype = lc_relation_type.
*
*        lst_formattahcment-doc_id = lst_docinfo-doc_id.
*        lst_formatt-doc_id = lst_docinfo-doc_id.
*      ENDIF.
*
**** Modify data as per data received from FE
*
*      MODIFY /odsmfe/tb_fmatt FROM @lst_formattahcment.
*      IF sy-subrc = 0.
*        "MOVE-CORRESPONDING lst_formatt TO gstib_entity.
*
*
*         "MOVE-CORRESPONDING lst_formatt to ex_response.
*
**        IF  gstib_entity  IS NOT INITIAL.
**          GET REFERENCE OF gstib_entity INTO ex_entity.
**        ENDIF.
*
** Reference
*    DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
*
*" updating work order exchange table
*    IF lst_formatt-wo_num IS NOT INITIAL.
**--Start of changes  - ES1K902967
*      IF lr_exchtab IS BOUND.
*        lr_exchtab->exch_table_update( lst_formatt-wo_num ).
*      ENDIF.
**--End of changes  - ES1K902967
*    ENDIF.
*      ELSE.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception  "changed
*          EXPORTing
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = text-t05.
*      ENDIF.
*    ELSE.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = text-t04.
*    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.
***********************************************************************
*********************** CREATED HISTORY **********************
** Program Author (SID)   : SKAMMARI
** Creation Date          : 15/05/2020
** Transport No.          : ES1K901774
** Program Description    : Method to Deletes Form attachment
************************************************************************
*********************** CHANGE HISTORY **********************
** Program Author (SID)   :
** Change Date            :
** Transport No.          :
** Change Description     :
************************************************************************
*
**-------------------------------------------------------------
**  Data declaration
**-------------------------------------------------------------
*    TYPES:
*      BEGIN OF ltys_formatt,
*        instanceid  TYPE c LENGTH 50,
*        formid      TYPE c LENGTH 30,
*        version     TYPE c LENGTH 3,
*        doc_count   TYPE i,
*        wo_num      TYPE c LENGTH 12,
*        vornr       TYPE c LENGTH 4,
*        equnr       TYPE c LENGTH 18,
*        imagedata   TYPE string,
*        created_on  TYPE timestamp,
*        created_by  TYPE c LENGTH 50,
*        modified_on TYPE timestamp,
*        modified_by TYPE c LENGTH 50,
*      END OF ltys_formatt.
*
*    DATA: lst_formatt  TYPE ltys_formatt,
*          lst_formatt1 TYPE /odsmfe/tb_fmatt.
*
** Variables
*    DATA: lv_instanceid       TYPE char50.
*
**-------------------------------------------------------------
** Main Section
**-------------------------------------------------------------
*
*    "im_data_provider->read_entry_data( IMPORTING es_data = lst_formatt ).
*    IF lst_formatt-instanceid IS NOT INITIAL.
*
**  checking from table for instance id
*      SELECT SINGLE instanceid
*             FROM /odsmfe/tb_fmatt
*             WHERE instanceid = @lst_formatt-instanceid
*             AND formid = @lst_formatt-formid
*             AND version = @lst_formatt-version
*             AND doc_count = @lst_formatt-doc_count INTO @lv_instanceid.
** delete from Table /odsmfe/tb_fmatt
*      IF sy-subrc = 0 AND lv_instanceid IS NOT INITIAL.
*        MOVE-CORRESPONDING lst_formatt TO lst_formatt1.
*        DELETE /odsmfe/tb_fmatt FROM @lst_formatt1.
*        IF sy-subrc <> 0.
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*            EXPORTING
*              textid  = /iwbep/cx_mgw_busi_exception=>business_error
*              message = text-t08.
*        ENDIF.
*      ELSE.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = text-t03.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
*
************************************************************************
*********************** CREATED HISTORY **********************
** Program Author (SID)   : ODS
** Creation Date          : 15/05/2020
** Transport No.          : ES1K901774
** Program Description    : Displays form FormMaster data
************************************************************************
*********************** CHANGE HISTORY **********************
** Program Author (SID)   : ODS
** Change Date            : 2020/08/11
** Transport No.          : ES1K902140
** Change Description     : Added logic to Update work order exchange table
*********************** CHANGE HISTORY **********************
** Program Author (SID)   : ODS
** Change Date            : 2022/03/11
** Transport No.          : ES1K902967
** Change Description     : ODSMFE Refactoring
************************************************************************
**-------------------------------------------------------------
**  Data declaration
**-------------------------------------------------------------
*
*    DATA: lst_formatt TYPE /odsmfe/tb_fmatt.
*
*    DATA:  lv_date           TYPE datum,        "Date
*           lv_time           TYPE uzeit,        "Time
*           lv_sys_time_token TYPE string,
*           lv_count          TYPE i,
*           lv_sys_tzone      TYPE tznzonesys,   "System Time Zone
*           lv_timestamp      TYPE timestamp.    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
*************************************************************************
** Main Section
*************************************************************************
** Read data from FE request
*    "im_data_provider->read_entry_data( IMPORTING es_data = lst_formatt ).
*
*    IF lst_formatt IS NOT INITIAL.
** Modify data as per data received from FE
*      MODIFY /odsmfe/tb_fmatt FROM @lst_formatt.          "#EC CI_SUBRC.
*      IF sy-subrc = 0.
*        "MOVE-CORRESPONDING lst_formatt TO gstib_entity.
*         "MOVE-CORRESPONDING lst_formatt TO ex_response.
**        IF  gstib_entity  IS NOT INITIAL.
**          GET REFERENCE OF gstib_entity INTO ex_entity.
**        ENDIF.
*
*
** Reference
*    DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
*
*" updating work order exchange table
*    IF lst_formatt-wo_num IS NOT INITIAL.
**--Start of changes  - ES1K902967
*      IF lr_exchtab IS BOUND.
*        lr_exchtab->exch_table_update( lst_formatt-wo_num ).
*      ENDIF.
**--End of changes  - ES1K902967
*    ENDIF.
*
*      ELSE.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid  = /iwbep/cx_mgw_busi_exception=>business_error
*            message = text-t01.
*      ENDIF.
*    ELSE.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid  = /iwbep/cx_mgw_busi_exception=>business_error
*          message = text-t02.
*    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 15/05/2020
* Transport No.          : ES1K901774
* Program Description    : Displays form FormMaster data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS
* Change Date            : 29/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added logic to fetch attachents based on mjc relevant orders
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : ODS
* Change Date            : 11.03.2022
* Transport No.          : ES1K902967
* Change Description     : ODSMFE Refactoring SP07
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
    DATA:  lit_formatt TYPE STANDARD TABLE OF /odsmfe/tb_fmatt,
                lit_formattachment TYPE TABLE OF /ODSMFE/CE_FORMATTACHMENT.           "lst_entity  TYPE /odsmfe/cl_pr_formui_mpc=>tt_formattachment,
           "lit_return  TYPE /iwbep/t_mgw_select_option,
           "lst_return  TYPE /iwbep/s_mgw_select_option.

    "DATA: "lst_formatt_get_entityset TYPE LINE OF /odsmfe/cl_pr_formui_mpc=>tt_formattachment,
            "lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair.

    DATA: lrs_it_instanceid TYPE /odsmfe/st_core_range_str,
          lrt_instanceid    TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_formid        TYPE TABLE OF /odsmfe/st_core_range_str,
          lrs_it_formid     TYPE /odsmfe/st_core_range_str,
          lrs_it_version    TYPE /odsmfe/st_core_range_str,
          lrt_version       TYPE TABLE OF /odsmfe/st_core_range_str,
          lrs_it_attcounter TYPE /odsmfe/st_core_range_str,
          lrt_attcounter    TYPE TABLE OF /odsmfe/st_core_range_str.

*    DATA: lit_select_options TYPE /iwbep/t_cod_select_options,
*          lst_select_options TYPE /iwbep/s_cod_select_option.
          "lo_filter          TYPE  REF TO /iwbep/if_mgw_req_filter.

     DATA: lo_filter  TYPE REF TO if_rap_query_filter,
           lit_return TYPE  if_rap_query_filter=>tt_name_range_pairs.

    DATA: IM_TECH_REQUEST_CONTEXT type ref to if_rap_query_request.



* SOC by ODS - ES1K902140
    DATA:  lit_valid_wo        TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str,
           lit_workorders_resp TYPE TABLE OF /odsmfe/tb_forsp.

* Variables
    DATA: lv_mobileuser  TYPE string,
          lv_delta_token TYPE timestamp.
* Constants
    CONSTANTS:lc_i             TYPE string VALUE 'I',
              lc_eq            TYPE string VALUE 'EQ',
              lc_instanceid    TYPE string VALUE 'INSTANCEID',
              lc_formid        TYPE string VALUE 'FORMID',
              lc_version       TYPE string VALUE 'VERSION',
              lc_attachcounter TYPE string VALUE 'DOC_COUNT',
              lc_wonum         TYPE string VALUE 'WORKORDERNUM'.

* Field Symbols
    FIELD-SYMBOLS: <lfsit_delta_tab> TYPE STANDARD TABLE,
                   "<lfsst_ls_return> TYPE /iwbep/s_mgw_select_option,
                   <lfsst_form>      TYPE /odsmfe/tb_fmatt.
* EOC by ODS- ES1K902140

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
 " SOC Pratheesh
** Maps key fields to function module parameters
      LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
          CASE ls_filter_select_options-name.
              WHEN lc_instanceid.
                 lrt_instanceid = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_instanceid WHERE low IS INITIAL.
              when lc_formid.
                 lrt_formid = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_formid WHERE low IS INITIAL.

               when lc_version.
                 lrt_version = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_version WHERE low IS INITIAL.

               when lc_attachcounter.
                lrt_attcounter = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_attcounter WHERE low IS INITIAL.

           ENDCASE.
       ENDLOOP.
   " ENDIF.

*    IF im_tech_request_context IS SUPPLIED AND im_key_tab IS INITIAL.
*      REFRESH : lit_return, lit_select_options.
*      lo_filter = im_tech_request_context->get_filter( ).
*      lit_return = lo_filter->get_filter_select_options( ).
*      READ TABLE lit_return INTO lst_return INDEX 1.
*      IF lst_return-property = text-010."InstanceId ."
*        lit_select_options =  lst_return-select_options.
*
*        IF lit_select_options IS NOT INITIAL.
*          CLEAR: lrt_instanceid,lst_select_options.
*
*          LOOP AT lit_select_options INTO lst_select_options.
*            IF lst_select_options-low IS NOT INITIAL.
*              lrs_it_instanceid-sign = lc_i.
*              lrs_it_instanceid-option = lc_eq.
*              lrs_it_instanceid-low = lst_select_options-low.
*              APPEND lrs_it_instanceid TO lrt_instanceid.
*              CLEAR lrs_it_instanceid.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
* SOC ODS- ES1K902140
    " fetching response based on Loggedin User.
   " IF im_key_tab IS INITIAL AND lrt_instanceid IS INITIAL. "Changed by Pratheesh
   IF im_filter_select_options IS INITIAL AND lrt_instanceid IS INITIAL.
* If EnteredyBy is empty
      IF lv_mobileuser IS INITIAL.
        lv_mobileuser = sy-uname.
      ENDIF.

*--Start of changes - ES1K902967
      DATA(lr_getworkorder) = NEW /odsmfe/cl_get_workorder_data( im_entity_name ). "changed by pratheesh

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
*--End of changes - ES1K902967

      SORT lit_valid_wo.
      DELETE ADJACENT DUPLICATES FROM lit_valid_wo COMPARING ALL FIELDS.
* Fetch Response Capture from /odsmfe/tb_forsp for entered user
      IF lit_valid_wo IS NOT INITIAL.
* Fetch the Forms associated with work orders
        SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
        FOR ALL ENTRIES IN @lit_valid_wo
        WHERE wo_num = @lit_valid_wo-aufnr
        INTO CORRESPONDING FIELDS OF TABLE @lit_workorders_resp.              "#EC CI_NOFIELD

        IF lit_workorders_resp IS NOT INITIAL.
          SORT lit_workorders_resp BY instanceid.
          SELECT *
          FROM /odsmfe/tb_fmatt
          FOR ALL ENTRIES IN @lit_workorders_resp
          WHERE instanceid EQ @lit_workorders_resp-instanceid
          AND formid EQ @lit_workorders_resp-formid
          AND version EQ @lit_workorders_resp-version
          INTO TABLE @lit_formatt.
          IF sy-subrc = 0 .
            SORT lit_formatt BY formid version.
          ENDIF.
        ENDIF.
      ENDIF.
* Fetch the non associated forms
      SELECT * FROM /odsmfe/tb_fmatt
        WHERE instanceid NE @space
              AND wo_num EQ @space
              AND equnr EQ @space
              AND tplnr EQ @space
              AND created_by = @lv_mobileuser
              APPENDING TABLE @lit_formatt.
      IF sy-subrc = 0.
        SORT lit_formatt BY instanceid.
      ENDIF.
    ENDIF.

* EOC by ODS- ES1K902140
    IF lrt_instanceid IS NOT INITIAL.
      SELECT * FROM /odsmfe/tb_fmatt
      WHERE instanceid IN @lrt_instanceid
      AND formid IN @lrt_formid
      AND version IN @lrt_version
      AND doc_count IN @lrt_attcounter
      INTO TABLE @lit_formatt.
      IF sy-subrc = 0.
        SORT lit_formatt BY formid version.
      ENDIF.
    ENDIF.

*    ENDIF." ODS- ES1K902140

    lit_formattachment = VALUE #( FOR lst_formattachment in lit_formatt
                                ( InstanceId = lst_formattachment-instanceid
                                  FormId = lst_formattachment-formid
                                  Version = lst_formattachment-version
                                  AttachCounter = lst_formattachment-doc_count
                                  FileName = lst_formattachment-file_name
                                  MimeType = lst_formattachment-mimetype
                                  Description = lst_formattachment-description
                                  ObjectNum = lst_formattachment-wo_num
                                  OperationNum = lst_formattachment-vornr
                                  Equipment = lst_formattachment-equnr
                                  FunctionalLoc = lst_formattachment-tplnr
                                  Imagedata = lst_formattachment-imagedata
                                  QuestionId = lst_formattachment-questionid
                                  CreatedOn = lst_formattachment-created_on
                                  CreatedBy = lst_formattachment-created_by
                                  ModifiedOn = lst_formattachment-modified_on
                                  ModifiedBy = lst_formattachment-modified_by ) ).



    MOVE-CORRESPONDING lit_formattachment TO ex_response_data.

  ENDMETHOD.


  METHOD modify_delta_table.

*    DATA : lst_delta             TYPE /odsmfe/tb_wo_ex,
*           lv_count              TYPE i,
*           lv_exchange_table(20) TYPE c.
*
*    CONSTANTS : lc_mobile_app_mfe    TYPE string VALUE '/ODSMFE/SAP_WM_MOBILE_APP'.
*
*    " Insert data
*    lst_delta-mandt      = sy-mandt.
*    lst_delta-mobile_app = lc_mobile_app_mfe.
*    lst_delta-changed_ts = time_token.
*    lst_delta-changed_by = sy-uname.
*
*    IF order_number IS NOT INITIAL.
*
*      SELECT COUNT(*) FROM /odsmfe/tb_wo_ex INTO lv_count
*            WHERE objkey = order_number.
*      IF lv_count = 0.
*        lst_delta-action = 'I'.
*      ELSE.
*        lst_delta-action = 'U'.
*      ENDIF.
*
*      lst_delta-objkey     = order_number.
*
*      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
*                          WHERE entitysetname = 'FormAttachmentSet'
*                            AND tabname = 'WORKORDER'
*                            AND field = 'EXCHANGE_TABLE'.
*
*    ELSEIF notification IS NOT INITIAL.
*
*      SELECT COUNT(*) FROM /odsmfe/tb_no_ex INTO lv_count
*          WHERE objkey = notification.
*
*      IF lv_count = 0.
*        lst_delta-action = 'I'.
*      ELSE.
*        lst_delta-action = 'U'.
*      ENDIF.
*
*      lst_delta-objkey     = notification.
*
*      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
*                          WHERE entitysetname = 'FormAttachmentSet'
*                            AND tabname = 'NOTIFICATION'
*                            AND field = 'EXCHANGE_TABLE'.
*
*    ENDIF.
*
*    IF lv_exchange_table IS NOT INITIAL.
*
*      MODIFY (lv_exchange_table) FROM lst_delta.
*      IF sy-subrc = 0 .
*        COMMIT WORK .
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.


  METHOD read_delta_table.

*Variables
*    DATA: lv_exchobj        TYPE /syclo/core_exchobj_dte,
*          lv_mobile_app     TYPE /syclo/core_mobile_app_dte,
*          lv_bor_objtyp     TYPE oj_name,
*          lv_sys_tzone      TYPE tznzonesys,
*          lv_ts             TYPE timestamp,
*          lv_ts_str         TYPE string,
*          lv_date           TYPE datum,
*          lv_time           TYPE uzeit,
*          lv_sys_time_token TYPE string.
*    DATA: lv_filter TYPE string.
*
**Tables & Structures
*    DATA: BEGIN OF ls_date_time,
*            date TYPE datum,
*            time TYPE uzeit,
*          END OF ls_date_time.
*
*    DATA : lit_ex_aufnr          TYPE TABLE OF /odsmfe/ex_aufnr,
*           lv_exchange_table(20) TYPE c.
*
*    CONSTANTS :  lc_tzone     TYPE tznzonesys VALUE 'UTC',
*                 lc_tzone_utc TYPE tznzonesys VALUE 'UTC'.
*
*    lv_ts_str = delta_token.
*    ls_date_time = lv_ts_str.
*
*    /syclo/cl_core_bapi_tools=>get_system_time(
*      IMPORTING ev_sys_tzone = lv_sys_tzone ).
*    IF lv_sys_tzone = lc_tzone.
*      lv_sys_time_token = delta_token.
*    ELSEIF lc_tzone = lc_tzone_utc.
*      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
*        INTO DATE lv_date
*             TIME lv_time.
*      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
*    ELSE.
*      lv_date = ls_date_time-date.
*      lv_time = ls_date_time-time.
*      CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_ts TIME ZONE lc_tzone.
*      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
*       INTO DATE lv_date
*            TIME lv_time.
*      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
*    ENDIF.
*
*
*    CONCATENATE 'CHANGED_TS > ' '''' lv_sys_time_token '''' INTO lv_filter SEPARATED BY space.
*    CONCATENATE lv_filter ' AND MOBILE_APP = ' '''' lv_mobile_app ''''
*      INTO lv_filter.
*
*    SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
*                    WHERE entitysetname = 'FormAttachmentSet'
*                      AND field = 'EXCHANGE_TABLE'.
*
*    IF lv_exchange_table IS NOT INITIAL.
*
*      SELECT * FROM (lv_exchange_table) INTO TABLE lit_ex_aufnr
*                                     WHERE (lv_filter).
*
*
*      IF lit_ex_aufnr IS NOT INITIAL.
*        ex_aufnr_data[] = lit_ex_aufnr[].
*      ENDIF.
*    ENDIF.


  ENDMETHOD.
ENDCLASS.
