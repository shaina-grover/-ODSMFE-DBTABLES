CLASS lhc_CE_FormAttachment DEFINITION INHERITING FROM cl_abap_behavior_handler.
*  INTERFACES /odsmfe/if_get_entityset_bapi.
  PUBLIC SECTION.
       INTERFACES if_oo_adt_classrun.

      METHODS get_cloud_dest
      EXPORTING
        ex_dest TYPE rfcdest.

      "/*******************Data Declaration for Create Method***************/"

       "/Types
       TYPES: BEGIN OF ltys_data,
                        wa(512) TYPE c,
                  END OF ltys_data,

                  BEGIN OF ltys_option,
                        text(72) TYPE c,
                  END OF ltys_option,

                  BEGIN OF ltys_fields,
                        fieldname(30)   TYPE c,
                        offset(6)   TYPE n,
                        length(6)   TYPE n,
                        type(1) TYPE c,
                        fieldtext(60)   TYPE c,
                  END OF ltys_fields.

        "/Tables and Structures
        DATA: lst_fmatt TYPE /odsmfe/tb_fmatt,
                  lit_option TYPE TABLE OF ltys_option,
                  lit_data    TYPE TABLE OF ltys_data,
                  lit_fields   TYPE TABLE OF ltys_fields,
                  lit_signature TYPE TABLE OF /odsmfe/st_bapisignat,
                  lst_signature TYPE /odsmfe/st_bapisignat,
                  lit_component TYPE TABLE OF /odsmfe/st_bapicompon,
                  lst_uploadonent TYPE /odsmfe/st_bapicompon,
                  lit_file_content_binary TYPE TABLE OF /odsmfe/st_bapiconten,
                  lst_file_content_binary TYPE /odsmfe/st_bapiconten,
                  lit_filter_vals   TYPE TABLE OF /odsmfe/tb_filtr,
                  lst_filter_vals   TYPE /odsmfe/tb_filtr,
                  lit_file_content_ascii TYPE TABLE OF /odsmfe/st_bapiascont,
                  lst_folderid TYPE /odsmfe/st_soodk,
                  lst_docdata TYPE /odsmfe/st_sodocchgi1,
                  lit_objhdr    TYPE TABLE OF /odsmfe/st_solisti1,
                  lst_objhdr    TYPE /odsmfe/st_solisti1,
                  lit_hexcont TYPE TABLE OF /odsmfe/st_solix,
                  lst_hexcont TYPE  /odsmfe/st_solix,
                  lst_bizojb    TYPE /odsmfe/st_borident,
                  lst_attachment TYPE  /odsmfe/st_borident.



       "/Reference Class
       DATA: lr_request     TYPE REF TO if_rap_query_request ,
                 lr_base64_decode TYPE REF TO cl_web_http_utility.



        "/Variables_
        DATA: lv_tempid                         TYPE /odsmfe/de_middleware_objkey,
                   lv_var                               TYPE c LENGTH 1,
                   lv_rowskip                       TYPE int4,
                   lv_rowcount                     TYPE int4,
                   lv_Dest                            TYPE string,
                   lv_line                             TYPE string,
                   lv_description                 TYPE string,
                   lv_filetype                       TYPE string,
                   lv_desc                           TYPE string,
                   lv_mimetype                   TYPE c LENGTH 128,
                   lv_wonum                       TYPE aufnr,
                   lv_auart                          TYPE c LENGTH 4,
                   lv_qmnum                       TYPE c LENGTH 12,
                   lv_object_key                  TYPE c LENGTH 70,
                   lv_operationnum              TYPE c LENGTH 4,
                   lv_binary_flag                  TYPE c LENGTH 1,
                   lv_file_content_bin          TYPE xstring,
                   lv_classname                   TYPE c LENGTH 30,
                   lv_bds_active                TYPE c LENGTH 1,
                   lv_gos_active                TYPE c LENGTH 1,
                   lv_wo_classname          TYPE c LENGTH 30,
                   lv_no_classname           TYPE c LENGTH 30,
                   lv_srv_classname           TYPE c LENGTH 30,
                   lv_string                           TYPE string,
                   lv_service                          TYPE c LENGTH 1,
                   lv_folder_objid                TYPE c LENGTH 17,
                   lv_name                           TYPE string,
                   lv_extension                     TYPE c LENGTH 3,
                   lv_doc_id                          TYPE c LENGTH 46.

         "/Constants
           CONSTANTS:   lc_dest            TYPE string VALUE  'CLOUD_DEST',
                                  lc_a                 TYPE c LENGTH 1 VALUE 'A',                      "Single-Character Indicator
                                  lc_b                 TYPE c LENGTH 1 VALUE 'B',                      "Single-Character Indicator
                                  lc_x                 TYPE c LENGTH 1 VALUE 'X',                      "Single-Character Indicator
                                  lc_i                 TYPE string VALUE 'I',                     "Single-Character Indicator
                                  lc_l                 TYPE c LENGTH 1 VALUE 'L',                      "Single-Character Indicator
                                  lc_entity_set_name   TYPE string VALUE '/ODSMFE/CE_FORMATTACHMENT',     "Filter Options: FormAttachmentSet
                                  lc_relation_type     TYPE c LENGTH 4  VALUE 'ATTA',         "Filter Options: ATTA
                                  lc_language          TYPE c LENGTH 25  VALUE 'LANGUAGE',          "Filter Options: LANGUAGE
                                  lc_description       TYPE c LENGTH 25   VALUE 'DESCRIPTION',       "Filter Options: DESCRIPTION
                                  lc_bds_documenttype  TYPE c LENGTH 25 VALUE 'BDS_DOCUMENTTYPE',  "Filter Options: BDS_DOCUMENTTYPE
                                  lc_bds_contrep       TYPE c LENGTH 25 VALUE 'BDS_CONTREP',       "Filter Options: BDS_CONTREP
                                  lc_bds_documentclass TYPE c LENGTH 25 VALUE 'BDS_DOCUMENTCLASS', "Filter Options: BDS_DOCUMENTCLASS
                                  lc_message           TYPE c LENGTH 10 VALUE 'MESSAGE',               "Filter Options: MESSAGE
                                  lc_wo_classtype      TYPE c LENGTH 2 VALUE 'BO'.                "Filter Options: BO

  PRIVATE SECTION.
    METHODS modify FOR behavior IMPORTING
        roots_to_create     FOR CREATE FormAttachment
        roots_to_update    FOR UPDATE FormAttachment
        roots_to_delete    FOR DELETE  FormAttachment.

    METHODS read FOR behavior IMPORTING
        lit_fmatt FOR READ FormAttachment RESULT et_fmatt.

ENDCLASS.

CLASS lhc_CE_FormAttachment IMPLEMENTATION.

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


    METHOD modify.

        "/******************Handle Create Method*****************************/"
        if roots_to_create IS NOT INITIAL.
            LOOP AT roots_to_create INTO DATA(lst_to_create).
                lst_fmatt-instanceid       = lst_to_create-InstanceId.
                lst_fmatt-formid             = lst_to_create-FormId.
                lst_fmatt-version            = lst_to_create-Version.
                lst_fmatt-doc_count       = lst_to_create-AttachCounter.
                lst_fmatt-file_name        = lst_to_create-FileName.
                lst_fmatt-mimetype        = lst_to_create-MimeType.
                lst_fmatt-description      = lst_to_create-Description.
                lst_fmatt-wo_num          = lst_to_create-ObjectNum.
                lst_fmatt-order_type      = lst_to_create-OrderType.
                lst_fmatt-vornr               = lst_to_create-OperationNum.
                lst_fmatt-equnr              = lst_to_create-Equipment.
                lst_fmatt-tplnr                = lst_to_create-FunctionalLoc.
                lst_fmatt-imagedata      = lst_to_create-ImageData.
                lst_fmatt-questionid      = lst_to_create-QuestionId.
                lst_fmatt-created_on     = lst_to_create-CreatedOn.
                lst_fmatt-created_by     = lst_to_create-CreatedBy.
                lst_fmatt-modified_on   = lst_to_create-ModifiedOn.
                lst_fmatt-modified_by    = lst_to_create-ModifiedBy.
                lst_fmatt-int_data_points  = lst_to_create-IntDataPoints.
            ENDLOOP. "/LOOP AT roots_to_create INTO DATA(lst_to_create).

            if lst_fmatt IS NOT INITIAL.
             if lst_fmatt-created_by IS INITIAL.
                lst_fmatt-created_by = sy-uname.
             endif. "/if lst_fmatt-created_by IS INITIAL.
                Clear: lv_tempid, lv_var.
                lv_tempid   = lst_to_create-InstanceId.
                CALL METHOD me->get_cloud_dest
                      IMPORTING
                        ex_dest = DATA(lv_rfc) .
                if lst_to_create-ObjectNum IS NOT INITIAL.
*                     DATA(lv_top) = me->lr_request->get_paging( )->get_page_size( ).
*                     DATA(lv_skip) = me->lr_request->get_paging(  )->get_offset(  ).

                     lit_fields = VALUE #( ( fieldname = 'AUFNR' ) ( fieldname = 'AUART' ) ).                                                     .
                     lit_option = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_to_create-ObjectNum }| & |'| ) ).
*                     lv_rowskip = lv_skip.
*                     if lv_top > 0.
*                        lv_rowcount = lv_top.
*                     endif. "/ if lv_top > 0.

                     "/Call RFC to get workorder
                     CALL FUNCTION 'RFC_READ_TABLE'
                     DESTINATION lv_rfc
                     EXPORTING
                     query_table = 'AUFK'
                     rowcount     = lv_rowcount
                     rowskips     = lv_rowskip
                     TABLES
                     data       = lit_data
                     fields     = lit_fields
                     options  = lit_option .

                     LOOP AT lit_data INTO DATA(lst_data).
                        lv_wonum = lst_data+0(12).
                        lv_auart    = lst_data+12(15).
                     ENDLOOP. "/LOOP AT lit_data INTO DATA(lst_data).
                     clear:  lv_rowskip, lv_rowcount, lst_data, lit_data.

                     if sy-subrc EQ 0 AND lv_wonum IS NOT INITIAL.
                        lv_wonum = |{ lv_wonum ALPHA = IN }|.
                     else.
*                         lv_top = lr_request->get_paging(  )->get_page_size(  ).
*                         lv_skip = lr_request->get_paging(  )->get_offset(  ).

                         lit_fields  = VALUE #( ( fieldname = 'QMNUM' ) ).
                         lit_option = VALUE #( ( text = |QMNUM| & | | & |EQ| & |'| & |{ lst_to_create-ObjectNum }| & |'| ) ).
*                         lv_rowskip = lv_skip.
*                         if lv_top > 0.
*                           lv_rowcount = lv_top.
*                        endif. "/ if lv_top > 0.

                        "/Call RFC to get Notification
                        CALL FUNCTION 'RFC_READ_TABLE'
                        DESTINATION lv_rfc
                        EXPORTING
                        query_table    = 'QMEL'
                        rowcount        = lv_rowcount
                        rowskips         = lv_rowskip
                        TABLES
                        data                = lit_data
                        fields              = lit_fields
                        options           = lit_option .

                        LOOP AT lit_data INTO lst_data.
                            lv_qmnum = lst_data+0(12).
                        ENDLOOP. "/LOOP AT lit_data INTO lst_data.
                        clear: lv_rowcount, lv_rowskip, lst_data, lit_data.

                        if sy-subrc EQ 0 AND lv_qmnum IS NOT INITIAL.
                            lv_qmnum = |{ lv_qmnum ALPHA = IN }|.
                        endif. "/if sy-subrc EQ 0 AND lv_qmnum IS NOT INITIAL.

                     endif. "/if sy-subrc EQ 0 AND lv_wonum IS NOT INITIAL.

                endif. "/ if lst_to_create-WO_Num IS NOT INITIAL.
                lv_line  = lst_to_create-ImageData.
                SPLIT lst_to_create-FileName AT '.' INTO lv_description lv_filetype.
                lv_desc         = lv_description.
                lv_mimetype = lst_to_create-Mimetype.
                if lv_wonum IS NOT INITIAL.
                    lv_object_key      = lv_wonum.
                    lv_operationnum = lst_to_create-OperationNum.

                    if lv_operationnum IS NOT INITIAL.
                        CONCATENATE lv_object_key lv_operationnum INTO lv_object_key.
                    endif. "/ if lv_operationnum IS NOT INITIAL.

                elseif lv_qmnum IS NOT INITIAL. "/if lv_wonum IS NOT INITIAL.
                    lv_object_key = lv_qmnum.
                endif. "/ if lv_wonum IS NOT INITIAL.
                lv_binary_flag = lc_x.

                lst_signature-doc_count  = 1.
                lst_signature-prop_name = lc_bds_documentclass.
                lst_signature-prop_value = lv_filetype.
                APPEND lst_signature TO lit_signature.

                lst_signature-prop_name  = lc_bds_contrep.
                lst_signature-prop_value = ' '.
                APPEND lst_signature TO lit_signature.

                lst_signature-prop_name  = lc_bds_documenttype.
                lst_signature-prop_value = ' '.
                APPEND lst_signature TO lit_signature.

                lst_signature-prop_name  = lc_description.
                lst_signature-prop_value = lv_desc.
                APPEND lst_signature TO lit_signature.

                lst_signature-prop_name  = lc_language.
                lst_signature-prop_value = sy-langu.
                APPEND lst_signature TO lit_signature.

                lst_uploadonent-doc_count  = 1.
                lst_uploadonent-comp_count = 1.
                lst_uploadonent-mimetype   = lv_mimetype.
                lst_uploadonent-comp_id    = lst_to_create-FileName.
                lst_uploadonent-comp_size  = lst_to_create-FileSize.
                APPEND lst_uploadonent TO lit_component.

                CREATE OBJECT lr_base64_decode.
                "/Convert SCMS Base64 Decode
                CALL METHOD lr_base64_decode->decode_x_base64
                  EXPORTING
                    encoded = lv_line
                  RECEIVING
                    decoded = lv_file_content_bin .


               "/ **************Source code from function module XString_to_Binary*******************/"

               field-symbols:  <binary> TYPE x.


              data: binary_len type i,
                        i type i,
                         pos type i.

              DATA: lit_file_content_binary TYPE TABLE OF /odsmfe/st_bapiconten.


               DATA(bin_size) = xstrlen( lv_file_content_bin ).

               ASSIGN  COMPONENT 1 OF STRUCTURE  lst_file_content_binary TO  <binary> CASTING TYPE x .
               if sy-subrc = 4.
*                  assign component 0 of structure binary_tab to <binary> CASTING TYPE x RANGE 1022.
                  ASSIGN COMPONENT 0 OF STRUCTURE lst_file_content_binary TO <binary> CASTING TYPE x.
               endif.

*                describe field <binary> length binary_len in byte mode.

                DATA(lo_len) = cl_abap_elemdescr=>describe_by_data( <binary> ).

                ADD lo_len->length TO binary_len.
                pos = 0.
                i = ( bin_size + binary_len - 1 ) div binary_len.

                do i times.
                    <binary> = lv_file_content_bin+pos.
                    pos = pos + binary_len.
                    APPEND lst_file_content_binary TO lit_file_content_binary.

             enddo.
"/**********************EOC************************************/"
                 SELECT entitysetname, tabname, field, recordno,
                       field_descr, sign, options, low, high, active
                    FROM /odsmfe/tb_filtr
                    WHERE entitysetname = @lc_entity_set_name
                    AND active = @abap_on
                    INTO CORRESPONDING FIELDS OF TABLE @lit_filter_vals.

                 if sy-subrc EQ 0.
                    SORT lit_filter_vals BY field.
                 endif. "/if sy-subrc EQ 0.
                 READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'BDS_ACTIVE' active = lc_x BINARY SEARCH.
                 if sy-subrc EQ 0.
                        lv_bds_active = lst_filter_vals-low .
                    clear: lst_filter_vals.
                 endif. "/if sy-subrc EQ 0.

                 READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'GOS_ACTIVE' active = lc_x BINARY SEARCH.
                 if sy-subrc EQ 0.
                    lv_gos_active = lst_filter_vals-low.
                    clear: lst_filter_vals.
                 endif. "/if sy-subrc EQ 0.

                 READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'WO_CLASSNAME' active = lc_x BINARY SEARCH.
                 if sy-subrc EQ 0.
                    lv_wo_classname = lst_filter_vals-low.
                    clear: lst_filter_vals.
                 endif. "/ if sy-subrc EQ 0.

                 READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'NO_CLASSNAME' active = lc_x BINARY SEARCH.
                 if sy-subrc EQ 0.
                    lv_no_classname = lst_filter_vals-low.
                    clear: lst_filter_vals.
                 endif. "/if sy-subrc EQ 0.

                 READ TABLE lit_filter_vals INTO lst_filter_vals WITH KEY field = 'SRV_CLASSNAME' active = lc_x BINARY SEARCH.
                 if sy-subrc EQ 0.
                    lv_srv_classname = lst_filter_vals-low.
                    clear: lst_filter_vals.
                 endif. "/if sy-subrc EQ 0.

                 if lv_auart IS NOT INITIAL.
*                    lv_top = lr_request->get_paging(  )->get_page_size(  ).
*                    lv_skip = lr_request->get_paging(  )->get_offset(  ).

                    lit_fields  = VALUE #( ( fieldname = 'SERVICE' ) ).
                    lit_option = VALUE #( ( text = |AUART| & | | & |EQ| & |'| & |{ lv_auart }| & |'| ) ).
*                    lv_rowskip = lv_skip.
*                    if lv_top > 0.
*                       lv_rowcount = lv_top.
*                    endif. "/ if lv_top > 0.

                    "/Call RFC to get Notification
                     CALL FUNCTION 'RFC_READ_TABLE'
                     DESTINATION lv_rfc
                     EXPORTING
                        query_table    = 't350'
                        rowcount        = lv_rowcount
                        rowskips         = lv_rowskip
                     TABLES
                        data                = lit_data
                        fields              = lit_fields
                        options           = lit_option .

                      LOOP AT lit_data INTO lst_data.
                        lv_service = lst_data+0(1).
                      ENDLOOP. "/LOOP AT lit_data INTO lst_data.
                      clear:  lv_rowcount, lv_rowskip, lst_data, lit_data.

                      if sy-subrc EQ 0 AND lv_service = lc_x.
                        lv_classname = lv_srv_classname.
                      else.
                        lv_classname = lv_wo_classname.
                      endif. "/if sy-subrc EQ 0 AND lv_service = lc_x.

                 else. "/if lv_auart IS NOT INITIAL.
                    lv_classname = lv_no_classname.
                 endif. "/ if lv_auart IS NOT INITIAL.

                 if lv_bds_active = lc_x.
                      CALL FUNCTION 'BDS_BUSINESSDOCUMENT_CREA_TAB'
                      DESTINATION lv_rfc
                      EXPORTING
                        classname       = lv_classname
                        classtype       = lc_wo_classtype
                        client             = sy-mandt
                        object_key      = lv_object_key
                        binary_flag     = lv_binary_flag
                      TABLES
                        signature       = lit_signature
                        components  = lit_component
                        content          = lit_file_content_binary
                        ascii_content = lit_file_content_ascii.

                      READ TABLE lit_signature INTO lst_signature INDEX 1.
                      lst_fmatt-doc_id = lst_signature-doc_id.

                 elseif lv_gos_active = lc_x.   "/ if lv_bds_active = lc_x.
                    clear: lv_folder_objid.
                    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
                    DESTINATION lv_rfc
                    EXPORTING
                        region  = lc_b
                    IMPORTING
                        folder_id  = lst_folderid.
                    if sy-subrc <> 0.
                        clear: lst_folderid.
                    endif. "/if sy-subrc <> 0.
                    lst_docdata-obj_name  = lc_message.
                    lst_docdata-obj_descr  = lst_to_create-Description.
                    lst_docdata-obj_langu  = 'EN'.
                    lv_folder_objid             = lst_folderid.

                    CONCATENATE '&SO_FILENAME=' lst_to_create-FileName INTO lst_objhdr-line.
                    APPEND lst_objhdr TO lit_objhdr.
                    lst_objhdr-line = '&SO_FORMAT=BIN'.
                     APPEND lst_objhdr TO lit_objhdr.

                     "/**********************Xstring to Solix Conversion******************/"
                       data: lv_size type i,
                                lv_off type i,
                                 ls_solix type /odsmfe/st_solix,
                                 lv_rows type i,
                                lv_last_row_len type i,
                                 lv_row_len type i.

                       lv_row_len = sy-tleng.
                       lv_size = xstrlen( lv_file_content_bin ).

                      lv_rows = lv_size div lv_row_len.
                      lv_last_row_len = lv_size mod lv_row_len.
                     do lv_rows times.
                        ls_solix-line = lv_file_content_bin+lv_off(lv_row_len).
                        append ls_solix to lit_hexcont.
                        add lv_row_len to lv_off.
                   enddo.
                   if lv_last_row_len > 0.
                        ls_solix-line = lv_file_content_bin+lv_off(lv_last_row_len).
                         append ls_solix to lit_hexcont..
                   endif.

                   "/****************End of Xstring to Solix Conversion**********/"

                     SPLIT lst_to_create-FileName AT '.' INTO lv_name lv_extension.

                     CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
                     DESTINATION lv_rfc
                     EXPORTING
                        folder_id                  = lv_folder_objid
                       document_data              = lst_docdata
                       document_type              = lv_extension
                    IMPORTING
                       document_info              = lv_doc_id
                   TABLES
                       object_header              = lit_objhdr
                       contents_hex               =  lit_hexcont
                  EXCEPTIONS
                     folder_not_exist           = 1
                    document_type_not_exist    = 2
                    operation_no_authorization = 3
                    parameter_error            = 4
                    x_error                    = 5
                    enqueue_error              = 6
                    OTHERS                     = 7.

                    if sy-subrc <> 0.


                    endif. "/ if sy-subrc <> 0.

                    if lv_wonum IS NOT INITIAL.
                        lst_bizojb-objkey = lv_wonum.
                    elseif lv_qmnum IS NOT INITIAL.  "/if lv_wonum IS NOT INITIAL.
                        lst_bizojb-objkey = lv_qmnum.
                    endif. "/ if lv_wonum IS NOT INITIAL.

                    lst_bizojb-objtype = lv_classname.
                    lst_attachment-objkey = lv_doc_id.
                    lst_attachment-objtype = lc_message.

                    CALL FUNCTION 'BINARY_RELATION_CREATE'
                    DESTINATION lv_rfc
                    EXPORTING
                       obj_rolea    = lst_bizojb
                       obj_roleb    = lst_attachment
                      relationtype = lc_relation_type.

                   lst_fmatt-doc_id = lv_doc_id.

                 endif. "/ if lv_bds_active = lc_x.
                 MODIFY /odsmfe/tb_fmatt FROM @lst_fmatt.

            endif. "/if lst_fmatt IS NOT INITIAL.
            clear: lst_fmatt, lv_wonum.

        endif. "/if roots_to_create IS NOT INITIAL.

         "/******************Handle Update Method*****************************/"
         if roots_to_update IS NOT INITIAL.
            LOOP AT roots_to_update INTO DATA(lst_to_update).
                if lst_to_update IS NOT INITIAL.
                    lst_fmatt-instanceid       = lst_to_update-InstanceId.
                lst_fmatt-formid             = lst_to_update-FormId.
                lst_fmatt-version            = lst_to_update-Version.
                lst_fmatt-doc_count       = lst_to_update-AttachCounter.
                lst_fmatt-file_name        = lst_to_update-FileName.
                lst_fmatt-mimetype        = lst_to_update-MimeType.
                lst_fmatt-description      = lst_to_update-Description.
                lst_fmatt-wo_num          = lst_to_update-ObjectNum.
                lst_fmatt-order_type      = lst_to_update-OrderType.
                lst_fmatt-vornr               = lst_to_update-OperationNum.
                lst_fmatt-equnr              = lst_to_update-Equipment.
                lst_fmatt-tplnr                = lst_to_update-FunctionalLoc.
                lst_fmatt-imagedata      = lst_to_update-ImageData.
                lst_fmatt-questionid      = lst_to_update-QuestionId.
                lst_fmatt-created_on     = lst_to_update-CreatedOn.
                lst_fmatt-created_by     = lst_to_update-CreatedBy.
                lst_fmatt-modified_on   = lst_to_update-ModifiedOn.
                lst_fmatt-modified_by    = lst_to_update-ModifiedBy.
                lst_fmatt-int_data_points  = lst_to_update-IntDataPoints.
                endif. "/if lst_to_update IS NOT INITIAL.
                MODIFY /odsmfe/tb_fmatt FROM @lst_fmatt.

                DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo(  ).

                if lst_to_update-ObjectNum IS NOT INITIAL.
*                    lv_top = lr_request->get_paging(  )->get_page_size(  ).
*                    lv_skip = lr_request->get_paging(  )->get_offset(  ).

                     lit_fields = VALUE #( ( fieldname = 'AUFNR' ) ).
                     lit_option = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_to_update-ObjectNum }| & |'| ) ).
*                     lv_rowskip = lv_skip.
*                     if lv_top > 0.
*                        lv_rowcount = lv_top.
*                     endif. "/ if lv_top > 0.
                    CALL METHOD me->get_cloud_dest
                       IMPORTING
                         ex_dest = lv_rfc .

                     "/Call RFC to get workorder
                     CALL FUNCTION 'RFC_READ_TABLE'
                     DESTINATION lv_rfc
                     EXPORTING
                     query_table = 'AUFK'
*                     rowcount     = lv_rowcount
*                     rowskips     = lv_rowskip
                     TABLES
                     data       = lit_data
                     fields     = lit_fields
                     options  = lit_option .

                     LOOP AT lit_data INTO lst_data.
                        lv_wonum = lst_data+0(12).
                     ENDLOOP. "/LOOP AT lit_data INTO DATA(lst_data).
                     clear:  lv_rowskip, lv_rowcount, lst_data, lit_data.

                     if sy-subrc EQ 0 AND lv_wonum IS NOT INITIAL.
                        if lr_exchtab IS BOUND.
                            lr_exchtab->exch_table_update( lst_to_update-ObjectNum ).
                        endif. "/if lr_exchtab IS BOUND.

                     endif. "/if sy-subrc EQ 0 AND lv_wonum IS NOT INITIAL.

                endif. "/if lst_to_update IS NOT INITIAL.
                clear: lst_fmatt.

            ENDLOOP. "/LOOP AT roots_to_update INTO DATA(lst_to_update).

         endif. "/if roots_to_update IS NOT INITIAL.

         "/**********************Handle Delete Method************************/"
         if roots_to_delete IS NOT INITIAL.
            LOOP AT roots_to_delete INTO DATA(lst_to_delete).
                if lst_to_delete-InstanceId IS NOT INITIAL.
                    SELECT SINGLE instanceid
                        FROM /odsmfe/tb_fmatt
                        WHERE instanceid = @lst_to_delete-InstanceId
                        AND      formid       = @lst_to_delete-FormId
                        AND      version     = @lst_to_delete-Version
                        AND      doc_count = @lst_to_delete-AttachCounter
                        INTO @data(lv_instanceid).

                     if sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
                        lst_fmatt-instanceid       = lst_to_delete-InstanceId.
                       lst_fmatt-formid             = lst_to_delete-FormId.
                       lst_fmatt-version            = lst_to_delete-Version.
                       lst_fmatt-doc_count       = lst_to_delete-AttachCounter.

                       if lst_fmatt IS NOT INITIAL.
                            DELETE /odsmfe/tb_fmatt FROM @lst_fmatt.
                       endif. "/if lst_fmatt IS NOT INITIAL.
                     endif. "/if lv_instanceid IS NOT INITIAL.

                endif. "/ if lst_to_delete-InstanceId IS NOT INITIAL.

            ENDLOOP. "/ LOOP AT roots_to_delete INTO DATA(lst_to_create).

         endif. "/if roots_to_delete IS NOT INITIAL.

    ENDMETHOD. "/ METHOD modify



    METHOD read.
    ENDMETHOD.

ENDCLASS.
