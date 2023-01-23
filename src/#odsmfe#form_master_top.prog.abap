*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/FORM_MASTER_TOP
*&---------------------------------------------------------------------*
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  /06/2020
* Transport No.          : ES1K901774
* Program Description    :
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
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

TYPES:BEGIN OF gtyt_int_tab1,
        int_txt(10000) TYPE x,
      END OF gtyt_int_tab1.
* Tables & Strctures
DATA : git_int_tab1              TYPE STANDARD TABLE OF gtyt_int_tab1,
       git_upd_tab               TYPE STANDARD TABLE OF /odsmfe/tb_fomst,
       gst_upd_tab               LIKE LINE OF git_upd_tab,
       gst_filetab               TYPE filetable,
       git_authorlist            TYPE STANDARD TABLE OF bapiscts12,
       gst_authorlist            TYPE bapiscts12,
       git_task_list             TYPE TABLE OF bapiscts07,
       gst_task_list             TYPE bapiscts07,
       git_e071                  TYPE STANDARD TABLE OF e071,
       gst_e071                  TYPE e071,
       git_e071k                 TYPE STANDARD TABLE OF e071k,
       gst_e071k                 TYPE e071k,
       git_e070                  TYPE STANDARD TABLE OF e070,
       gst_e070                  TYPE e070,
       git_e07t                  TYPE STANDARD TABLE OF e07t,
       git_xml_data              TYPE TABLE OF smum_xmltb,
       gst_xml_data              TYPE smum_xmltb,
       gst_xml_data1             TYPE smum_xmltb,
       gst_code_group            TYPE bapiqpgr_qpgr,
       gst_code_grp_shorttexttab TYPE bapiqpgr_qpgt,
       git_code_grp_shorttexttab TYPE STANDARD TABLE OF bapiqpgr_qpgt,
       git_qpgt                  TYPE STANDARD TABLE OF qpgt,
       gst_qpgt                  TYPE qpgt,
       git_qpgt1                 TYPE STANDARD TABLE OF qpgt,
       gst_temp                  TYPE qpgt,
       gst_codes_of_code_grp     TYPE bapiqpgr_qpcd,
       git_codes_of_code_grp     TYPE STANDARD TABLE OF bapiqpgr_qpcd,
       gst_code_shorttexttab     TYPE bapiqpgr_qpct,
       git_code_shorttexttab     TYPE STANDARD TABLE OF bapiqpgr_qpct,
       git_code_longtexttab      TYPE STANDARD TABLE OF bapiqpgr_cd_ltxt,
       gst_code_longtexttab      TYPE bapiqpgr_cd_ltxt,
       gst_xml_tag               TYPE /odsmfe/tb_apcon.

DATA: git_list  TYPE vrm_values,
      gst_list  TYPE vrm_value,
      git_list1 TYPE vrm_values. "++

DATA: git_values TYPE TABLE OF dynpread,
      gst_values TYPE dynpread.

DATA : git_e071_temp  TYPE TABLE OF /odsmfe/form_tr,
       git_e071k_temp TYPE TABLE OF /odsmfe/formtask.

FIELD-SYMBOLS : <gfsst_e071k>      TYPE e071k,
                <gfsst_e071>       TYPE e071,
                <gfsst_e071k_temp> TYPE /odsmfe/formtask,
                <gfsst_e071_temp>  TYPE /odsmfe/form_tr.

* Class
DATA :  go_ref_xml         TYPE REF TO cl_xml_document.

DATA :  go_ref_xsltp        TYPE REF TO cl_xslt_processor,
        gv_doc_form_string  TYPE string,
        go_doc_form_string  TYPE REF TO string,
        gv_doc_model_string TYPE string,
        go_doc_model_string TYPE REF TO string,
        gv_msg_count        TYPE i,
        gv_xml_form_string  TYPE xstring,
        gv_xml_model_string TYPE xstring,
        gv_selected_value   TYPE char30.

* Variables
DATA :                gv_subrc          TYPE sy-subrc , " ##NEEDED 701-nshyamala,
                      gv_xml_string     TYPE xstring,
                      gv_filerc         TYPE i,
                      gv_size           TYPE sytabix , " ##NEEDED 701-nshyamala,
                      gv_type           TYPE string,
                      gv_message        TYPE bapi_msg,
                      gv_filename       TYPE string,
                      git_return        TYPE TABLE OF bapiret2,
                      gst_return        TYPE bapiret2,
                      gv_localfilename  TYPE localfile,
                      gv_formid         TYPE /odsmfe/de_formid,
                      gv_code_group     TYPE qcodegrp,
                      gv_desc           TYPE qktextgr,
                      gv_read           TYPE char3,
*                     lv_create         TYPE flag,                                     "Flag field
                      gv_max_no         TYPE char2,
                      gv_num            TYPE char2,
                      gv_group          TYPE char255,
                      gv_code           TYPE qcode,
                      gv_version        TYPE /odsmfe/de_version,
                      gv_ver            TYPE /odsmfe/de_version,
                      gv_shorttext      TYPE qktextgr , " ##NEEDED 701-nshyamala,
                      gv_codegruppe     TYPE qcodegrp , "##NEEDED 701-nshyamala,
                      gv_counter        TYPE num4,
                      gv_description    TYPE e07t-as4text,
                      gv_transport_kind TYPE e070-trfunction,
                      gv_langu          TYPE sy-langu,
                      gv_requestid      TYPE bapiscts01-requestid,
                      gst_commfile      TYPE e070-trkorr,
                      gv_createdon      TYPE timestamp,
                      gv_changedon      TYPE timestamp,
                      gv_wi_trkorr      TYPE e070-trkorr,
                      gv_do             TYPE string,
                      gv_tabix          TYPE sy-tabix,
                      gv_text           TYPE string,
                      gv_theme          TYPE char255,
                      gv_mesg           TYPE REF TO cx_root,
                      gv_xslt_html      TYPE progname,
                      gv_xslt_model     TYPE progname,
                      gv_mesg1          TYPE string,
                      gv_msg            TYPE string,
                      gv_xml_class      TYPE string,
                      gv_xml_meth       TYPE string,
                      gv_auart          TYPE aufk-auart,
                      gv_steus          TYPE afvc-steus,
                      gv_eqtyp          TYPE equi-eqtyp, "++
                      gv_fltyp          TYPE iflot-fltyp, "++
                      gv_eqart          TYPE equi-eqart, "++
*   SOC by ODS
                      gv_plnty          TYPE afko-plnty,
                      gv_plnnr          TYPE afko-plnnr,
                      gv_plnal          TYPE afko-plnal,
                      gv_zaehl          TYPE afko-zaehl,
*   EOC by ODS
                      gv_xml_cb         TYPE char1,
                      gv_cvalue         TYPE char255,
                      gv_cvalue1        TYPE char255.

* Constants
CONSTANTS: gc_wild                TYPE char1 VALUE '%',
           gc_zero                TYPE char1 VALUE '0',
           gc_zero1               TYPE char3 VALUE '00',
           gc_version             TYPE char3 VALUE '000',
           gc_katalog_b           TYPE qkatart VALUE 'B',
           gc_num                 TYPE char2 VALUE '00',
           gc_title               TYPE string VALUE 'Select the File',
           gc_filetype            TYPE char10 VALUE 'BIN',
           gc_transport_kind      TYPE e070-trfunction VALUE 'K',
           gc_e                   TYPE char1 VALUE 'E',
           gc_trstatus            TYPE trstatus VALUE 'R',
           gc_rel                 TYPE qloeschkz1 VALUE '2',
           gc_cat_type            TYPE qkatart VALUE 'A',
           gc_status              TYPE qloeschkz1 VALUE '2',
           gc_pgmid               TYPE pgmid VALUE 'R3TR',
           gc_cdat                TYPE trobjtype VALUE 'CDAT',
           gc_tabu                TYPE trobjtype VALUE 'TABU',
           gc_v_qpgr_cl           TYPE tabname VALUE 'V_QPGR_CL',
           gc_/odsmfe/form_master TYPE tabname VALUE '/ODSMFE/TB_FOMST',
           gc_qpcd                TYPE tabname VALUE 'QPCD',
           gc_qpct                TYPE tabname VALUE 'QPCT',
           gc_qpgr                TYPE tabname VALUE 'QPGR',
           gc_qpgt                TYPE tabname VALUE 'QPGT',
           gc_objfunc             TYPE updid VALUE 'K',
           gc_activity            TYPE tractivity VALUE 'SIMG_CMMENUOLQNOQNS1',
           gc_cdat1               TYPE trobjtype VALUE 'CDAT',
           gc_mastername1         TYPE sobj_name VALUE 'V_QPGR_CL',
           gc_viewname1           TYPE viewname VALUE 'V_QPCD_KAT',
           gc_viewname2           TYPE viewname VALUE 'V_QPGR_KAT',
           gc_ignore              TYPE string VALUE '_IGNORE',
           gc_a                   TYPE string VALUE 'A',
           gc_ref                 TYPE string VALUE 'ref',
           gc_end                 TYPE string VALUE 'end',
           gc_group               TYPE string VALUE '_group',
           gc_p                   TYPE string VALUE 'P',
           gc_x                   TYPE char01 VALUE 'X',
           gc_s                   TYPE string VALUE 'S',
           gc_eq                  TYPE string VALUE 'EQ',
           gc_i                   TYPE string VALUE 'I',
           gc_formmasterset       TYPE tabletext VALUE 'FormMasterSet',
           gc_xml_model           TYPE char30 VALUE 'XML_MODEL',
           gc_xml_html            TYPE char30 VALUE 'XML_HTML',
           gc_abap_on             TYPE abap_bool VALUE 'X',
           gc_num6                TYPE char6 VALUE '000001',
           gc_p_asinmt            TYPE  vrm_id VALUE 'P_ASINMT', "
           gc_workorder           TYPE vrm_value-text VALUE 'WORKORDER', "
           gc_operation           TYPE vrm_value-text VALUE 'OPERATION', "
           gc_uc1                 TYPE syucomm VALUE 'UC1', "
           gc_cli1                TYPE syucomm VALUE 'CLI1' , "
           gc_p_xyz               TYPE vrm_id VALUE 'P_XYZ', "++
           gc_equip               TYPE vrm_value-text VALUE 'EQUIPMENT', "++
           gc_floc                TYPE vrm_value-text VALUE 'FUNCTIONALLOC', "++
           gc_tasklist            TYPE vrm_value-text VALUE 'TASK LIST', "++ ES1K902363
           gc_themegrid           TYPE string VALUE 'theme-grid', "++ ES1K902363
           gc_xml_tag             TYPE string     VALUE 'CB_XML_TAG',
           gc_label1              TYPE string     VALUE '-label',
           gc_inst_id             TYPE string     VALUE 'instanceID',
           gc_text                TYPE string     VALUE 'text',
           gc_id                  TYPE string     VALUE 'id',
           gc_value               TYPE string     VALUE 'value'.
