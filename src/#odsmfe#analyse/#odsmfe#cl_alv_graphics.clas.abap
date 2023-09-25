class /ODSMFE/CL_ALV_GRAPHICS definition
  public
  create private .

public section.
protected section.
*" protected components of class CL_ALV_GRAPHICS
*" do not include other source files here!!!

private section.
*"* private components of class /ODSMFE/CL_ALV_GRAPHICS
*"* do not include other source files here!!!

  data MR_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MR_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MR_ID_AT_DC type I .
  data MR_DC type ref to LCL_DC_PREST .
  data MT_OUTTAB type ref to DATA .
  data M_PARENT type ref to CL_GUI_CONTAINER .
  data MT_FIELDCATALOG type LVC_T_FCAT .
  data MT_COLUMNS type LVC_T_FNAM .
  data MS_VARIANT type DISVARIANT .
  data MT_ROWS type LVC_T_ROID .
  data MR_CU type ref to CL_ALV_GRAPHICS_CU .
  data MR_GP type ref to CL_GUI_GP_PRES .
  data M_CU_GUID type GUID_32 .
  data MT_XFIELDNAMES type LVC_T_FNAM .
  data MT_YFIELDNAMES type LVC_T_FNAM .
  data M_XVALUES type I .
  data MR_DIALOGBOX_CONTAINER type ref to CL_GUI_DIALOGBOX_CONTAINER .

  methods HANDLE_TOOLBAR_SELECT_FC
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  class-methods CONVERT_CURRFIELD
    importing
      !I_CURRFIELD type ANY
      !I_CURRKEY type SYCURR
    exporting
      !E_FLOAT type FLOAT .
  methods HANDLE_DBOX_ON_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  methods HANDLE_ON_CUSTOM_CHANGED
    for event GRAPHIC_CUSTOM_CHANGED of IF_GRAPHIC_PROXY .
  methods FILL_DATA_CONTAINER
    exceptions
      ERROR_INSERT_VALUES .
  type-pools GFW .
ENDCLASS.



CLASS /ODSMFE/CL_ALV_GRAPHICS IMPLEMENTATION.


method CONSTRUCTOR.
* Y9DK024447 150402 event customizing changed no longer registered as
*                   application event


data: ACC_text type CHAR40.  "Y9CK054116

  ACC_text = text-001.       "Y9CK054116


* set attributes
  m_parent           = i_parent.
  mt_fieldcatalog    = it_fieldcatalog.
  get reference of it_outtab into mt_outtab.
  mt_columns         = it_columns.
  mt_rows            = it_rows.

* no container specified and not in web mode ==> create dialogbox
  if ( m_parent is initial ) and ( i_web_mode is initial ).
    data: l_repid      type sy-repid,
          l_dynnr      type sy-dynnr.

    l_repid = sy-repid.
    l_dynnr = sy-dynnr.
    create object mr_dialogbox_container
         exporting
             caption = ACC_text    "Y9CK054116
             width  = 700
             height = 300
         exceptions
             others = 1.

    if sy-subrc <> 0.
      raise error_create_dialogbox.
    endif.

    call method mr_dialogbox_container->set_visible
            exporting visible = '1'.
    m_parent = mr_dialogbox_container.
*   set event receiver
    set handler me->handle_dbox_on_close for mr_dialogbox_container.
  endif.

* Y6DK049074>>>>>>>>>>>>>
  CREATE OBJECT mr_splitter
       EXPORTING
            parent     = m_parent
*             shellstyle = m_style
            rows       = 2
            columns    = 1
       EXCEPTIONS
           cntl_error        = 1
           cntl_system_error = 2.
  IF sy-subrc NE 0.

  ENDIF.

  DATA: l_graphic_parent TYPE REF TO cl_gui_container,
        l_toolbar_parent TYPE REF TO cl_gui_container.

  CALL METHOD mr_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = l_graphic_parent.
  CALL METHOD mr_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = l_toolbar_parent.

  call method mr_splitter->set_row_mode
    exporting
      mode = mr_splitter->mode_relative.

  CALL METHOD mr_splitter->set_row_sash
    EXPORTING
      id    = 1
      type  = mr_splitter->type_movable
      value = mr_splitter->false.

  call method mr_splitter->set_row_sash
      exporting
        id    = 1
        type  = mr_splitter->type_sashvisible
        value = mr_splitter->false.

  call method mr_splitter->set_row_height
        exporting
          id     = 2
          height = 5.
* <<<<<<<<<<Y6DK049074
* create and initialize data container
  data: lr_dc_manage type ref to if_dc_management,
        l_retval     type symsgno.
  create object mr_dc.
  if sy-subrc <> 0.
    raise error_create_dc.
  endif.

  lr_dc_manage = mr_dc.
  call method lr_dc_manage->init
          importing
              id     = mr_id_at_dc
              retval = l_retval.
  if l_retval <> cl_gfw=>ok.
    raise error_init_dc.
  endif.

* create and initialize graphics proxy
  data: lt_eventcode type cntl_simple_events,
        l_eventcode  type cntl_simple_event,
        l_prod_id    type TGPN-PRODUKT.

  create object mr_gp.

  l_eventcode-appl_event = ' '.  " correction Y9DK024447
  l_eventcode-eventid    = if_graphic_proxy=>co_evt_custom_changed.
  append l_eventcode to lt_eventcode.

  if i_web_mode is initial.
    l_prod_id = cl_gui_gp_pres=>co_prod_chart.
  else.
    l_prod_id = cl_gui_gp_pres=>CO_PROD_EXPORT.
  endif.

  call method mr_gp->if_graphic_proxy~init
          exporting
             dc           = mr_dc
             parent       = l_graphic_parent  "Y6DK049074
             prod_id      = l_prod_id
             evtcode_list = lt_eventcode
         importing
             retval       = l_retval.

*<<<Y6BK069722 ACC mode
field-symbols: <r_alv_grid> type ref to cl_gui_control.

  read table m_parent->children index 1 assigning <r_alv_grid>.

  call method cl_gui_control=>set_focus
          exporting CONTROL = <r_alv_grid>.
*>>>Y6BK069722

  if l_retval <> cl_gfw=>ok.
    call method mr_gp->if_graphic_proxy~free.
    clear mr_gp.
    raise error_init_gp.
  endif.

* enable interactive change of customizing objects
  set handler me->handle_on_custom_changed for mr_gp.

* create mapping between dc and gp data fields
  call method mr_gp->set_dc_names
          exporting obj_id    = 'OBJID'
                    dim1      = 'X_VAL'
                    dim2      = 'Y_VAL'
                    grp_id    = 'GRPID'
                    objref_id = 'CU_REFOBJ'
                    t_dim1    = 'T_X_VAL'
                    t_grp_id  = 'T_GRPID'
          importing retval    = l_retval.

* fill data container and set customizing
  call method me->refresh
          exporting i_cu_guid       = i_cu_guid
                    it_fieldcatalog = it_fieldcatalog
                    it_outtab       = it_outtab
                    it_columns      = it_columns
                    it_rows         = it_rows
                    it_properties_appl = it_properties  "Y6BK084562
          exceptions others         = 1.
  if sy-subrc <> 0.
    raise error_fill_dc.
  endif.

* activate graphics proxy
  if l_retval = cl_gfw=>ok.
    call method mr_gp->if_graphic_proxy~activate
             importing retval = l_retval.
    if l_retval <> cl_gfw=>ok.
      call method cl_gfw=>show_msg
              exporting msgno = l_retval.
    endif.
  endif.

* activate mux (handling of external graphics and synchronization)
  call method cl_gfw_mux=>activate
         importing retval = l_retval.
  if l_retval <> cl_gfw=>ok.
    call method cl_gfw=>show_msg exporting msgno = l_retval.
  endif.

* Y6DK049074 >>>>>>>>>>>
  include <icon>.

  CREATE OBJECT mr_toolbar
    EXPORTING
      parent = l_toolbar_parent.

  mr_toolbar->set_visible( '1' ).

  cl_gui_control=>set_focus( mr_toolbar ).

  DATA: l_align TYPE i,
        l_info type iconquick,
        l_text type text40.

  l_align = mr_toolbar->align_at_left.
  l_align = l_align + mr_toolbar->align_at_bottom.
  l_align = l_align + mr_toolbar->align_at_right.
  CALL METHOD mr_toolbar->set_position
    EXPORTING
      height = 13
      left   = 0
      top    = 2
      width  = 300.
  CALL METHOD mr_toolbar->set_alignment
    EXPORTING
      alignment = l_align.

  l_info = text-002.
  l_text = text-003.

*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = '&CLOSE'
*      icon      = ICON_okay
*      butn_type = 0
*      text      = l_text
*      quickinfo = l_info.

*   register events for toolbar
    data: lt_event type cntl_simple_events,
    ls_event type cntl_simple_event.

    ls_event-eventid = mr_toolbar->m_id_function_selected.
    append ls_event to lt_event.
    call method mr_toolbar->set_registered_events
      exporting
        events = lt_event.

    set handler me->handle_toolbar_select_fc for mr_toolbar.
*<<<<<<<<<<<Y6DK049074

endmethod.


method CONVERT_CURRFIELD .
* ...
  statics: lt_tcurx type standard table of tcurx,
           ls_tcurx type tcurx,
           l_flag_tcurx_read,
           l_factor type float.

  if i_currkey is initial.
    l_factor = 1.
  elseif i_currkey <> ls_tcurx-currkey.
    if l_flag_tcurx_read is initial.
      l_flag_tcurx_read = 'X'.
      select * from tcurx into table lt_tcurx.
      sort lt_tcurx by currkey.
    endif.
    read table lt_tcurx into ls_tcurx
                        with key currkey = i_currkey
                        binary search.
    if sy-subrc <> 0. "no entry in TCURX ==> set default
      ls_tcurx-currkey = i_currkey.
      ls_tcurx-currdec = 2.
    endif.
    l_factor = 10 ** ( 2 - ls_tcurx-currdec  ).
  endif.

  e_float = i_currfield * l_factor.

endmethod.


method DEFAULT_TITLES .

  data: l_column type lvc_fname,
        l_tabix  type sy-tabix.
  field-symbols: <fcat> type lvc_s_fcat.

  clear: e_xtitle, e_ytitle.

* set title X-axis
  if e_xtitle is requested.
    loop at it_xfieldnames into l_column.
      l_tabix = sy-tabix.
      read table it_fieldcatalog assigning <fcat>
                 with key fieldname = l_column.
      if sy-subrc <> 0. continue. endif.
      if l_tabix = 1.
        e_xtitle = <fcat>-seltext.
      else.
        concatenate e_xtitle '/' <fcat>-seltext into e_xtitle.
      endif.
    endloop.
  endif.

* set title Y-axis
  if e_ytitle is requested.
    loop at it_yfieldnames into l_column.
      l_tabix = sy-tabix.
      read table it_fieldcatalog assigning <fcat>
                 with key fieldname = l_column.
      if sy-subrc <> 0. continue. endif.
      if l_tabix = 1.
        e_ytitle = <fcat>-seltext.
      else.
        concatenate e_ytitle '/' <fcat>-seltext into e_ytitle.
      endif.
    endloop.
  endif.

endmethod.


method FILL_DATA_CONTAINER.
* preconditions:
* (1) data container exists and is initialized
* (2) X-/Y-fieldnames have been determined

  data: l_xfieldname    type lvc_fname,
        l_yfieldname    type lvc_fname,
        l_retval        type symsgno.
  field-symbols: <fcat> like line of mt_fieldcatalog.

* clear data container
  call method mr_dc->if_dc_access~clear
          exporting
                id     = mr_id_at_dc
          importing
                retval = l_retval.
  clear m_xvalues.

* for better performance: set index table to connect each x/y-fieldname
* with the corresponding enty in the fieldcatalog. This is used to
* format the x/y-values.
  data: lt_x_fcat_index type standard table of sy-tabix,
        lt_y_fcat_index type standard table of sy-tabix.
  loop at mt_xfieldnames into l_xfieldname.
    read table mt_fieldcatalog with key fieldname = l_xfieldname
                               transporting no fields.
    append sy-tabix to lt_x_fcat_index.
  endloop.
  loop at mt_yfieldnames into l_yfieldname.
    read table mt_fieldcatalog with key fieldname = l_yfieldname
                               transporting no fields.
    append sy-tabix to lt_y_fcat_index.
  endloop.

  field-symbols: <line>        type any,
                 <tab>         type table,
                 <tab2>        type table,
                 <row_id>      type lvc_s_roid,
                 <x>           type any,
                 <y>           type any,
                 <curr>        type any.

  data: l_ref_tab2   type ref to data,
        ls_data      type gfwdcprest,
        l_x_val      type gfwdcprest-x_val value '0',
        l_t_x_val    type gfwdcprest-t_x_val,
        l_objid      type gfwdcprest-objid value '0000',
        l_external   type lvc_value,
        l_index      type sy-tabix,
        l_fcat_index type sy-tabix,
        l_number(2)  type n.
  data: lt_data type standard table of gfwdcprest.

  assign mt_outtab->* to <tab>.

* process lines of data table
  loop at mt_rows assigning <row_id>.
    l_index = sy-tabix.
    read table <tab> index <row_id>-row_id
                     assigning <line>.
    if sy-subrc <> 0. continue. endif.
    add 1 to m_xvalues.
    l_x_val = l_index. "line number represents x-value (category)
    if mt_xfieldnames is initial.
      l_t_x_val(5) = l_index.
    else.
*     set X-value (text) by concatenating the X-fields
      clear l_t_x_val.
      loop at mt_xfieldnames into l_xfieldname.
        l_index = sy-tabix.
        assign component l_xfieldname of structure <line> to <x>.
        read table lt_x_fcat_index index l_index
                                   into l_fcat_index.
        read table mt_fieldcatalog index l_fcat_index
                                   assigning <fcat>.
        call method cl_gui_alv_grid=>cell_display
          exporting is_data     = <line>
                    i_int_value = <x>
          importing e_ext_value = l_external
          changing  cs_fieldcat = <fcat>.
        shift l_external left deleting leading space. "Y9CK027127

        if not l_t_x_val is initial.
          concatenate l_t_x_val l_external into l_t_x_val
                                           separated by ' '.
        else.
          l_t_x_val = l_external.
        endif.
      endloop.
    endif.
*   set all Y-values
    loop at mt_yfieldnames into l_yfieldname.
      l_index  = sy-tabix.
      l_number = l_index.
      l_objid  = l_objid + 1.
      read table lt_y_fcat_index index l_index
                                 into l_fcat_index.
      read table mt_fieldcatalog index l_fcat_index
                                 assigning <fcat>.

      clear ls_data.
      ls_data-objid   = l_objid.
      ls_data-grpid   = l_number.
      ls_data-x_val   = l_x_val.
      ls_data-t_x_val = l_t_x_val.
      ls_data-t_grpid = <fcat>-seltext.

      assign component l_yfieldname of structure <line> to <y>.
*     currency field ==> convert to float
      if not <fcat>-cfieldname is initial.
        assign component <fcat>-cfieldname of structure <line>
                                           to <curr>.
        data: l_currkey type tcurx-currkey.
        l_currkey = <curr>.
        call method me->convert_currfield
          exporting i_currfield = <y>
                    i_currkey   = l_currkey
          importing e_float     = ls_data-y_val.
*     any other field ==> no conversion needed
      else.
        ls_data-y_val   = <y>.
      endif.

*     append to table for data container
      append ls_data to lt_data.
    endloop.
  endloop.

* set all values into data container
  call method mr_dc->set_obj_tab
         exporting
            objtab    = lt_data
         importing
            retval    = l_retval.
  if l_retval <> cl_gfw=>ok.
    raise error_insert_values.
  endif.

endmethod.


method FREE.
* ...
* free dialogbox => graphic proxy is automatically removed
  if not ( mr_dialogbox_container is initial ).
    call method mr_dialogbox_container->free
            exceptions others = 1.
    if sy-subrc <> 0.
      raise error.
    endif.
* free graphic proxy
  elseif not ( mr_gp is initial ).
    data: l_retval type symsgno.
    call method mr_gp->if_graphic_proxy~free
            importing retval = l_retval.
    if l_retval <> cl_gfw=>ok.
      raise error.
    endif.
  endif.
endmethod.


method GET_ATTRIBUTES.
* Returns attributes which are used for saving the layout

  er_cu      = mr_cu.
  et_columns = mt_columns.
  e_guid     = m_cu_guid.

endmethod.


method GET_CHAR_AND_FIGURES.
* Y9CK037754 280403 Fields which are supplied with a decimals reference
*                   field (decmlfield) are treated as keyfigures.
* Y9CK029138 280602 INT2 and INT1 included as inttype s and b (valid key
*                   figures).
* Y9CK027127 170402 Fields, which are internally numbers but externally
*                   strings (via conversion exit) are treated as
*                   characteristics if no_sum is true.


  data: l_column    type lvc_fname,
        lt_fieldcat type lvc_t_fcat,
        l_tabix     type sy-tabix.
  field-symbols: <fcat> type lvc_s_fcat.

  clear: et_keyfigures, et_characteristics.

* The characteristics / keyfigures have to be returned in the
* right order (defined by the column positions in the fieldcatalog)
  lt_fieldcat = it_fieldcatalog.
  sort lt_fieldcat by col_pos.

  loop at lt_fieldcat assigning <fcat>.
    read table it_columns from <fcat>-fieldname
                          transporting no fields.
    if sy-subrc = 0.
      case <fcat>-inttype.
*       keyfigure (if no_sum is initial)
        when 'P' or 'I' or 'F' or 's' or 'b'.  "Y9CK029138
          if <fcat>-no_sum is initial or
             not <fcat>-decmlfield is initial. "Y9CK037754
            append <fcat>-fieldname to et_keyfigures.
          else.
            append <fcat>-fieldname to et_characteristics.
          endif.
*       characteristic
        when others.
          append <fcat>-fieldname to et_characteristics.
      endcase.
    endif.
  endloop.

endmethod.


method GET_PICTURE.
* ...
  call method mr_gp->if_graphic_proxy~export
    exporting
      format         = i_format
      width          = i_width
      height         = i_height
      name           = i_name
    importing
      content_type   = e_content_type
      content_length = e_content_length
      content        = e_content
      retval         = e_retval
      .
endmethod.


method GET_PROPERTY .
* ...
  call method mr_cu->get_property
    exporting i_id    = i_id
    importing e_value = e_value.
endmethod.


method GET_XY_FIELDS .
* Determine the X- and Y-fields by analysing the selected columns.
* (1) Separate characteristics and keyfigures of the selected columns
* (2) If no characteristics or no keyfigures are selected, set default
*     for X- and Y-fields.

  data: lt_columns  type lvc_t_fnam,
        l_column    type lvc_fname.
  field-symbols: <fcat> type lvc_s_fcat.

  clear: et_xfieldnames, et_yfieldnames.

* separate characteristics and keyfigures of the selected columns
  call method cl_alv_graphics=>get_char_and_figures
          exporting
              it_columns         = it_columns
              it_fieldcatalog    = it_fieldcatalog
          importing
              et_characteristics = et_xfieldnames
              et_keyfigures      = et_yfieldnames.

* no characteristics selected ==> use all displayed key fields
  if et_xfieldnames is initial.
    loop at it_fieldcatalog assigning <fcat>
            where tech    is initial and
                  no_out  is initial and
                  not key is initial.
      append <fcat>-fieldname to lt_columns.
    endloop.
*   sort by position of these columns
    call method cl_alv_graphics=>get_char_and_figures
            exporting
                it_columns         = lt_columns
                it_fieldcatalog    = it_fieldcatalog
            importing
                et_characteristics = et_xfieldnames.
  endif.

* no keyfigures selected ==> use all displayed keyfigures
  if et_yfieldnames is initial.
*   use sum fields (if any exist)
    loop at it_fieldcatalog assigning <fcat>
            where tech   is initial and
                  no_out is initial and
                  not ( do_sum is initial ).
      append <fcat>-fieldname to lt_columns.
    endloop.
*   there are no sum fields ==> search for any keyfigures
    if sy-subrc <> 0.
      loop at it_fieldcatalog assigning <fcat>
              where tech   is initial and
                    no_out is initial.
        append <fcat>-fieldname to lt_columns.
      endloop.
    endif.
*   sort by position of these columns
    call method cl_alv_graphics=>get_char_and_figures
            exporting
                it_columns         = lt_columns
                it_fieldcatalog    = it_fieldcatalog
            importing
                et_keyfigures      = et_yfieldnames.

*  Es werden nur die ersten 99  spalten angezeigt
   Delete et_yfieldnames from 100.
   If sy-subrc = 0.
     e_yfieldnames_deleted = 'X'.
   Endif.

  endif.

endmethod.


method HANDLE_DBOX_ON_CLOSE.

* event: close graphics
  raise event onclose.

* free dialogbox
  call method sender->free.

endmethod.


method HANDLE_ON_CUSTOM_CHANGED.
* If chart type is changed, the colors of border lines for data series
* might have to be modified (to black in one case, to the background
* color in the other case).

*  data: l_chart_type type i.        "current chart type
*
** get current chart type
*  call method mr_cu->get_chart_type
*    importing e_value = l_chart_type.
*
** set border lines
*  if l_chart_type between 1 and 7            and
*    not ( m_chart_type between 1 and 7 ).
**      call method l_bundle_display->if_customizing~set
**        exporting attr_id = cl_cu_display_context=>co_bl_clr_plt_id
**                  value   = l_color.
*
*  endif.
*
** set point colors
**  if l_chart_type
*
** set chart type
*  m_chart_type = l_chart_type.

** set CU objects for points if needed
*  data: l_number_series type i.
*  describe table mt_yfieldnames lines l_number_series.
*  call method mr_cu->set_points
*          exporting i_number_series = l_number_series
*                    i_xvalues       = m_xvalues.

endmethod.


method HANDLE_TOOLBAR_SELECT_FC.

  CASE fcode.
    WHEN '&CLOSE'.

      CALL METHOD m_parent->dispatch
        EXPORTING
          cargo             = sy-ucomm
          eventid           = '8'
          is_shellevent     = ''
          IS_SYSTEMDISPATCH = ''
        EXCEPTIONS
          CNTL_ERROR        = 1
          others            = 2.
      IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.



  ENDCASE.

endmethod.


method REFRESH .
  data: l_cu_guid_old       like m_cu_guid,
        lt_yfieldnames_old  like mt_yfieldnames.

  data: xfieldname      type lvc_fname,
        yfieldname      type lvc_fname,
        lt_xfieldnames  type lvc_t_fnam,
        lt_yfieldnames  type lvc_t_fnam,
        l_yfieldnames_deleted type abap_bool,
        l_title         type string,
        l_xtitle        type string,
        l_ytitle        type string,
        lt_columns      like mt_columns,
        l_retval        type symsgno.
  field-symbols: <fcat> like line of mt_fieldcatalog.

* save current values of some global attributes
  l_cu_guid_old      = m_cu_guid.
  lt_yfieldnames_old = mt_yfieldnames.

* set new values into global attributes
  get reference of it_outtab into mt_outtab.
  mt_fieldcatalog = it_fieldcatalog.
  mt_columns      = it_columns.
  mt_rows         = it_rows.
  m_cu_guid       = i_cu_guid.

* determine X-/Y-fields
  call method me->get_xy_fields
          exporting
              it_columns      = mt_columns
              it_fieldcatalog = mt_fieldcatalog
          importing
              et_xfieldnames  = mt_xfieldnames
              et_yfieldnames  = mt_yfieldnames
              e_yfieldnames_deleted = l_yfieldnames_deleted.

  if l_yfieldnames_deleted = 'X'.
    message i661(0K).
  endif.


* determine default titles
  call method me->default_titles
          exporting
              it_fieldcatalog    = mt_fieldcatalog
              it_xfieldnames     = mt_xfieldnames
              it_yfieldnames     = mt_yfieldnames
          importing
              e_xtitle           = l_xtitle.
  data: ls_prop               type dtc_s_tc,
        lt_properties_default type dtc_t_tc.
  ls_prop-prop_id = cl_alv_graphics_cu=>co_propid_x_title.
  ls_prop-prop_val = l_xtitle.
  append ls_prop to lt_properties_default.

* fill data container
  call method me->fill_data_container.

*-----------------------------------------------------------------------
* create/modify CU objects (customizing objects)
*-----------------------------------------------------------------------
  data: l_number_series type i.

* create CU objects: new CU objects have to be loaded or there are no CU
* objects yet.
  if ( m_cu_guid <> l_cu_guid_old ) or ( mr_cu is initial ).
*   remove "old" CU objects from graphic proxy
    if not ( mr_cu is initial ).
      call method mr_cu->remove_from_graphic_proxy
              exporting i_gp = mr_gp.
    endif.
*   create CU objects and add to graphic proxy
    describe table mt_yfieldnames lines l_number_series.
    create object mr_cu
            exporting i_guid          = m_cu_guid
                      i_number_series = l_number_series.
    call method mr_cu->add_to_graphic_proxy
            exporting i_gp = mr_gp
            importing e_retval = l_retval.

*   set default titles
    call method mr_cu->set_properties_default
            exporting it_properties = lt_properties_default.
*   set titles of application
    call method mr_cu->set_properties_appl
            exporting it_properties = it_properties_appl.

*   display titles by analysing the loaded, default and application
*   titles
    call method mr_cu->init_properties.

*   set scrollbar if there are too many categories
    if m_cu_guid is initial.
      if m_xvalues > 5.
        call method mr_cu->bundle_axis1_x->if_customizing~set
                exporting
                  attr_id = cl_cu_axis=>co_visible_categories_auto
                  value   = gfw_false.
        call method mr_cu->bundle_axis1_x->if_customizing~set
                exporting
                  attr_id = cl_cu_axis=>co_visible_categories
                  value   = 5.
      endif.
    endif.

* modify CU objects
  else.
*   other y-fields are selected
    if mt_yfieldnames <> lt_yfieldnames_old.
      describe table mt_yfieldnames lines l_number_series.
      call method mr_cu->set_series
              exporting i_number_series = l_number_series.
    endif.
*   set title = new default title for x-axis if the current one is equal
*   to the old default title
    data: l_xtitle_default type string,
          l_xtitle_current type string.

    call method mr_cu->get_property_default
            exporting i_id    = cl_alv_graphics_cu=>co_propid_x_title
            importing e_value = l_xtitle_default.
    call method mr_cu->get_property
            exporting i_id    = cl_alv_graphics_cu=>co_propid_x_title
            importing e_value = l_xtitle_current.
    if l_xtitle_current = l_xtitle_default.
      call method mr_cu->set_property
              exporting i_id    = cl_alv_graphics_cu=>co_propid_x_title
                        i_value = l_xtitle. "new default title
    endif.
*   set default titles
    call method mr_cu->set_properties_default
            exporting it_properties = lt_properties_default.
  endif.

** set CU objects for points if needed
*  call method mr_cu->set_points
*          exporting i_number_series = l_number_series
*                    i_xvalues       = m_xvalues.

* distribute changes to graphics proxy
  data: lr_dc_manage type ref to if_dc_management.
  lr_dc_manage = mr_dc.

  if not lr_dc_manage is initial.
    call method lr_dc_manage->distribute_changes
            importing
               retval = l_retval.
    if l_retval <> cl_gfw=>ok.
      call method cl_gfw=>show_msg
             exporting msgno = l_retval.
      raise error_distribute_changes.
    endif.
  endif.

endmethod.


method SET_PROPERTY.
* ...
  call method mr_cu->set_property
    exporting i_id    = i_id
              i_value = i_value.
endmethod.
ENDCLASS.
