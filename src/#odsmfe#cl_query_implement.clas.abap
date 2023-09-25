
CLASS /odsmfe/cl_query_implement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_QUERY_IMPLEMENT IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
*--------------------------------------------------------------
    DATA: lo_data_provider TYPE REF TO /odsmfe/if_get_entityset_main,
          lo_filter        TYPE REF TO if_rap_query_filter,
          lo_paging        TYPE REF TO if_rap_query_paging,
          lt_response_data TYPE REF TO data.
*--------------------------------------------------------------
    IF io_request->is_data_requested( ).

      lo_filter = io_request->get_filter( ).

      DATA(lt_filter_select_options) = lo_filter->get_as_ranges( ).
      DATA(lv_filter_string) = lo_filter->get_as_sql_string(  ).
      lo_paging ?= io_request->get_paging( ).

      DATA(lv_entity_set_name) =  io_request->get_entity_id(  ).

          lo_data_provider    ?=  /odsmfe/cl_model_factory=>gosb_obj->/odsmfe/if_model_factory~gmib_get_instance_model(

                             im_entity_set_name = lv_entity_set_name ).

      CREATE DATA lt_response_data TYPE TABLE OF (lv_entity_set_name).
      ASSIGN lt_response_data->* TO FIELD-SYMBOL(<lt_response_data>).

      IF lo_data_provider IS BOUND.

        " Call The Method To Get The Data.

       lo_data_provider->gmib_get_entityset_data(
        EXPORTING
          im_entity_name                 = lv_entity_set_name
          im_entity_set_name             = lv_entity_set_name
          im_filter_select_options       = lt_filter_select_options
          im_paging                      = lo_paging
          im_filter_string               = lv_filter_string
          im_request                     = io_request
        IMPORTING
*          ex_response                      = io_response
          ex_response_data           = <lt_response_data> ).

      ENDIF.

      " Set total no. of records
      IF io_request->is_total_numb_of_rec_requested( ).
        io_response->set_total_number_of_records( lines( <lt_response_data>  ) ).
      ENDIF.

      " Set response data
      io_response->set_data( <lt_response_data> ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
