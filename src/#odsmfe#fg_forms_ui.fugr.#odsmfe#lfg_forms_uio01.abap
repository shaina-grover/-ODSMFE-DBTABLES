*----------------------------------------------------------------------*
***/ODSMFE/LFG_FORMS_UIO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  IF go_dock_container IS BOUND.
    FREE go_alv.
    go_dock_container->free( ).
    FREE go_dock_container.
  ENDIF.
  IF go_dock_container1 IS BOUND.
    FREE go_alv1.
    go_dock_container1->free( ).
    FREE go_dock_container1.
  ENDIF.
  REFRESH:go_control->gvib_model->gitib_final,
  go_control->gvib_model->gitib_foass.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.
