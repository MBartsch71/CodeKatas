*----------------------------------------------------------------------*
***INCLUDE YMBH_BOUNDED_QUEUE_USER_COMI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  cl_gui_cfw=>dispatch( ).
  DATA(save_okcode) = ok_code.

  IF go_application IS NOT BOUND.
    go_application = NEW #(  ).
  ENDIF.


  CASE save_okcode.
    WHEN 'SEND_MSG'.
      gv_sender_count = gv_sender_count + 1.
      go_application->send_item_to_queue( NEW lcl_item( gv_sender_count ) ).

    WHEN 'READ_MSG'.
      DATA(lo_received_item) = go_application->get_item_from_queue( ).

    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
