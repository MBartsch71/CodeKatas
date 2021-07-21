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
  IF go_queue IS NOT BOUND.
    go_queue = NEW #( 2 ).
  ENDIF.

  CASE save_okcode.
    WHEN 'SEND_MSG'.
      gv_sender_count = gv_sender_count + 1.
      DATA(lo_sender) = NEW lcl_sender( NEW lcl_item( gv_sender_count ) ).
      SET HANDLER go_queue->enqueue FOR lo_sender.
      lo_sender->lif_sender~send( ).

    WHEN 'READ_MSG'.
      DATA(lo_receiver) = NEW lcl_receiver( ).
      SET HANDLER go_queue->dequeue FOR lo_receiver.
      lo_receiver->lif_receiver~receive( ).
    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
