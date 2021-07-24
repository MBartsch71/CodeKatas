*----------------------------------------------------------------------*
***INCLUDE YMBH_BOUNDED_QUEUE_STATUS_1O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_1000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'STATUS_1000'.
  SET TITLEBAR 'TITLE_1000'.

  IF go_gui IS NOT BOUND.
    go_gui = NEW #( ).
    go_textedit_senders   = go_gui->build_textedits( NEW #( container_name = 'CCTRL_SENDERS' ) ).
    go_textedit_messages  = go_gui->build_textedits( NEW #( container_name = 'CCTRL_MESSAGES' ) ).
    go_textedit_receivers = go_gui->build_textedits( NEW #( container_name = 'CCTRL_RECEIVERS' ) ).
  ENDIF.

  IF go_application IS BOUND.
    go_textedit_senders->set_text_as_stream( go_gui->prepare_queue_content( go_application->get_sender_queue( ) ) ).
    go_textedit_messages->set_text_as_stream( go_gui->prepare_queue_content( go_application->get_message_queue( ) ) ).
    go_textedit_receivers->set_text_as_stream( go_gui->prepare_queue_content( go_application->get_receiver_queue( ) ) ).
  ENDIF.

ENDMODULE.
