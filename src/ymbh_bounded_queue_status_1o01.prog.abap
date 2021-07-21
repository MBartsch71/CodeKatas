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


  IF go_container IS NOT BOUND.
    go_container = NEW #( container_name = 'CCTRL_MESSAGES' ).

    go_textedit = NEW #( parent = go_container ).
    go_textedit->set_readonly_mode( cl_gui_textedit=>true ).
    go_textedit->auto_redraw( cl_gui_textedit=>false ).
    go_textedit->set_statusbar_mode( 0 ).
    go_textedit->set_toolbar_mode( 0 ).
  ENDIF.

  IF go_queue IS BOUND.
    DATA(lt_queue) = go_queue->get( ).
    gt_outtab = VALUE #( FOR <line> IN lt_queue
                            LET lo_item = CAST lcl_item( <line> )
                                lt_id = COND tt_textline( WHEN lo_item IS BOUND
                                                            THEN VALUE #( ( |{ lo_item->get_id( ) }{ cl_abap_char_utilities=>newline }| )  ) )
                            IN
                            ( LINES OF lt_id ) ).
    go_textedit->set_text_as_stream( text = gt_outtab ).
  ENDIF.
ENDMODULE.
