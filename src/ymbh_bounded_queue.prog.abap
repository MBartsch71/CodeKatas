REPORT ymbh_bounded_queue.

CLASS lcx_queue DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcl_item DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_id TYPE i.
    METHODS get_id      RETURNING VALUE(rv_id) TYPE i.

  PRIVATE SECTION.
    DATA mv_id TYPE i VALUE 1.

ENDCLASS.

CLASS lcl_item IMPLEMENTATION.

  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.

  METHOD get_id.
    rv_id = mv_id.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui DEFINITION.

  PUBLIC SECTION.
    TYPES tt_queue   TYPE STANDARD TABLE OF REF TO object WITH DEFAULT KEY.
    TYPES tt_content TYPE STANDARD TABLE OF text30 WITH DEFAULT KEY.

    METHODS build_textedits       IMPORTING io_container       TYPE REF TO cl_gui_custom_container
                                  RETURNING VALUE(ro_textedit) TYPE REF TO cl_gui_textedit.

    METHODS prepare_queue_content IMPORTING it_queue         TYPE tt_queue
                                  RETURNING VALUE(rt_outtab) TYPE tt_content.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD build_textedits.
    ro_textedit = NEW #( parent = io_container ).
    ro_textedit->set_readonly_mode( cl_gui_textedit=>true ).
    ro_textedit->auto_redraw( cl_gui_textedit=>false ).
    ro_textedit->set_statusbar_mode( 0 ).
    ro_textedit->set_toolbar_mode( 0 ).
  ENDMETHOD.

  METHOD prepare_queue_content.
    rt_outtab = VALUE #( FOR <line> IN it_queue
                          LET lo_item = CAST lcl_item( <line> )
                              lt_id = COND tt_content( WHEN lo_item IS BOUND
                                                          THEN VALUE #( ( |{ lo_item->get_id( ) }{ cl_abap_char_utilities=>newline }| )  ) )
                          IN
                          ( LINES OF lt_id ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_queue DEFINITION.

  PUBLIC SECTION.
    TYPES tt_queue TYPE STANDARD TABLE OF REF TO object WITH DEFAULT KEY.

    METHODS dequeue   RETURNING VALUE(ro_item) TYPE REF TO object.
    METHODS enqueue   IMPORTING io_item TYPE REF TO object.

    METHODS get_count RETURNING VALUE(rv_count) TYPE i.
    METHODS get       RETURNING VALUE(rt_queue) TYPE lcl_queue=>tt_queue.

  PROTECTED SECTION.
    DATA mt_queue TYPE tt_queue.

    METHODS read_first_item RETURNING VALUE(ro_item) TYPE REF TO object.
    METHODS remove_first_item.

ENDCLASS.

CLASS lcl_queue IMPLEMENTATION.

  METHOD get_count.
    rv_count = lines( mt_queue ).
  ENDMETHOD.

  METHOD get.
    rt_queue = mt_queue.
  ENDMETHOD.

  METHOD enqueue.
    IF io_item IS BOUND.
      APPEND io_item TO mt_queue.
    ELSE.
      DATA(lo_dummy) = NEW lcl_item( 0 ).
      APPEND lo_dummy TO mt_queue.
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    ro_item = read_first_item( ).
    remove_first_item( ).
  ENDMETHOD.

  METHOD read_first_item.
    TRY.
        ro_item = mt_queue[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_queue.
    ENDTRY.
  ENDMETHOD.

  METHOD remove_first_item.
    DELETE mt_queue INDEX 1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_bounded_queue DEFINITION INHERITING FROM lcl_queue.

  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_size TYPE i.

    METHODS enqueue REDEFINITION.
    METHODS dequeue REDEFINITION.

  PRIVATE SECTION.

    METHODS get_next_line_for_storing_item RETURNING VALUE(rv_line) TYPE i.

    METHODS add_item_to_determined_line    IMPORTING iv_line TYPE i
                                                     io_item TYPE REF TO object.

    METHODS exception_at_full_queue        IMPORTING iv_line TYPE i.
    METHODS exception_at_empty_queue.

    METHODS reorganize_queue.

ENDCLASS.

CLASS lcl_bounded_queue IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mt_queue = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = iv_size + 1 (  ) ).
  ENDMETHOD.

  METHOD enqueue.
    DATA(lv_line) = get_next_line_for_storing_item( ).
    exception_at_full_queue( lv_line ).
    add_item_to_determined_line( iv_line = lv_line
                                 io_item = io_item ).
  ENDMETHOD.

  METHOD dequeue.
    exception_at_empty_queue( ).
    ro_item = read_first_item( ).
    reorganize_queue( ).
  ENDMETHOD.

  METHOD get_next_line_for_storing_item.
    DATA lo_item TYPE REF TO object.
    rv_line = line_index( mt_queue[ table_line = lo_item ] ).
  ENDMETHOD.

  METHOD exception_at_full_queue.
    IF iv_line <= 0.
      RAISE EXCEPTION TYPE lcx_queue EXPORTING textid = |Queue full => Request blocked|.
    ENDIF.
  ENDMETHOD.

  METHOD add_item_to_determined_line.
    mt_queue[ iv_line ] = io_item.
  ENDMETHOD.

  METHOD exception_at_empty_queue.
    IF mt_queue[ 1 ] IS INITIAL.
      RAISE EXCEPTION TYPE lcx_queue EXPORTING textid = |Queue empty => Request blocked|.
    ENDIF.
  ENDMETHOD.

  METHOD reorganize_queue.
    remove_first_item( ).
    APPEND INITIAL LINE TO mt_queue.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS send_item_to_queue  IMPORTING io_item          TYPE REF TO object
                                RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS get_item_from_queue RETURNING VALUE(ro_item)   TYPE REF TO object.
    METHODS get_sender_queue    RETURNING VALUE(rt_queue) TYPE lcl_queue=>tt_queue.
    METHODS get_receiver_queue  RETURNING VALUE(rt_queue) TYPE lcl_queue=>tt_queue.
    METHODS get_message_queue   RETURNING VALUE(rt_queue) TYPE lcl_queue=>tt_queue.

  PRIVATE SECTION.
    DATA mo_message_queue  TYPE REF TO lcl_queue.
    DATA mo_sender_queue   TYPE REF TO lcl_queue.
    DATA mo_receiver_queue TYPE REF TO lcl_queue.
    METHODS process_bocked_sender.
    METHODS process_blocked_receiver.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD constructor.
    mo_message_queue  = NEW lcl_bounded_queue( 2 ).
    mo_sender_queue   = NEW lcl_queue( ).
    mo_receiver_queue = NEW lcl_queue( ).
  ENDMETHOD.

  METHOD send_item_to_queue.
    TRY.
        mo_message_queue->enqueue( io_item ).
        rv_result = abap_true.
        process_blocked_receiver( ).
      CATCH lcx_queue.
        mo_sender_queue->enqueue( io_item ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_item_from_queue.
    TRY.
        ro_item = mo_message_queue->dequeue( ).
        process_bocked_sender( ).
      CATCH lcx_queue.
        mo_receiver_queue->enqueue( ro_item ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_sender_queue.
    rt_queue = mo_sender_queue->get( ).
  ENDMETHOD.

  METHOD get_receiver_queue.
    rt_queue = mo_receiver_queue->get( ).
  ENDMETHOD.

  METHOD get_message_queue.
    rt_queue = mo_message_queue->get( ).
  ENDMETHOD.


  METHOD process_bocked_sender.
    TRY.
        DATA(lo_item) = mo_sender_queue->dequeue( ).
        mo_message_queue->enqueue( lo_item ).
      CATCH lcx_queue.
    ENDTRY.
  ENDMETHOD.

  METHOD process_blocked_receiver.
    TRY.
        DATA(lo_item) = mo_receiver_queue->dequeue( ).
        lo_item = mo_message_queue->dequeue( ).
      CATCH lcx_queue.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_item DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_item.
    METHODS get_specific_item_id FOR TESTING.
ENDCLASS.

CLASS ltc_item IMPLEMENTATION.

  METHOD get_specific_item_id.
    mo_cut = NEW #( 2 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = mo_cut->get_id( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_queue.

    METHODS setup.
    METHODS send_request_x_times    IMPORTING iv_times TYPE i.
    METHODS receive_request_x_times IMPORTING iv_times       TYPE i
                                    RETURNING VALUE(ro_item) TYPE REF TO object.

    METHODS add_item_to_queue   FOR TESTING.
    METHODS get_item_from_queue FOR TESTING.
    METHODS get_whole_queue     FOR TESTING.
ENDCLASS.

CLASS ltc_queue IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD send_request_x_times.
    DO iv_times TIMES.
      mo_cut->enqueue( NEW lcl_item( sy-index ) ).
    ENDDO.
  ENDMETHOD.

  METHOD receive_request_x_times.
    DO iv_times TIMES.
      ro_item = mo_cut->dequeue( ).
    ENDDO.
  ENDMETHOD.

  METHOD add_item_to_queue.
    send_request_x_times( 1 ) .
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = mo_cut->get_count( ) ).
  ENDMETHOD.

  METHOD get_item_from_queue.
    send_request_x_times( 2 ).
    DATA(lo_received_item) = CAST lcl_item( receive_request_x_times( 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lo_received_item->get_id( ) ).
  ENDMETHOD.

  METHOD get_whole_queue.
    send_request_x_times( 4 ).

    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = lines( mo_cut->get( ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_bounded_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_bounded_queue.

    METHODS setup.

    METHODS send_requests_x_times    IMPORTING iv_times TYPE i.
    METHODS receive_requests_x_times IMPORTING iv_times       TYPE i
                                     RETURNING VALUE(ro_item) TYPE REF TO object.

    METHODS error_at_queue_size_violation FOR TESTING.
    METHODS get_second_object_from_queue  FOR TESTING.
    METHODS error_at_empty_queue          FOR TESTING.
    METHODS get_stack_after_2_events      FOR TESTING.

ENDCLASS.

CLASS ltc_bounded_queue IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 2 ).
  ENDMETHOD.

  METHOD send_requests_x_times.
    DO iv_times TIMES.
      mo_cut->enqueue( NEW lcl_item( sy-index ) ).
    ENDDO.
  ENDMETHOD.

  METHOD receive_requests_x_times.
    DO iv_times TIMES.
      ro_item = mo_cut->dequeue( ).
    ENDDO.
  ENDMETHOD.

  METHOD error_at_queue_size_violation.
    TRY.
        send_requests_x_times( 3 ).
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error ).
  ENDMETHOD.

  METHOD get_second_object_from_queue.
    send_requests_x_times( 2 ).
    DATA(lo_received_item) = receive_requests_x_times( 2 ).

    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = CAST lcl_item( lo_received_item )->get_id( ) ).
  ENDMETHOD.

  METHOD error_at_empty_queue.
    TRY.
        receive_requests_x_times( 1 ).
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error ).
  ENDMETHOD.

  METHOD get_stack_after_2_events.
    send_requests_x_times( 2 ).
    DATA(lt_stack) = mo_cut->get( ).

    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = CAST lcl_item( lt_stack[ 2 ] )->get_id( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_application.

    METHODS setup.
    METHODS send_request_x_times     IMPORTING iv_times TYPE i.
    METHODS receiver_request_x_times IMPORTING iv_times       TYPE i
                                     RETURNING VALUE(ro_item) TYPE REF TO object.

    METHODS send_request_to_queue          FOR TESTING.
    METHODS get_item_from_queue            FOR TESTING.
    METHODS fill_sender_queue_at_full_msgs FOR TESTING.
    METHODS fill_recvr_queue_at_empty_msgs FOR TESTING.

ENDCLASS.

CLASS ltc_application IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD send_request_x_times.
    DO iv_times TIMES.
      mo_cut->send_item_to_queue( NEW lcl_item( sy-index ) ).
    ENDDO.
  ENDMETHOD.

  METHOD receiver_request_x_times.
    DO iv_times TIMES.
      ro_item = mo_cut->get_item_from_queue( ).
    ENDDO.
  ENDMETHOD.

  METHOD send_request_to_queue.
    cl_abap_unit_assert=>assert_true(
        act =  mo_cut->send_item_to_queue( NEW lcl_item( 1 ) ) ).
  ENDMETHOD.

  METHOD get_item_from_queue.
    send_request_x_times( 2 ).
    DATA(lo_received_item) = receiver_request_x_times( 2 ).

    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = CAST lcl_item( lo_received_item )->get_id( ) ).
  ENDMETHOD.

  METHOD fill_sender_queue_at_full_msgs.
    send_request_x_times( 3 ).
    DATA(lt_sender_queue) = mo_cut->get_sender_queue( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( lt_sender_queue ) ).
  ENDMETHOD.

  METHOD fill_recvr_queue_at_empty_msgs.
    DATA(lo_item) = mo_cut->get_item_from_queue( ).
    DATA(lt_receiver) = mo_cut->get_receiver_queue( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( lt_receiver ) ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  CALL SCREEN 1000.
  INCLUDE ymbh_bounded_queue_top.
  INCLUDE ymbh_bounded_queue_status_1o01.
  INCLUDE ymbh_bounded_queue_user_comi01.
