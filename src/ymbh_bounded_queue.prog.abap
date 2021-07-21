REPORT ymbh_bounded_queue.

CLASS lcx_queue DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

INTERFACE lif_sender.
  EVENTS enqueue   EXPORTING VALUE(o_sender) TYPE REF TO lif_sender.

  METHODS get_item RETURNING VALUE(ro_item) TYPE REF TO object.
  METHODS send.

ENDINTERFACE.

INTERFACE lif_receiver.
  EVENTS dequeue    EXPORTING VALUE(o_receiver) TYPE REF TO lif_receiver.

  METHODS set_item IMPORTING io_item TYPE REF TO object.
  METHODS get_item  RETURNING VALUE(ro_item) TYPE REF TO object.
  METHODS receive.
ENDINTERFACE.


CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD constructor.
    CALL SCREEN 1000.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_queue DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES tt_queue TYPE STANDARD TABLE OF REF TO object WITH DEFAULT KEY.
    METHODS enqueue FOR EVENT enqueue OF lif_sender IMPORTING o_sender.
    METHODS dequeue FOR EVENT dequeue OF lif_receiver IMPORTING o_receiver.

    METHODS constructor IMPORTING iv_size TYPE i.

    METHODS get         RETURNING VALUE(rt_queue) TYPE tt_queue.

  PRIVATE SECTION.
    DATA mt_queue TYPE tt_queue.

    METHODS push                           IMPORTING io_item TYPE REF TO object.

    METHODS pop                            RETURNING VALUE(ro_object) TYPE REF TO object.

    METHODS get_next_line_for_storing_item RETURNING VALUE(rv_line) TYPE i.

    METHODS exception_at_full_queue        IMPORTING iv_line TYPE i.

    METHODS add_item_to_determined_line    IMPORTING iv_line TYPE i
                                                     io_item TYPE REF TO object.

    METHODS exception_at_empty_queue.

    METHODS get_first_item_from_queue      RETURNING VALUE(ro_object) TYPE REF TO object.

    METHODS reorganize_queue.

ENDCLASS.

CLASS lcl_queue IMPLEMENTATION.

  METHOD constructor.
    mt_queue = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = iv_size + 1 (  ) ).
  ENDMETHOD.

  METHOD get.
    rt_queue = mt_queue.
  ENDMETHOD.

  METHOD enqueue.
    push( o_sender->get_item( ) ).
  ENDMETHOD.

  METHOD dequeue.
    o_receiver->set_item( pop( ) ).
  ENDMETHOD.

  METHOD push.
    DATA(lv_line) = get_next_line_for_storing_item( ).
    exception_at_full_queue( lv_line ).
    add_item_to_determined_line( iv_line = lv_line
                                 io_item = io_item ).
  ENDMETHOD.

  METHOD pop.
    exception_at_empty_queue( ).
    ro_object = get_first_item_from_queue( ).
    reorganize_queue( ).
  ENDMETHOD.

  METHOD get_next_line_for_storing_item.
    DATA lo_item TYPE REF TO object.
    rv_line = line_index( mt_queue[ table_line = lo_item ] ).
  ENDMETHOD.

  METHOD exception_at_full_queue.
    IF iv_line <= 0.
      RAISE EXCEPTION TYPE lcx_queue.
    ENDIF.
  ENDMETHOD.

  METHOD add_item_to_determined_line.
    mt_queue[ iv_line ] = io_item.
  ENDMETHOD.

  METHOD exception_at_empty_queue.
    IF mt_queue[ 1 ] IS INITIAL.
      RAISE EXCEPTION TYPE lcx_queue.
    ENDIF.
  ENDMETHOD.

  METHOD get_first_item_from_queue.
    ro_object = mt_queue[ 1 ].
  ENDMETHOD.

  METHOD reorganize_queue.
    DELETE mt_queue INDEX 1.
    APPEND INITIAL LINE TO mt_queue.
  ENDMETHOD.



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

CLASS lcl_sender DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_sender.
    METHODS constructor IMPORTING io_item TYPE REF TO object.

  PRIVATE SECTION.
    DATA mo_item TYPE REF TO object.

ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.

  METHOD constructor.
    mo_item = io_item.
  ENDMETHOD.

  METHOD lif_sender~get_item.
    ro_item = mo_item.
  ENDMETHOD.

  METHOD lif_sender~send.
    RAISE EVENT lif_sender~enqueue EXPORTING o_sender = me.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_receiver DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_receiver.

  PRIVATE SECTION.
    DATA mo_item TYPE REF TO object.

ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.

  METHOD lif_receiver~receive.
    RAISE EVENT lif_receiver~dequeue EXPORTING o_receiver = me.
  ENDMETHOD.

  METHOD lif_receiver~set_item.
    mo_item = io_item.
  ENDMETHOD.

  METHOD lif_receiver~get_item.
    ro_item = mo_item.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_queue.

    METHODS setup.

    METHODS error_at_queue_size_violation FOR TESTING.
    METHODS get_second_object_from_queue  FOR TESTING.
    METHODS error_at_empty_queue          FOR TESTING.

ENDCLASS.

CLASS ltc_queue IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 2 ).
  ENDMETHOD.

  METHOD error_at_queue_size_violation.
    TRY.
        DO 3 TIMES.
          DATA(lo_sender) = NEW lcl_sender( NEW lcl_item( sy-index ) ).
          SET HANDLER mo_cut->enqueue FOR lo_sender.
          lo_sender->lif_sender~send( ).
        ENDDO.
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error ).
  ENDMETHOD.

  METHOD get_second_object_from_queue.
    DO 2 TIMES.
      DATA(lo_sender) = NEW lcl_sender( NEW lcl_item( sy-index ) ).
      SET HANDLER mo_cut->enqueue FOR lo_sender.
      lo_sender->lif_sender~send( ).
    ENDDO.
    DO 2 TIMES.
      DATA(lo_receiver) = NEW lcl_receiver( ).
      SET HANDLER mo_cut->dequeue FOR lo_receiver.
      lo_receiver->lif_receiver~receive( ).
      DATA(lo_item) = CAST lcl_item( lo_receiver->lif_receiver~get_item( ) ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lo_item->get_id( ) ).
  ENDMETHOD.

  METHOD error_at_empty_queue.
    TRY.
        DATA(lo_receiver) = NEW lcl_receiver( ).
        SET HANDLER mo_cut->dequeue FOR lo_receiver.
        lo_receiver->lif_receiver~receive( ).
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error ).
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

CLASS ltc_sender DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_sender.
    METHODS build_sender_with_item FOR TESTING.

ENDCLASS.


CLASS ltc_sender IMPLEMENTATION.

  METHOD build_sender_with_item.
    mo_cut = NEW #( NEW lcl_item( 1 ) ).

    cl_abap_unit_assert=>assert_bound(
        act = mo_cut->lif_sender~get_item( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_receiver DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_receiver.
    METHODS get_item_from_receiver FOR TESTING.
ENDCLASS.


CLASS ltc_receiver IMPLEMENTATION.

  METHOD get_item_from_receiver.
    mo_cut = NEW #( ).
    mo_cut->lif_receiver~set_item( NEW lcl_item( 1 ) ).

    cl_abap_unit_assert=>assert_bound(
        act = mo_cut->lif_receiver~get_item( ) ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  NEW lcl_application( ).
  INCLUDE ymbh_bounded_queue_top.
  INCLUDE ymbh_bounded_queue_status_1o01.
  INCLUDE ymbh_bounded_queue_user_comi01.
