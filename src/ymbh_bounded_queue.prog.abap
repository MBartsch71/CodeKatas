REPORT ymbh_bounded_queue.

CLASS lcx_queue DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcl_queue DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING size TYPE i.

    METHODS append      IMPORTING object        TYPE REF TO object
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS pop RETURNING VALUE(result) TYPE REF TO object.

  PRIVATE SECTION.
    DATA size  TYPE i.
    DATA queue TYPE TABLE OF REF TO object.

    METHODS object_appended                IMPORTING object        TYPE REF TO object
                                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS append_object_to_queue         IMPORTING object TYPE REF TO object.
    METHODS pop_object_from_queue          RETURNING VALUE(result) TYPE REF TO object.
    METHODS get_first_object_from_queue    RETURNING VALUE(result) TYPE REF TO object.
    METHODS delete_first_object_from_queue.

    METHODS abort_at_full_queue.
    METHODS abort_at_empty_queue.


ENDCLASS.

CLASS lcl_queue IMPLEMENTATION.

  METHOD constructor.
    me->size = size.
  ENDMETHOD.

  METHOD append.
    result = object_appended( object ).
  ENDMETHOD.

  METHOD pop.
    result = pop_object_from_queue( ).
  ENDMETHOD.

  METHOD object_appended.
    abort_at_full_queue( ).
    append_object_to_queue( object ).
    result = abap_true.
  ENDMETHOD.

  METHOD append_object_to_queue.
    APPEND object TO queue.
  ENDMETHOD.

  METHOD abort_at_full_queue.
    IF lines( queue ) = size.
      RAISE EXCEPTION TYPE lcx_queue.
    ENDIF.
  ENDMETHOD.

  METHOD pop_object_from_queue.
    abort_at_empty_queue( ).
    get_first_object_from_queue( ).
    delete_first_object_from_queue( ).
  ENDMETHOD.

  METHOD abort_at_empty_queue.
    IF lines( queue ) = 0.
      RAISE EXCEPTION TYPE lcx_queue.
    ENDIF.
  ENDMETHOD.

  METHOD get_first_object_from_queue.
    result = queue[ 1 ].
  ENDMETHOD.

  METHOD delete_first_object_from_queue.
    DELETE queue INDEX 1.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_queue.

    METHODS setup.

    METHODS queue_append_pop_2x_append_ok FOR TESTING.
    METHODS queue_append_2x_pop_failure   FOR TESTING.
    METHODS queue_3x_append_failure       FOR TESTING.
    METHODS pop_on_empty_queue_failure    FOR TESTING.

ENDCLASS.


CLASS ltc_queue IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( 2 ).
  ENDMETHOD.

  METHOD queue_append_pop_2x_append_ok.
    TRY.
        cut->append( NEW lcl_queue( 1 ) ).
        cut->pop( ).
        cut->append( NEW lcl_queue( 1 ) ).
        cut->append( NEW lcl_queue( 1 ) ).
      CATCH lcx_queue.
        cl_abap_unit_assert=>fail( msg = |An exception shouldn't be raised.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD queue_append_2x_pop_failure.
    TRY.
        cut->append( NEW lcl_queue( 1 ) ).
        cut->pop( ).
        cut->pop( ).
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lx_error ).
  ENDMETHOD.

  METHOD queue_3x_append_failure.
    TRY.
        DO 3 TIMES.
          cut->append( NEW lcl_queue( 1 ) ).
        ENDDO.
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error ).
  ENDMETHOD.

  METHOD pop_on_empty_queue_failure.
    TRY.
        DATA(result) = cut->pop( ).
      CATCH lcx_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lx_error ).
  ENDMETHOD.

ENDCLASS.
