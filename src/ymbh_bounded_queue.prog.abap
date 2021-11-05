REPORT ymbh_bounded_queue.

INTERFACE lif_queue.
  METHODS append IMPORTING element       TYPE REF TO object
                 RETURNING VALUE(result) TYPE abap_bool.

  METHODS pop RETURNING VALUE(result) TYPE REF TO object.
ENDINTERFACE.

CLASS lcx_queue DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcx_empty_queue DEFINITION INHERITING FROM lcx_queue.
ENDCLASS.

CLASS lcx_queue_limit_reached DEFINITION INHERITING FROM lcx_queue.
ENDCLASS.

CLASS lcl_queue DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_queue.

  PROTECTED SECTION.
    DATA queue TYPE STANDARD TABLE OF REF TO object.

    METHODS element_is_appended           IMPORTING element       TYPE REF TO object
                                          RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS abort_at_empty_queue.
    METHODS get_first_element_from_queue  RETURNING VALUE(result) TYPE REF TO object.
    METHODS delete_1st_element_from_queue.

ENDCLASS.

CLASS lcl_queue IMPLEMENTATION.

  METHOD lif_queue~append.
    result = element_is_appended( element ).
  ENDMETHOD.

  METHOD lif_queue~pop.
    abort_at_empty_queue( ).
    result = get_first_element_from_queue( ).
    delete_1st_element_from_queue( ).
  ENDMETHOD.

  METHOD element_is_appended.
    APPEND element TO queue.
    result = abap_true.
  ENDMETHOD.

  METHOD abort_at_empty_queue.
    IF queue IS INITIAL.
      RAISE EXCEPTION TYPE lcx_empty_queue.
    ENDIF.
  ENDMETHOD.

  METHOD get_first_element_from_queue.
    result = queue[ 1 ].
  ENDMETHOD.

  METHOD delete_1st_element_from_queue.
    DELETE queue INDEX 1.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_simple_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_queue.

    METHODS setup.
    METHODS add_object_to_queue          FOR TESTING.
    METHODS pop_object_from_queue        FOR TESTING.
    METHODS exception_pop_at_empty_queue FOR TESTING.
ENDCLASS.

CLASS lcl_bounded_queue DEFINITION INHERITING FROM lcl_queue.

  PUBLIC SECTION.
    METHODS constructor IMPORTING size TYPE i.

    METHODS lif_queue~append REDEFINITION.

  PRIVATE SECTION.
    DATA size TYPE i.
    METHODS abort_at_queue_limit_reached.

ENDCLASS.

CLASS lcl_bounded_queue IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->size = size.
  ENDMETHOD.

  METHOD lif_queue~append.
    abort_at_queue_limit_reached( ).
    result = element_is_appended( element ).
  ENDMETHOD.

  METHOD abort_at_queue_limit_reached.
    IF lines( queue ) = size.
      RAISE EXCEPTION TYPE lcx_queue_limit_reached.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_simple_queue IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD add_object_to_queue.
    cl_abap_unit_assert=>assert_true( act = cut->lif_queue~append( NEW lcl_queue( ) ) ).
  ENDMETHOD.

  METHOD pop_object_from_queue.
    cut->lif_queue~append( NEW lcl_queue( ) ).
    cl_abap_unit_assert=>assert_bound( act = cut->lif_queue~pop( ) ).
  ENDMETHOD.

  METHOD exception_pop_at_empty_queue.
    TRY.
        DATA(element) = cut->lif_queue~pop( ).
      CATCH lcx_empty_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lx_error ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_bounded_queue DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_bounded_queue.

    METHODS setup.
    METHODS add_object_to_bounded_queue   FOR TESTING.
    METHODS error_at_appending_3_elements FOR TESTING.
    METHODS error_at_empty_queue          FOR TESTING.
ENDCLASS.

CLASS ltc_bounded_queue IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( 2 ).
  ENDMETHOD.

  METHOD add_object_to_bounded_queue.
    cl_abap_unit_assert=>assert_true(
        act =  cut->lif_queue~append( NEW lcl_queue( )  ) ).
  ENDMETHOD.

  METHOD error_at_appending_3_elements.
    TRY.
        DO 3 TIMES.
          cut->lif_queue~append( NEW lcl_queue( ) ).
        ENDDO.
      CATCH lcx_queue_limit_reached INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lx_error ).
  ENDMETHOD.

  METHOD error_at_empty_queue.
    TRY.
        cut->lif_queue~pop( ).
      CATCH lcx_empty_queue INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lx_error ).
  ENDMETHOD.

ENDCLASS.
