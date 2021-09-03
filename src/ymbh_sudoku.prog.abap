REPORT ymbh_sudoku.

CLASS lcx_number_generation DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

INTERFACE lif_number_generator.
  TYPES numbers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  METHODS get_random_number_from_range
    IMPORTING
      upper_range_bound TYPE i
    RETURNING
      VALUE(number)     TYPE i
    RAISING
      lcx_number_generation.
ENDINTERFACE.

INTERFACE lif_board.

  TYPES board TYPE REF TO ycl_table.

ENDINTERFACE.

CLASS lcl_number_generator_fm DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_number_generator.

ENDCLASS.

CLASS lcl_number_generator_fm IMPLEMENTATION.

  METHOD lif_number_generator~get_random_number_from_range.
    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_max   = upper_range_bound
        ran_int_min   = 1
      IMPORTING
        ran_int       = number
      EXCEPTIONS
        invalid_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_number_generation.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_number_generator_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_number_generator.
ENDCLASS.

CLASS lcl_number_generator_class IMPLEMENTATION.

  METHOD lif_number_generator~get_random_number_from_range.
    TRY.
        DATA(seed) = cl_abap_random=>seed( ).
        DATA(number_object) = cl_abap_random_int=>create( seed = seed
                                                          min  = 1
                                                          max  = upper_range_bound ).
        number = number_object->get_next( ).
      CATCH cx_abap_random.
        RAISE EXCEPTION TYPE lcx_number_generation.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_number_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut         TYPE REF TO lif_number_generator.
    DATA mt_number_pool TYPE lif_number_generator=>numbers.

    METHODS setup.
    METHODS get_random_number_till_9 FOR TESTING.
    METHODS get_random_number_from_8 FOR TESTING.
ENDCLASS.


CLASS ltc_number_generator IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW lcl_number_generator_class( ).

    mt_number_pool =  VALUE lif_number_generator=>numbers( ( 1 )
                                                           ( 2 )
                                                           ( 3 )
                                                           ( 4 )
                                                           ( 5 )
                                                           ( 6 )
                                                           ( 7 )
                                                           ( 8 )
                                                           ( 9 ) ).
  ENDMETHOD.

  METHOD get_random_number_till_9.
    TRY.
        DATA(lv_number) = mo_cut->get_random_number_from_range( lines( mt_number_pool ) ).

        cl_abap_unit_assert=>assert_table_contains(
            line  = lv_number
            table = mt_number_pool
            msg   = |The expected line should be in the table| ).
      CATCH lcx_number_generation.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_random_number_from_8.
    TRY.
        DATA(lv_number) = mo_cut->get_random_number_from_range( lines( mt_number_pool ) ).
        DELETE mt_number_pool WHERE table_line = lv_number.
        lv_number = mo_cut->get_random_number_from_range( lines( mt_number_pool ) ).
        cl_abap_unit_assert=>assert_table_contains(
            line  = lv_number
            table = mt_number_pool
            msg   = |The expected line should be in the table| ).
      CATCH lcx_number_generation.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sudoku_board DEFINITION FINAL.

  PUBLIC SECTION.

    DATA board TYPE lif_board=>board.
    METHODS constructor.
    METHODS set_value
      IMPORTING
        row   TYPE i
        col   TYPE i
        value TYPE i.
    METHODS get_cell_value
      IMPORTING
        row             TYPE i
        col             TYPE i
      RETURNING
        VALUE(r_result) TYPE REF TO data.

ENDCLASS.

CLASS lcl_sudoku_board IMPLEMENTATION.

  METHOD constructor.
    board = ycl_table=>new( rows = 9  cols = 9 ).
  ENDMETHOD.

  METHOD set_value.
    board->set_cell_value( row = row col = col value = value ).
  ENDMETHOD.

  METHOD get_cell_value.
    r_result = board->get_cell_value( row = row col = col ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_board DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_sudoku_board.

    METHODS setup.
    METHODS build_sudoku_board     FOR TESTING.
    METHODS set_value_in_row3_col4 FOR TESTING.

ENDCLASS.


CLASS ltc_board IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD build_sudoku_board.
    cl_abap_unit_assert=>assert_equals(
        exp = 81
        act = lines( mo_cut->board->get_cells( ) )
        msg = 'The board should have 81 cells' ).
  ENDMETHOD.

  METHOD set_value_in_row3_col4.
    DATA valref TYPE REF TO data.

    mo_cut->set_value( row = 3 col = 3 value = 5 ).

    valref = mo_cut->get_cell_value( row = 3 col = 3 ).
    ASSIGN valref->* TO FIELD-SYMBOL(<data>).
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = <data>
        msg = 'The expected value should be in the table' ).
  ENDMETHOD.

ENDCLASS.
