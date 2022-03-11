##ToDo
" * get a value from a cell
" - set a value to a cell and get it back
" - build a cell
" - retrieve the value
" - build more than one cell in a collection
" - introduce some kind of coordinates

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.


CLASS tc_tictactoe DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cell_value TYPE string.
    METHODS cell_value_is_o FOR TESTING.
    METHODS set_cell_value_get_back FOR TESTING.

    METHODS get_cell_value RETURNING VALUE(result) TYPE char1.
    METHODS set_cell_value
      IMPORTING
        value TYPE string.
ENDCLASS.


CLASS tc_tictactoe IMPLEMENTATION.

  METHOD cell_value_is_o.
    set_cell_value( |o| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |o|
        act = get_cell_value( ) ).

  ENDMETHOD.

  METHOD get_cell_value.
    result = cell_value.
  ENDMETHOD.

  METHOD set_cell_value_get_back.
    set_cell_value( |x| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |x|
        act = get_cell_value( ) ).
  ENDMETHOD.


  METHOD set_cell_value.
    cell_value = value.
  ENDMETHOD.

ENDCLASS.
