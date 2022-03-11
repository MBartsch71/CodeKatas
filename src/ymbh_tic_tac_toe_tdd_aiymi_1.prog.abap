##ToDo
" - build a cell
" - give cell a value
" - retrieve the value
" - build more than one cell in a collection
" - introduce some kind of coordinates

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.


CLASS tc_tictactoe DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS cell_value_is_o FOR TESTING.

    METHODS get_cell_value RETURNING VALUE(result) TYPE char1.
ENDCLASS.


CLASS tc_tictactoe IMPLEMENTATION.

  METHOD cell_value_is_o.
    cl_abap_unit_assert=>assert_equals(
        exp = |o|
        act = get_cell_value( ) ).

  ENDMETHOD.


  METHOD get_cell_value.
    result = |o|.
  ENDMETHOD.

ENDCLASS.
