##ToDo
" * get a value from a cell
" * set a value to a cell and get it back
" - build a cell object
" - set value to the object
" - retrieve the value
" - build more than one cell in a collection
" - introduce some kind of coordinates

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.

CLASS cell DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING value TYPE char1.
    METHODS get_value
      RETURNING
        VALUE(result) TYPE char1.
  PRIVATE SECTION.
    DATA value TYPE char1.

ENDCLASS.

CLASS cell IMPLEMENTATION.

  METHOD constructor.
    me->value = value.
  ENDMETHOD.

  METHOD get_value.
    result = value.
  ENDMETHOD.

ENDCLASS.

CLASS tc_tictactoe DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cell_value TYPE string.

    METHODS set_cell_value_get_back FOR TESTING.
    METHODS get_a_cell_object   FOR TESTING.
    METHODS set_value_x_to_cell_object FOR TESTING.

    METHODS get_cell_value RETURNING VALUE(result) TYPE char1.
    METHODS set_cell_value IMPORTING value TYPE string.

    METHODS get_cell_object RETURNING VALUE(result) TYPE REF TO cell.
ENDCLASS.


CLASS tc_tictactoe IMPLEMENTATION.

  METHOD get_cell_value.
    result = cell_value.
  ENDMETHOD.

  METHOD set_cell_value.
    cell_value = value.
  ENDMETHOD.

  METHOD set_cell_value_get_back.
    set_cell_value( |x| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |x|
        act = get_cell_value( ) ).
  ENDMETHOD.

  METHOD get_a_cell_object.
    cl_abap_unit_assert=>assert_bound(
        msg = |The object should be bound!|
        act = get_cell_object( ) ).
  ENDMETHOD.

  METHOD get_cell_object.
    result = NEW cell( |o| ).
  ENDMETHOD.

  METHOD set_value_x_to_cell_object.
    DATA(cell) = NEW cell( |X| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |X|
        act = cell->get_value( ) ).
  ENDMETHOD.

ENDCLASS.
