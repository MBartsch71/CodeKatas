##ToDo
" * get a value from a cell
" * set a value to a cell and get it back
" * build a cell object
" * set value to the object
" * retrieve the value
" * check if it makes sense to build a x and an o object
" - build more than one cell in a collection
" - introduce some kind of coordinates

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.

INTERFACE if_cell.
  DATA value TYPE char1 READ-ONLY.
  METHODS get_value RETURNING VALUE(result) TYPE char1.

ENDINTERFACE.

CLASS x_cell DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_cell.

ENDCLASS.

CLASS x_cell IMPLEMENTATION.

  METHOD if_cell~get_value.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS o_cell DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_cell.

ENDCLASS.

CLASS o_cell IMPLEMENTATION.

  METHOD if_cell~get_value.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS object_checker DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_constraint.
ENDCLASS.

CLASS object_checker IMPLEMENTATION.

  METHOD if_constraint~get_description.

  ENDMETHOD.

  METHOD if_constraint~is_valid.
    IF data_object IS INSTANCE OF x_cell.
      result = abap_true.
    ELSEIF data_object IS INSTANCE OF o_cell.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cell_factory DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS get_instance IMPORTING value         TYPE string
                               RETURNING VALUE(result) TYPE REF TO if_cell.

ENDCLASS.

CLASS cell_factory IMPLEMENTATION.



  METHOD get_instance.
    result = SWITCH #( value WHEN 'X' THEN NEW x_cell( )
                             WHEN 'O' THEN NEW o_cell( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_tictactoe DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cell_value TYPE string.


    METHODS build_a_x_object FOR TESTING.
    METHODS build_a_o_object FOR TESTING.

    METHODS get_cell_value RETURNING VALUE(result) TYPE char1.
    METHODS set_cell_value IMPORTING value TYPE string.


ENDCLASS.


CLASS tc_tictactoe IMPLEMENTATION.

  METHOD get_cell_value.
    result = cell_value.
  ENDMETHOD.

  METHOD set_cell_value.
    cell_value = value.
  ENDMETHOD.

  METHOD build_a_x_object.
    DATA(cell) = cell_factory=>get_instance( |X| ).
    cl_abap_unit_assert=>assert_that(
      EXPORTING
        act              = cell
        exp              = NEW object_checker( ) ).
  ENDMETHOD.

  METHOD build_a_o_object.
    DATA(cell) = cell_factory=>get_instance( |O| ).
    cl_abap_unit_assert=>assert_that(
   EXPORTING
     act              = cell
     exp              = NEW object_checker( ) ).
  ENDMETHOD.

ENDCLASS.
