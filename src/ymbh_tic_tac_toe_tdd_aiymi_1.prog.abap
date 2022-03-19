##ToDo
"* get a value
"* set a value and retrieve it
"* building a stone object
"* set the expected value to the object
"* retrieve the expected value from the object
"- building a O stone
"- create designated objects for X and O

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.

CLASS stone DEFINITION.

  PUBLIC SECTION.
    METHODS set_value IMPORTING value TYPE char1.
    METHODS get_value RETURNING VALUE(result) TYPE char1.

  PRIVATE SECTION.
    DATA value TYPE c LENGTH 1.

ENDCLASS.

CLASS stone IMPLEMENTATION.

  METHOD set_value.
    me->value = value.
  ENDMETHOD.

  METHOD get_value.
    result = value.
  ENDMETHOD.

ENDCLASS.

CLASS tc_stone DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA value TYPE c LENGTH 1.
    DATA cut TYPE REF TO stone.

    METHODS build_a_stone_object FOR TESTING.
    METHODS set_value_to_stone_object FOR TESTING.
    METHODS get_value_from_stone_object FOR TESTING.
ENDCLASS.


CLASS tc_stone IMPLEMENTATION.

  METHOD build_a_stone_object.
    cut = NEW #( ).
    cl_abap_unit_assert=>assert_bound(
        msg = |The object should be bound!|
        act = cut ).
  ENDMETHOD.

  METHOD set_value_to_stone_object.
    cut = NEW #( ).
    cut->set_value( |X| ).
    cl_abap_unit_assert=>assert_bound(
        msg = |The object should be bound!|
        act = cut ).
  ENDMETHOD.

  METHOD get_value_from_stone_object.
    cut = NEW #(  ).
    cut->set_value( |O| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |O|
        act = cut->get_value( ) ).
  ENDMETHOD.


ENDCLASS.
