"* get value O back
"* get value X back
"* set a specific value and get it back
"* set value with a method
"* build a stone object
"* build a dedicated X stone object
"* build the opponent stone object
"* build a player
"* assign stone to player
"- assure that every player has different stones

REPORT ymbh_tic_tac_toe_tdd_aiymi_1.

CLASS lcx_player_factory DEFINITION INHERITING FROM cx_no_check FINAL.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcx_player_factory IMPLEMENTATION.

ENDCLASS.


CLASS stone DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING value TYPE char1.
    METHODS get_value RETURNING VALUE(result) TYPE char1.

  PRIVATE SECTION.
    DATA value TYPE c LENGTH 1.

ENDCLASS.

CLASS stone IMPLEMENTATION.

  METHOD constructor.
    me->value = value.
  ENDMETHOD.

  METHOD get_value.
    result = value.
  ENDMETHOD.

ENDCLASS.

CLASS x_stone DEFINITION INHERITING FROM stone.
  PUBLIC SECTION.
    METHODS constructor.

ENDCLASS.

CLASS x_stone IMPLEMENTATION.

  METHOD constructor.
    super->constructor( |X| ).
  ENDMETHOD.

ENDCLASS.

CLASS o_stone DEFINITION INHERITING FROM stone.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS o_stone IMPLEMENTATION.

  METHOD constructor.
    super->constructor( |O| ).
  ENDMETHOD.

ENDCLASS.

CLASS player DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS new IMPORTING value         TYPE char1
                      RETURNING VALUE(result) TYPE REF TO player.

    METHODS get_stone_value RETURNING VALUE(result) TYPE char1.

  PRIVATE SECTION.
    DATA collection TYPE STANDARD TABLE OF REF TO player WITH EMPTY KEY.
    DATA stone TYPE REF TO stone.

    METHODS set_stone IMPORTING stone TYPE REF TO stone.

ENDCLASS.

CLASS player IMPLEMENTATION.

  METHOD new.
    result = NEW player( ).
    result->set_stone( SWITCH #( value WHEN 'X' THEN NEW x_stone( )
                                       WHEN 'O' THEN NEW o_stone( ) ) ).
  ENDMETHOD.

  METHOD set_stone.
    me->stone = stone.
  ENDMETHOD.

  METHOD get_stone_value.
    result = stone->get_value( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_stone DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA value TYPE c LENGTH 1.
    DATA stone TYPE REF TO stone.

    METHODS get_value_x_back FOR TESTING.
    METHODS get_value_o_back FOR TESTING.

ENDCLASS.


CLASS tc_stone IMPLEMENTATION.

  METHOD get_value_x_back.
    stone = NEW x_stone( ).
    cl_abap_unit_assert=>assert_equals(
         exp = |X|
         act = stone->get_value( ) ).
  ENDMETHOD.

  METHOD get_value_o_back.
    stone = NEW o_stone( ).
    cl_abap_unit_assert=>assert_equals(
        exp = |O|
        act = stone->get_value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_player_factory DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_first_player_x FOR TESTING.
    METHODS get_second_player_o FOR TESTING.

ENDCLASS.


CLASS tc_player_factory IMPLEMENTATION.

  METHOD get_first_player_x.
    DATA(player) = player=>new( |X| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |X|
        act = player->get_stone_value( ) ).
  ENDMETHOD.

  METHOD get_second_player_o.
    DATA(player_x) = player=>new( |X| ).
    DATA(player_o) = player=>new( |O| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |O|
        act = player_o->get_stone_value( ) ).
  ENDMETHOD.

ENDCLASS.
