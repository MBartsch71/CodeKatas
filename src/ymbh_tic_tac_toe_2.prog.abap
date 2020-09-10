REPORT ymbh_tic_tac_toe_2.

INTERFACE lif_state_machine.
  TYPES: BEGIN OF status,
           status    TYPE char1,
           successor TYPE char1,
           initial   TYPE abap_bool,
         END OF status.
  TYPES status_net TYPE STANDARD TABLE OF status WITH DEFAULT KEY.

  METHODS next.
  METHODS final.
  METHODS state
    RETURNING
      VALUE(r_state) TYPE char1.
ENDINTERFACE.

CLASS lcl_state DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_state_machine.

    ALIASES next  FOR lif_state_machine~next.
    ALIASES state FOR lif_state_machine~state.
    ALIASES final FOR lif_state_machine~final.

    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS c_final_state TYPE char1 VALUE 'E' ##NO_TEXT.
    CONSTANTS c_state_x     TYPE char1 VALUE 'X' ##NO_TEXT.
    CONSTANTS c_state_o     TYPE char1 VALUE 'O' ##NO_TEXT.

    DATA status_net    TYPE lif_state_machine=>status_net.
    DATA current_state TYPE lif_state_machine=>status-status.

    METHODS init.
    METHODS build_status_net.

ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD constructor.
    build_status_net( ).
    init( ).
  ENDMETHOD.

  METHOD build_status_net.
    status_net = VALUE lif_state_machine=>status_net( ( status = c_state_x successor = c_state_o initial = c_state_x )
                                                      ( status = c_state_o successor = c_state_x initial = '' ) ).
  ENDMETHOD.

  METHOD init.
    current_state = status_net[ initial = abap_true ]-status.
  ENDMETHOD.

  METHOD lif_state_machine~next.
    current_state = status_net[ status = current_state ]-successor.
  ENDMETHOD.

  METHOD lif_state_machine~state.
    r_state = current_state.
  ENDMETHOD.

  METHOD lif_state_machine~final.
    current_state = c_final_state.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_state DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_state.

    METHODS setup.

    METHODS init_state_machine FOR TESTING.
    METHODS next_state_o       FOR TESTING.
    METHODS next_state_x       FOR TESTING.
    METHODS reach_final_state  FOR TESTING.

ENDCLASS.


CLASS ltc_state IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD init_state_machine.
    cl_abap_unit_assert=>assert_equals(
        exp = 'X'
        act = cut->state( ) ).
  ENDMETHOD.

  METHOD next_state_o.
    cut->next( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'O'
        act = cut->state( ) ).
  ENDMETHOD.

  METHOD next_state_x.
    cut->next( ).
    cut->next( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'X'
        act = cut->state( ) ).
  ENDMETHOD.

  METHOD reach_final_state.
    cut->final( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'E'
        act = cut->state( ) ).
  ENDMETHOD.

ENDCLASS.
