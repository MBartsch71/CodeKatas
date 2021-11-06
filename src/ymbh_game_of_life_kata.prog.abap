REPORT ymbh_game_of_life_kata.

INTERFACE lif_cell.
  METHODS is_alive RETURNING VALUE(rv_result) TYPE abap_bool.
  METHODS activate.
  METHODS deactivate.
ENDINTERFACE.

INTERFACE lif_board.
  TYPES: BEGIN OF ts_board,
           row  TYPE i,
           col  TYPE i,
           cell TYPE REF TO lif_cell,
         END OF ts_board.
  TYPES: tt_board TYPE SORTED TABLE OF ts_board WITH UNIQUE KEY primary_key COMPONENTS row col.

  METHODS get_size              RETURNING VALUE(rv_result) TYPE i.
  METHODS activate_cell         IMPORTING iv_row TYPE i
                                          iv_col TYPE i.

  METHODS get_cell              IMPORTING iv_row         TYPE i
                                          iv_col         TYPE i
                                RETURNING VALUE(ro_cell) TYPE REF TO lif_cell.

  METHODS deactivate_cell       IMPORTING iv_row TYPE i
                                          iv_col TYPE i.

  METHODS get_active_neighbours IMPORTING iv_row           TYPE i
                                          iv_col           TYPE i
                                RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.

INTERFACE lif_rules.
  METHODS set_new_cell_status IMPORTING io_cell       TYPE REF TO lif_cell
                                        iv_neighbours TYPE i.
ENDINTERFACE.

CLASS lcl_cell DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_cell.

  PRIVATE SECTION.
    DATA mv_is_alive TYPE abap_bool.

ENDCLASS.


CLASS lcl_cell IMPLEMENTATION.

  METHOD lif_cell~is_alive.
    rv_result = mv_is_alive.
  ENDMETHOD.

  METHOD lif_cell~activate.
    mv_is_alive = abap_true.
  ENDMETHOD.

  METHOD lif_cell~deactivate.
    mv_is_alive = abap_false.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_board DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_board.

    METHODS constructor IMPORTING iv_rows TYPE i
                                  iv_cols TYPE i.

  PRIVATE SECTION.
    DATA mt_board TYPE lif_board=>tt_board.
    DATA mv_rows TYPE i.
    DATA mv_cols TYPE i.

    METHODS build_initial_board.

    METHODS determine_start_row IMPORTING iv_row              TYPE i
                                RETURNING VALUE(rv_start_row) TYPE i.

    METHODS determine_end_row   IMPORTING iv_row            TYPE i
                                RETURNING VALUE(rv_end_row) TYPE i.

    METHODS determine_start_col IMPORTING iv_col              TYPE i
                                RETURNING VALUE(rv_start_col) TYPE i.

    METHODS determine_end_col   IMPORTING iv_col            TYPE i
                                RETURNING VALUE(rv_end_col) TYPE i.

ENDCLASS.

CLASS lcl_board IMPLEMENTATION.

  METHOD constructor.
    mv_rows = iv_rows.
    mv_cols = iv_cols.
    build_initial_board( ).
  ENDMETHOD.

  METHOD lif_board~get_size.
    rv_result = lines( mt_board ).
  ENDMETHOD.

  METHOD build_initial_board.
    mt_board = VALUE #( FOR i = 1 WHILE i <= mv_rows
                        FOR j = 1 WHILE j <= mv_cols
                            ( row  = i
                              col  = j
                              cell = NEW lcl_cell( ) ) ).
  ENDMETHOD.

  METHOD lif_board~activate_cell.
    mt_board[ row = iv_row col = iv_col ]-cell->activate( ).
  ENDMETHOD.

  METHOD lif_board~get_cell.
    ro_cell = mt_board[ row = iv_row col = iv_col ]-cell.
  ENDMETHOD.

  METHOD lif_board~deactivate_cell.
    mt_board[ row = iv_row col = iv_col ]-cell->deactivate( ).
  ENDMETHOD.

  METHOD lif_board~get_active_neighbours.
    rv_result = REDUCE #( INIT neighbours = 0
                         FOR i = determine_start_row( iv_row ) WHILE i <= determine_end_row( iv_row )
                         FOR j = determine_start_col( iv_col ) WHILE j <= determine_end_col( iv_col )
                         NEXT neighbours = COND #( WHEN mt_board[ row = i col = j ]-cell->is_alive( ) = abap_true THEN neighbours + 1
                                                   ELSE neighbours ) ).
  ENDMETHOD.

  METHOD determine_end_col.
    rv_end_col  = COND i( WHEN iv_col < mv_cols THEN iv_col + 1
                         ELSE mv_cols ).
  ENDMETHOD.

  METHOD determine_start_col.
    rv_start_col  = COND i( WHEN iv_col > 1 THEN iv_col - 1
                           ELSE 1 ).
  ENDMETHOD.

  METHOD determine_end_row.
    rv_end_row  = COND i( WHEN iv_row < mv_rows THEN iv_row + 1
                         ELSE mv_rows ).
  ENDMETHOD.

  METHOD determine_start_row.
    rv_start_row  = COND i( WHEN iv_row > 1 THEN iv_row - 1
                            ELSE 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_rules DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_rules.

  PRIVATE SECTION.
    METHODS cell_is_lonely          IMPORTING iv_neighbours    TYPE i
                                    RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS cell_is_overcrowded     IMPORTING iv_neighbours    TYPE i
                                    RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS cell_survive            IMPORTING iv_neighbours    TYPE i
                                    RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS cell_can_be_reactivated IMPORTING iv_neighbours    TYPE i
                                    RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS deactivate_cell         IMPORTING io_cell            TYPE REF TO lif_cell
                                    RETURNING VALUE(rv_is_alive) TYPE abap_bool.

    METHODS reactivate_cell         IMPORTING io_cell            TYPE REF TO lif_cell
                                    RETURNING VALUE(rv_is_alive) TYPE abap_bool.
ENDCLASS.

CLASS lcl_rules IMPLEMENTATION.

  METHOD lif_rules~set_new_cell_status.
    DATA(lv_status) = COND #( WHEN cell_is_lonely( iv_neighbours )          THEN deactivate_cell( io_cell )
                              WHEN cell_is_overcrowded( iv_neighbours )     THEN deactivate_cell( io_cell )
                              WHEN cell_can_be_reactivated( iv_neighbours ) THEN reactivate_cell( io_cell )
                              WHEN cell_survive( iv_neighbours )            THEN reactivate_cell( io_cell ) ).
  ENDMETHOD.

  METHOD cell_is_lonely.
    rv_result = xsdbool( iv_neighbours <= 1 ).
  ENDMETHOD.

  METHOD cell_is_overcrowded.
    rv_result = xsdbool( iv_neighbours >= 4 ).
  ENDMETHOD.

  METHOD cell_survive.
    rv_result = xsdbool( iv_neighbours = 2 OR
                         iv_neighbours = 3 ).
  ENDMETHOD.

  METHOD cell_can_be_reactivated.
    rv_result = xsdbool( iv_neighbours = 3 ).
  ENDMETHOD.

  METHOD deactivate_cell.
    io_cell->deactivate( ).
  ENDMETHOD.

  METHOD reactivate_cell.
    io_cell->activate( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_cell.

    METHODS setup.
    METHODS create_dead_cell FOR TESTING.
    METHODS activate_cell    FOR TESTING.
    METHODS deactivate_cell  FOR TESTING.

ENDCLASS.

CLASS ltc_cell IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD create_dead_cell.
    cl_abap_unit_assert=>assert_false(
        act =  mo_cut->lif_cell~is_alive( ) ).

  ENDMETHOD.

  METHOD activate_cell.
    mo_cut->lif_cell~activate( ).
    cl_abap_unit_assert=>assert_true(
        act =  mo_cut->lif_cell~is_alive( ) ).
  ENDMETHOD.

  METHOD deactivate_cell.
    mo_cut->lif_cell~activate( ).
    mo_cut->lif_cell~deactivate( ).
    cl_abap_unit_assert=>assert_false(
        act =  mo_cut->lif_cell~is_alive( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_board DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_board.

    METHODS setup.
    METHODS build_initial_board_10_x_10 FOR TESTING.
    METHODS activate_specific_cell      FOR TESTING.
    METHODS deactivate_specific_cell    FOR TESTING.
    METHODS get_active_neighbours_of_cell FOR TESTING.
ENDCLASS.

CLASS ltcl_board IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( iv_rows = 10 iv_cols = 10 ).
  ENDMETHOD.

  METHOD build_initial_board_10_x_10.
    cl_abap_unit_assert=>assert_equals(
        exp = 100
        act = mo_cut->lif_board~get_size( ) ).
  ENDMETHOD.

  METHOD activate_specific_cell.
    mo_cut->lif_board~activate_cell( iv_row = 2 iv_col = 4 ).
    DATA(lo_cell) = mo_cut->lif_board~get_cell( iv_row = 2 iv_col = 4 ).
    cl_abap_unit_assert=>assert_true(
        act =  lo_cell->is_alive( ) ).
  ENDMETHOD.

  METHOD deactivate_specific_cell.
    mo_cut->lif_board~activate_cell( iv_row = 3  iv_col = 6 ).
    mo_cut->lif_board~deactivate_cell( iv_row = 3 iv_col = 6 ).
    DATA(lo_cell) = mo_cut->lif_board~get_cell( iv_row = 3 iv_col = 6
                    ) .
    cl_abap_unit_assert=>assert_false(
        act =  lo_cell->is_alive( ) ).
  ENDMETHOD.

  METHOD get_active_neighbours_of_cell.
    mo_cut->lif_board~activate_cell( iv_row = 2 iv_col = 4 ).
    mo_cut->lif_board~activate_cell( iv_row = 4 iv_col = 3 ).
    mo_cut->lif_board~activate_cell( iv_row = 3 iv_col = 5 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = mo_cut->lif_board~get_active_neighbours( iv_row = 3 iv_col = 4 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_rules DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_rules.

    METHODS setup.
    METHODS deactivate_overcrowded_cell FOR TESTING.
    METHODS deactivate_lonely_cell      FOR TESTING.
    METHODS cell_survives               FOR TESTING.
    METHODS reactivate_dead_cell        FOR TESTING.

ENDCLASS.

CLASS ltc_rules IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD deactivate_overcrowded_cell.
    DATA(lo_cell) = NEW lcl_cell( ).
    lo_cell->lif_cell~activate( ).
    mo_cut->lif_rules~set_new_cell_status( io_cell = lo_cell iv_neighbours = 4 ).
    cl_abap_unit_assert=>assert_false(
        act =  lo_cell->lif_cell~is_alive( ) ).
  ENDMETHOD.

  METHOD deactivate_lonely_cell.
    DATA(lo_cell) = NEW lcl_cell( ).
    lo_cell->lif_cell~activate( ).
    mo_cut->lif_rules~set_new_cell_status( io_cell = lo_cell iv_neighbours = 1  ).
    cl_abap_unit_assert=>assert_false(
        act =  lo_cell->lif_cell~is_alive( ) ).
  ENDMETHOD.

  METHOD cell_survives.
    DATA(lo_cell) = NEW lcl_cell( ).
    lo_cell->lif_cell~activate( ).
    mo_cut->lif_rules~set_new_cell_status( io_cell = lo_cell iv_neighbours = 3 ).
    cl_abap_unit_assert=>assert_true(
        act =  lo_cell->lif_cell~is_alive( ) ).
  ENDMETHOD.

  METHOD reactivate_dead_cell.
    DATA(lo_cell) = NEW lcl_cell( ).
    mo_cut->lif_rules~set_new_cell_status( io_cell = lo_cell iv_neighbours = 3 ).
    cl_abap_unit_assert=>assert_true(
        act =  lo_cell->lif_cell~is_alive( ) ).
  ENDMETHOD.

ENDCLASS.
