REPORT ymbh_connect_4.

INTERFACE lif_board.
  TYPES: BEGIN OF ts_cell,
           row   TYPE i,
           col   TYPE i,
           value TYPE char1,
         END OF ts_cell.
  TYPES: tt_cells TYPE SORTED TABLE OF ts_cell WITH UNIQUE KEY primary_key COMPONENTS row col
                                               WITH NON-UNIQUE SORTED KEY col COMPONENTS col
                                               WITH NON-UNIQUE SORTED KEY value COMPONENTS value.
ENDINTERFACE.

CLASS lcl_c4_board DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_board                  RETURNING VALUE(rt_board) TYPE lif_board=>tt_cells.

    METHODS determine_all_filled_cells RETURNING VALUE(rt_cells) TYPE lif_board=>tt_cells.

    METHODS update_board_in_col        IMPORTING iv_col TYPE i.

  PRIVATE SECTION.
    DATA mt_board TYPE lif_board=>tt_cells.

    METHODS find_first_empty_cell_in_col IMPORTING iv_col        TYPE i
                                         RETURNING VALUE(rv_row) TYPE i.

ENDCLASS.

CLASS lcl_c4_board IMPLEMENTATION.

  METHOD constructor.
    mt_board = VALUE #( FOR i = 1 WHILE i <= 6
                        FOR j = 1 WHILE j <= 7
                        ( row = i col = j ) ).
  ENDMETHOD.

  METHOD find_first_empty_cell_in_col.
    DATA(lt_cols) = FILTER #( mt_board USING KEY col WHERE col = iv_col ).
    rv_row = REDUCE #( INIT row = 0
                       FOR <line> IN lt_cols
                       NEXT row = COND #( WHEN <line>-value IS INITIAL THEN <line>-row
                                          ELSE row ) ).
  ENDMETHOD.

  METHOD determine_all_filled_cells.
    rt_cells = FILTER #( mt_board USING KEY value WHERE value <> space ).
  ENDMETHOD.

  METHOD update_board_in_col.
    mt_board[ row = find_first_empty_cell_in_col( iv_col ) col = iv_col ]-value = '1'.
  ENDMETHOD.

  METHOD get_board.
    rt_board = mt_board.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_c4_board DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_c4_board.

    METHODS setup.

    METHODS find_all_filled_cells    FOR TESTING.
    METHODS board_after_first_player FOR TESTING.
    METHODS board_after_6_rounds     FOR TESTING.

ENDCLASS.


CLASS ltc_c4_board IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD find_all_filled_cells.
    DATA(lt_expected_values) = VALUE lif_board=>tt_cells( ( row = 6 col = 1 value = '1' )
                                                          ( row = 6 col = 2 value = '1' )
                                                          ( row = 6 col = 4 value = '1' )
                                                          ( row = 5 col = 4 value = '1' ) ).

    mo_cut->update_board_in_col( 1 ).
    mo_cut->update_board_in_col( 2 ).
    mo_cut->update_board_in_col( 4 ).
    mo_cut->update_board_in_col( 4 ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_expected_values
        act = mo_cut->determine_all_filled_cells( ) ).
  ENDMETHOD.

  METHOD board_after_first_player.
    DATA(lt_check_board) = mo_cut->get_board( ).
    lt_check_board[ row = 6 col = 1 ]-value = '1'.

    mo_cut->update_board_in_col( 1 ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_check_board
        act = mo_cut->get_board( ) ).
  ENDMETHOD.

  METHOD board_after_6_rounds.
    DATA(lt_check_board) = mo_cut->get_board( ).
    lt_check_board[ row = 6 col = 1 ]-value = '1'.
    lt_check_board[ row = 5 col = 1 ]-value = '1'.
    lt_check_board[ row = 6 col = 4 ]-value = '1'.
    lt_check_board[ row = 5 col = 4 ]-value = '1'.
    lt_check_board[ row = 4 col = 4 ]-value = '1'.
    lt_check_board[ row = 6 col = 5 ]-value = '1'.

    mo_cut->update_board_in_col( 1 ).
    mo_cut->update_board_in_col( 1 ).
    mo_cut->update_board_in_col( 4 ).
    mo_cut->update_board_in_col( 4 ).
    mo_cut->update_board_in_col( 4 ).
    mo_cut->update_board_in_col( 5 ).
    cl_abap_unit_assert=>assert_equals(
        exp = lt_check_board
        act = mo_cut->get_board( ) ).
  ENDMETHOD.

ENDCLASS.
