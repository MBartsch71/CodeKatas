REPORT ymbh_tic_tac_toe_2.

CLASS lcx_tictactoe DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
ENDCLASS.

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

INTERFACE lif_board.
  TYPES: BEGIN OF board_line,
           column1 TYPE char1,
           column2 TYPE char1,
           column3 TYPE char1,
         END OF board_line.
  TYPES board TYPE STANDARD TABLE OF board_line WITH DEFAULT KEY.

  METHODS get_board
    RETURNING
      VALUE(board) TYPE lif_board=>board.

  METHODS place_turn
    IMPORTING
      row    TYPE i
      column TYPE i
      value  TYPE string
    RAISING
      lcx_tictactoe.

  METHODS stringify
    RETURNING
      VALUE(result) TYPE string.
ENDINTERFACE.

INTERFACE lif_validator.
  METHODS player_wins
    IMPORTING
      player          TYPE string
      board_as_string TYPE string
    RETURNING
      VALUE(result)   TYPE abap_bool.

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

CLASS lcl_board DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_board.
    ALIASES get_board FOR lif_board~get_board.
    ALIASES place_turn FOR lif_board~place_turn.
    ALIASES stringify FOR lif_board~stringify.

    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS c_cell_initial TYPE char1 VALUE '-'.
    DATA board TYPE lif_board=>board.

ENDCLASS.

CLASS lcl_board IMPLEMENTATION.

  METHOD constructor.
    board = VALUE lif_board=>board( ( column1 = c_cell_initial column2 = c_cell_initial column3 = c_cell_initial )
                                    ( column1 = c_cell_initial column2 = c_cell_initial column3 = c_cell_initial )
                                    ( column1 = c_cell_initial column2 = c_cell_initial column3 = c_cell_initial ) ).
  ENDMETHOD.

  METHOD lif_board~get_board.
    board = me->board.
  ENDMETHOD.

  METHOD lif_board~place_turn.
    TRY.
        ASSIGN COMPONENT column OF STRUCTURE board[ row ] TO FIELD-SYMBOL(<value>).
        IF sy-subrc = 0.
          IF <value> <> c_cell_initial.
            RAISE EXCEPTION TYPE lcx_tictactoe
                            MESSAGE i001(00) WITH |The field { row }/{ column } is already played.|.
          ENDIF.
          <value> = value.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_tictactoe
                        MESSAGE i001(00) WITH |Row or column number out of bounds.|.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_board~stringify.
    result = REDUCE #( INIT string = ||
                       FOR row IN board
                       NEXT string = |{ string }{ row-column1 }{ row-column2 }{ row-column3 }| ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_validator DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_validator.
    ALIASES player_wins FOR lif_validator~player_wins.

    METHODS constructor.

  PRIVATE SECTION.
    DATA winning_patterns TYPE stringtab.

ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.

  METHOD constructor.
    winning_patterns = VALUE stringtab( ( |XXX++++++| )
                                        ( |+++XXX+++| )
                                        ( |++++++XXX| )
                                        ( |X++X++X++| )
                                        ( |+X++X++X+| )
                                        ( |++X++X++X| )
                                        ( |X+++X+++X| )
                                        ( |++X+X+X++| )
                                        ( |OOO++++++| )
                                        ( |+++OOO+++| )
                                        ( |++++++OOO| )
                                        ( |O++O++O++| )
                                        ( |+O++O++O+| )
                                        ( |++O++O++O| )
                                        ( |O+++O+++O| )
                                        ( |++O+O+O++| ) ).
  ENDMETHOD.

  METHOD lif_validator~player_wins.
    LOOP AT winning_patterns ASSIGNING FIELD-SYMBOL(<win_line>) WHERE table_line CS player.
      IF board_as_string CP <win_line>.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_display DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS display.
  PRIVATE SECTION.
    DATA html_tab TYPE cl_abap_browser=>html_table.

    METHODS at_click FOR EVENT sapevent OF cl_abap_browser IMPORTING action.




ENDCLASS.

CLASS lcl_display IMPLEMENTATION.

  METHOD constructor.
    html_tab = VALUE stringtab(
                ( `<html>` )
                (  `<script>function setloc(e){window.location=e;}</script>` )
                ( `  <style>` )
                ( `    body { background-color: #d9aa78 }` )
                ( `    button { background-color: #2d545e;` )
                ( `             width: 100%;` )
                ( `             height: 100%;` )
                ( `             border-top: 0px;` )
                ( `             border-left: 0px;` )
                ( `             font-family:Verdana, Geneva, Tahoma, sans-serif;` )
                ( `             font-size: 4rem;` )
                ( `             color: #fff;` )
                ( `             transition-duration: 0.4s }` )
                ( `    button:hover { background-color: #12343b} `)
                ( `    .board { display: flexbox;` )
                ( `             width: 50%;` )
                ( `             height: 100%;` )
                ( `             margin: auto;` )
                ( `             padding: 0px;` )
                ( `             background-color: #e1b382;`)
                ( `             box-shadow: 10px 0px 5px rgba(200, 150, 102, 1) }` )
                ( `    table { margin: 4rem auto;` )
                ( `            width: 60%;` )
                ( `            height: 60%}` )
                ( `    tr { height: 33%;}` )
                ( `    td { width: 33%; }` )
                ( `    h1 { font-family:Verdana, Geneva, Tahoma, sans-serif;` )
                ( `         font-size: 2rem;` )
                ( `         color: #12343b ;` )
                ( `         text-align: center;` )
                ( `         text-shadow: 1px 1px 4px #fff;}` )
                ( `    .info { font-family: Verdana, Geneva, Tahoma, sans-serif;` )
                ( `            color: #12343b;` )
                ( `            font-size: 1.5rem;` )
                ( `            font-weight: bold;` )
                ( `            text-align: center; }` )
                ( `</style>` )
                ( `<head>` )
                ( `    <title>Tic Tac Toe @ ABAP</title>` )
                ( `</head>` )
                ( `<body>` )
                ( `    <div class="board">` )
                ( `        <h1>TicTacToe @ ABAP</h1>` )
                ( `        <div class="info">Current Player: X</div>` )
                ( `        <table>` )
                ( `             <tr>` )
                ( `                 <td><button type="button" name="1" value="-" onclick="setloc('sapevent:ovr');"></button></td>` )
                ( `                 <td><button type="button" name="2" value="-"></button></td>` )
                ( `                 <td><button type="button" name="3" value="-"></button></td>` )
                ( `             </tr>` )
                ( `             <tr>` )
                ( `                 <td><button type="button" name="4" value="-">X</button></td>` )
                ( `                 <td><button type="button" name="5" value="-"></button></td>` )
                ( `                 <td><button type="button" name="6" value="-"></button></td>` )
                ( `             </tr>` )
                ( `             <tr>` )
                ( `                 <td><button type="button" name="7" value="-"></button></td>` )
                ( `                 <td><button type="button" name="8" value="-">O</button></td>` )
                ( `                 <td><button type="button" name="9" value="-"></button></td>` )
                ( `             </tr>` )
                ( `         </table>` )
                ( `    </div>` )
                ( ` </body>` )
                ( `</html>` ) ).

    SET HANDLER at_click.
  ENDMETHOD.

  METHOD display.
    cl_abap_browser=>show_html(
     EXPORTING
       html         = html_tab
       title        = 'TicTacToe'
       size         = cl_abap_browser=>large
       modal        = abap_true
       format       = cl_abap_browser=>landscape ).
  ENDMETHOD.

  METHOD at_click.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_game DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS init_game.
    METHODS place_turn
      IMPORTING
        row           TYPE i
        column        TYPE i
      RETURNING
        VALUE(winner) TYPE char1
      RAISING
        lcx_tictactoe.

  PRIVATE SECTION.
    DATA board         TYPE REF TO lif_board.
    DATA validator     TYPE REF TO lif_validator.
    DATA state_machine TYPE REF TO lif_state_machine.
    DATA display       TYPE REF TO lcl_display.
    METHODS as_game_is_running
      RAISING
        lcx_tictactoe.
    METHODS check_winner
      RETURNING
        VALUE(winner) TYPE char1.

ENDCLASS.

CLASS lcl_game IMPLEMENTATION.

  METHOD init_game.
    board         = NEW lcl_board( ).
    validator     = NEW lcl_validator( ).
    state_machine = NEW lcl_state( ).
    display       = NEW lcl_display( ).
    display->display( ).
  ENDMETHOD.

  METHOD place_turn.
    as_game_is_running( ).
    board->place_turn(  row    = row
                        column = column
                        value  = state_machine->state( ) ).
    winner = check_winner( ).
  ENDMETHOD.

  METHOD check_winner.
    IF validator->player_wins( player          = state_machine->state( )
                               board_as_string = board->stringify( ) ) .
      winner = state_machine->state( ).
      state_machine->final( ).
    ELSE.
      state_machine->next( ).
    ENDIF.
  ENDMETHOD.

  METHOD as_game_is_running.
    IF state_machine->state( ) = 'E'.
      RAISE EXCEPTION TYPE lcx_tictactoe
                      MESSAGE e001(00) WITH |The game is over, no more turns allowed!|.
    ENDIF.
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

CLASS ltc_board DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_board.

    METHODS setup.
    METHODS place_x_to_center             FOR TESTING.
    METHODS get_board_as_string           FOR TESTING.
    METHODS error_at_already_played_field FOR TESTING.
    METHODS error_at_wrong_row_number     FOR TESTING.
    METHODS error_at_wrong_column_number  FOR TESTING.

ENDCLASS.


CLASS ltc_board IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD place_x_to_center.
    TRY.
        cut->place_turn( row = 2 column = 2 value = 'X' ).
        cl_abap_unit_assert=>assert_equals(
            exp = VALUE lif_board=>board( ( column1 = '-' column2 = '-' column3 = '-' )
                                          ( column1 = '-' column2 = 'X' column3 = '-' )
                                          ( column1 = '-' column2 = '-' column3 = '-' ) )
            act = cut->get_board( ) ).
      CATCH lcx_tictactoe.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_board_as_string.
    TRY.
        cut->place_turn( row = 1 column = 1 value = 'S' ).
        cut->place_turn( row = 1 column = 2 value = 'T' ).
        cut->place_turn( row = 1 column = 3 value = 'R' ).
        cut->place_turn( row = 2 column = 1 value = 'I' ).
        cut->place_turn( row = 2 column = 2 value = 'N' ).
        cut->place_turn( row = 2 column = 3 value = 'G' ).
        cut->place_turn( row = 3 column = 1 value = 'I' ).
        cut->place_turn( row = 3 column = 2 value = 'F' ).
        cut->place_turn( row = 3 column = 3 value = 'Y' ).

        cl_abap_unit_assert=>assert_equals(
            exp = |STRINGIFY|
            act = cut->stringify( ) ).
      CATCH lcx_tictactoe.
        cl_abap_unit_assert=>fail(
            msg = |'An exception should not occur.| ).
    ENDTRY.

  ENDMETHOD.

  METHOD error_at_already_played_field.
    TRY.
        cut->place_turn( row = 1 column = 1 value = 'X' ).
        cut->place_turn( row = 1 column = 1 value = 'X' ).
      CATCH lcx_tictactoe INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error
        msg = |Object should be bound.| ).
  ENDMETHOD.

  METHOD error_at_wrong_row_number.
    TRY.
        cut->place_turn( row = 4  column = 1 value = 'X' ).
      CATCH lcx_tictactoe INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error
        msg = |Object should be bound.| ).
  ENDMETHOD.

  METHOD error_at_wrong_column_number.
    TRY.
        cut->place_turn( row = 3  column = 0 value = 'X' ).
      CATCH lcx_tictactoe INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error
        msg = |Object should be bound.| ).
  ENDMETHOD.



ENDCLASS.

CLASS ltc_validator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_validator.

    METHODS setup.

    METHODS player_o_has_a_win     FOR TESTING.
    METHODS player_x_has_no_win    FOR TESTING.
    METHODS player_x_has_a_win     FOR TESTING.
    METHODS player_o_has_no_win    FOR TESTING.

ENDCLASS.


CLASS ltc_validator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD player_o_has_a_win.
    cl_abap_unit_assert=>assert_true(
            act = cut->player_wins( player = 'O'
                                    board_as_string = |OOOX-X--X| ) ).
  ENDMETHOD.

  METHOD player_x_has_no_win.
    cl_abap_unit_assert=>assert_false(
            act = cut->player_wins( player = 'X'
                                    board_as_string = |OOOX-X--X| ) ).
  ENDMETHOD.

  METHOD player_x_has_a_win.
    cl_abap_unit_assert=>assert_true(
            act = cut->player_wins( player = 'X'
                                    board_as_string = |X--OXO--X| ) ).
  ENDMETHOD.

  METHOD player_o_has_no_win.
    cl_abap_unit_assert=>assert_false(
                act = cut->player_wins( player = 'O'
                                        board_as_string = |O--XO-O-X| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_game DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_game.

    METHODS setup.
    METHODS player_x_win_with_middle_row  FOR TESTING.
    METHODS error_at_turn_after_game_over FOR TESTING.
ENDCLASS.


CLASS ltc_game IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->init_game( ).
  ENDMETHOD.

  METHOD player_x_win_with_middle_row.
    TRY.
        cut->place_turn( row = 2 column = 2 ).
        cut->place_turn( row = 1 column = 1 ).
        cut->place_turn( row = 2 column = 1 ).
        cut->place_turn( row = 1 column = 2 ).

        cl_abap_unit_assert=>assert_equals(
            exp = 'X'
            act = cut->place_turn( row = 2 column = 3 )
            msg = 'Player X should be the winner.' ).
      CATCH lcx_tictactoe.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD error_at_turn_after_game_over.
    DATA winner TYPE char1.
    TRY.
        winner = cut->place_turn( row = 2 column = 2 ).
        winner = cut->place_turn( row = 1 column = 1 ).
        winner = cut->place_turn( row = 2 column = 1 ).
        winner = cut->place_turn( row = 1 column = 2 ).
        winner = cut->place_turn( row = 2 column = 3 ).
        winner = cut->place_turn( row = 3 column = 1 ).

      CATCH lcx_tictactoe INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
        act = lx_error
        msg = |Object should be bound.| ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(lo_game) = NEW lcl_game( ).
  lo_game->init_game( ).
