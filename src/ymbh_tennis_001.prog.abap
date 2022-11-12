REPORT ymbh_tennis_001.

CLASS player DEFINITION.
  PUBLIC SECTION.
    METHODS score.
    METHODS get_points RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA points TYPE i.

ENDCLASS.

CLASS player IMPLEMENTATION.

  METHOD score.
    points = points + 1.
  ENDMETHOD.

  METHOD get_points.
    result = points.
  ENDMETHOD.

ENDCLASS.

CLASS score DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM points,
             love,
             fifteen,
             thirty,
             fourty,
             advantage,
             game,
           END OF ENUM points.

    TYPES: BEGIN OF ts_translation,
             source_expression TYPE string,
             translation       TYPE string,
           END OF ts_translation.
    TYPES tt_translation TYPE STANDARD TABLE OF ts_translation WITH EMPTY KEY.

    METHODS constructor.
    METHODS display IMPORTING player_1      TYPE REF TO player
                              player_2      TYPE REF TO player
                    RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA translation TYPE tt_translation.

    METHODS check_score IMPORTING score         TYPE string
                        RETURNING VALUE(result) TYPE string.

    METHODS translate IMPORTING player_score  TYPE i
                      RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS score IMPLEMENTATION.

  METHOD constructor.
    translation = VALUE #(
                    ( source_expression = |FOURTY FOURTY|    translation = |DEUCE| )
                    ( source_expression = |ADVANTAGE FOURTY| translation = |ADVANTAGE Player 1| )
                    ( source_expression = |FOURTY ADVANTAGE| translation = |ADVANTAGE Player 2| )
                    ( source_expression = |GAME FOURTY|      translation = |GAME Player 1| )
                    ( source_expression = |FOURTY GAME|      translation = |GAME Player 2| ) ).
  ENDMETHOD.

  METHOD display.
    result = check_score( |{ translate( player_1->get_points( ) ) } { translate( player_2->get_points( ) ) }| ).
  ENDMETHOD.

  METHOD check_score.
    result = VALUE #( translation[ source_expression = score ]-translation DEFAULT score ).
  ENDMETHOD.

  METHOD translate.
    result = CONV points( player_score ).
  ENDMETHOD.

ENDCLASS.


CLASS tc_player DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO player.

    METHODS setup.
    METHODS get_one_point FOR TESTING.

ENDCLASS.

CLASS tc_player IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_one_point.
    cut->score( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->get_points( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_score DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_test,
             description         TYPE string,
             player1_scores      TYPE i,
             player2_scores      TYPE i,
             expected_game_score TYPE string,
           END OF ts_test.
    TYPES tt_tests TYPE STANDARD TABLE OF ts_test WITH EMPTY KEY.

    TYPES: BEGIN OF ENUM points,
             love,
             fifteen,
             thirty,
             fourty,
             advantage,
             game,
           END OF ENUM points.

  PRIVATE SECTION.
    METHODS run_test_suite FOR TESTING.
    METHODS execute_tests IMPORTING tests TYPE tt_tests.

ENDCLASS.

CLASS tc_score IMPLEMENTATION.

  METHOD execute_tests.
    DATA(cut) = NEW score( ).

    LOOP AT tests REFERENCE INTO DATA(test).
      DATA(player1) = NEW player( ).
      DATA(player2) = NEW player( ).
      DO test->player1_scores TIMES.
        player1->score( ).
      ENDDO.
      DO test->player2_scores TIMES.
        player2->score( ).
      ENDDO.
      cl_abap_unit_assert=>assert_equals( exp = test->expected_game_score
                                          act = cut->display( player_1 = player1 player_2 = player2 )
                                          msg = test->description
                                          quit = if_aunit_constants=>no  ).
    ENDLOOP.
  ENDMETHOD.

  METHOD run_test_suite.
    DATA(tests_setup) = VALUE tt_tests(
      ( description = |Report the initial score|    expected_game_score = |LOVE LOVE|          player1_scores = 0 player2_scores = 0 )
      ( description = |Player 1 scores once|        expected_game_score = |FIFTEEN LOVE|       player1_scores = 1 player2_scores = 0 )
      ( description = |Player 1 scores twice|       expected_game_score = |THIRTY LOVE|        player1_scores = 2 player2_scores = 0 )
      ( description = |Player 2 scores once|        expected_game_score = |LOVE FIFTEEN|       player1_scores = 0 player2_scores = 1 )
      ( description = |Player 2 scores 3 times|     expected_game_score = |LOVE FOURTY|        player1_scores = 0 player2_scores = 3 )
      ( description = |Both players scores 3 times| expected_game_score = |DEUCE|              player1_scores = 3 player2_scores = 3 )
      ( description = |Player 1 advantage|          expected_game_score = |ADVANTAGE Player 1| player1_scores = 4 player2_scores = 3 )
      ( description = |Player 2 advantage|          expected_game_score = |ADVANTAGE Player 2| player1_scores = 3 player2_scores = 4 )
      ( description = |Player 1 win|                expected_game_score = |GAME Player 1|      player1_scores = 5 player2_scores = 3 )
      ( description = |Player 2 win|                expected_game_score = |GAME Player 2|      player1_scores = 3 player2_scores = 5 )
     ).
    execute_tests( tests_setup ).
  ENDMETHOD.

ENDCLASS.
