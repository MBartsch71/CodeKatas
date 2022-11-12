REPORT ymbh_tennis_001.

CLASS player DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING name TYPE string.

    METHODS score.

    METHODS get_points RETURNING VALUE(result) TYPE i.
    METHODS get_name RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA points TYPE i.
    DATA name TYPE string.

ENDCLASS.

CLASS player IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
  ENDMETHOD.

  METHOD score.
    points = points + 1.
  ENDMETHOD.

  METHOD get_points.
    result = points.
  ENDMETHOD.

  METHOD get_name.
    result = name.
  ENDMETHOD.

ENDCLASS.

CLASS tc_player DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO player.

    METHODS setup.
    METHODS get_one_point FOR TESTING.
    METHODS get_name FOR TESTING.

ENDCLASS.

CLASS tc_player IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |1| ).
  ENDMETHOD.

  METHOD get_one_point.
    cut->score( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->get_points( )  ).
  ENDMETHOD.

  METHOD get_name.
    cl_abap_unit_assert=>assert_equals( exp = |1| act = cut->get_name( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_tennis DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM points,
             love,
             fifteen,
             thirty,
             fourty,
             advantage,
             game,
           END OF ENUM points.

  PRIVATE SECTION.
    DATA player1 TYPE REF TO player.
    DATA player2 TYPE REF TO player.

    METHODS setup.
    METHODS report_the_initial_score FOR TESTING.
    METHODS player_one_scored_once FOR TESTING.
    METHODS player_one_scores_twice FOR TESTING.

    METHODS player_2_scored_once FOR TESTING.
    METHODS player_2_scored_3_times FOR TESTING.

    METHODS both_players_score_3_times FOR TESTING.

    METHODS player_one_advantage FOR TESTING.
    METHODS player_two_advantage FOR TESTING.

    METHODS player_one_win FOR TESTING.
    METHODS player_two_win FOR TESTING.

    METHODS score RETURNING VALUE(result) TYPE string.

    METHODS translate IMPORTING player_score  TYPE i
                      RETURNING VALUE(result) TYPE string.

    METHODS check_score IMPORTING score         TYPE string
                        RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS tc_tennis IMPLEMENTATION.

  METHOD setup.
    player1 = NEW #( |1| ).
    player2 = NEW #( |2| ).
  ENDMETHOD.

  METHOD score.
    result = check_score( |{ translate( player1->get_points( ) ) } { translate( player2->get_points( ) ) }| ).
  ENDMETHOD.

  METHOD translate.
    result = CONV points( player_score ).
  ENDMETHOD.

  METHOD check_score.
    result = SWITCH #( score  WHEN 'FOURTY FOURTY' THEN 'DEUCE'
                              WHEN 'ADVANTAGE FOURTY' THEN 'ADVANTAGE Player 1'
                              WHEN 'FOURTY ADVANTAGE' THEN 'ADVANTAGE Player 2'
                              WHEN 'GAME FOURTY' THEN 'GAME Player 1'
                              WHEN 'FOURTY GAME' THEN 'GAME Player 2'
                              ELSE score ).
  ENDMETHOD.

  METHOD report_the_initial_score.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE LOVE| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_scored_once.
    player2->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |LOVE FIFTEEN| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_scores_twice.
    DO 2 TIMES.
      player2->score( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE THIRTY| act = score( ) ).
  ENDMETHOD.

  METHOD player_2_scored_once.
    player1->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |FIFTEEN LOVE| act = score( )  ).
  ENDMETHOD.

  METHOD player_2_scored_3_times.
    DO 3 TIMES.
      player2->score( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE FOURTY| act = score( )  ).
  ENDMETHOD.

  METHOD both_players_score_3_times.
    DO 3 TIMES.
      player1->score( ).
      player2->score( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = |DEUCE| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_advantage.
    DO 3 TIMES.
      player1->score( ).
      player2->score( ).
    ENDDO.
    player1->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |ADVANTAGE Player 1| act = score( ) ).
  ENDMETHOD.

  METHOD player_two_advantage.
    DO 3 TIMES.
      player1->score( ).
      player2->score( ).
    ENDDO.
    player2->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |ADVANTAGE Player 2| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_win.
    DO 3 TIMES.
      player1->score( ).
      player2->score( ).
    ENDDO.
    player1->score( ).
    player1->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |GAME Player 1| act = score( ) ).
  ENDMETHOD.

  METHOD player_two_win.
    DO 3 TIMES.
      player1->score( ).
      player2->score( ).
    ENDDO.
    player2->score( ).
    player2->score( ).
    cl_abap_unit_assert=>assert_equals( exp = |GAME Player 2| act = score( ) ).
  ENDMETHOD.

ENDCLASS.
