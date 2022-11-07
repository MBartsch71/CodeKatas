REPORT ymbh_tennis_001.

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
    DATA player_one_score TYPE i.
    DATA player_two_score TYPE i.

    METHODS report_the_initial_score FOR TESTING.
    METHODS player_one_scored_once FOR TESTING.
    METHODS player_one_scores_twice FOR TESTING.

    METHODS player_2_scored_once FOR TESTING.
    METHODS player_2_scored_3_times FOR TESTING.

    METHODS both_players_score_3_times FOR TESTING.

    METHODS player_one_advantage FOR TESTING.

    METHODS score RETURNING VALUE(result) TYPE string.
    METHODS translate IMPORTING player_score  TYPE i
                      RETURNING VALUE(result) TYPE string.
    METHODS check_score
      IMPORTING
        score         TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS tc_tennis IMPLEMENTATION.

  METHOD score.
    result = check_score( |{ translate( player_one_score ) } { translate( player_two_score ) }| ).
  ENDMETHOD.

  METHOD translate.
    result = CONV points( player_score ).
  ENDMETHOD.

  METHOD check_score.
    result = SWITCH #( score  WHEN 'FOURTY FOURTY' THEN 'DEUCE'
                              WHEN 'ADVANTAGE FOURTY' THEN 'ADVANTAGE Player 1'
                              ELSE score ).
  ENDMETHOD.

  METHOD report_the_initial_score.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE LOVE| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_scored_once.
    player_two_score = 1.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE FIFTEEN| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_scores_twice.
    player_two_score = 2.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE THIRTY| act = score( ) ).
  ENDMETHOD.

  METHOD player_2_scored_once.
    player_one_score = 1.
    cl_abap_unit_assert=>assert_equals( exp = |FIFTEEN LOVE| act = score( )  ).
  ENDMETHOD.

  METHOD player_2_scored_3_times.
    player_two_score = 3.
    cl_abap_unit_assert=>assert_equals( exp = |LOVE FOURTY| act = score( )  ).
  ENDMETHOD.

  METHOD both_players_score_3_times.
    player_one_score = 3.
    player_two_score = 3.
    cl_abap_unit_assert=>assert_equals( exp = |DEUCE| act = score( ) ).
  ENDMETHOD.

  METHOD player_one_advantage.
    player_one_score = 4.
    player_two_score = 3.
    cl_abap_unit_assert=>assert_equals( exp = |ADVANTAGE Player 1| act = score( ) ).
  ENDMETHOD.

ENDCLASS.
