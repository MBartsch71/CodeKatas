##TODO
" * aufgabe entgegennehmen
" - linke Seite durch 2 teilen
" - rechte Seite verdoppeln
" - Abbruchkriterium ( linke Seite =1 )
" - Schritte in Tabelle merken
" - Zeile linke Seite = gerade Zahl ignorieren
" - Ergebnis berechnen
REPORT ymbh_russian_peasant_5.

CLASS russian_peasant DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF operation,
             left  TYPE i,
             right TYPE i,
           END OF operation.
    TYPES operations TYPE STANDARD TABLE OF operation WITH EMPTY KEY.

    METHODS calculate       IMPORTING operation TYPE operation.
    METHODS get_result_line RETURNING VALUE(result) TYPE operations.

    METHODS divide_number_by_2 IMPORTING number        TYPE i
                               RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA result TYPE operations.

ENDCLASS.

CLASS russian_peasant IMPLEMENTATION.

  METHOD calculate.
    result = VALUE #( ( operation ) ).
  ENDMETHOD.

  METHOD get_result_line.
    result = me->result.
  ENDMETHOD.

  METHOD divide_number_by_2.
    result = number DIV 2.
  ENDMETHOD.

ENDCLASS.


CLASS tc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF test_case,
             left   TYPE i,
             right  TYPE i,
             result TYPE russian_peasant=>operations,
           END OF test_case.
    TYPES test_cases TYPE STANDARD TABLE OF test_case WITH EMPTY KEY.

    DATA cut TYPE REF TO russian_peasant.

    METHODS setup.
    METHODS initialize_test_cases RETURNING VALUE(result) TYPE test_cases.

    METHODS perform_tests        FOR TESTING.
    METHODS divide_number_by_2   FOR TESTING.


ENDCLASS.


CLASS tc_russian_peasant IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD initialize_test_cases.
    result = VALUE #( ( left = 9 right = 2 result = VALUE #( ( left = 9  right = 2 ) ) )
                      ( left = 9 right = 2 result = VALUE #( ( left = 9  right = 2 ) ) ) ).
  ENDMETHOD.

  METHOD perform_tests.
    DATA(test_cases) = initialize_test_cases( ).
    LOOP AT test_cases REFERENCE INTO DATA(test_case).
      cut->calculate( VALUE russian_peasant=>operation( left = test_case->left right = test_case->right ) ).
      cl_abap_unit_assert=>assert_equals(
          exp = test_case->result
          act = cut->get_result_line( ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD divide_number_by_2.
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->divide_number_by_2( 9 ) ).
  ENDMETHOD.



ENDCLASS.
