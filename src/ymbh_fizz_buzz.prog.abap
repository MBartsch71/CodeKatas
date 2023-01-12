REPORT ymbh_fizz_buzz.

CLASS lcl_fizzbuzz DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS calculate IMPORTING input         TYPE i
                      RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_fizzbuzz IMPLEMENTATION.


  METHOD calculate.
    result = input.
  ENDMETHOD.

ENDCLASS.



CLASS tc_fizzbuzz DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS number_2_should_be_2 FOR TESTING.
    METHODS number_7_should_be_7 FOR TESTING.
ENDCLASS.


CLASS tc_fizzbuzz IMPLEMENTATION.

  METHOD number_2_should_be_2.
    DATA(cut) = NEW lcl_fizzbuzz( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->calculate( 2 ) ).
  ENDMETHOD.

  METHOD number_7_should_be_7.
    DATA(cut) = NEW lcl_fizzbuzz( ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = cut->calculate( 7 ) ).
  ENDMETHOD.

ENDCLASS.
