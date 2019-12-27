REPORT ymbh_odd_even_prime.

CLASS lcl_kata_odd_even DEFINITION.
  PUBLIC SECTION.

    TYPES tt_range TYPE RANGE OF i.

    DATA mv_check_start_number TYPE i.
    DATA mv_check_end_number   TYPE i.

    "! Constructor method
    "! @parameter it_input_range | Input: Range of numbers
    METHODS constructor
      IMPORTING
        it_input_range TYPE tt_range.

    "! Method which checks the range of Number after ODD, EVEN and Prime
    "! @parameter rv_result_string | Resulting string
    METHODS check_prime_range
      RETURNING
        VALUE(rv_result_string) TYPE string.

  PRIVATE SECTION.
    CONSTANTS mc_odd  TYPE string VALUE 'Odd'  ##NO_TEXT.
    CONSTANTS mc_even TYPE string VALUE 'Even' ##NO_TEXT.
    CONSTANTS mc_one  TYPE i      VALUE 1.

    METHODS determine_number_types
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS number_is_even
      IMPORTING
        iv_number_to_check       TYPE i
      RETURNING
        VALUE(rv_number_is_even) TYPE abap_bool.

    METHODS number_is_prime
      IMPORTING
        iv_check_number           TYPE i
      RETURNING
        VALUE(rv_number_is_prime) TYPE abap_bool.

    METHODS increase_value
      IMPORTING
        iv_value        TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

ENDCLASS.

CLASS lcl_kata_odd_even IMPLEMENTATION.

  METHOD constructor.
    mv_check_start_number = it_input_range[ 1 ]-low.
    mv_check_end_number   = it_input_range[ 1 ]-high.
  ENDMETHOD.

  METHOD check_prime_range.
    IF mv_check_start_number <= mv_check_end_number.
      DATA(lv_result) = determine_number_types( ).
      mv_check_start_number = increase_value( mv_check_start_number ).
      rv_result_string = check_prime_range( ).
    ENDIF.
    rv_result_string = condense( |{ lv_result } { rv_result_string }| ).
  ENDMETHOD.

  METHOD determine_number_types.
    rv_result  = COND #( WHEN mv_check_start_number = mc_one               THEN mc_odd
                         WHEN ( number_is_prime( mv_check_start_number ) ) THEN mv_check_start_number
                         WHEN ( number_is_even( mv_check_start_number ) )  THEN mc_even
                         ELSE mc_odd ).
  ENDMETHOD.

  METHOD number_is_prime.
    DATA lv_divisor TYPE i VALUE 2.

    WHILE lv_divisor <= sqrt( iv_check_number ).
      IF iv_check_number MOD lv_divisor = 0.
        RETURN.
      ENDIF.
      lv_divisor = increase_value( lv_divisor ).
    ENDWHILE.
    rv_number_is_prime = abap_true.
  ENDMETHOD.

  METHOD number_is_even.
    rv_number_is_even = boolc( iv_number_to_check MOD 2 = 0 ).
  ENDMETHOD.

  METHOD increase_value.
    rv_value = iv_value + 1.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_unit_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS mc_check_string TYPE string VALUE 'Odd 2 3 Even 5 Even 7 Even Odd Even' ##NO_TEXT.
    DATA mo_cut TYPE REF TO lcl_kata_odd_even.

    METHODS setup.
    METHODS check_prime_range FOR TESTING.

ENDCLASS.

CLASS ltc_unit_tests IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( VALUE lcl_kata_odd_even=>tt_range( ( option = 'EQ' sign = 'I' low = 1 high = 10 ) ) ).
  ENDMETHOD.

  METHOD check_prime_range.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Der Ergebnis String sollte gleich dem Check String sein.'
        exp = mc_check_string
        act = mo_cut->check_prime_range( ) ).
  ENDMETHOD.

ENDCLASS.

DATA gv_range TYPE i.
SELECT-OPTIONS: s_range FOR gv_range NO-EXTENSION.

START-OF-SELECTION.

  DATA(go_main) = NEW lcl_kata_odd_even( s_range[] ).
  DATA(gv_check_string) = go_main->check_prime_range( ).
  WRITE gv_check_string.
