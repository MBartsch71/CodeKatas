REPORT ymbh_odd_even.

INTERFACE lif_prime_number.

  "! Method for checking if a given number is a prime number
  "! @parameter iv_number | Number to check
  "! @parameter rv_number_is_prime | Result if number is a prime number
  METHODS validate_prime_number
    IMPORTING
      iv_number                 TYPE i
    RETURNING
      VALUE(rv_number_is_prime) TYPE abap_bool.

ENDINTERFACE.

CLASS lcl_prime_number DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_prime_number.
    ALIASES validate_prime_number FOR lif_prime_number~validate_prime_number.

    "! Constructor of the prime number class
    "! @parameter iv_initial_check_divisor | Initial value of divisor, default at 2
    METHODS constructor
      IMPORTING
        iv_initial_check_divisor TYPE i OPTIONAL.

  PRIVATE SECTION.
    DATA mv_check_divisor TYPE i.
    DATA mo_prime_number TYPE REF TO lif_prime_number.

    METHODS numbr_square_less_then_divisor
      IMPORTING
        iv_check_number  TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS is_num_dividable_by_divisor
      IMPORTING
        iv_check_number  TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS increase_divisor_by_one.
    METHODS mark_number_as_prime
      RETURNING
        VALUE(rv_number_is_prime) TYPE abap_bool.
    METHODS mark_number_as_non_prime
      RETURNING
        VALUE(rv_number_is_prime) TYPE abap_bool.

ENDCLASS.

CLASS lcl_prime_number IMPLEMENTATION.

  METHOD constructor.
    mv_check_divisor = COND i( WHEN iv_initial_check_divisor IS NOT INITIAL THEN iv_initial_check_divisor
                               ELSE 2 ).
  ENDMETHOD.

  METHOD validate_prime_number.
    WHILE numbr_square_less_then_divisor( iv_number ).
      IF is_num_dividable_by_divisor( iv_number ).
        rv_number_is_prime = mark_number_as_non_prime( ).
        RETURN.
      ENDIF.
      increase_divisor_by_one( ).
    ENDWHILE.
    rv_number_is_prime = mark_number_as_prime( ).
  ENDMETHOD.

  METHOD numbr_square_less_then_divisor.
    rv_result = xsdbool( mv_check_divisor <= sqrt( iv_check_number ) ).
  ENDMETHOD.

  METHOD is_num_dividable_by_divisor.
    rv_result = xsdbool( iv_check_number MOD mv_check_divisor = 0 ).
  ENDMETHOD.

  METHOD increase_divisor_by_one.
    mv_check_divisor = mv_check_divisor + 1.
  ENDMETHOD.

  METHOD mark_number_as_prime.
    rv_number_is_prime = abap_true.
  ENDMETHOD.

  METHOD mark_number_as_non_prime.
    rv_number_is_prime = abap_false.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_prime_number DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lif_prime_number.

    METHODS setup.
    METHODS approve_13_as_prime_number    FOR TESTING.
    METHODS approve_14_as_non_prime_number FOR TESTING.

ENDCLASS.

CLASS ltc_prime_number IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW lcl_prime_number( ).
  ENDMETHOD.

  METHOD approve_13_as_prime_number.
    cl_abap_unit_assert=>assert_equals(
        exp = abap_true
        act = mo_cut->validate_prime_number( 13 ) ).
  ENDMETHOD.

  METHOD approve_14_as_non_prime_number.
    cl_abap_unit_assert=>assert_equals(
        exp = abap_false
        act = mo_cut->validate_prime_number( 14 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_kata_odd_even DEFINITION.
  PUBLIC SECTION.

    TYPES tt_range TYPE RANGE OF i.

    "! Constructor method
    "! @parameter it_input_range | Input: Range of numbers
    METHODS constructor
      IMPORTING
        it_input_range TYPE tt_range.

    "! Method which converts a number range to ODD, EVEN and Prime
    "! @parameter rv_result_string | Resulting string
    METHODS convert_number_range
      RETURNING
        VALUE(rv_result_string) TYPE string.

  PRIVATE SECTION.
    CONSTANTS mc_odd  TYPE string VALUE 'Odd'  ##NO_TEXT.
    CONSTANTS mc_even TYPE string VALUE 'Even' ##NO_TEXT.
    CONSTANTS mc_one  TYPE i      VALUE 1.

    DATA mo_prime_number   TYPE REF TO lif_prime_number.
    DATA mv_start_number   TYPE i.
    DATA mv_end_number     TYPE i.
    DATA mv_current_number TYPE i.

    METHODS determine_type_of_number
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS number_is_even
      RETURNING
        VALUE(rv_number_is_even) TYPE abap_bool.

    METHODS increase_current_number_by_one.

    METHODS special_case_for_one
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS init_prime_number_validator.

    METHODS number_is_prime
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS current_number_is_in_range
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_kata_odd_even IMPLEMENTATION.

  METHOD constructor.
    mv_start_number   = it_input_range[ 1 ]-low.
    mv_end_number     = it_input_range[ 1 ]-high.
    mv_current_number = mv_start_number.
  ENDMETHOD.

  METHOD convert_number_range.
    IF ( current_number_is_in_range( ) ).
      DATA(lv_current_type) = determine_type_of_number( ).
      increase_current_number_by_one( ).
      rv_result_string = convert_number_range(  ).
    ENDIF.
    rv_result_string = condense( |{ lv_current_type } { rv_result_string }| ).
  ENDMETHOD.

  METHOD current_number_is_in_range.
    rv_result = boolc( mv_current_number <= mv_end_number ).
  ENDMETHOD.

  METHOD determine_type_of_number.
    rv_result  = COND #( WHEN ( special_case_for_one( ) ) THEN mc_odd
                         WHEN ( number_is_prime( ) )      THEN mv_current_number
                         WHEN ( number_is_even( ) )       THEN mc_even
                         ELSE mc_odd ).
  ENDMETHOD.

  METHOD special_case_for_one.
    rv_result = xsdbool( mv_current_number = mc_one ).
  ENDMETHOD.

  METHOD number_is_prime.
    init_prime_number_validator( ).
    rv_result = mo_prime_number->validate_prime_number( mv_current_number ).
  ENDMETHOD.

  METHOD number_is_even.
    rv_number_is_even = xsdbool( mv_current_number MOD 2 = 0 ).
  ENDMETHOD.

  METHOD init_prime_number_validator.
    mo_prime_number = NEW lcl_prime_number( ).
  ENDMETHOD.

  METHOD increase_current_number_by_one.
    mv_current_number = mv_current_number + 1.
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
        act = mo_cut->convert_number_range( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        iv_range TYPE lcl_kata_odd_even=>tt_range.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD run.
    DATA(lo_kata) = NEW lcl_kata_odd_even( iv_range ).
    cl_demo_output=>display( lo_kata->convert_number_range( ) ).
  ENDMETHOD.

ENDCLASS.

DATA gv_range TYPE i.
SELECT-OPTIONS: s_range FOR gv_range NO-EXTENSION.

START-OF-SELECTION.
  DATA(lo_kata) = NEW lcl_main( ).
  lo_kata->run( s_range[] ).
