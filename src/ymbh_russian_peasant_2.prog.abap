REPORT ymbh_russian_peasant_2.

INTERFACE lif_number.

  METHODS get_next_value
    RETURNING
      VALUE(rv_value) TYPE i.

  METHODS reset_number.

ENDINTERFACE.


INTERFACE lif_binary_number.
  TYPES tt_binary TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  METHODS to_table
    RETURNING
      VALUE(rt_bin_table) TYPE tt_binary.

  METHODS get_next_binary_digit
    RETURNING
      VALUE(rv_bin_digit) TYPE i.

  METHODS reset_digit_count.

ENDINTERFACE.


CLASS lcl_left_operand DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_binary_number.

    METHODS constructor
      IMPORTING
        iv_value TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_digit_end_marker   TYPE i VALUE -1.
    CONSTANTS mc_number_lower_bound TYPE i VALUE 0.
    CONSTANTS mc_number_1           TYPE i VALUE 1.
    CONSTANTS mc_number_2           TYPE i VALUE 2.

    DATA mt_value_binary_table TYPE lif_binary_number=>tt_binary.
    DATA mv_value              TYPE i.
    DATA mv_digit_count        TYPE i.

    METHODS build_binary_table
      IMPORTING
        iv_number           TYPE i
      RETURNING
        VALUE(rt_bin_table) TYPE lif_binary_number=>tt_binary.

    METHODS calculate_whole_part
      IMPORTING
        iv_number       TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS calculate_remainder
      IMPORTING
        iv_number       TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS increase_digit_count_by_one.

    METHODS determine_digit
      RETURNING
        VALUE(rv_bin_digit) TYPE i.

    METHODS numbr_greater_then_lower_bound
      IMPORTING
        iv_number        TYPE i
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS add_digit_to_binary_table
      IMPORTING
        iv_number              TYPE i
      RETURNING
        VALUE(rt_binary_table) TYPE lif_binary_number=>tt_binary.

ENDCLASS.

CLASS lcl_left_operand IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
    mt_value_binary_table = build_binary_table( mv_value ).
  ENDMETHOD.

  METHOD lif_binary_number~to_table.
    rt_bin_table = mt_value_binary_table.
  ENDMETHOD.

  METHOD lif_binary_number~get_next_binary_digit.
    increase_digit_count_by_one( ).
    rv_bin_digit = determine_digit( ).
  ENDMETHOD.

  METHOD increase_digit_count_by_one.
    mv_digit_count = mv_digit_count + mc_number_1.
  ENDMETHOD.

  METHOD determine_digit.
    TRY.
        rv_bin_digit = mt_value_binary_table[ mv_digit_count ].
      CATCH cx_sy_itab_line_not_found.
        rv_bin_digit = mc_digit_end_marker.
    ENDTRY.
  ENDMETHOD.

  METHOD build_binary_table.
    DATA(lv_whole_part) = calculate_whole_part( iv_number ).
    IF numbr_greater_then_lower_bound( lv_whole_part ).
      rt_bin_table = build_binary_table( lv_whole_part ).
    ENDIF.
    rt_bin_table = add_digit_to_binary_table( calculate_remainder( iv_number ) ).
  ENDMETHOD.

  METHOD add_digit_to_binary_table.
    mt_value_binary_table = VALUE #( BASE mt_value_binary_table ( iv_number ) ).
    rt_binary_table = mt_value_binary_table.
  ENDMETHOD.

  METHOD calculate_remainder.
    r_result = iv_number MOD mc_number_2.
  ENDMETHOD.

  METHOD calculate_whole_part.
    r_result = iv_number DIV mc_number_2.
  ENDMETHOD.

  METHOD lif_binary_number~reset_digit_count.
    CLEAR mv_digit_count.
  ENDMETHOD.

  METHOD numbr_greater_then_lower_bound.
    rv_result = xsdbool( iv_number > mc_number_lower_bound ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_right_operand DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_number.
    METHODS constructor
      IMPORTING
        iv_value TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_number_2 TYPE i VALUE 2.

    DATA mv_init_value         TYPE i.
    DATA mv_current_value      TYPE i.
    DATA mv_number_initialized TYPE abap_bool.

    METHODS double_current_value.

ENDCLASS.

CLASS lcl_right_operand IMPLEMENTATION.

  METHOD constructor.
    mv_init_value         = iv_value.
    mv_number_initialized = abap_true.
  ENDMETHOD.

  METHOD lif_number~get_next_value.
    double_current_value(  ).
    rv_value = mv_current_value.
  ENDMETHOD.

  METHOD double_current_value.
    mv_current_value = SWITCH #( mv_number_initialized WHEN abap_true THEN mv_init_value
                                                       ELSE mv_current_value * mc_number_2 ).
    mv_number_initialized = abap_false.
  ENDMETHOD.

  METHOD lif_number~reset_number.
    mv_number_initialized = abap_true.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_left_operand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_left_operand.

    METHODS setup.
    METHODS build_expected_table
      RETURNING
        VALUE(rt_expected) TYPE lif_binary_number=>tt_binary.

    METHODS get_binary_table_for_56      FOR TESTING.
    METHODS check_processing_end_for_56  FOR TESTING.
    METHODS get_binary_digit_after_reset FOR TESTING.

ENDCLASS.


CLASS ltc_left_operand IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 56 ).
  ENDMETHOD.

  METHOD build_expected_table.
    rt_expected = VALUE #( ( |1| )
                           ( |1| )
                           ( |1| )
                           ( |0| )
                           ( |0| )
                           ( |0| ) ).
  ENDMETHOD.

  METHOD get_binary_table_for_56.
    cl_abap_unit_assert=>assert_equals(
        msg = |The binary table should be like expected.|
        exp = build_expected_table(  )
        act = mo_cut->lif_binary_number~to_table( ) ).
  ENDMETHOD.

  METHOD check_processing_end_for_56.
    DO 6 TIMES.
      mo_cut->lif_binary_number~get_next_binary_digit( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        msg = |The expected_result should be delivered.|
        exp = -1
        act = mo_cut->lif_binary_number~get_next_binary_digit( ) ).
  ENDMETHOD.

  METHOD get_binary_digit_after_reset.
    DO 4 TIMES.
      mo_cut->lif_binary_number~get_next_binary_digit( ).
    ENDDO.
    mo_cut->lif_binary_number~reset_digit_count( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The returned binary digit should be the expected one.|
        exp = 1
        act = mo_cut->lif_binary_number~get_next_binary_digit( ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_right_operand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_right_operand.

    METHODS setup.
    METHODS get_dbl_value_after_1st_round  FOR TESTING.
    METHODS get_dbl_value_after_6th_round  FOR TESTING.
    METHODS get_original_value_after_reset FOR TESTING.
ENDCLASS.


CLASS ltc_right_operand IMPLEMENTATION.

  METHOD get_dbl_value_after_1st_round.
    cl_abap_unit_assert=>assert_equals(
        msg = |The value of the operandf should be like expected.|
        exp = 23
        act = mo_cut->lif_number~get_next_value( ) ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW #( 23 ).
  ENDMETHOD.

  METHOD get_dbl_value_after_6th_round.
    DO 5 TIMES.
      mo_cut->lif_number~get_next_value( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        msg = |The returned value should be like the expected one.|
        exp = 736
        act = mo_cut->lif_number~get_next_value( ) ).
  ENDMETHOD.

  METHOD get_original_value_after_reset.
    DO 3 TIMES.
      mo_cut->lif_number~get_next_value( ).
    ENDDO.
    mo_cut->lif_number~reset_number( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The original value should be returned.|
        exp = 23
        act = mo_cut->lif_number~get_next_value( ) ).
  ENDMETHOD.

ENDCLASS.
