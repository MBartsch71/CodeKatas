REPORT ymbh_russian_peasant_2.

##TODO "Task 1
##TODO "Naming auf allen Komponenten
##TODO "Erkläre komplexe Situationen -> Don't make me think, don't make others think
##TODO "IOSP verfeinern, auch bei Tests

##TODO "2. Task
##TODO "Ausgabe der Schritte


INTERFACE lif_number.

  "! Method delivers next double value of number
  "! @parameter rv_value | Returned double value
  METHODS get_next_double_value
    RETURNING
      VALUE(rv_value) TYPE i.

ENDINTERFACE.

INTERFACE lif_binary_number.

  TYPES tt_binary TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  "! Method delivers the next binary digit of the number
  "! @parameter rv_bin_digit | Returned binary digit
  METHODS get_next_binary_digit
    RETURNING
      VALUE(rv_bin_digit) TYPE i.

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
    INSERT iv_number INTO mt_value_binary_table INDEX 1.
    rt_binary_table = mt_value_binary_table.
  ENDMETHOD.

  METHOD calculate_remainder.
    r_result = iv_number MOD mc_number_2.
  ENDMETHOD.

  METHOD calculate_whole_part.
    r_result = iv_number DIV mc_number_2.
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

  METHOD lif_number~get_next_double_value.
    double_current_value(  ).
    rv_value = mv_current_value.
  ENDMETHOD.

  METHOD double_current_value.
    mv_current_value = SWITCH #( mv_number_initialized WHEN abap_true THEN mv_init_value
                                                       ELSE mv_current_value * mc_number_2 ).
    mv_number_initialized = abap_false.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_russian_peasant DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_operand_1 TYPE i
        iv_operand_2 TYPE i.

    "! Method performs the multiplication
    "! @parameter rv_product | Delivers the result
    METHODS multiply
      RETURNING
        VALUE(rv_product) TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_left_operand_processed TYPE i VALUE -1.
    CONSTANTS mc_odd_indicator TYPE i VALUE 1.

    DATA mo_right_operand TYPE REF TO lcl_right_operand.
    DATA mo_left_operand  TYPE REF TO lcl_left_operand.
    DATA mv_product       TYPE i.

    METHODS digit_is_applicable
      IMPORTING
        iv_value           TYPE i
      RETURNING
        VALUE(rv_is_valid) TYPE abap_bool.

    METHODS increase_product
      IMPORTING
        iv_number         TYPE i
      RETURNING
        VALUE(rv_product) TYPE i.

    METHODS determine_product
      IMPORTING
        iv_left_op_digit  TYPE i
        iv_right_op_value TYPE i
      RETURNING
        VALUE(rv_product) TYPE i.
    METHODS determine_next_right_value
      RETURNING
        VALUE(rv_next_right_value) TYPE i.

ENDCLASS.

CLASS lcl_russian_peasant IMPLEMENTATION.

  METHOD constructor.
    mo_left_operand  = NEW #( iv_operand_1 ).
    mo_right_operand = NEW #( iv_operand_2 ).
  ENDMETHOD.

  METHOD multiply.
    DATA lv_right_operand_double_value TYPE i.

    DATA(lv_left_operand_binary_digit) = mo_left_operand->lif_binary_number~get_next_binary_digit( ).
    WHILE digit_is_applicable( lv_left_operand_binary_digit ).

      rv_product = determine_product( iv_left_op_digit  = lv_left_operand_binary_digit
                                      iv_right_op_value = determine_next_right_value( )
 ).
      lv_left_operand_binary_digit = mo_left_operand->lif_binary_number~get_next_binary_digit( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD digit_is_applicable.
    rv_is_valid = xsdbool( iv_value > mc_left_operand_processed ).
  ENDMETHOD.

  METHOD increase_product.
    mv_product = mv_product + iv_number.
    rv_product = mv_product.
  ENDMETHOD.

  METHOD determine_product.
    rv_product = SWITCH #( iv_left_op_digit
                                       WHEN mc_odd_indicator
                                           THEN increase_product( iv_right_op_value )
                                       ELSE mv_product ).
  ENDMETHOD.



  METHOD determine_next_right_value.
    rv_next_right_value = mo_right_operand->lif_number~get_next_double_value( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_left_operand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_left_operand.

    METHODS setup.
    METHODS check_processing_end_for_56  FOR TESTING.

ENDCLASS.

CLASS ltc_left_operand IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 56 ).
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

ENDCLASS.

CLASS ltc_right_operand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_right_operand.

    METHODS setup.
    METHODS get_dbl_value_after_1st_round  FOR TESTING.
    METHODS get_dbl_value_after_6th_round  FOR TESTING.

ENDCLASS.

CLASS ltc_right_operand IMPLEMENTATION.

  METHOD get_dbl_value_after_1st_round.
    cl_abap_unit_assert=>assert_equals(
        msg = |The value of the operandf should be like expected.|
        exp = 23
        act = mo_cut->lif_number~get_next_double_value( ) ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW #( 23 ).
  ENDMETHOD.

  METHOD get_dbl_value_after_6th_round.
    DO 5 TIMES.
      mo_cut->lif_number~get_next_double_value( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        msg = |The returned value should be like the expected one.|
        exp = 736
        act = mo_cut->lif_number~get_next_double_value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_russian_peasant.

    METHODS setup.
    METHODS calculate_56_with_42 FOR TESTING.
ENDCLASS.

CLASS ltc_russian_peasant IMPLEMENTATION.

  METHOD calculate_56_with_42.
    cl_abap_unit_assert=>assert_equals(
        msg = |The expected result should be delivered.|
        exp = 2394
        act = mo_cut->multiply( ) ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW #( iv_operand_1 = 57 iv_operand_2 = 42 ).
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-001 FOR FIELD left_op.
PARAMETERS: left_op  TYPE i VISIBLE LENGTH 3,
            right_op TYPE i VISIBLE LENGTH 3.

SELECTION-SCREEN END OF LINE.

START-OF-SELECTION.
  DATA(lo_russian_peasant) = NEW lcl_russian_peasant( iv_operand_1 = left_op
                                                      iv_operand_2 = right_op ).

  WRITE: / |The result of the operation { left_op } * { right_op } is { lo_russian_peasant->multiply( ) } .|.
