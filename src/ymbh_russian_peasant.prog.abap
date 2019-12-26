REPORT ymbh_russian_peasant.

CLASS lcl_russian_peasant DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_russian_table,
             num1 TYPE i,
             num2 TYPE i,
           END OF ty_russian_table.

    "! Method which perform russian peasant multiplication
    "! @parameter is_term | Term with both numbers to multiplicate
    "! @parameter rs_result | Result from multiplication
    METHODS perform_russian_multiplication
      IMPORTING
        is_term          TYPE lcl_russian_peasant=>ty_russian_table
      RETURNING
        VALUE(rs_result) TYPE lcl_russian_peasant=>ty_russian_table.

  PRIVATE SECTION.
    METHODS first_term_is_odd
      IMPORTING
        iv_value      TYPE i
      RETURNING
        VALUE(rv_odd) TYPE abap_bool.

    METHODS divide_by_2
      IMPORTING
        iv_number       TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

    METHODS double_number
      IMPORTING
        iv_number       TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

ENDCLASS.

CLASS lcl_russian_peasant IMPLEMENTATION.

  METHOD divide_by_2.
    rv_value = iv_number DIV 2.
  ENDMETHOD.

  METHOD double_number.
    rv_value = iv_number * 2.
  ENDMETHOD.

  METHOD perform_russian_multiplication.
    IF is_term-num1 > 1.
      rs_result = VALUE #( num1 = divide_by_2( is_term-num1 )
                           num2 = double_number( is_term-num2 ) ).
      rs_result = perform_russian_multiplication( rs_result ).
    ENDIF.
    rs_result-num2 = SWITCH #( first_term_is_odd( is_term-num1 )
                                WHEN abap_true THEN rs_result-num2 + is_term-num2
                                ELSE rs_result-num2 ).
  ENDMETHOD.

  METHOD first_term_is_odd.
    rv_odd = SWITCH #( iv_value MOD 2
                          WHEN 0 THEN abap_false
                          ELSE abap_true ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS process_russian_mult_50_32 FOR TESTING.

ENDCLASS.

CLASS ltc_peasant IMPLEMENTATION.

  METHOD process_russian_mult_50_32.
    DATA(lo_cut) = NEW lcl_russian_peasant( ).
    DATA(ls_result) = lo_cut->perform_russian_multiplication(
                            VALUE lcl_russian_peasant=>ty_russian_table( num1 = 50 num2 = 32 ) ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The result of the russian peasant multiplication with 50 and 32 should be 1600.'
        exp = 1600
        act = ls_result-num2 ).
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b01.
PARAMETERS: p_num1 TYPE i,
            p_num2 TYPE i.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA(lo_russian_peasant) = NEW lcl_russian_peasant( ).
  DATA(lv_result) = lo_russian_peasant->perform_russian_multiplication(
                              VALUE lcl_russian_peasant=>ty_russian_table( num1 = p_num1 num2 = p_num2 )
                    ).
  WRITE |The result is { lv_result-num2 }|.
