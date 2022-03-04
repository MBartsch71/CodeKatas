REPORT ymbh_russian_peasant_4.

CLASS russian_peasant DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF operation,
             left_operand  TYPE i,
             right_operand TYPE i,
           END OF operation.
    TYPES operations TYPE STANDARD TABLE OF operation WITH EMPTY KEY.

    METHODS constructor        IMPORTING calculation_factor TYPE i DEFAULT 2.

    METHODS calculate          IMPORTING start_values  TYPE operation
                               RETURNING VALUE(result) TYPE operations.

    METHODS get_sum_of_right_operands IMPORTING calculation_list TYPE russian_peasant=>operations
                                      RETURNING VALUE(result)    TYPE i.

  PRIVATE SECTION.
    DATA calculation_factor TYPE i.

    CONSTANTS c_end_of_loops TYPE i VALUE 1.

    METHODS del_lines_with_odd_left_part IMPORTING temp_results  TYPE russian_peasant=>operations
                                         RETURNING VALUE(result) TYPE russian_peasant=>operations.

    METHODS is_odd_number                IMPORTING number        TYPE i
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS divide_number_by_factor      IMPORTING number        TYPE i
                                         RETURNING VALUE(result) TYPE i.

    METHODS multiply_number_with_factor  IMPORTING number        TYPE i
                                         RETURNING VALUE(result) TYPE i.

    METHODS process_step                 IMPORTING operation     TYPE russian_peasant=>operation
                                         RETURNING VALUE(result) TYPE russian_peasant=>operation.

    METHODS calculation_ended            IMPORTING left_operator TYPE i
                                         RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS russian_peasant IMPLEMENTATION.

  METHOD constructor.
    me->calculation_factor = calculation_factor.
  ENDMETHOD.

  METHOD divide_number_by_factor.
    result = number DIV calculation_factor.
  ENDMETHOD.

  METHOD multiply_number_with_factor.
    result = number * calculation_factor.
  ENDMETHOD.

  METHOD process_step.
    result = VALUE #( left_operand  = divide_number_by_factor( operation-left_operand )
                      right_operand = multiply_number_with_factor( operation-right_operand ) ).
  ENDMETHOD.

  METHOD calculate.
    DATA(result_line) = start_values.
    result = VALUE #( ( start_values ) ).
    WHILE NOT calculation_ended( result_line-left_operand ).
      result_line = process_step( result_line ).
      result = VALUE #( BASE result ( result_line ) ).
    ENDWHILE.
    result = del_lines_with_odd_left_part( result ).
  ENDMETHOD.

  METHOD calculation_ended.
    result = boolc( left_operator = c_end_of_loops ).
  ENDMETHOD.

  METHOD del_lines_with_odd_left_part.
    LOOP AT temp_results REFERENCE INTO DATA(line).
      IF is_odd_number( line->left_operand ).
        result = VALUE #( BASE result ( line->* ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_odd_number.
    result = xsdbool( number MOD calculation_factor <> 0 ).
  ENDMETHOD.

  METHOD get_sum_of_right_operands.
    result = REDUCE #( INIT sum = 0
                       FOR line IN calculation_list
                       NEXT sum = sum + line-right_operand ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF test_case,
             left_operand    TYPE i,
             right_operand   TYPE i,
             expected_result TYPE russian_peasant=>operations,
           END OF test_case.
    TYPES test_cases TYPE STANDARD TABLE OF test_case WITH EMPTY KEY.

    DATA cut TYPE REF TO russian_peasant.

    METHODS setup.
    METHODS initialize_test_cases RETURNING VALUE(result) TYPE test_cases.

    METHODS acceptance_test      FOR TESTING.
    METHODS calculate_operations FOR TESTING.

ENDCLASS.

CLASS tc_russian_peasant IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD acceptance_test.
    DATA(calculation_list) = cut->calculate( VALUE #( left_operand = 47 right_operand = 42 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1974
        act = cut->get_sum_of_right_operands( calculation_list ) ).
  ENDMETHOD.

  METHOD initialize_test_cases.
    result = VALUE #( ( left_operand =  9 right_operand =  2 expected_result = VALUE #( ( left_operand =  9 right_operand =  2  )
                                                                                        ( left_operand =  1 right_operand = 16 ) ) )
                      ( left_operand = 47 right_operand = 42 expected_result = VALUE #( ( left_operand = 47 right_operand = 42 )
                                                                                        ( left_operand = 23 right_operand = 84 )
                                                                                        ( left_operand = 11 right_operand = 168 )
                                                                                        ( left_operand =  5 right_operand = 336 )
                                                                                        ( left_operand =  1 right_operand = 1344 )  ) ) ).
  ENDMETHOD.

  METHOD calculate_operations.
    DATA(test_cases) = initialize_test_cases( ).
    LOOP AT test_cases REFERENCE INTO DATA(test_case).
      cl_abap_unit_assert=>assert_equals(
          exp = test_case->expected_result
          act = cut->calculate( VALUE #( left_operand = test_case->left_operand right_operand = test_case->right_operand ) )
          quit = if_aunit_constants=>no ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
