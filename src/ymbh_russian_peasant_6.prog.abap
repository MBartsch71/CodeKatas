REPORT ymbh_russian_peasant_6.

CLASS russian_peasant DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF calculation_entry,
             multiplicator TYPE i,
             multiplicant  TYPE i,
           END OF calculation_entry.
    TYPES calculation TYPE STANDARD TABLE OF calculation_entry WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE string.

    METHODS get_result_table RETURNING VALUE(result) TYPE calculation.

    METHODS calculate RETURNING VALUE(result) TYPE i.

    METHODS calculate_rows.

  PRIVATE SECTION.
    DATA calculation_steps TYPE calculation.

    METHODS split_input IMPORTING input TYPE string.

ENDCLASS.

CLASS russian_peasant IMPLEMENTATION.

  METHOD constructor.
    split_input( input ).
  ENDMETHOD.

  METHOD get_result_table.
    result = calculation_steps.
  ENDMETHOD.

  METHOD split_input.
    SPLIT input AT |*| INTO DATA(part1) DATA(part2).
    calculation_steps = VALUE #( ( multiplicator = part1 multiplicant = part2 ) ).
  ENDMETHOD.

  METHOD calculate_rows.
    WHILE calculation_steps[ lines( calculation_steps ) ]-multiplicator > 1.
      calculation_steps = VALUE #( BASE calculation_steps ( multiplicator = calculation_steps[ lines( calculation_steps ) ]-multiplicator DIV 2
                                                            multiplicant  = calculation_steps[ lines( calculation_steps ) ]-multiplicant * 2 ) ).
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate.
    result = REDUCE #( INIT sum = 0
                       FOR line IN calculation_steps
                       NEXT sum = COND #( WHEN line-multiplicator MOD 2 = 0 THEN sum
                                          ELSE sum + line-multiplicant ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF calculation_entry,
             multiplicator TYPE i,
             multiplicant  TYPE i,
           END OF calculation_entry.
    TYPES calculation TYPE STANDARD TABLE OF calculation_entry WITH EMPTY KEY.
    DATA cut TYPE REF TO russian_peasant.

    METHODS setup.

    METHODS get_result_after_init FOR TESTING.
    METHODS perform_full_operation FOR TESTING.
    METHODS calculate_result_full_operatn FOR TESTING.

ENDCLASS.

CLASS tc_russian_peasant IMPLEMENTATION.

  METHOD setup.
    cut = NEW russian_peasant( |47 * 42| ).
  ENDMETHOD.

  METHOD perform_full_operation.
    DATA(expected_values) = VALUE calculation( ( multiplicator = 47 multiplicant = 42 )
                                               ( multiplicator = 23 multiplicant = 84 )
                                               ( multiplicator = 11 multiplicant = 168 )
                                               ( multiplicator = 5  multiplicant = 336 )
                                               ( multiplicator = 2  multiplicant = 672 )
                                               ( multiplicator = 1  multiplicant = 1344 ) ).
    cut->calculate_rows( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_result_table( )  ).
  ENDMETHOD.

  METHOD calculate_result_full_operatn.
    cut->calculate_rows( ).
    cl_abap_unit_assert=>assert_equals( exp = 1974 act = cut->calculate( ) ).
  ENDMETHOD.

  METHOD get_result_after_init.
    DATA(expected_values) = VALUE calculation( ( multiplicator = 47 multiplicant = 42 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_result_table( )  ).
  ENDMETHOD.

ENDCLASS.
