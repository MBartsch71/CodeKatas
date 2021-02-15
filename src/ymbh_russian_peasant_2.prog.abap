REPORT ymbh_russian_peasant_2.
" The Russian Peasant Multiplication is a way of multiplying two numbers.
"
" The algorithm works as follows:
"   - The left operand is divided by 2, as whole number without rest, till the value is 1
"   - at the same time the right operand is doubled
"   - As soon as the left operand reaches the value 1 the result is calculated
"   - If the left value is an odd number then the right value is added to the result
"   - If the left value is even then the right value is ignored
"
" Example: 12 x 12
" -------------------------------------------------------
" Step 1 | 12 | 12 | Not relevant, left operand is even
" Step 2 |  6 | 24 | Not relevant, left operand is even
" Step 3 |  3 | 48 | Relevant, left operand is odd
" Step 4 |  1 | 96 | Relevant, left operand is odd
" =======================================================
" Result: Sum of all lines, where the left operand is odd ( 48 + 96 ) = 144


CLASS lcx_russian_peasant DEFINITION INHERITING FROM cx_dynamic_check.
ENDCLASS.

"! Interface of the operands.<br/>
"! This interface contains the template of the common function for both operands.
INTERFACE lif_operand.

  "! This method delivers the current value of the operand.
  "! @parameter rv_value | Returning value of the operand
  METHODS value     RETURNING VALUE(rv_value) TYPE i.

  "! This method delivers the successive value of the current operand.<br/>
  "! <p>Here the both implementations should differ:</p>
  "! <ul><li>The successor of the left operand is always the <strong><em>half</em></strong> of the value of the current operand</li>
  "! <li>The successor of the right operand is always the <strong><em>double</em></strong> of the value of the current operand</li></ul>
  "! <p>An exception is thrown if the value of the operand will be lesser then 1.</p>
  "! @parameter ro_result | Returns the successor
  "! @raising lcx_russian_peasant | Exception
  METHODS successor RETURNING VALUE(ro_result) TYPE REF TO lif_operand
                    RAISING   lcx_russian_peasant.
ENDINTERFACE.

"! This interface contains the result table of the multiplication.
"! <p>The table is used for displaying the result as well as the calculation of the result.<br/>
"! All lines which are marked as relevant are added up to the result.</p>
INTERFACE lif_russian_peasant.
  TYPES: BEGIN OF ts_step,
           number    TYPE i,
           operand_1 TYPE i,
           operand_2 TYPE i,
           relevant  TYPE abap_bool,
         END OF ts_step.
  TYPES tt_steps TYPE SORTED TABLE OF ts_step WITH UNIQUE KEY primary_key COMPONENTS number.

  "! This method is the template for processing the single steps of the operation.
  "! @parameter io_left_operand | The left operand
  "! @parameter io_right_operand | The right operand
  "! @parameter rt_steps | Should return the complete list of the operations
  METHODS process_steps    IMPORTING io_left_operand  TYPE REF TO lif_operand
                                     io_right_operand TYPE REF TO lif_operand
                           RETURNING VALUE(rt_steps)  TYPE lif_russian_peasant=>tt_steps.

  "! This method is the template for calculating the final result
  "! @parameter rv_result | The final result should be returned.
  METHODS calculate_result RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.


CLASS lcl_left_operand DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_operand.

    METHODS constructor IMPORTING iv_number TYPE i.

    METHODS is_odd      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS mc_even_divisor TYPE i VALUE 2.
    CONSTANTS mc_lower_bound  TYPE i VALUE 1.

    DATA mv_value TYPE i.

ENDCLASS.

CLASS lcl_left_operand IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_number.
  ENDMETHOD.

  METHOD lif_operand~value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD lif_operand~successor.
    ro_result = COND #( WHEN mv_value > mc_lower_bound THEN NEW lcl_left_operand( mv_value DIV mc_even_divisor )
                                                       ELSE THROW lcx_russian_peasant(  )  ).
  ENDMETHOD.

  METHOD is_odd.
    rv_result = xsdbool( mv_value MOD mc_even_divisor <> 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_right_operand DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_operand.
    METHODS constructor  IMPORTING iv_value TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_double_multiplicator TYPE i VALUE 2.
    DATA mv_value TYPE i.

ENDCLASS.

CLASS lcl_right_operand IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD lif_operand~value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD lif_operand~successor.
    ro_result = NEW lcl_right_operand( mv_value * mc_double_multiplicator ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_russian_peasant DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_russian_peasant.

  PRIVATE SECTION.
    CONSTANTS mc_start_step TYPE i  VALUE 1.

    DATA mt_steps TYPE lif_russian_peasant=>tt_steps.

    METHODS process_first_step    IMPORTING io_operand_2 TYPE REF TO lif_operand
                                            io_operand_1 TYPE REF TO lif_operand.

    METHODS process_further_steps IMPORTING iv_step_number TYPE i
                                            io_operand_1   TYPE REF TO lif_operand
                                            io_operand_2   TYPE REF TO lif_operand.

    METHODS increase_step_by_one  IMPORTING iv_step_number   TYPE i
                                  RETURNING VALUE(rv_result) TYPE i.

ENDCLASS.


CLASS lcl_russian_peasant IMPLEMENTATION.

  METHOD lif_russian_peasant~calculate_result.
    rv_result = REDUCE #( INIT sum = 0
                          FOR step IN mt_steps
                          NEXT sum = COND #( WHEN step-relevant = abap_true THEN sum + step-operand_2
                                             ELSE sum ) ).
  ENDMETHOD.

  METHOD lif_russian_peasant~process_steps.
    process_first_step( io_operand_1 = io_left_operand
                        io_operand_2 = io_right_operand ).

    process_further_steps( iv_step_number = mc_start_step
                           io_operand_1   = io_left_operand->successor( )
                           io_operand_2   = io_right_operand->successor( ) ).
    rt_steps = mt_steps.
  ENDMETHOD.

  METHOD process_first_step.
    mt_steps = VALUE #( ( number    = mc_start_step
                          operand_1 = io_operand_1->value( )
                          operand_2 = io_operand_2->value( )
                          relevant  = CAST lcl_left_operand( io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD process_further_steps.
    TRY.
        process_further_steps( iv_step_number = increase_step_by_one( iv_step_number )
                               io_operand_1   = io_operand_1->successor( )
                               io_operand_2   = io_operand_2->successor( ) ).
      CATCH lcx_russian_peasant.
    ENDTRY.

    mt_steps = VALUE #( BASE mt_steps ( number    = increase_step_by_one( iv_step_number )
                                        operand_1 = io_operand_1->value( )
                                        operand_2 = io_operand_2->value( )
                                        relevant  = CAST lcl_left_operand(  io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD increase_step_by_one.
    rv_result = iv_step_number + 1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run.

  PRIVATE SECTION.
    DATA mv_left_operand  TYPE i.
    DATA mv_right_operand TYPE i.
    DATA mt_steps  TYPE lif_russian_peasant=>tt_steps.
    DATA mv_result TYPE i.

    METHODS request_input_values.
    METHODS process_multiplication.
    METHODS output_result.

    METHODS print_header.
    METHODS print_total.
    METHODS print_steps.

    METHODS determine_line_color IMPORTING iv_line_relevant     TYPE abap_bool
                                 RETURNING VALUE(rv_line_color) TYPE i.

    METHODS print_step           IMPORTING iv_line TYPE lif_russian_peasant=>ts_step.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD run.
    request_input_values( ).
    process_multiplication( ).
    output_result( ).
  ENDMETHOD.

  METHOD request_input_values.
    cl_demo_input=>add_text( text = |Russian peasant multiplication - Input operands.| ).
    cl_demo_input=>add_field( EXPORTING text  = |Operand 1: |
                              CHANGING  field = mv_left_operand ).

    cl_demo_input=>add_field( EXPORTING text  = |Operand 2: |
                              CHANGING  field = mv_right_operand ).
    cl_demo_input=>request( ).
  ENDMETHOD.

  METHOD process_multiplication.
    DATA(lo_russian_peasant)  = NEW lcl_russian_peasant( ).
    mt_steps  = lo_russian_peasant->lif_russian_peasant~process_steps( io_left_operand  = NEW lcl_left_operand( mv_left_operand )
                                                                       io_right_operand = NEW lcl_right_operand( mv_right_operand ) ).
    mv_result = lo_russian_peasant->lif_russian_peasant~calculate_result( ).
  ENDMETHOD.

  METHOD output_result.
    print_header( ).
    print_steps( ).
    print_total( ).
  ENDMETHOD.

  METHOD print_header.
    WRITE / |{ mv_left_operand  WIDTH = 15 ALIGN = RIGHT } x { mv_right_operand WIDTH = 4 ALIGN = RIGHT } |.
    WRITE / |-----------------------|.
  ENDMETHOD.

  METHOD print_steps.
    LOOP AT mt_steps ASSIGNING FIELD-SYMBOL(<line>).
      print_step( <line> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD print_total.
    WRITE / |=======================|.
    WRITE / |{ mv_result WIDTH = 22 ALIGN = RIGHT }|.
  ENDMETHOD.

  METHOD print_step.
    DATA(color) = determine_line_color( iv_line-relevant ).
    WRITE / |Step: { iv_line-number WIDTH = 2 ALIGN = RIGHT } \| { iv_line-operand_1 WIDTH = 4 ALIGN = RIGHT } \| { iv_line-operand_2 WIDTH = 4 ALIGN = RIGHT } | COLOR = color.
  ENDMETHOD.

  METHOD determine_line_color.
    rv_line_color = SWITCH #( iv_line_relevant WHEN abap_true THEN col_positive
                                               ELSE col_negative ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_left_operaand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_left_operand.

    METHODS setup.

    METHODS check_if_value_is_even      FOR TESTING.
    METHODS get_value_of_successor      FOR TESTING.
    METHODS no_more_operands_at_value_1 FOR TESTING.

ENDCLASS.


CLASS ltc_left_operaand IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 47 ).
  ENDMETHOD.

  METHOD check_if_value_is_even.
    cl_abap_unit_assert=>assert_true(
        act =  mo_cut->is_odd( )
        msg =  |The value should be odd.| ).
  ENDMETHOD.

  METHOD get_value_of_successor.
    cl_abap_unit_assert=>assert_equals(
        msg = |The value of the first successor should be 23.|
        exp = 23
        act = mo_cut->lif_operand~successor( )->value( ) ).
  ENDMETHOD.

  METHOD no_more_operands_at_value_1.
    DO 7 TIMES.
      TRY.
          mo_cut = CAST lcl_left_operand( mo_cut->lif_operand~successor( ) ).
        CATCH lcx_russian_peasant INTO DATA(lx_error).
          EXIT.
      ENDTRY.
    ENDDO.

    cl_abap_unit_assert=>assert_bound(
        msg = |An exception should be thrown|
        act = lx_error ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_right_operand DEFINITION FINAL FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_right_operand.

    METHODS setup.
    METHODS get_value_of_successor FOR TESTING.

ENDCLASS.

CLASS ltc_right_operand IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 42 ).
  ENDMETHOD.

  METHOD get_value_of_successor.
    cl_abap_unit_assert=>assert_equals(
        msg = |The value should be 84.|
        exp = 84
        act = mo_cut->lif_operand~successor( )->value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_russian_peasant.

    METHODS setup.
    METHODS build_expected_result  RETURNING VALUE(rt_result) TYPE lif_russian_peasant=>tt_steps.
    METHODS process_multiplication RETURNING VALUE(rt_steps) TYPE lif_russian_peasant=>tt_steps.
    METHODS calculate_result       RETURNING VALUE(rv_result) TYPE i.

    METHODS get_all_steps_for_operation FOR TESTING.
    METHODS get_result_of_operation     FOR TESTING.

ENDCLASS.

CLASS ltc_russian_peasant IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD get_all_steps_for_operation.
    cl_abap_unit_assert=>assert_equals(
        msg = |The resulting table should be like exected.|
        exp = build_expected_result( )
        act = process_multiplication( ) ).
  ENDMETHOD.

  METHOD get_result_of_operation.
    process_multiplication( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The result should be 1974.|
        exp = 1974
        act = calculate_result( ) ).
  ENDMETHOD.

  METHOD build_expected_result.
    rt_result = VALUE lif_russian_peasant=>tt_steps( ( number = 1 operand_1 = 47 operand_2 =   42 relevant = |X| )
                                                     ( number = 2 operand_1 = 23 operand_2 =   84 relevant = |X| )
                                                     ( number = 3 operand_1 = 11 operand_2 =  168 relevant = |X| )
                                                     ( number = 4 operand_1 =  5 operand_2 =  336 relevant = |X| )
                                                     ( number = 5 operand_1 =  2 operand_2 =  672 relevant = | | )
                                                     ( number = 6 operand_1 =  1 operand_2 = 1344 relevant = |X| ) ).
  ENDMETHOD.

  METHOD process_multiplication.
    rt_steps = mo_cut->lif_russian_peasant~process_steps(
                 io_left_operand  = NEW lcl_left_operand( 47 )
                 io_right_operand = NEW lcl_right_operand( 42 ) ).
  ENDMETHOD.

  METHOD calculate_result.
    rv_result = mo_cut->lif_russian_peasant~calculate_result( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->run( ).
