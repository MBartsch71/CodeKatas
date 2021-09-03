REPORT ymbh_russian_peasant_3.
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
INTERFACE lif_number.

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
  METHODS successor RETURNING VALUE(ro_result) TYPE REF TO lif_number
                    RAISING   lcx_russian_peasant.
ENDINTERFACE.

"! This interface contains the result table of the multiplication.
"! <p>The table is used for displaying the result as well as the calculation of the result.<br/>
"! All lines which are marked as relevant are added up to the result.</p>
INTERFACE lif_russian_peasant.
  ##TODO "Index prüfen, ob notwendig
  TYPES: BEGIN OF ts_step,
           index         TYPE i,
           left_number   TYPE i,
           right_number TYPE i,
           is_relevant   TYPE abap_bool,
         END OF ts_step.
  TYPES tt_steps TYPE SORTED TABLE OF ts_step WITH UNIQUE KEY primary_key COMPONENTS index.

  "! This method is the template for processing the single steps of the operation.
  "! @parameter io_left_number | The left operand
  "! @parameter io_right_number | The right operand
  "! @parameter rt_steps | Should return the complete list of the operations
  METHODS multiply    IMPORTING io_left_number  TYPE REF TO lif_number
                                io_right_number TYPE REF TO lif_number
                      RETURNING VALUE(rt_steps) TYPE lif_russian_peasant=>tt_steps.

  "! This method is the template for calculating the final result
  "! @parameter rv_result | The final result should be returned.
  METHODS calculate_result RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.


CLASS lcl_left_number DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_number.

    METHODS constructor IMPORTING iv_number TYPE i.
    METHODS is_odd      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS mc_even_divisor TYPE i VALUE 2.
    CONSTANTS mc_lower_bound  TYPE i VALUE 1.

    DATA mv_value TYPE i.

ENDCLASS.

CLASS lcl_left_number IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_number.
  ENDMETHOD.

  METHOD lif_number~value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD lif_number~successor.
    ro_result = COND #( WHEN mv_value > mc_lower_bound THEN NEW lcl_left_number( mv_value DIV mc_even_divisor )
                                                       ELSE THROW lcx_russian_peasant(  )  ).
  ENDMETHOD.

  METHOD is_odd.
    rv_result = xsdbool( mv_value MOD mc_even_divisor <> 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_right_number DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_number.
    METHODS constructor  IMPORTING iv_value TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_double_multiplicator TYPE i VALUE 2.
    DATA mv_value TYPE i.

ENDCLASS.

CLASS lcl_right_number IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD lif_number~value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD lif_number~successor.
    ro_result = NEW lcl_right_number( mv_value * mc_double_multiplicator ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_multiplication DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_russian_peasant.

  PRIVATE SECTION.
    CONSTANTS mc_start_step TYPE i  VALUE 1.

    DATA mt_steps TYPE lif_russian_peasant=>tt_steps.

    METHODS process_first_step    IMPORTING io_operand_2 TYPE REF TO lif_number
                                            io_operand_1 TYPE REF TO lif_number.

    METHODS process_further_steps IMPORTING iv_step_number TYPE i
                                            io_operand_1   TYPE REF TO lif_number
                                            io_operand_2   TYPE REF TO lif_number.

    METHODS increase_step_by_one  IMPORTING iv_step_number   TYPE i
                                  RETURNING VALUE(rv_result) TYPE i.

ENDCLASS.


CLASS lcl_multiplication IMPLEMENTATION.

  METHOD lif_russian_peasant~calculate_result.
    rv_result = REDUCE #( INIT sum = 0
                          FOR step IN mt_steps
                          NEXT sum = COND #( WHEN step-is_relevant = abap_true THEN sum + step-right_number
                                             ELSE sum ) ).
  ENDMETHOD.

  METHOD lif_russian_peasant~multiply.
    process_first_step( io_operand_1 = io_left_number
                        io_operand_2 = io_right_number ).

    process_further_steps( iv_step_number = mc_start_step
                           io_operand_1   = io_left_number->successor( )
                           io_operand_2   = io_right_number->successor( ) ).
    rt_steps = mt_steps.
  ENDMETHOD.

  METHOD process_first_step.
    mt_steps = VALUE #( ( index    = mc_start_step
                          left_number = io_operand_1->value( )
                          right_number = io_operand_2->value( )
                          is_relevant  = CAST lcl_left_number( io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD process_further_steps.
    TRY.
        process_further_steps( iv_step_number = increase_step_by_one( iv_step_number )
                               io_operand_1   = io_operand_1->successor( )
                               io_operand_2   = io_operand_2->successor( ) ).
      CATCH lcx_russian_peasant.
    ENDTRY.

    mt_steps = VALUE #( BASE mt_steps ( index    = increase_step_by_one( iv_step_number )
                                        left_number = io_operand_1->value( )
                                        right_number = io_operand_2->value( )
                                        is_relevant  = CAST lcl_left_number(  io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD increase_step_by_one.
    rv_result = iv_step_number + 1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run.

  PRIVATE SECTION.
    DATA mv_left_operand  TYPE i.
    DATA mv_right_operand TYPE i.
    DATA mt_steps  TYPE lif_russian_peasant=>tt_steps.
    DATA mv_result TYPE i.

    METHODS request_input_values.
    METHODS multiplication.
    METHODS output_result.

    METHODS print_header.
    METHODS print_body.

    METHODS prepare_output.
    METHODS print_table_header.
    METHODS print_table_footer.
    METHODS print_table_lines.
    METHODS print_result_line.
    METHODS display.

    METHODS start_new_table_line.

    METHODS write_left_number  IMPORTING i_line TYPE lif_russian_peasant=>ts_step.
    METHODS write_right_number IMPORTING i_line TYPE lif_russian_peasant=>ts_step.
    METHODS end_current_table_line.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD run.
    request_input_values( ).
    multiplication( ).
    output_result( ).
  ENDMETHOD.

  METHOD request_input_values.
    cl_demo_input=>add_text( |Russian peasant multiplication - Input operands.| ).
    cl_demo_input=>add_field( EXPORTING text  = |Operand 1: |
                              CHANGING  field = mv_left_operand ).

    cl_demo_input=>add_field( EXPORTING text  = |Operand 2: |
                              CHANGING  field = mv_right_operand ).
    cl_demo_input=>request( ).
  ENDMETHOD.

  METHOD multiplication.
    DATA(lo_russian_peasant)  = NEW lcl_multiplication( ).
    mt_steps  = lo_russian_peasant->lif_russian_peasant~multiply( io_left_number  = NEW lcl_left_number( mv_left_operand )
                                                                  io_right_number = NEW lcl_right_number( mv_right_operand ) ).
    mv_result = lo_russian_peasant->lif_russian_peasant~calculate_result( ).
  ENDMETHOD.

  METHOD output_result.
    prepare_output( ).
    print_header( ).
    print_body( ).
    display( ).
  ENDMETHOD.

  METHOD prepare_output.
    DATA(lv_css_styling) = |<style>| &&
                           |table \{ border-collapse: collapse; \}| &&
                           |th \{ padding: 5px; background-color: #e2e2e2; text-align: center \}| &&
                           |tr:nth-child(odd) \{ background-color: #f2f2f2 \}| &&
                           |td \{ padding: 5px; text-align: right \}| &&
                           |.centered \{ text-align: center \}| &&
                           |.total \{ font-weight: bold; background-color: #e2e2e2; border-top: 1px solid gray; border-bottom: double gray \}| &&
                           |</style>|.
    cl_demo_output=>write_html( lv_css_styling ).
  ENDMETHOD.

  METHOD print_header.
    cl_demo_output=>begin_section( |Multiplication: { mv_left_operand } x { mv_right_operand } .| ).
    cl_demo_output=>line( ).
  ENDMETHOD.

  METHOD print_body.
    print_table_header( ).
    print_table_lines( ).
    print_result_line( ).
    print_table_footer( ).
  ENDMETHOD.

  METHOD display.
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD print_table_header.
    cl_demo_output=>write_html( |<table>| ).
    cl_demo_output=>write_html( |<th align="right">Left number</th><th align="right">Right number</th>|  ).
  ENDMETHOD.

  METHOD print_table_lines.
    LOOP AT mt_steps ASSIGNING FIELD-SYMBOL(<line>).
      start_new_table_line( ).
      write_left_number( <line> ).
      write_right_number( <line> ).
      end_current_table_line( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD print_result_line.
    cl_demo_output=>write_html( |<tr class="total"><td>Total:</td><td>{ mv_result }</td></tr>| ).
  ENDMETHOD.

  METHOD print_table_footer.
    cl_demo_output=>write_html( |</table>| ).
  ENDMETHOD.

  METHOD write_left_number.
    cl_demo_output=>write_html( |<td>{ i_line-left_number }</td>| ).
  ENDMETHOD.

  METHOD write_right_number.
    DATA(lv_line) = SWITCH #( i_line-is_relevant
                                WHEN abap_true THEN |<td>{ i_line-right_number }</td>|
                                ELSE |<td><s>{ i_line-right_number } </s></td>| ).
    cl_demo_output=>write_html( lv_line ).
  ENDMETHOD.

  METHOD start_new_table_line.
    cl_demo_output=>write_html( |<tr>| ).
  ENDMETHOD.

  METHOD end_current_table_line.
    cl_demo_output=>write_html( |</tr>| ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_left_operaand DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_left_number.

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
        act = mo_cut->lif_number~successor( )->value( ) ).
  ENDMETHOD.

  METHOD no_more_operands_at_value_1.
    DO 7 TIMES.
      TRY.
          mo_cut = CAST lcl_left_number( mo_cut->lif_number~successor( ) ).
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
    DATA mo_cut TYPE REF TO lcl_right_number.

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
        act = mo_cut->lif_number~successor( )->value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_russian_peasant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_multiplication.

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
    rt_result = VALUE lif_russian_peasant=>tt_steps( ( index = 1 left_number = 47 right_number =   42 is_relevant = |X| )
                                                     ( index = 2 left_number = 23 right_number =   84 is_relevant = |X| )
                                                     ( index = 3 left_number = 11 right_number =  168 is_relevant = |X| )
                                                     ( index = 4 left_number =  5 right_number =  336 is_relevant = |X| )
                                                     ( index = 5 left_number =  2 right_number =  672 is_relevant = | | )
                                                     ( index = 6 left_number =  1 right_number = 1344 is_relevant = |X| ) ).
  ENDMETHOD.

  METHOD process_multiplication.
    rt_steps = mo_cut->lif_russian_peasant~multiply(
                 io_left_number  = NEW lcl_left_number( 47 )
                 io_right_number = NEW lcl_right_number( 42 ) ).
  ENDMETHOD.

  METHOD calculate_result.
    rv_result = mo_cut->lif_russian_peasant~calculate_result( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_application( )->run( ).
