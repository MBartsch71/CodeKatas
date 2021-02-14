REPORT ymbh_russian_peasant_2.
##TODO "Task 1
##TODO "Naming auf allen Komponenten
##TODO "Erkläre komplexe Situationen -> Don't make me think, don't make others think
##TODO "IOSP verfeinern, auch bei Tests

" The Russian Peasant Multiplication is a way of multiplying two numbers.
"
" The algorithm works as follows:
"   - The left operand is divided by 2, as whole number without rest, till the value is 1
"   - at the same time the right operand is doubled
"   - As far as the left operand has the value 1 then the result is calculated
"   - If the left value is an odd number then the right value is added to the result
"   - If the left value is even then the right value is ignored


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
           index     TYPE i,
           operand_1 TYPE i,
           operand_2 TYPE i,
           relevant  TYPE abap_bool,
         END OF ts_step.
  TYPES tt_steps TYPE SORTED TABLE OF ts_step WITH UNIQUE KEY primary_key COMPONENTS index.

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


CLASS lcl_left_operand DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_operand.

    METHODS constructor IMPORTING iv_number TYPE i.
    METHODS is_odd      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
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
    ro_result = COND #( WHEN mv_value > 1 THEN NEW lcl_left_operand( mv_value DIV 2 )
                                          ELSE THROW lcx_russian_peasant(  )  ).
  ENDMETHOD.

  METHOD is_odd.
    rv_result = xsdbool( mv_value MOD 2 <> 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_right_operand DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_operand.
    METHODS constructor  IMPORTING iv_value TYPE i.

  PRIVATE SECTION.
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
    ro_result = NEW lcl_right_operand( mv_value * 2 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_russian_peasant DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_russian_peasant.

  PRIVATE SECTION.
    DATA mt_steps TYPE lif_russian_peasant=>tt_steps.

    METHODS process_first_step    IMPORTING io_operand_2 TYPE REF TO lif_operand
                                            io_operand_1 TYPE REF TO lif_operand.

    METHODS process_further_steps IMPORTING iv_index        TYPE i
                                            io_operand_1    TYPE REF TO lif_operand
                                            io_operand_2    TYPE REF TO lif_operand
                                  RETURNING VALUE(rt_steps) TYPE lif_russian_peasant=>tt_steps.

ENDCLASS.


CLASS lcl_russian_peasant IMPLEMENTATION.

  METHOD lif_russian_peasant~process_steps.
    process_first_step( io_operand_1 = io_left_operand
                        io_operand_2 = io_right_operand ).

    mt_steps = VALUE #( BASE mt_steps
                        ( LINES OF process_further_steps( iv_index     = 1
                                                          io_operand_1 = io_left_operand->successor( )
                                                          io_operand_2 = io_right_operand->successor( ) ) ) ).
    rt_steps = mt_steps.
  ENDMETHOD.

  METHOD process_first_step.
    mt_steps = VALUE #( ( index = 1
                          operand_1 = io_operand_1->value( )
                          operand_2 = io_operand_2->value( )
                          relevant  = CAST lcl_left_operand( io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD process_further_steps.
    TRY.
        rt_steps = process_further_steps( iv_index     = iv_index + 1
                                          io_operand_1 = io_operand_1->successor( )
                                          io_operand_2 = io_operand_2->successor( ) ).
      CATCH lcx_russian_peasant.
    ENDTRY.

    rt_steps = VALUE #( BASE rt_steps ( index = iv_index + 1
                                        operand_1 = io_operand_1->value( )
                                        operand_2 = io_operand_2->value( )
                                        relevant = CAST lcl_left_operand(  io_operand_1 )->is_odd( ) ) ).
  ENDMETHOD.

  METHOD lif_russian_peasant~calculate_result.
    rv_result = REDUCE #( INIT sum = 0
                          FOR step IN mt_steps
                          NEXT sum = COND #( WHEN step-relevant = abap_true THEN sum + step-operand_2
                                             ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.


  METHOD run.
    DATA lv_field1 TYPE i.
    DATA lv_field2 TYPE i.

    cl_demo_input=>add_text( text = |Russian peasant multiplication - Input operands.| ).
    cl_demo_input=>add_field( EXPORTING text  = |Operand 1: |
                              CHANGING  field = lv_field1 ).

    cl_demo_input=>add_field( EXPORTING text  = |Operand 2: |
                              CHANGING  field = lv_field2 ).
    cl_demo_input=>request( ).

    DATA(lo_russian_peasant) = NEW lcl_russian_peasant( ).
    DATA(lt_result) = lo_russian_peasant->lif_russian_peasant~process_steps( io_left_operand = NEW lcl_left_operand( lv_field1 )
                                                                             io_right_operand = NEW lcl_right_operand( lv_field2 ) ).

    WRITE / |{ lv_field1  WIDTH = 15 ALIGN = RIGHT } x { lv_field2 WIDTH = 4 ALIGN = RIGHT } |.
    WRITE / |-----------------------|.
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<line>).
      DATA(color) = SWITCH #( <line>-relevant WHEN abap_true THEN col_positive
                                              ELSE col_negative ).

      WRITE / |Step: { <line>-index WIDTH = 2 ALIGN = RIGHT } \| { <line>-operand_1 WIDTH = 4 ALIGN = RIGHT } \| { <line>-operand_2 WIDTH = 4 ALIGN = RIGHT } | COLOR = color.
    ENDLOOP.
    WRITE / |=======================|.
    WRITE / |{ lo_russian_peasant->lif_russian_peasant~calculate_result( ) WIDTH = 22 ALIGN = RIGHT }|.
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
    METHODS get_all_steps_for_operation FOR TESTING.
    METHODS get_result_of_operation     FOR TESTING.
ENDCLASS.

CLASS ltc_russian_peasant IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD get_all_steps_for_operation.
    DATA(lt_expectect_values) = VALUE lif_russian_peasant=>tt_steps(
                                       ( index = 1 operand_1 = 47 operand_2 = 42   relevant = |X| )
                                       ( index = 2 operand_1 = 23 operand_2 = 84   relevant = |X| )
                                       ( index = 3 operand_1 = 11 operand_2 = 168  relevant = |X| )
                                       ( index = 4 operand_1 = 5  operand_2 = 336  relevant = |X| )
                                       ( index = 5 operand_1 = 2  operand_2 = 672  relevant = | | )
                                       ( index = 6 operand_1 = 1  operand_2 = 1344 relevant = |X| ) ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The resulting table should be like exected.|
        exp = lt_expectect_values
        act = mo_cut->lif_russian_peasant~process_steps( io_left_operand = NEW lcl_left_operand( 47 )
                                     io_right_operand = NEW lcl_right_operand( 42 ) ) ).
  ENDMETHOD.

  METHOD get_result_of_operation.
    mo_cut->lif_russian_peasant~process_steps( io_left_operand = NEW lcl_left_operand( 47 )
                           io_right_operand = NEW lcl_right_operand( 42 ) ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The result should be 1974.|
        exp = 1974
        act = mo_cut->lif_russian_peasant~calculate_result( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->run( ).
