##TODO
"* create input text with 2 words
"* store input text in object
"* try different blank settings (1 in front, 1 in the end, multiple between words
"* build parameterized tests
"* cut word from text
"* check if text is empty
"* build an output text line
"* check the output limit
"* create output text object
"* build output text table
"* add 2 lines to output text table
"- application class
"- simulate run in application class
"- possibility to input text
"- possibility to input the output text width
"- display the whole output text
"- visualize limit range in output
REPORT ymbh_word_wrap_2.

CLASS x_input_text_empty DEFINITION INHERITING FROM cx_no_check FINAL.
ENDCLASS.

CLASS x_line_limit_exceeded DEFINITION INHERITING FROM cx_no_check FINAL.
ENDCLASS.

CLASS input_text DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor          IMPORTING content TYPE string.

    METHODS get_text_wordwise    RETURNING VALUE(result) TYPE stringtab.

    METHODS extract_leading_word RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA content_as_table TYPE stringtab.

    METHODS split_text_to_table IMPORTING content TYPE string.

ENDCLASS.

CLASS input_text IMPLEMENTATION.

  METHOD constructor.
    split_text_to_table( content ).
  ENDMETHOD.

  METHOD get_text_wordwise.
    result = content_as_table.
  ENDMETHOD.

  METHOD split_text_to_table.
    SPLIT content AT space INTO TABLE content_as_table.
    DELETE content_as_table WHERE table_line = space.
  ENDMETHOD.

  METHOD extract_leading_word.
    TRY.
        result = content_as_table[ 1 ].
        DELETE content_as_table INDEX 1.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE x_input_text_empty.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS output_line DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING text TYPE string.

    METHODS content     RETURNING VALUE(result) TYPE string.

    METHODS add_word    IMPORTING word          TYPE string
                        RETURNING VALUE(result) TYPE REF TO output_line.

    METHODS limit       RETURNING VALUE(result) TYPE i.

    METHODS set_limit   IMPORTING line_width TYPE i.

  PRIVATE SECTION.
    DATA text TYPE string.
    DATA line_width_limit TYPE i VALUE 80.

ENDCLASS.

CLASS output_line IMPLEMENTATION.

  METHOD constructor.
    me->text = text.
  ENDMETHOD.

  METHOD content.
    result = text.
  ENDMETHOD.

  METHOD add_word.
    DATA(new_line_length) = strlen( text ) + strlen( word ) + 1.
    IF new_line_length > line_width_limit.
      RAISE EXCEPTION TYPE x_line_limit_exceeded.
    ENDIF.
    result = NEW #( |{ text } { word }| ).
    result->set_limit( line_width_limit ).
  ENDMETHOD.

  METHOD limit.
    result = line_width_limit.
  ENDMETHOD.

  METHOD set_limit.
    line_width_limit = line_width.
  ENDMETHOD.

ENDCLASS.

CLASS output_text DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS add_line IMPORTING line TYPE string.

    METHODS output   RETURNING VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    DATA content TYPE stringtab.

ENDCLASS.

CLASS output_text IMPLEMENTATION.

  METHOD output.
    result = content.
  ENDMETHOD.

  METHOD add_line.
    content = VALUE #( BASE content ( line ) ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS main IMPORTING text          TYPE string
                           length        TYPE i
                 RETURNING VALUE(result) TYPE stringtab.

  PRIVATE SECTION.

ENDCLASS.

CLASS application IMPLEMENTATION.


  METHOD main.
    DATA(input_text) = NEW input_text( text ).
    DATA(output_line) = NEW output_line( || ).
    DATA(output_text) = NEW output_text( ).
    output_line->set_limit( length ).
    DO.
      TRY.
          DO.
            TRY.
                DATA(word) = input_text->extract_leading_word( ).
                output_line = output_line->add_word( word ).
              CATCH x_line_limit_exceeded.
                output_text->add_line( output_line->content( ) ).
                output_line = NEW output_line( || ).
                output_line->set_limit( length ).
            ENDTRY.
          ENDDO.
        CATCH x_input_text_empty.
          EXIT.
      ENDTRY.
    ENDDO.

    result = output_text->output( ).
  ENDMETHOD.

ENDCLASS.


CLASS tc_input_text DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF test_running_data,
             description     TYPE string,
             input_text      TYPE string,
             expected_values TYPE stringtab,
           END OF test_running_data.
    TYPES test_running_data_set TYPE STANDARD TABLE OF test_running_data WITH EMPTY KEY.

    DATA cut TYPE REF TO input_text.
    DATA test_suite TYPE test_running_data_set.

    METHODS setup.
    METHODS build_test_suite RETURNING VALUE(result) TYPE test_running_data_set.

    METHODS run_word_combination_tests    FOR TESTING.
    METHODS cut_leading_word_from_text    FOR TESTING.
    METHODS exception_at_empty_input_text FOR TESTING.

ENDCLASS.

CLASS tc_input_text IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |Hello world!| ).
    test_suite = build_test_suite( ).
  ENDMETHOD.

  METHOD build_test_suite.
    result = VALUE #( ( description     = |Store text wordwise in object|
                        input_text      = |Hello world!|
                        expected_values = VALUE #( ( |Hello| )
                                                   ( |world!| ) ) )

                      ( description     = |Input text with two spaces between words|
                        input_text      = |Hello  world!|
                        expected_values = VALUE #( ( |Hello| )
                                                   ( |world!| ) ) )

                      ( description     = |Input text with leading space|
                        input_text      = | Hello world!|
                        expected_values = VALUE #( ( |Hello| )
                                                   ( |world!| ) ) )

                      ( description     = |Input text with trailing spaces|
                        input_text      = |Hello world!  |
                        expected_values = VALUE #( ( |Hello| )
                                                   ( |world!| ) ) )

                      ( description     = |Longer input text with varying spaces between words|
                        input_text      = |Hello  world in   five     words!|
                        expected_values = VALUE #( ( |Hello| )
                                                   ( |world| )
                                                   ( |in| )
                                                   ( |five| )
                                                   ( |words!| ) ) ) ).

  ENDMETHOD.

  METHOD run_word_combination_tests.
    LOOP AT test_suite REFERENCE INTO DATA(test_case).
      cut = NEW #( test_case->input_text ).
      cl_abap_unit_assert=>assert_equals( exp = test_case->expected_values act = cut->get_text_wordwise( ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD cut_leading_word_from_text.
    DATA(expected_text_table) = VALUE stringtab( ( |world!| ) ).
    cut->extract_leading_word( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

  METHOD exception_at_empty_input_text.
    TRY.
        DO 3 TIMES.
          cut->extract_leading_word( ).
        ENDDO.
        cl_abap_unit_assert=>fail( ).
      CATCH x_input_text_empty INTO DATA(error).
        cl_abap_unit_assert=>assert_bound( act = error msg = |The object should be bound!| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS tc_output_line DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO output_line.

    METHODS setup.
    METHODS get_output_line               FOR TESTING.
    METHODS add_word_to_output_line       FOR TESTING.
    METHODS get_the_default_line_limit    FOR TESTING.
    METHODS set_specifiv_line_limit       FOR TESTING.
    METHODS exception_at_exceeding_line   FOR TESTING.
    METHODS build_specific_line_wo_excptn FOR TESTING.

ENDCLASS.

CLASS tc_output_line IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |Hello| ).
  ENDMETHOD.

  METHOD get_output_line.
    cl_abap_unit_assert=>assert_equals( exp = |Hello| act = cut->content( )  ).
  ENDMETHOD.

  METHOD add_word_to_output_line.
    cut = cut->add_word( |world!| ).
    cl_abap_unit_assert=>assert_equals( exp = |Hello world!| act = cut->content( )  ).
  ENDMETHOD.

  METHOD get_the_default_line_limit.
    cl_abap_unit_assert=>assert_equals( exp = 80 act = cut->limit( )  ).
  ENDMETHOD.

  METHOD set_specifiv_line_limit.
    cut->set_limit( 34 ).
    cl_abap_unit_assert=>assert_equals( exp = 34 act = cut->limit( )  ).
  ENDMETHOD.

  METHOD exception_at_exceeding_line.
    cut->set_limit( 10 ).
    TRY.
        cut->add_word( |world!| ).
        DATA(content) = cut->content( ).
        cl_abap_unit_assert=>fail( ).
      CATCH x_line_limit_exceeded INTO DATA(error).
        cl_abap_unit_assert=>assert_bound( act = error msg = |The object should be bound!| ).
    ENDTRY.
  ENDMETHOD.

  METHOD build_specific_line_wo_excptn.
    cut->set_limit( 30 ).
    TRY.
        cut = cut->add_word( |world| ).
        cut = cut->add_word( |with| ).
        cut = cut->add_word( |five| ).
        cut = cut->add_word( |words!| ).
        cl_abap_unit_assert=>assert_equals( exp = |Hello world with five words!| act = cut->content( ) ).
      CATCH x_line_limit_exceeded.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS tc_output_text DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO output_text.

    METHODS setup.
    METHODS create_output_text_object FOR TESTING.
    METHODS build_output_text_table   FOR TESTING.

ENDCLASS.

CLASS tc_output_text IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD create_output_text_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD build_output_text_table.
    cut->add_line( |Hello world in| ).
    cut->add_line( |five words!| ).
    DATA(expected_output_table) = VALUE stringtab( ( |Hello world in| )
                                                   ( |five words!| ) ).

    cl_abap_unit_assert=>assert_equals( exp = expected_output_table act = cut->output( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS create_application_object FOR TESTING.
    METHODS execute_complete_word_wrapping FOR TESTING.

ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD create_application_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD execute_complete_word_wrapping.
    DATA(input_text) = |Es blaut die Nacht, die Sternlein blinken, | &&
                       |Schneeflöcklein leis hernieder sinken.|.
    DATA(expected_output_table) = VALUE stringtab(  ( |Es blaut die| )
                                                    ( |Nacht, die| )
                                                    ( |Sternlein| )
                                                    ( |blinken,| )
                                                    ( |Schneeflöcklein| )
                                                    ( |leis hernieder| )
                                                    ( |sinken.| ) ).
    cl_abap_unit_assert=>assert_equals( exp =  expected_output_table act = cut->main( text = input_text length = 15 )  ).
  ENDMETHOD.

ENDCLASS.
