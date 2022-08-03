##TODO
"* create input text with 2 words
"* store input text in object
"* try different blank settings (1 in front, 1 in the end, multiple between words
"* build parameterized tests
"* cut word from text
"- create output text object
"- build an output text line
"- check the output limit
"- display the whole output text
"- visualize limit range in output
REPORT ymbh_word_wrap_2.

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
    result = content_as_table[ 1 ].
    DELETE content_as_table INDEX 1.
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

    METHODS run_word_combination_tests FOR TESTING.
    METHODS cut_leading_word_from_text FOR TESTING.

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

ENDCLASS.
