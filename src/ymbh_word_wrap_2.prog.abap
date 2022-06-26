"* build class for test
"* store content in class
"* get a word from the text
"* shorten the text by one word
"- get a line of definite length
"- fill line with words till limit
"- add line to a text table
"- display the text
"_ build surroundings
REPORT ymbh_word_wrap_2.

CLASS text DEFINITION.

  PUBLIC SECTION.

    METHODS constructor IMPORTING text TYPE string.
    METHODS get_content RETURNING VALUE(result) TYPE string.
    METHODS get_word    RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    CONSTANTS c_regex_for_one_word TYPE string VALUE `(\w+\s){1}`.
    DATA content TYPE string.

    METHODS cut_word_from_content.
    METHODS get_length_of_leading_word RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS text IMPLEMENTATION.

  METHOD constructor.
    content = text.
  ENDMETHOD.

  METHOD get_content.
    result = content.
  ENDMETHOD.

  METHOD get_word.
    DATA(length) = get_length_of_leading_word( ).
    result = condense( substring( val = content off = 0 len = length ) ).
    cut_word_from_content( ).
  ENDMETHOD.

  METHOD get_length_of_leading_word.
    DATA(regex) = |(\\w+\\s)\{1\}|.
    FIND FIRST OCCURRENCE OF REGEX c_regex_for_one_word IN content MATCH LENGTH result .
  ENDMETHOD.

  METHOD cut_word_from_content.
    DATA(length) = get_length_of_leading_word( ).
    content = condense( substring( val = content off = length len = strlen( content ) - length ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_text DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO text.

    METHODS setup.
    METHODS store_content_in_text_object FOR TESTING.
    METHODS get_first_word_from_text     FOR TESTING.
    METHODS get_shortend_text            FOR TESTING.

ENDCLASS.


CLASS tc_text IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |Hello world| ).
  ENDMETHOD.

  METHOD store_content_in_text_object.
    cut = NEW #( |Hello world| ).
    cl_abap_unit_assert=>assert_equals( exp = |Hello world| act = cut->get_content( ) ).
  ENDMETHOD.

  METHOD get_first_word_from_text.
    cl_abap_unit_assert=>assert_equals( exp = |Hello| act = cut->get_word( )  ).
  ENDMETHOD.

  METHOD get_shortend_text.
    cut->get_word( ).
    cl_abap_unit_assert=>assert_equals( exp = |world| act = cut->get_content( ) ).
  ENDMETHOD.

ENDCLASS.
