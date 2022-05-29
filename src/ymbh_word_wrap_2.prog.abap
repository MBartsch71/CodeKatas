REPORT ymbh_word_wrap_2.

CLASS text DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING content TYPE string.

    METHODS get_content RETURNING VALUE(result) TYPE string.

    METHODS shorten_sentence_one_word    RETURNING VALUE(result) TYPE REF TO text.

  PRIVATE SECTION.
    DATA content TYPE string.

    METHODS determine_length_of_1st_word IMPORTING i_sentence    TYPE string
                                         RETURNING VALUE(result) TYPE i.

ENDCLASS.


CLASS text IMPLEMENTATION.

  METHOD constructor.
    me->content = content.
  ENDMETHOD.

  METHOD get_content.
    result = content.
  ENDMETHOD.

  METHOD shorten_sentence_one_word.
    DATA(length) = determine_length_of_1st_word( content ).
    DATA(new_text) = content.
    SHIFT new_text LEFT BY length PLACES.
    result = NEW text( new_text ).
  ENDMETHOD.

  METHOD determine_length_of_1st_word.
    DATA(lv_regex) = |(\\w+\\s)\{1\}|.
    FIND FIRST OCCURRENCE OF REGEX lv_regex IN i_sentence MATCH LENGTH result .
  ENDMETHOD.

ENDCLASS.

CLASS tc_word_wrap DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS build_text_object         FOR TESTING.
    METHODS get_back_the_content_from_text FOR TESTING.
    METHODS get_text_shorten_by_one_word FOR TESTING.

ENDCLASS.


CLASS tc_word_wrap IMPLEMENTATION.

  METHOD build_text_object.
    DATA(text) = NEW text( || ).
    cl_abap_unit_assert=>assert_bound(
        msg = |The object should be bound!|
        act = text ).
  ENDMETHOD.

  METHOD get_back_the_content_from_text.
    DATA(text) = NEW text( |Hello world.| ).
    cl_abap_unit_assert=>assert_equals(
        exp = |Hello world.|
        act = text->get_content( ) ).
  ENDMETHOD.

  METHOD get_text_shorten_by_one_word.
    DATA(text) = NEW text( |Hello world in five words.| ).
    text = text->shorten_sentence_one_word( ).
    cl_abap_unit_assert=>assert_equals(
       exp = |world in five words.|
       act = text->get_content( ) ).
  ENDMETHOD.

ENDCLASS.
