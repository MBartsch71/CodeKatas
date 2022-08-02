##TODO
"* create input text with 2 words
"* store input text in object
"* try different blank settings (1 in front, 1 in the end, multiple between words
"- cut word from text
"- create output text object
"- build an output text line
"- check the output limit
"- display the whole output text
"- visualize limit range in output
REPORT ymbh_word_wrap_2.

CLASS input_text DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor       IMPORTING content TYPE string.

    METHODS get_text          RETURNING VALUE(result) TYPE string.

    METHODS get_text_wordwise RETURNING VALUE(result) TYPE stringtab.
    METHODS extract_leading_word
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA content TYPE string.
    DATA content_as_table TYPE stringtab.

    METHODS split_text_to_table.

ENDCLASS.

CLASS input_text IMPLEMENTATION.

  METHOD constructor.
    me->content = content.
    split_text_to_table( ).
  ENDMETHOD.

  METHOD get_text.
    result = content.
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
    DATA cut TYPE REF TO input_text.

    METHODS setup.
    METHODS create_input_text_object       FOR TESTING.
    METHODS store_text_in_object           FOR TESTING.
    METHODS store_input_text_wordwise      FOR TESTING.
    METHODS check_sentence_with_two_spaces FOR TESTING.
    METHODS check_longer_sentence_w_spaces FOR TESTING.
    METHODS check_text_with_leading_space  FOR TESTING.
    METHODS check_text_with_trailing_space FOR TESTING.

    METHODS cut_leading_word_from_text     FOR TESTING.
ENDCLASS.

CLASS tc_input_text IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |Hello world!| ).
  ENDMETHOD.

  METHOD create_input_text_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD store_text_in_object.
    cl_abap_unit_assert=>assert_equals( exp = |Hello world!| act = cut->get_text( )  ).
  ENDMETHOD.

  METHOD store_input_text_wordwise.
    DATA(expected_text_table) = VALUE stringtab( ( |Hello|  )
                                                 ( |world!| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( )  ).
  ENDMETHOD.

  METHOD check_sentence_with_two_spaces.
    cut = NEW input_text( |Hello  world!| ).
    DATA(expected_text_table) = VALUE stringtab( ( |Hello| )
                                                 ( |world!| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

  METHOD check_longer_sentence_w_spaces.
    cut = NEW #( |Hello  world in   five     words!| ).
    DATA(expected_text_table) = VALUE stringtab( ( |Hello| )
                                                 ( |world| )
                                                 ( |in| )
                                                 ( |five| )
                                                 ( |words!| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_text_table
        act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

  METHOD check_text_with_leading_space.
    cut = NEW #( | Hello world!| ).
    DATA(expected_text_table) = VALUE stringtab( ( |Hello| )
                                                 ( |world!| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

  METHOD check_text_with_trailing_space.
    cut = NEW #( |Hello world! | ).
    DATA(expected_text_table) = VALUE stringtab( ( |Hello| )
                                                 ( |world!| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

  METHOD cut_leading_word_from_text.
    DATA(expected_text_table) = VALUE stringtab( ( |world!| ) ).
    cut->extract_leading_word( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_text_table act = cut->get_text_wordwise( ) ).
  ENDMETHOD.

ENDCLASS.
