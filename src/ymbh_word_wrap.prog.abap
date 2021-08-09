REPORT ymbh_word_wrap.

CLASS lcl_wrapper DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor             IMPORTING iv_width TYPE i.
    METHODS get_text_width_in_chars RETURNING VALUE(rv_width) TYPE i.
    METHODS convert_text            IMPORTING iv_text                TYPE string
                                    RETURNING VALUE(rv_wrapped_text) TYPE stringtab.

  PRIVATE SECTION.
    DATA mv_width TYPE i.

ENDCLASS.

CLASS lcl_wrapper IMPLEMENTATION.

  METHOD constructor.
    mv_width = iv_width.
  ENDMETHOD.

  METHOD get_text_width_in_chars.
    rv_width = mv_width.
  ENDMETHOD.

  METHOD convert_text.
    DATA new_line TYPE string.
    DATA(lv_text) = iv_text.
    WHILE lv_text IS NOT INITIAL.
      SPLIT lv_text AT space INTO DATA(word) lv_text.
      IF strlen( new_line ) + 1 + strlen( word ) <= mv_width.
        new_line = |{ new_line } { word }|.
        CONDENSE new_line.
      ELSE.
        APPEND new_line TO rv_wrapped_text.
        new_line = word.
      ENDIF.
    ENDWHILE.
    APPEND new_line TO rv_wrapped_text.
  ENDMETHOD.

ENDCLASS.




CLASS ltc_wrapper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_wrapper.

    METHODS setup.
    METHODS create_wrapper_with_word_limit FOR TESTING.
    METHODS deliver_wrapped_text           FOR TESTING.
ENDCLASS.


CLASS ltc_wrapper IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 25 ).
  ENDMETHOD.

  METHOD create_wrapper_with_word_limit.
    cl_abap_unit_assert=>assert_equals(
        msg = |The wrapper should have the limit 30.|
        exp = 25
        act = mo_cut->get_text_width_in_chars( ) ).
  ENDMETHOD.


  METHOD deliver_wrapped_text.
    DATA(lv_input_text) = |Hansel and Gretel are young children whose father is a woodcutter. | &&
                          |When a great famine settles over the land, the woodcutter's abusive | &&
                          |second wife decides to take the children into the woods...|.
    DATA(lt_wrapped_text) = VALUE stringtab( ( |Hansel and Gretel are|   )
                                             ( |young children whose|     )
                                             ( |father is a woodcutter.|  )
                                             ( |When a great famine|      )
                                             ( |settles over the land,|   )
                                             ( |the woodcutter's abusive| )
                                             ( |second wife decides to|   )
                                             ( |take the children into|   )
                                             ( |the woods...| ) ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The delivered text should be wrapped accordingly.|
        exp = lt_wrapped_text
        act = mo_cut->convert_text( lv_input_text ) ).
  ENDMETHOD.

ENDCLASS.
