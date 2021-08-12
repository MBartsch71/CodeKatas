REPORT ymbh_word_wrap.

CLASS lcl_wrapper DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor  IMPORTING iv_width TYPE i.

    METHODS convert_text IMPORTING iv_text                TYPE string
                         RETURNING VALUE(rv_wrapped_text) TYPE stringtab.

  PRIVATE SECTION.
    DATA mv_width        TYPE i.
    DATA mv_input_text   TYPE string.
    DATA mt_wrapped_text TYPE stringtab.

    METHODS store_text                   IMPORTING iv_text TYPE string.

    METHODS wrap_text.

    METHODS return_wrapped_text          RETURNING VALUE(rt_wrapped_text) TYPE stringtab.

    METHODS text_has_not_ended           RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS extract_first_word_from_text RETURNING VALUE(rv_word) TYPE string.

    METHODS word_fits_still_in_line      IMPORTING iv_line          TYPE string
                                                   iv_word          TYPE string
                                         RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS add_word_to_line             IMPORTING iv_line        TYPE string
                                                   iv_word        TYPE string
                                         RETURNING VALUE(rv_line) TYPE string.

    METHODS store_line                   IMPORTING iv_line TYPE string.

    METHODS store_last_line              IMPORTING iv_line TYPE string.

    METHODS create_new_line              IMPORTING iv_word        TYPE string
                                         RETURNING VALUE(rv_line) TYPE string.

ENDCLASS.

CLASS lcl_wrapper IMPLEMENTATION.

  METHOD constructor.
    mv_width = iv_width.
  ENDMETHOD.

  METHOD convert_text.
    store_text( iv_text ).
    wrap_text( ).
    rv_wrapped_text = return_wrapped_text( ).
  ENDMETHOD.

  METHOD wrap_text.
    DATA lv_line TYPE string.

    WHILE text_has_not_ended( ).
      DATA(lv_word) = extract_first_word_from_text( ).
      IF word_fits_still_in_line( iv_line = lv_line
                                  iv_word = lv_word ).
        lv_line = add_word_to_line( iv_line = lv_line
                                    iv_word = lv_word ).
      ELSE.
        store_line( lv_line ).
        lv_line = create_new_line( lv_word ).
      ENDIF.
    ENDWHILE.

    store_last_line( lv_line ).
  ENDMETHOD.

  METHOD text_has_not_ended.
    rv_result = xsdbool( mv_input_text IS NOT INITIAL ).
  ENDMETHOD.

  METHOD extract_first_word_from_text.
    SPLIT mv_input_text AT space INTO rv_word mv_input_text.
  ENDMETHOD.

  METHOD add_word_to_line.
    rv_line = |{ iv_line } { iv_word }|.
    CONDENSE rv_line.
  ENDMETHOD.

  METHOD word_fits_still_in_line.
    rv_result = xsdbool( strlen( iv_line ) + 1 + strlen( iv_word ) <= mv_width ).
  ENDMETHOD.

  METHOD store_text.
    mv_input_text = iv_text.
  ENDMETHOD.

  METHOD store_line.
    mt_wrapped_text = VALUE #( BASE mt_wrapped_text ( iv_line ) ).
  ENDMETHOD.

  METHOD store_last_line.
    store_line( iv_line ).
  ENDMETHOD.

  METHOD create_new_line.
    rv_line = iv_word.
  ENDMETHOD.

  METHOD return_wrapped_text.
    rt_wrapped_text = mt_wrapped_text.
  ENDMETHOD.

ENDCLASS.




CLASS ltc_wrapper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_wrapper.

    METHODS setup.

    METHODS deliver_wrapped_text           FOR TESTING.
ENDCLASS.


CLASS ltc_wrapper IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( 25 ).
  ENDMETHOD.

  METHOD deliver_wrapped_text.
    DATA(lv_input_text) = |Hansel and Gretel are young children whose father is a woodcutter. | &&
                          |When a great famine settles over the land, the woodcutter's abusive | &&
                          |second wife decides to take the children into the woods...|.
    DATA(lt_wrapped_text) = VALUE stringtab( ( |Hansel and Gretel are|    )
                                             ( |young children whose|     )
                                             ( |father is a woodcutter.|  )
                                             ( |When a great famine|      )
                                             ( |settles over the land,|   )
                                             ( |the woodcutter's abusive| )
                                             ( |second wife decides to|   )
                                             ( |take the children into|   )
                                             ( |the woods...|             ) ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The delivered text should be wrapped accordingly.|
        exp = lt_wrapped_text
        act = mo_cut->convert_text( lv_input_text ) ).
  ENDMETHOD.

ENDCLASS.
