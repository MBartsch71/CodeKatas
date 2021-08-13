REPORT ymbh_word_wrap.

INTERFACE lif_line.
  METHODS add_word IMPORTING iv_word TYPE string.
  METHODS get      RETURNING VALUE(rv_line) TYPE string.
ENDINTERFACE.

CLASS lcx_line_limit_overflow DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcl_line DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_line.

    METHODS constructor IMPORTING iv_line_limit TYPE i OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS mc_line_unlimited TYPE i VALUE 0.
    CONSTANTS mc_space_length   TYPE i VALUE 1.

    DATA mv_line       TYPE string.
    DATA mv_line_limit TYPE i.

    METHODS check_line_limit_violation IMPORTING iv_word TYPE string.

    METHODS add_word_to_line           IMPORTING iv_word TYPE string.

    METHODS insufficient_space_in_line IMPORTING iv_word          TYPE string
                                       RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_line IMPLEMENTATION.

  METHOD constructor.
    mv_line_limit = iv_line_limit.
  ENDMETHOD.

  METHOD lif_line~add_word.
    check_line_limit_violation( iv_word ).
    add_word_to_line( iv_word ).
  ENDMETHOD.

  METHOD lif_line~get.
    rv_line = mv_line.
  ENDMETHOD.

  METHOD check_line_limit_violation.
    IF insufficient_space_in_line( iv_word ).
      RAISE EXCEPTION TYPE lcx_line_limit_overflow.
    ENDIF.
  ENDMETHOD.

  METHOD add_word_to_line.
    mv_line = condense( |{ mv_line } { iv_word }| ).
  ENDMETHOD.

  METHOD insufficient_space_in_line.
    rv_result = xsdbool( strlen( mv_line ) + mc_space_length + strlen( iv_word ) > mv_line_limit AND
                         mv_line_limit <> mc_line_unlimited ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_line_factory DEFINITION.

  PUBLIC SECTION.
    METHODS constructor  IMPORTING iv_line_limit TYPE i OPTIONAL.
    METHODS get_new_line RETURNING VALUE(ro_line) TYPE REF TO lif_line.

  PRIVATE SECTION.
    DATA mv_line_limit TYPE i.
ENDCLASS.

CLASS lcl_line_factory IMPLEMENTATION.

  METHOD constructor.
    mv_line_limit = iv_line_limit.
  ENDMETHOD.

  METHOD get_new_line.
    ro_line = NEW lcl_line( mv_line_limit ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_wrapper DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor  IMPORTING iv_width TYPE i.

    METHODS convert_text IMPORTING iv_text                TYPE string
                         RETURNING VALUE(rv_wrapped_text) TYPE stringtab.

  PRIVATE SECTION.
    DATA mo_line         TYPE REF TO lif_line.
    DATA mo_line_factory TYPE REF TO lcl_line_factory.

    DATA mt_wrapped_text TYPE stringtab.
    DATA mv_input_text   TYPE string.

    METHODS store_text                   IMPORTING iv_text TYPE string.

    METHODS wrap_text.

    METHODS return_wrapped_text          RETURNING VALUE(rt_wrapped_text) TYPE stringtab.

    METHODS text_has_not_ended           RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS extract_first_word_from_text RETURNING VALUE(rv_word) TYPE string.

    METHODS store_line                   IMPORTING iv_line TYPE string.

ENDCLASS.

CLASS lcl_wrapper IMPLEMENTATION.

  METHOD constructor.
    mo_line_factory = NEW lcl_line_factory( iv_width ).
  ENDMETHOD.

  METHOD convert_text.
    store_text( iv_text ).
    wrap_text( ).
    rv_wrapped_text = return_wrapped_text( ).
  ENDMETHOD.

  METHOD wrap_text.
    mo_line = mo_line_factory->get_new_line( ).

    WHILE text_has_not_ended( ).
      DATA(lv_word) = extract_first_word_from_text( ).
      TRY.
          mo_line->add_word( lv_word ).
        CATCH lcx_line_limit_overflow.
          store_line( mo_line->get( ) ).
          mo_line = mo_line_factory->get_new_line( ).
          mo_line->add_word( lv_word ).
      ENDTRY.
    ENDWHILE.

    store_line( mo_line->get( ) ).
  ENDMETHOD.

  METHOD text_has_not_ended.
    rv_result = xsdbool( mv_input_text IS NOT INITIAL ).
  ENDMETHOD.

  METHOD extract_first_word_from_text.
    SPLIT mv_input_text AT space INTO rv_word mv_input_text.
  ENDMETHOD.

  METHOD store_text.
    mv_input_text = iv_text.
  ENDMETHOD.

  METHOD store_line.
    mt_wrapped_text = VALUE #( BASE mt_wrapped_text ( iv_line ) ).
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

    METHODS deliver_wrapped_text FOR TESTING.
ENDCLASS.


CLASS ltc_wrapper IMPLEMENTATION.

  METHOD deliver_wrapped_text.
    mo_cut = NEW #( 25 ).
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

CLASS ltc_line DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_line.

    METHODS add_words_to_limited_line    FOR TESTING.
    METHODS error_at_line_limit_overflow FOR TESTING.
    METHODS add_word_at_unlimited_line   FOR TESTING.

ENDCLASS.

CLASS ltc_line IMPLEMENTATION.

  METHOD add_words_to_limited_line.
    mo_cut = NEW #( 25 ).
    TRY.
        mo_cut->lif_line~add_word( |Hello| ).
        mo_cut->lif_line~add_word( |World!| ).
        cl_abap_unit_assert=>assert_equals(
            msg = |The resulting line should look like expected.|
            exp = |Hello World!|
            act = mo_cut->lif_line~get( ) ).
      CATCH lcx_line_limit_overflow.
        cl_abap_unit_assert=>fail( msg = |An exception shouldn't be raised.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD error_at_line_limit_overflow.
    mo_cut = NEW #( 10 ).
    TRY.
        mo_cut->lif_line~add_word( |Hello| ).
        mo_cut->lif_line~add_word( |World!| ).
      CATCH lcx_line_limit_overflow INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_bound(
            msg = |An exception should be raised|
            act = lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_word_at_unlimited_line.
    mo_cut = NEW #( ).
    TRY.
        mo_cut->lif_line~add_word( |Hello| ).
        cl_abap_unit_assert=>assert_equals(
            msg = |The resulting line should be like expected.|
            exp = |Hello|
            act = mo_cut->lif_line~get( ) ).
      CATCH lcx_line_limit_overflow.
        cl_abap_unit_assert=>fail( msg = |An exception shouldn't be raised.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_line_factory DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_line_factory.

    METHODS get_limited_line_from_factory FOR TESTING.
ENDCLASS.

CLASS ltc_line_factory IMPLEMENTATION.

  METHOD get_limited_line_from_factory.
    mo_cut = NEW #( 25 ).
    DATA(lo_line) = mo_cut->get_new_line( ).
    TRY.
        lo_line->add_word( |Hello| ).
        lo_line->add_word( |World!| ).
        cl_abap_unit_assert=>assert_equals(
            msg = |The reulting line should look like expected.|
            exp = |Hello World!|
            act = lo_line->get( ) ).
      CATCH lcx_line_limit_overflow.
        cl_abap_unit_assert=>fail( msg = |An exception shouldn't be raised.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
