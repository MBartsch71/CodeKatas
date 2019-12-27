REPORT ymbh_for_let_sample.

INTERFACE lif_album.
  TYPES: BEGIN OF ty_album,
           artist TYPE char50,
           title  TYPE char100,
           year   TYPE char4,
         END OF ty_album.
  TYPES tt_album TYPE STANDARD TABLE OF ty_album WITH DEFAULT KEY.
ENDINTERFACE.

CLASS lcl_cd_archive DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS read_albums_of_year
      IMPORTING
        iv_year          TYPE i
      RETURNING
        VALUE(rt_albums) TYPE lif_album=>tt_album.

    METHODS get_publishing_infos
      RETURNING
        VALUE(rt_publishing_infos) TYPE string_table.

  PRIVATE SECTION.
    DATA mt_archive TYPE lif_album=>tt_album.

    METHODS create_archive.

ENDCLASS.

CLASS lcl_cd_archive IMPLEMENTATION.

  METHOD constructor.
    create_archive( ).
  ENDMETHOD.

  METHOD create_archive.
    mt_archive = VALUE #( ( artist = |AC/DC|       title = |Black Ice|         year = |2008| )
                          ( artist = |AC/DC|       title = |Rock or Bust|      year = |2014| )
                          ( artist = |AC/DC|       title = |Back in Black|     year = |1980| )
                          ( artist = |Metallica|   title = |Hardwired|         year = |2016| )
                          ( artist = |Metallica|   title = |Metallica|         year = |1991| )
                          ( artist = |Metallica|   title = |St. Anger|         year = |2003| )
                          ( artist = |Nirvana|     title = |Nevermind|         year = |1991| )
                          ( artist = |Nirvana|     title = |In Utero|          year = |1993| )
                          ( artist = |Iron Maiden| title = |The Book of Souls| year = |2015| )
                          ( artist = |Iron Maiden| title = |Dance of Death|    year = |2003| )
                          ( artist = |Iron Maiden| title = |Fear of the Dark|  year = |1992| )
                          ( artist = |Iron Maiden| title = |Brave New World|   year = |2000| ) ).
  ENDMETHOD.

  METHOD read_albums_of_year.
    rt_albums = VALUE #( FOR ls_album IN mt_archive
                         WHERE ( year = CONV char4( iv_year ) )
                               ( CORRESPONDING #( ls_album ) ) ).
  ENDMETHOD.

  METHOD get_publishing_infos.
    rt_publishing_infos = VALUE #( FOR ls_album IN mt_archive LET difference = sy-datum(4) - ls_album-year
                                                                  artist = ls_album-artist
                                                                  album = ls_album-title
                                       IN ( |{ artist } published { album } { difference } years before.| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_cd_archive DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_cd_archive.

    METHODS setup.
    METHODS read_all_albums_from_1991     FOR TESTING.
    METHODS investgt_archive_for_pub_date FOR TESTING.
ENDCLASS.

CLASS ltc_cd_archive IMPLEMENTATION.

  METHOD read_all_albums_from_1991.
    cl_abap_unit_assert=>assert_equals(
        msg = 'The function should return 2 albums.'
        exp = 2
        act = lines( mo_cut->read_albums_of_year( 1991 ) ) ).
  ENDMETHOD.

  METHOD investgt_archive_for_pub_date.
    cl_abap_unit_assert=>assert_table_contains(
        line  = |AC/DC published Black Ice 11 years before.|
        table = mo_cut->get_publishing_infos( )
        msg   = |The expected string should be returned.| ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

ENDCLASS.
