
DATA ok_code         TYPE sy-ucomm.
DATA gv_sender_count TYPE i.

DATA go_gui         TYPE REF TO lcl_gui.
DATA go_application TYPE REF TO lcl_application.

DATA go_textedit_senders   TYPE REF TO cl_gui_textedit.
DATA go_textedit_messages  TYPE REF TO cl_gui_textedit.
DATA go_textedit_receivers TYPE REF TO cl_gui_textedit.

DATA gt_outtab TYPE lcl_gui=>tt_content.
