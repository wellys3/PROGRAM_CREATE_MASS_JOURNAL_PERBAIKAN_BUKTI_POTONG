*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIDT00322......................................*
DATA:  BEGIN OF STATUS_ZFIDT00322                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIDT00322                    .
CONTROLS: TCTRL_ZFIDT00322
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIDT00322                    .
TABLES: ZFIDT00322                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
