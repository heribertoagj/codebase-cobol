      *---------------------------------------------------------------*
      * -INC I#BVVEAA                                                 *
      *                                                               *
      *      ARQUIVO SEQUENCIAL DA TABELA BACCV042   -  LRECL = 074   *
      *              (DB2PRD.V01HIST_CTA_CATAO)                       *
      *                                                               *
      *                        KAMBARA - GP87 - DL06 - FEVEREIRO/1999 *
      *---------------------------------------------------------------*

      *-=============================================================-*

       01  LOGA-REGISTRO.
           03  LOGA-CHAVE.
               05  LOGA-AGENCIA        PIC S9(05)              COMP-3.
               05  LOGA-CONTA          PIC S9(07)              COMP-3.
               05  LOGA-PRODUTO        PIC S9(05)              COMP-3.
           03  LOGA-TIMESTAMP          PIC  X(26).
           03  FILLER  REDEFINES  LOGA-TIMESTAMP.
               05  LOGA-DT-ALT.
                   07  LOGA-AAAA-ALT   PIC  9(04).
                   07  FILLER          PIC  X(01).
                   07  LOGA-MM-ALT     PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  LOGA-DD-ALT     PIC  9(02).
               05  LOGA-HH-ALT.
                   07  FILLER          PIC  X(01).
                   07  LOGA-H-ALT      PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  LOGA-M-ALT      PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  LOGA-S-ALT      PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  LOGA-N-ALT      PIC  9(06).
           03  LOGA-COD-OPERADOR       PIC  S9(09)             COMP-3.
           03  LOGA-COD-TERMINAL       PIC  X(08).
           03  LOGA-DISP-ANT           PIC S9(13)V9(02)        COMP-3.
           03  LOGA-DISP-ATU           PIC S9(13)V9(02)        COMP-3.
           03  LOGA-PERCENTUAL-ANT     PIC S9(03)V9(02)        COMP-3.
           03  LOGA-VLRNOMINAL-ANT     PIC S9(13)V9(02)        COMP-3.


      *-=============================================================-*
