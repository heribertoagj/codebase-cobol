      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB090: DB2PRD.TMEIO_PGTO_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM490S                                       *
      *    -> FETCH   - DCOM490F                                       *
      *    -> INSERT  - DCOM490I                                       *
      *    -> UPDATE  - DCOM490U                                       *
      *    -> DELETE  - DCOM490D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW90E-READER.
          10 DCOMW90E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW90E'.
          10 DCOMW90E-TAM-LAYOUT       PIC  9(005) VALUE 00600.
      *
       05 DCOMW90E-BLOCO-ENTRADA.
          10 DCOMW90E-INSTRUCAO               PIC X(010).
      *
          10 DCOMW90E-ENTRADA.
             15 DCOMW90E-CMEIO-PGTO-DESC      PIC S9(003) COMP-3.
             15 DCOMW90E-IMEIO-PGTO-DESC      PIC  X(040).
             15 DCOMW90E-IRSUMO-MEIO-PGTO     PIC  X(015).
             15 DCOMW90E-CSGL-MEIO-PGTO       PIC  X(006).
             15 DCOMW90E-CINDCD-MEIO-CNDDO    PIC  X(001).
             15 DCOMW90E-HULT-ATULZ           PIC  X(026).
             15 DCOMW90E-CTRANS-PROG-ATULZ    PIC  X(008).
      *
          10 DCOMW90E-BLOCO-VARIAVEL.
             15 DCOMW90E-V-FILLER             PIC  X(300).
      *
          10 DCOMW90E-BLOCO-PAGINACAO.
             15 DCOMW90E-MAX-OCORR            PIC  9(003).
             15 DCOMW90E-INDICADOR-PAGINACAO  PIC  X(001).
                88 DCOMW90E-P-INICIAL         VALUE 'I'.
                88 DCOMW90E-P-PRIMEIRA        VALUE 'P'.
                88 DCOMW90E-P-SEGUINTE        VALUE 'S'.
                88 DCOMW90E-P-ANTERIOR        VALUE 'A'.
                88 DCOMW90E-P-ULTIMA          VALUE 'U'.
      *
          10 DCOMW90E-BLOCO-RESTART.
             15 DCOMW90E-CHAVE-INI.
                20 DCOMW90E-I-CMEIO-PGTO-DESC PIC S9(003) COMP-3.
                20 FILLER                     PIC  X(018).
             15 DCOMW90E-CHAVE-FIM.
                20 DCOMW90E-F-CMEIO-PGTO-DESC PIC S9(003) COMP-3.
                20 FILLER                     PIC  X(018).
      *
          10 DCOMW90E-FILLER                  PIC  X(135).
      *
