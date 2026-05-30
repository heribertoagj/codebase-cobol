      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB085: DB2PRD.TTPO_ISENC_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM485S                                       *
      *    -> FETCH   - DCOM485F                                       *
      *    -> INSERT  - DCOM485I                                       *
      *    -> UPDATE  - DCOM485U                                       *
      *    -> DELETE  - DCOM485D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW85E-READER.
          10 DCOMW85E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW85E'.
          10 DCOMW85E-TAM-LAYOUT       PIC  9(005) VALUE 00600.
      *
       05 DCOMW85E-BLOCO-ENTRADA.
          10 DCOMW85E-INSTRUCAO               PIC  X(010).
      *
          10 DCOMW85E-ENTRADA.
             15 DCOMW85E-CTPO-ISENC-DESC      PIC S9(003) COMP-3.
             15 DCOMW85E-ITPO-ISENC-DESC      PIC  X(030).
             15 DCOMW85E-IRSUMO-TPO-ISENC     PIC  X(015).
             15 DCOMW85E-HULT-ATULZ           PIC  X(026).
             15 DCOMW85E-CTRANS-PROG-ATULZ    PIC  X(008).
      *
          10 DCOMW85E-BLOCO-VARIAVEL.
             15 DCOMW85E-V-FILLER             PIC  X(300).
      *
          10 DCOMW85E-BLOCO-PAGINACAO.
             15 DCOMW85E-MAX-OCORR            PIC  9(003).
             15 DCOMW85E-INDICADOR-PAGINACAO  PIC  X(001).
                88 DCOMW85E-P-INICIAL         VALUE 'I'.
                88 DCOMW85E-P-PRIMEIRA        VALUE 'P'.
                88 DCOMW85E-P-SEGUINTE        VALUE 'S'.
                88 DCOMW85E-P-ANTERIOR        VALUE 'A'.
                88 DCOMW85E-P-ULTIMA          VALUE 'U'.
      *
          10 DCOMW85E-BLOCO-RESTART.
             15 DCOMW85E-CHAVE-INI.
                20 DCOMW85E-I-CTPO-ISENC-DESC PIC S9(003) COMP-3.
                20 FILLER                     PIC  X(018).
             15 DCOMW85E-CHAVE-FIM.
                20 DCOMW85E-F-CTPO-ISENC-DESC PIC S9(003) COMP-3.
                20 FILLER                     PIC  X(018).
      *
          10 DCOMW85E-FILLER                  PIC  X(152).
      *
