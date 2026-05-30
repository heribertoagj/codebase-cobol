      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB051: DB2PRD.TREGRA_MORA_GERC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM451S                                       *
      *    -> FETCH   - DCOM451F                                       *
      *    -> INSERT  - DCOM451I                                       *
      *    -> UPDATE  - DCOM451U                                       *
      *    -> DELETE  - DCOM451D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW51E-HEADER.
          10 DCOMW51E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW51E'.
          10 DCOMW51E-TAM-LAYOUT       PIC  9(005) VALUE 00576.
      *
       05 DCOMW51E-BLOCO-ENTRADA.
          10 DCOMW51E-INSTRUCAO                    PIC  X(010).
          10 DCOMW51E-ENTRADA.
             15 DCOMW51E-CREGRA-MORA-GERC          PIC S9(003) COMP-3.
             15 DCOMW51E-IREGRA-MORA-GERC          PIC  X(040).
             15 DCOMW51E-TINIC-VCTO-PCELA          PIC S9(005) COMP-3.
             15 DCOMW51E-TFNAL-VCTO-PCELA          PIC S9(005) COMP-3.
             15 DCOMW51E-CINDCD-CALC-MORA          PIC S9(001) COMP-3.
             15 DCOMW51E-CIDTFD-CORRC-DIVDA        PIC S9(005) COMP-3.
             15 DCOMW51E-CIDTFD-JURO-MORA          PIC S9(005) COMP-3.
             15 DCOMW51E-CINDCD-RECAL-VENCD        PIC  X(001).
             15 DCOMW51E-HULT-ATULZ                PIC  X(026).
             15 DCOMW51E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW51E-CTERM                     PIC  X(008).
             15 DCOMW51E-CIDTFD-JURO-REMUN         PIC S9(005) COMP-3.
             15 DCOMW51E-PTX-JURO-REMUN            PIC S9(003) COMP-3.
             15 DCOMW51E-CIDTFD-MORA-VENCD         PIC S9(005) COMP-3.
             15 DCOMW51E-PTX-MORA-VENCD            PIC S9(003) COMP-3.
             15 DCOMW51E-CIDTFD-MULTA-VENCD        PIC S9(005) COMP-3.
             15 DCOMW51E-PTX-MULTA-VENCD           PIC S9(003) COMP-3.
          10 DCOMW51E-BLOCO-VARIAVEL.
             15 DCOMW51E-V-FILLER                  PIC  X(300).
          10 DCOMW51E-BLOCO-PAGINACAO.
             15 DCOMW51E-MAX-OCORR                 PIC  9(003).
             15 DCOMW51E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW51E-P-INICIAL              VALUE 'I'.
                88 DCOMW51E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW51E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW51E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW51E-P-ULTIMA               VALUE 'U'.
          10 DCOMW51E-BLOCO-RESTART.
             15 DCOMW51E-CHAVE-INI.
                20 DCOMW51E-I-CREGRA-MORA-GERC     PIC S9(003) COMP-3.
             15 DCOMW51E-CHAVE-FIM.
                20 DCOMW51E-F-CREGRA-MORA-GERC     PIC S9(003) COMP-3.
          10 DCOMW51E-FILLER                       PIC  X(135).
      *                                                                *
