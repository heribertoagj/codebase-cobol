      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0P7: DB2PRD.TCONTR_OPCAO_PGTO                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4P7S                                       *
      *    -> FETCH   - DCOM4P7F                                       *
      *    -> INSERT  - DCOM4P7I                                       *
      *    -> UPDATE  - DCOM4P7U                                       *
      *    -> DELETE  - DCOM4P7D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWP7E-HEADER.
          10 DCOMWP7E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP7E'.
          10 DCOMWP7E-TAM-LAYOUT       PIC  9(005) VALUE 00581.
      *
       05 DCOMWP7E-BLOCO-ENTRADA.
          10 DCOMWP7E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWP7E-ENTRADA.
             15 DCOMWP7E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMWP7E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMWP7E-CINDCD-FORMA-LIQDC        PIC  X(001).
             15 DCOMWP7E-CINDCD-UTILZ-LIM          PIC  X(001).
             15 DCOMWP7E-CINDCD-DEB-PCIAL          PIC  X(001).
             15 DCOMWP7E-CINDCD-TENTV-DEB          PIC  X(001).
             15 DCOMWP7E-HULT-ATULZ                PIC  X(026).
             15 DCOMWP7E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWP7E-CTERM                     PIC  X(008).
             15 DCOMWP7E-FILLER                    PIC  X(041).
      *
          10 DCOMWP7E-BLOCO-VARIAVEL.
             15 DCOMWP7E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWP7E-BLOCO-PAGINACAO.
             15 DCOMWP7E-MAX-OCORR                 PIC  9(003).
             15 DCOMWP7E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWP7E-P-INICIAL              VALUE 'I'.
                88 DCOMWP7E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWP7E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWP7E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWP7E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWP7E-BLOCO-RESTART.
             15 DCOMWP7E-CHAVE-INI.
                20 DCOMWP7E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWP7E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWP7E-I-FILLER               PIC  X(018).
             15 DCOMWP7E-CHAVE-FIM.
                20 DCOMWP7E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWP7E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWP7E-F-FILLER               PIC  X(018).
      *
          10 DCOMWP7E-FILLER                       PIC  X(113).
      *
