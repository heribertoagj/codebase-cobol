      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWP7S-HEADER.
          10 DCOMWP7S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWP7S'.
          10 DCOMWP7S-TAM-LAYOUT       PIC  9(005) VALUE 04576.
      *
          10 DCOMWP7S-BLOCO-SAIDA.
             15 DCOMWP7S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWP7S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWP7S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWP7S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMWP7S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMWP7S-CINDCD-FORMA-LIQDC     PIC  X(001).
                20 DCOMWP7S-CINDCD-UTILZ-LIM       PIC  X(001).
                20 DCOMWP7S-CINDCD-DEB-PCIAL       PIC  X(001).
                20 DCOMWP7S-CINDCD-TENTV-DEB       PIC  X(001).
                20 DCOMWP7S-HULT-ATULZ             PIC  X(026).
                20 DCOMWP7S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWP7S-CTERM                  PIC  X(008).
                20 DCOMWP7S-FILLER                 PIC  X(041).
      *                                                                *
      *================================================================*
