      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0J7: DB2PRD.TTX_OPER_DESC_CLI                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4J7S                                       *
      *    -> FETCH   - DCOM4J7F                                       *
      *    -> INSERT  - DCOM4J7I                                       *
      *    -> UPDATE  - DCOM4J7U                                       *
      *    -> DELETE  - DCOM4J7D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWJ7S-HEADER.
          10 DCOMWJ7S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ7S'.
          10 DCOMWJ7S-TAM-LAYOUT       PIC  9(005) VALUE 08100.
      *
          10 DCOMWJ7S-BLOCO-SAIDA.
             15 DCOMWJ7S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWJ7S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWJ7S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWJ7S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWJ7S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWJ7S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWJ7S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWJ7S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWJ7S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMWJ7S-DINIC-TX-OPER-DESC     PIC  X(026).
                20 DCOMWJ7S-CINDCD-FREQ-TX         PIC  X(002).
                20 DCOMWJ7S-PTX-MAX-JURO-DESC      PIC S9(003)V9(002)
                                                               COMP-3.
                20 DCOMWJ7S-PTX-JURO-OPER-DESC     PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWJ7S-DFIM-TX-OPER-DESC      PIC  X(026).
                20 DCOMWJ7S-HULT-ATULZ             PIC  X(026).
                20 DCOMWJ7S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWJ7S-CTERM                  PIC  X(008).
                20 DCOMWJ7S-FILLER                 PIC  X(042).
      *                                                                *
          10 DCOMWJ7S-FILLER                       PIC  X(024).
      *                                                                *
      *================================================================*
