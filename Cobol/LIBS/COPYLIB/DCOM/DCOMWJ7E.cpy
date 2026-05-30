      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWJ7E-HEADER.
          10 DCOMWJ7E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ7E'.
          10 DCOMWJ7E-TAM-LAYOUT       PIC  9(005) VALUE 00700.
      *
       05 DCOMWJ7E-BLOCO-ENTRADA.
          10 DCOMWJ7E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWJ7E-ENTRADA.
             15 DCOMWJ7E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWJ7E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWJ7E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWJ7E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWJ7E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWJ7E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWJ7E-DINIC-TX-OPER-DESC        PIC  X(026).
             15 DCOMWJ7E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWJ7E-PTX-MAX-JURO-DESC         PIC S9(003)V9(002)
                                                               COMP-3.
             15 DCOMWJ7E-PTX-JURO-OPER-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWJ7E-DFIM-TX-OPER-DESC         PIC  X(026).
             15 DCOMWJ7E-HULT-ATULZ                PIC  X(026).
             15 DCOMWJ7E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWJ7E-CTERM                     PIC  X(008).
             15 DCOMWJ7E-FILLER                    PIC  X(042).
      *
          10 DCOMWJ7E-BLOCO-VARIAVEL.
             15 DCOMWJ7E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWJ7E-BLOCO-PAGINACAO.
             15 DCOMWJ7E-MAX-OCORR                 PIC  9(003).
             15 DCOMWJ7E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWJ7E-P-INICIAL              VALUE 'I'.
                88 DCOMWJ7E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWJ7E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWJ7E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWJ7E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWJ7E-BLOCO-RESTART.
             15 DCOMWJ7E-CHAVE-INI.
                20 DCOMWJ7E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWJ7E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWJ7E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWJ7E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWJ7E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWJ7E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWJ7E-I-DINIC-TX-OPER-DESC   PIC  X(026).
                20 DCOMWJ7E-I-FILLER               PIC  X(006).
             15 DCOMWJ7E-CHAVE-FIM.
                20 DCOMWJ7E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWJ7E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWJ7E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWJ7E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWJ7E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWJ7E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMWJ7E-F-DINIC-TX-OPER-DESC   PIC  X(026).
                20 DCOMWJ7E-F-FILLER               PIC  X(006).
      *
          10 DCOMWJ7E-FILLER                       PIC  X(112).
      *
