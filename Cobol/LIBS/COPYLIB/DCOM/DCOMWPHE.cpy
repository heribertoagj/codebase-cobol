      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0PH: DB2PRD.TTRASI_CONTR_DESC_COML
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4PHS                                       *
      *    -> FETCH   - DCOM4PHF                                       *
      *    -> INSERT  - DCOM4PHI                                       *
      *    -> UPDATE  - DCOM4PHU                                       *
      *    -> DELETE  - DCOM4PHD
      *================================================================*
      *                                                                *
       05 DCOMWPHE-HEADER.
          10 DCOMWPHE-COD-LAYOUT           PIC  X(008) VALUE 'DCOMWPHE'.
          10 DCOMWPHE-TAM-LAYOUT           PIC  9(005) VALUE 01000.
      *
       05 DCOMWPHE-BLOCO-ENTRADA.
          10 DCOMWPHE-INSTRUCAO            PIC  X(010).
      *
          10 DCOMWPHE-ENTRADA.
             15 DCOMWPHE-CPSSOA-JURID-CONTR     PIC S9(010) COMP-3.
             15 DCOMWPHE-CTPO-CONTR-NEGOC       PIC S9(003) COMP-3.
             15 DCOMWPHE-NSEQ-CONTR-NEGOC       PIC S9(010) COMP-3.
             15 DCOMWPHE-DANO-OPER-DESC         PIC S9(004) COMP-3.
             15 DCOMWPHE-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
             15 DCOMWPHE-CPRODT-SERVC-OPER      PIC S9(008) COMP-3.
             15 DCOMWPHE-HULT-ATULZ             PIC  X(026).
             15 DCOMWPHE-CFUNC-BDSCO            PIC S9(009) COMP-3.
             15 DCOMWPHE-CTERM                  PIC  X(008).
             15 DCOMWPHE-CPPSTA-PRODT           PIC S9(010) COMP-3.
             15 DCOMWPHE-NSEQ-CONTR-LIM         PIC S9(017) COMP-3.
      *
          10 DCOMWPHE-BLOCO-VARIAVEL.
             15 DCOMWPHE-V-FILLER                 PIC  X(300).

          10 DCOMWPHE-BLOCO-PAGINACAO.
             15 DCOMWPHE-MAX-OCORR                PIC  9(003).
             15 DCOMWPHE-INDICADOR-PAGINACAO      PIC  X(001).
                88 DCOMWPHE-P-INICIAL             VALUE 'I'.
                88 DCOMWPHE-P-PRIMEIRA            VALUE 'P'.
                88 DCOMWPHE-P-SEGUINTE            VALUE 'S'.
                88 DCOMWPHE-P-ANTERIOR            VALUE 'A'.
                88 DCOMWPHE-P-ULTIMA              VALUE 'U'.
      *
          10 DCOMWPHE-BLOCO-RESTART.
             15 DCOMWPHE-CHAVE-INI.
                20 DCOMWPHE-I-CPSSOA-JURID-CONTR  PIC S9(010) COMP-3.
                20 DCOMWPHE-I-CTPO-CONTR-NEGOC    PIC S9(003) COMP-3.
                20 DCOMWPHE-I-NSEQ-CONTR-NEGOC    PIC S9(010) COMP-3.
                20 FILLER                         PIC  X(190).
             15 DCOMWPHE-CHAVE-FIM.
                20 DCOMWPHE-F-CPSSOA-JURID-CONTR  PIC S9(010) COMP-3.
                20 DCOMWPHE-F-CTPO-CONTR-NEGOC    PIC S9(003) COMP-3.
                20 DCOMWPHE-F-NSEQ-CONTR-NEGOC    PIC S9(010) COMP-3.
                20 FILLER                         PIC  X(190).
      *
          10 DCOMWPHE-FILLER                      PIC  X(184).
      *
