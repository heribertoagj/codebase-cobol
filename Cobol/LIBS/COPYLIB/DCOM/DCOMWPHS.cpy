      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0PH: DB2PRD.TGARNT_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4PHS                                       *
      *    -> FETCH   - DCOM4PHF                                       *
      *    -> INSERT  - DCOM4PHI                                       *
      *    -> UPDATE  - DCOM4PHU                                       *
      *    -> DELETE  - DCOM4PHD                                       *
      *================================================================*
      *                                                                *
       05 DCOMWPHS-HEADER.
          10 DCOMWPHS-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWPHS'.
          10 DCOMWPHS-TAM-LAYOUT       PIC  9(005) VALUE 05000.
      *
          10 DCOMWPHS-BLOCO-SAIDA.
             15 DCOMWPHS-QTDE-TOTAL                PIC  9(010).
             15 DCOMWPHS-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWPHS-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWPHS-CPSSOA-JURID-CONTR     PIC S9(010) COMP-3.
                20 DCOMWPHS-CTPO-CONTR-NEGOC       PIC S9(003) COMP-3.
                20 DCOMWPHS-NSEQ-CONTR-NEGOC       PIC S9(010) COMP-3.
                20 DCOMWPHS-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWPHS-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWPHS-CPRODT-SERVC-OPER      PIC S9(008) COMP-3.
                20 DCOMWPHS-HULT-ATULZ             PIC  X(026).
                20 DCOMWPHS-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWPHS-CTERM                  PIC  X(008).
                20 DCOMWPHS-CPPSTA-PRODT           PIC S9(010) COMP-3.
                20 DCOMWPHS-NSEQ-CONTR-LIM         PIC S9(017) COMP-3.
             15 DCOMWPHS-FILLER                    PIC  X(924).
      *                                                                *
      *================================================================*
