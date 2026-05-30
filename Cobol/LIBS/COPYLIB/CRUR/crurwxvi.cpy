      ******************************************************************
      * NOME BOOK : CRURWXVI - BOOK ENTRADA ENTRE COORDENADOR/FRAMEWORK*
      * DATA      : 04/12/2013                                         *
      * AUTOR     : GUSTAVO SERRACCHIANI FERRARI                       *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * TAMANHO   : 06722 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CRURWXVI-HEADER.
             10 CRURWXVI-COD-LAYOUT    PIC X(08)   VALUE 'CRURWXVI'.
             10 CRURWXVI-TAM-LAYOUT    PIC 9(05)   VALUE 07772.
           05 CRURWXVI-REGISTRO.
            10 CRURWXVI-BLOCO-ENTRADA.
                15 CRURWXVI-E-MAX-OCOR                PIC 9(005).
                15 CRURWXVI-E-CPSSOA-JURID-CTA        PIC 9(010).
                15 CRURWXVI-E-CTPO-NEGOC-CTA          PIC 9(003).
                15 CRURWXVI-E-NSEQ-NEGOC-CTA          PIC 9(010).
             10 CRURWXVI-BLOCO-PAGINACAO.
               15 CRURWXVI-INDICADOR-PAGINACAO        PIC X(01).
                 88 CRURWXVI-P-INICIAL                VALUE 'I'.
                 88 CRURWXVI-P-PRIMEIRA               VALUE 'P'.
                 88 CRURWXVI-P-SEGUINTE               VALUE 'S'.
                 88 CRURWXVI-P-ANTERIOR               VALUE 'A'.
                 88 CRURWXVI-P-ULTIMA                 VALUE 'U'.
               15 CRURWXVI-CHAVE-INI.
                  20 CRURWXVI-INI-N-CONTR             PIC 9(004).
               15 CRURWXVI-CHAVE-FIM.
                  20 CRURWXVI-FIM-N-CONTR             PIC 9(004).
             10 CRURWXVI-BLOCO-SAIDA.
               15 CRURWXVI-NUM-CONSULTAS              PIC 9(005).
               15 CRURWXVI-S-TVLR-LIBRC-CONTR         PIC 9(15)V9(2).
               15 CRURWXVI-REG-SAIDA   OCCURS 0 TO 50 TIMES
                  DEPENDING ON         CRURWXVI-NUM-CONSULTAS.
                  20 CRURWXVI-S-CPSSOA-JURID-CONTR    PIC 9(010).
                  20 CRURWXVI-S-CTPO-CONTR-NEGOC      PIC 9(003).
                  20 CRURWXVI-S-NSEQ-CONTR-NEGOC      PIC 9(010).
                  20 CRURWXVI-S-NLIBRC-CONTR-CREDT    PIC 9(004).
                  20 CRURWXVI-S-CEMPTO-RURAL          PIC 9(006).
                  20 CRURWXVI-S-IEMPTO-RURAL          PIC X(040).
                  20 CRURWXVI-S-DATA-LIBRC-CONTR      PIC X(010).
                  20 CRURWXVI-S-VALOR-LIBRC-CONTR     PIC 9(15)V9(2).
                  20 CRURWXVI-S-CSIT-LIBRC-CONTR      PIC 9(001).
                  20 CRURWXVI-S-ISIT-LIBRC-CONTR      PIC X(020).
                  20 CRURWXVI-S-CFORMA-LIQDC          PIC 9(002).
                  20 CRURWXVI-S-IFORMA-LIQDC          PIC X(030).
                  20 CRURWXVI-S-CTPO-LIBRC-CONTR      PIC 9(001).
