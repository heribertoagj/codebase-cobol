      ******************************************************************
      * NOME BOOK.: FNEBWBZI                                           *
      * DESCRICAO.: BOOK DE INFERFACE COM FUNCIONAL FNEB39ZL           *
      * DATA......: 28/06/2013                                         *
      * AUTOR.....: STEVE MARCEL CAVALCANTE                            *
      * EMPRESA...: CAPGEMINI                                          *
      * GRUPO.....: FNEB                                               *
      * TAMANHO...: 4547 BYTES                                         *
      ******************************************************************
           05 FNEBWBZI-HEADER.
              10 FNEBWBZI-COD-LAYOUT     PIC X(008)   VALUE 'FNEBWBZI'.
              10 FNEBWBZI-TAM-LAYOUT     PIC 9(005)   VALUE 4547.
           05 FNEBWBZI-REGISTRO.
      *
              10 FNEBWBZI-BLOCO-ENTRADA.
                 15 FNEBWBZI-E-MAX-OCCOR              PIC 9(005).
                 15 FNEBWBZI-E-CPSSOA-JURID-VINC      PIC 9(010).
                 15 FNEBWBZI-E-CTPO-CONTR-VINC        PIC 9(003).
                 15 FNEBWBZI-E-NSEQ-CONTR-VINC        PIC 9(010).
      *
              10 FNEBWBZI-BLOCO-PAGINACAO.
                 15 FNEBWBZI-INDICADOR-PAGINACAO      PIC X(001).
                    88 FNEBWBZI-P-INICIAL              VALUE 'I'.
                    88 FNEBWBZI-P-PRIMEIRA             VALUE 'P'.
                    88 FNEBWBZI-P-SEGUINTE             VALUE 'S'.
                    88 FNEBWBZI-P-ANTERIOR             VALUE 'A'.
                    88 FNEBWBZI-P-ULTIMA               VALUE 'U'.
                 15 FNEBWBZI-CHAVE-INI.
                    20 FNEBWBZI-CPSSOA-PPSTA-NEGOC-I  PIC 9(010).
                    20 FNEBWBZI-CCLASF-PPSTA-NEGOC-I  PIC 9(003).
                    20 FNEBWBZI-NSEQ-PPSTA-NEGOC-I    PIC 9(010).
                    20 FNEBWBZI-NVNCLO-PPSTA-NEGOC-I  PIC 9(002).
                 15 FNEBWBZI-CHAVE-FIM.
                    20 FNEBWBZI-CPSSOA-PPSTA-NEGOC-F  PIC 9(010).
                    20 FNEBWBZI-CCLASF-PPSTA-NEGOC-F  PIC 9(003).
                    20 FNEBWBZI-NSEQ-PPSTA-NEGOC-F    PIC 9(010).
                    20 FNEBWBZI-NVNCLO-PPSTA-NEGOC-F  PIC 9(002).
      *
              10 FNEBWBZI-BLOCO-SAIDA.
                 15 FNEBWBZI-S-NUM-CONSULTAS          PIC 9(005).
                 15 FNEBWBZI-S-OCORRENCIAS  OCCURS 0 TO 50 TIMES
                    DEPENDING ON        FNEBWBZI-S-NUM-CONSULTAS.
                    20 FNEBWBZI-S-CPSSOA-PPSTA-NEGOC  PIC 9(010).
                    20 FNEBWBZI-S-CCLASF-PPSTA-NEGOC  PIC 9(003).
                    20 FNEBWBZI-S-NSEQ-PPSTA-NEGOC    PIC 9(010).
                    20 FNEBWBZI-S-HINCL-REG           PIC X(026).
                    20 FNEBWBZI-S-ISIT-PPSTA-NEGOC    PIC X(040).
