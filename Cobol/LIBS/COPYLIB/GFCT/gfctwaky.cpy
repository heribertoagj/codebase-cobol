      *****************************************************************
      * NOME DA INC - GFCTWAKY                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFERIMENTO      *
      *               COMUNICACAO WEB X IMS        - SAIDA            *
      * TAMANHO     - 1010                                            *
      * WAKY-LL     - 1010                              GFCT0979      *
      * DATA        - 23.01.2010                                      *
      * RESPONSAVEL - CIBELE - SONDA PROCWORK                         *
      *****************************************************************
      *
       01  WAKY-SAIDA.
           03 WAKY-LL                            PIC S9(004) COMP.
           03 WAKY-ZZ                            PIC S9(004) COMP.
           03 WAKY-TRANSACAO                     PIC  X(009).
           03 WAKY-FUNCAO                        PIC  X(008).
           03 WAKY-DADOS-RST.
               05 WAKY-QTDE-OCOR                  PIC  9(003).
               05 WAKY-QTDE-TOT-REG               PIC  9(006).
               05 WAKY-FUNC-BDSCO                 PIC  X(007).
               05 WAKY-FILTRO.
                   07 WAKY-AGENCIA-FIL            PIC  9(005).
                   07 WAKY-CONTA-FIL              PIC  9(013).
                   07 WAKY-TIMESTAMP-FIL          PIC  X(026).
                   07 WAKY-CFUNC-FIL              PIC  9(009).
                   07 WAKY-CLOTE-FIL              PIC  9(011).
JUN10              07 WAKY-CTARIFAS  OCCURS 20 TIMES.
                      10 WAKY-CTARIFA-LOTE        PIC  9(005).
           03  WAKY-ACESSO                        PIC  X(001).
      ***     'V' - VISUALIZAR IMPRESSAO
      ***     'I' - IMPRIME
           03  WAKY-TIMESTAMP                     PIC  X(026).
           03  WAKY-NOME-FUNC                     PIC  X(040).
           03  WAKY-DATA-ANALISE                  PIC  X(010).
           03  WAKY-HORA-ANALISE                  PIC  X(005).
           03  WAKY-FIM                           PIC  X(001).
           03  WAKY-ERRO                          PIC  9(001).
           03  WAKY-COD-MSG-ERRO                  PIC  9(007).
           03  WAKY-COD-SQL-ERRO                  PIC  9(003).
           03  WAKY-DESC-MSG-ERRO                 PIC  X(079).
      ***     'S'
      ***     'N'
JUN10      03  FILLER                             PIC  X(636).
