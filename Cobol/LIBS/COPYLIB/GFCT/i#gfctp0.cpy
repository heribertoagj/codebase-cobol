      *****************************************************************
      * NOME DA INC - I#GFCTP0                                        *
      * DESCRICAO   - TEXT DE NOME DO POSTO BANCARIO COM AGRUPAMENTO  *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTP0-ENTRADA.
           03  GFCTP0-LL                            PIC S9(004) COMP.
           03  GFCTP0-ZZ                            PIC S9(004) COMP.
           03  GFCTP0-TRANSACAO                     PIC  X(009).
           03  GFCTP0-FUNCAO                        PIC  X(008).
           03  GFCTP0-DADOS-RST.
               05  GFCTP0-FILTRO.
                   07  GFCTP0-FILTRO-AGRUP          PIC  9(003).
                   07  GFCTP0-FILTRO-AGENCIA        PIC  9(005).
                   07  GFCTP0-FILTRO-POSTO          PIC  9(005).
               05  GFCTP0-QTDE-OCOR                 PIC  9(007).
               05  GFCTP0-QTDE-TOT-REG              PIC  9(009).
               05  GFCTP0-FUNC-BDSCO                PIC  X(007).
               05  GFCTP0-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(042).
