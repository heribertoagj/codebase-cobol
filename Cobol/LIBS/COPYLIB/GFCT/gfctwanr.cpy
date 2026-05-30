      *****************************************************************
      * NOME DA INC - GFCTWANR                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFER DETALHE    *
      *               COMUNICACAO WEB X IMS        - ENTRADA (0876)   *
      * TAMANHO     - 500                                             *
      * GFCTNR-LL   - 500                                             *
      * DATA        - 21.06.2010                                      *
      * RESPONSAVEL - CIBELE   - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNR-ENTRADA.
           03  GFCTNR-LL                            PIC S9(004) COMP.
           03  GFCTNR-ZZ                            PIC S9(004) COMP.
           03  GFCTNR-TRANSACAO                     PIC  X(009).
           03  GFCTNR-FUNCAO                        PIC  X(008).
           03  GFCTNR-DADOS-RST.
               05  GFCTNR-QTDE-OCOR                 PIC  9(003).
               05  GFCTNR-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNR-FUNC-BDSCO                PIC  X(007).
               05  GFCTNR-CONS-DET.
                   07  GFCTNR-AGENCIA-CONS          PIC  9(005).
                   07  GFCTNR-CONTA-CONS            PIC  9(013).
JUN10              07  GFCTNR-TARIFAS-CONS OCCURS 20 TIMES.
                       10  GFCTNR-COD-TARI-CONS     PIC  9(005).
                   07  GFCTNR-LOTE-CONS             PIC  9(011).
                   07  GFCTNR-TIMESTAMP-CONS        PIC  X(026).
               05  GFCTNR-INIC-VIG-FLEX             PIC  X(010).
               05  GFCTNR-FIM-VIG-FLEX              PIC  X(010).
               05  GFCTNR-RJUSTF-DEFER              PIC  X(255).
               05  GFCTNR-OPERACAO                  PIC  X(001).
      * D = DEFERIDO
      * I = INDEFERIDO
      * T = TRANSFERIDO
               05  GFCTNR-FIM                       PIC  X(001).
JUN10          05  FILLER                           PIC  X(031).
