      *****************************************************************
      * NOME DA INC - I#GFCTO5                                        *
      * DESCRICAO   - TEXT DE AGENCIA E CONTA                         *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 24.03.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTO5-ENTRADA.
           03  GFCTO5-LL                            PIC S9(004) COMP.
           03  GFCTO5-ZZ                            PIC S9(004) COMP.
           03  GFCTO5-TRANSACAO                     PIC  X(009).
           03  GFCTO5-FUNCAO                        PIC  X(008).
           03  GFCTO5-DADOS-RST.
               05  GFCTO5-QTDE-OCOR                 PIC  9(003).
               05  GFCTO5-QTDE-TOT-REG              PIC  9(006).
               05  GFCTO5-FUNC-BDSCO                PIC  X(007).
               05  GFCTO5-COD-DEPDC                 PIC  9(005).
               05  GFCTO5-CCTA-CLI                  PIC  9(013).
               05  GFCTO5-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(044).
