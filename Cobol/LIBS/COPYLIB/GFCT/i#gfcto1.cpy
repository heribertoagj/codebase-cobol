      ******************************************************************
      * NOME DA INC - I#GFCTO1                                         *
      * DESCRICAO   - COMBO DE PACOTE - TABELA GFCTB001                *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTO1-ENTRADA.
           03  GFCTO1-LL                            PIC S9(004) COMP.
           03  GFCTO1-ZZ                            PIC S9(004) COMP.
           03  GFCTO1-TRANSACAO                     PIC  X(009).
           03  GFCTO1-FUNCAO                        PIC  X(008).
           03  GFCTO1-DADOS-RST.
               05  GFCTO1-FILTRO                    PIC  9(005).
               05  GFCTO1-QTDE-OCOR                 PIC  9(003).
               05  GFCTO1-QTDE-TOT-REG              PIC  9(006).
               05  GFCTO1-FUNC-BDSCO                PIC  X(007).
               05  GFCTO1-PONTEIRO.
                   07 GFCTO1-PONTEIRO-PACOTE        PIC  9(005).
               05  GFCTO1-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(052).
