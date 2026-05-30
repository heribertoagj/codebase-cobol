      ******************************************************************
      * NOME DA INC - GFCTWABC                                         *
      * DESCRICAO   - TEXT DE CORRESPONDENTE BANCARIO /                *
      *               BANCO POSTAL OU BRADESCO EXPRESSO                *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 25/08/2008                                       *
      * RESPONSAVEL - MARCUS VINICIUS - PROCWORK - TEL 4506-2297       *
      ******************************************************************
      *
       01  WABC-ENTRADA.
           03  WABC-LL                            PIC S9(004) COMP.
           03  WABC-ZZ                            PIC S9(004) COMP.
           03  WABC-TRANSACAO                     PIC  X(009).
           03  WABC-FUNCAO                        PIC  X(008).
           03  WABC-DADOS-RST.
               05  WABC-QTDE-OCOR                 PIC  9(003).
               05  WABC-QTDE-TOT-REG              PIC  9(006).
               05  WABC-FUNC-BDSCO                PIC  X(007).
               05  WABC-TERMINAL                  PIC  X(008).
               05  WABC-FIM                       PIC  X(001).
               05  WABC-COD-DEPDC                 PIC  9(005).
               05  WABC-COD-POSTO                 PIC  9(005).
               05  WABC-COD-AGRUP                 PIC  9(003).
               05  FILLER                         PIC  X(041).
