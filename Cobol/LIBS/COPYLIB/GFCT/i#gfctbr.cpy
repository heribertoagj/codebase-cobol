      *****************************************************************
      * NOME DA INC - I#GFCTBR                                        *
      * DESCRICAO   - TEXT DE NOME DE CLIENTE                         *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 30.05.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTBR-ENTRADA.
           03  GFCTBR-LL                            PIC S9(004) COMP.
           03  GFCTBR-ZZ                            PIC S9(004) COMP.
           03  GFCTBR-TRANSACAO                     PIC  X(009).
           03  GFCTBR-FUNCAO                        PIC  X(008).
           03  GFCTBR-QTDE-OCOR                     PIC  9(003).
           03  GFCTBR-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTBR-FUNC-BDSCO                    PIC  X(007).
           03  GFCTBR-TERMINAL                      PIC  X(008).
           03  GFCTBR-COD-DEPDC                     PIC  9(005).
           03  GFCTBR-CCTA-CLI                      PIC  9(007).
           03  GFCTBR-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(042).
