      *****************************************************************
      * NOME DA INC - I#GFCTBS                                        *
      * DESCRICAO   - TEXT DE NOME DE CLIENTE                         *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 30.05.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTBS-SAIDA.
           03  GFCTBS-LL                            PIC S9(004) COMP.
           03  GFCTBS-ZZ                            PIC S9(004) COMP.
           03  GFCTBS-TRANSACAO                     PIC  X(009).
           03  GFCTBS-FUNCAO                        PIC  X(008).
           03  GFCTBS-QTDE-OCOR                     PIC  9(003).
           03  GFCTBS-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTBS-FUNC-BDSCO                    PIC  X(007).
           03  GFCTBS-TERMINAL                      PIC  X(008).
           03  GFCTBS-ERRO                          PIC  9(001).
           03  GFCTBS-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTBS-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTBS-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTBS-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(089).
           03  GFCTBS-DADOS.
               05  FILLER                           OCCURS 5.
                   07  GFCTBS-CCTA-CLI              PIC  9(007).
                   07  GFCTBS-ERRO-CCTA-CLI         PIC  9(001).
                   07  GFCTBS-NOME-CLIENTE          PIC  X(060).
                   07  GFCTBS-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  GFCTBS-COD-SQL-ERRO-GEN      PIC  9(003).
                   07  GFCTBS-DESC-MSG-ERRO-GEN     PIC  X(079).
