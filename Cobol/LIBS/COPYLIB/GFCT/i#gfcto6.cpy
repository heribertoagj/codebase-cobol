      *****************************************************************
      * NOME DA INC - I#GFCTO6                                        *
      * DESCRICAO   - TEXT DE AGENCIA E CONTA                         *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 24.03.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTO6-SAIDA.
           03  GFCTO6-LL                            PIC S9(004) COMP.
           03  GFCTO6-ZZ                            PIC S9(004) COMP.
           03  GFCTO6-TRANSACAO                     PIC  X(009).
           03  GFCTO6-FUNCAO                        PIC  X(008).
           03  GFCTO6-QTDE-OCOR                     PIC  9(003).
           03  GFCTO6-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTO6-FUNC-BDSCO                    PIC  X(007).
           03  GFCTO6-COD-DEPDC                     PIC  9(005).
           03  GFCTO6-CCTA-CLI                      PIC  9(013).
           03  GFCTO6-ERRO                          PIC  9(001).
           03  GFCTO6-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTO6-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTO6-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTO6-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(776).
           03  GFCTO6-DADOS.
               05  GFCTO6-AGENCIA                   PIC  9(005).
               05  GFCTO6-CONTA                     PIC  9(013).
               05  GFCTO6-NOME-CLIENTE              PIC  X(070).
