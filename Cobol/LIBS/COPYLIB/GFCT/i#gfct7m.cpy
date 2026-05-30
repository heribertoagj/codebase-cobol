      *****************************************************************
      * NOME DA INC - I#GFCT7M                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO DE HISTORICO DE TARIFAS                   *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 23.03.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT7M-SAIDA.
           03  GFCT7M-LL                            PIC S9(004) COMP.
           03  GFCT7M-ZZ                            PIC S9(004) COMP.
           03  GFCT7M-TRANSACAO                     PIC  X(009).
           03  GFCT7M-FUNCAO                        PIC  X(008).
           03  GFCT7M-FILTRO.
               05  GFCT7M-CSERVC-TARIF              PIC  9(005).
           03  GFCT7M-QTDE-OCOR                     PIC  9(003).
           03  GFCT7M-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT7M-FUNC-BDSCO                    PIC  X(007).
           03  GFCT7M-TERMINAL                      PIC  X(008).
           03  GFCT7M-PONTEIRO                      PIC  9(005).
           03  GFCT7M-ERRO                          PIC  9(001).
           03  GFCT7M-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT7M-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT7M-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT7M-FIM                           PIC  X(001).
           03  GFCT7M-PONTEIRO-SITUACAO             PIC  9(002).
           03  GFCT7M-PONTEIRO-SEQ                  PIC  9(002).
           03  FILLER                               PIC  X(044).
           03  GFCT7M-DADOS.
               05  FILLER                           OCCURS 6.
                   07  GFCT7M-COD-TARIFA            PIC  9(005).
                   07  GFCT7M-DESC-TARIFA           PIC  X(100).
                   07  GFCT7M-HORA-MANUTENCAO       PIC  X(026).
                   07  GFCT7M-CSEQ-MANUT-SERVC      PIC  9(002).
                   07  GFCT7M-CSIT-LIBRC-TARIF      PIC  9(002).
                   07  GFCT7M-CTPO-SERVC-TARIF      PIC  9(001).
