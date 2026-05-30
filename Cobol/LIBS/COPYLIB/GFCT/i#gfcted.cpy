      *****************************************************************
      * NOME DA INC - I#GFCTED                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO DE HISTORICO DE TARIFAS                   *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - JULHO/2005                                      *
      * RESPONSAVEL - RENATO TAMANAHA - PROCWORK - TEL 41524352       *
      *****************************************************************
      *
       01  GFCTED-SAIDA.
           03  GFCTED-LL                            PIC S9(004) COMP.
           03  GFCTED-ZZ                            PIC S9(004) COMP.
           03  GFCTED-TRANSACAO                     PIC  X(009).
           03  GFCTED-FUNCAO                        PIC  X(008).
           03  GFCTED-FILTRO.
               05  GFCTED-CSERVC-TARIF              PIC  9(005).
           03  GFCTED-QTDE-OCOR                     PIC  9(003).
           03  GFCTED-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTED-FUNC-BDSCO                    PIC  X(007).
           03  GFCTED-TERMINAL                      PIC  X(008).
           03  GFCTED-PONTEIRO                      PIC  9(005).
           03  GFCTED-ERRO                          PIC  9(001).
           03  GFCTED-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTED-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTED-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTED-FIM                           PIC  X(001).
           03  GFCTED-PONTEIRO-SITUACAO             PIC  9(002).
           03  FILLER                               PIC  X(046).
           03  GFCTED-DADOS.
               05  FILLER                           OCCURS 6.
                   07  GFCTED-COD-TARIFA            PIC  9(005).
                   07  GFCTED-DESC-TARIFA           PIC  X(100).
                   07  GFCTED-HORA-MANUTENCAO       PIC  X(026).
                   07  GFCTED-CSEQ-MANUT-SERVC      PIC  9(002).
                   07  GFCTED-CSIT-LIBRC-TARIF      PIC  9(002).
                   07  GFCTED-CTPO-SERVC-TARIF      PIC  9(001).
