      *****************************************************************
      * NOME DA INC - I#GFCT6P                                        *
      * DESCRICAO   - TARIFAS - REMESSA PARA APROVACAO                *
      *               COMUNICACAO WEB X IMS - ENTRADA/ SAIDA          *
      * TAMANHO     - 415                                             *
      * DATA        - MAR.2005                                        *
      * RESPONSAVEL - RENATO   - PROCWORK           4195-6693         *
      *****************************************************************
      *
       01  GFCT6P-SAIDA.
           03 GFCT6P-LL                            PIC S9(004) COMP.
           03 GFCT6P-ZZ                            PIC S9(004) COMP.
           03 GFCT6P-TRANSACAO                     PIC  X(009).
           03 GFCT6P-FUNCAO                        PIC  X(008).
           03 GFCT6P-FUNC-BDSCO                    PIC  9(007).
           03 GFCT6P-TERMINAL                      PIC  X(008).
           03 GFCT6P-ERRO                          PIC  9(001).
           03 GFCT6P-COD-MSG-ERRO                  PIC  9(007).
           03 GFCT6P-COD-SQL-ERRO                  PIC  9(003).
           03 GFCT6P-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCT6P-FIM                           PIC  X(001).
           03 GFCT6P-DADOS.
              05 GFCT6P-CODIGO-TARIFA              PIC  9(005).
      *          CODIGO IDENTIFICADOR DA TARIFA
              05 GFCT6P-HIDTFD                     PIC  X(026).
      *          TIMESTAMP
              05 GFCT6P-CSEQ                       PIC  9(002).
      *          SEQUENCIA
              05 GFCT6P-COMENTARIO                 PIC  X(255).
      *          COMENTARIO PARA APROVACAO DA TARIFA
