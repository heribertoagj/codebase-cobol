      *****************************************************************
      * NOME DA INC - I#GFCT1X                                        *
      * DESCRICAO   - OBTER DESCRICAO DE DEPENDENCIA                  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCT1X-LL   - 680                                             *
      * DATA        - 20.11.2004                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT1X-SAIDA.
           03  GFCT1X-LL                            PIC S9(004) COMP.
           03  GFCT1X-ZZ                            PIC S9(004) COMP.
           03  GFCT1X-TRANSACAO                     PIC  X(009).
           03  GFCT1X-FUNCAO                        PIC  X(008).
           03  GFCT1X-QTDE-OCOR                     PIC  9(003).
           03  GFCT1X-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT1X-FUNC-BDSCO                    PIC  X(007).
           03  GFCT1X-ERRO                          PIC  9(001).
      *        0 - OK
           03  GFCT1X-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT1X-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT1X-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT1X-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(157).
           03  GFCT1X-DADOS.
               05  FILLER                           OCCURS 5.
                   07  GFCT1X-COD-DEPDC             PIC  9(005).
                   07  GFCT1X-ERRO-DEPDC            PIC  9(001).
      *                0 - OK
      *                1 - ERRO
                   07  GFCT1X-DESC-DEPDC            PIC  X(050).
                   07  GFCT1X-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  GFCT1X-COD-SQL-ERRO-GEN      PIC  9(003).
                   07  GFCT1X-DESC-MSG-ERRO-GEN     PIC  X(079).
