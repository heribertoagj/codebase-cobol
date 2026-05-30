      ******************************************************************
      * NOME DA INC - I#GFCTO8                                         *
      * DESCRICAO   - COMBO DE PACOTE - TABELA GFCTB048                *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTO8-SAIDA.
           03  GFCTO8-LL                            PIC S9(004) COMP.
           03  GFCTO8-ZZ                            PIC S9(004) COMP.
           03  GFCTO8-TRANSACAO                     PIC  X(009).
           03  GFCTO8-FUNCAO                        PIC  X(008).
           03  GFCTO8-FILTRO                        PIC  9(005).
           03  GFCTO8-COD-DEPDC                     PIC  9(005).
           03  GFCTO8-CCTA-CLI                      PIC  9(013).
           03  GFCTO8-QTDE-OCOR                     PIC  9(003).
           03  GFCTO8-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTO8-FUNC-BDSCO                    PIC  X(007).
           03  GFCTO8-PONTEIRO.
               05  GFCTO8-PONTEIRO-PACOTE           PIC  9(005).
               05  GFCTO8-PONTEIRO-DEPDC            PIC  9(005).
               05  GFCTO8-PONTEIRO-CCTA             PIC  9(013).
           03  GFCTO8-ERRO                          PIC  9(001).
           03  GFCTO8-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTO8-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTO8-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTO8-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(101).
           03  GFCTO8-DADOS.
               05  FILLER                           OCCURS 7.
                   07  GFCTO8-COD-PACOTE            PIC  9(005).
                   07  GFCTO8-DESC-PACOTE           PIC  X(100).
