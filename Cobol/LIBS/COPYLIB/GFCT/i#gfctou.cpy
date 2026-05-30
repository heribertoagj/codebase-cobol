      *****************************************************************
      * NOME DA INC - I#GFCTOU                                        *
      * DESCRICAO   - COMBO DE UF COM FLEXIBILIZACAO                  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOU-SAIDA.
           03  GFCTOU-LL                            PIC S9(004) COMP.
           03  GFCTOU-ZZ                            PIC S9(004) COMP.
           03  GFCTOU-TRANSACAO                     PIC  X(009).
           03  GFCTOU-FUNCAO                        PIC  X(008).
           03  GFCTOU-FILTRO                        PIC  X(002).
           03  GFCTOU-QTDE-OCOR                     PIC  9(007).
           03  GFCTOU-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTOU-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOU-PONTEIRO.
               05  GFCTOU-PONTEIRO-UF               PIC  X(002).
           03  GFCTOU-ERRO                          PIC  9(001).
           03  GFCTOU-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOU-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOU-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOU-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(071).
           03  GFCTOU-DADOS.
               05  FILLER                           OCCURS 25.
                   07  GFCTOU-COD-UF                PIC  X(002).
                   07  GFCTOU-DESC-UF               PIC  X(030).
