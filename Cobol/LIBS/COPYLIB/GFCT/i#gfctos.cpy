      *****************************************************************
      * NOME DA INC - I#GFCTOS                                        *
      * DESCRICAO   - COMBO DE SEGMENTO COM FLEXIBILIZACAO            *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 01.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOS-SAIDA.
           03  GFCTOS-LL                            PIC S9(004) COMP.
           03  GFCTOS-ZZ                            PIC S9(004) COMP.
           03  GFCTOS-TRANSACAO                     PIC  X(009).
           03  GFCTOS-FUNCAO                        PIC  X(008).
           03  GFCTOS-FILTRO                        PIC  9(003).
           03  GFCTOS-QTDE-OCOR                     PIC  9(007).
           03  GFCTOS-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTOS-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOS-PONTEIRO.
               05  GFCTOS-PONTEIRO-SEGTO            PIC  9(003).
           03  GFCTOS-ERRO                          PIC  9(001).
           03  GFCTOS-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOS-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOS-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOS-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(044).
           03  GFCTOS-DADOS.
               05  FILLER                           OCCURS 25.
                   07  GFCTOS-COD-SEGTO             PIC  9(003).
                   07  GFCTOS-DESC-SEGTO            PIC  X(030).
